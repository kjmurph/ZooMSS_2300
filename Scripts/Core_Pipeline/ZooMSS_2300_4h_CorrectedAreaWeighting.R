# ==============================================================================
# CORRECTED AREA-WEIGHTED BIOMASS ANALYSIS
# ==============================================================================
# Purpose: Fixed area weighting that maintains spatial consistency
# Version: 1.1.0 - Enhanced with logging and configuration
# ==============================================================================

library(tidyverse)
library(raster)
library(patchwork)
library(scales)
library(viridis)
library(RColorBrewer)
library(config)
library(logger)

# Load configuration and setup logging
config <- config::get()
source("R/logging_utils.R")

# Setup logging for this script
log_file <- setup_logging(
  log_dir = config$paths$logs_dir,
  log_level = config$logging$level,
  script_name = "CorrectedAreaWeighting"
)

# Set directories from config
output_dir <- config$paths$biomass_projections_dir
figure_dir <- config$paths$enhanced_figures_dir

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
  log_info("Created figure directory: {figure_dir}")
}

log_info("=== CORRECTED AREA-WEIGHTED BIOMASS ANALYSIS ===")
analysis_start_time <- Sys.time()

# Set random seed for reproducibility
set.seed(config$analysis$random_seed)
log_info("Random seed set to: {config$analysis$random_seed}")

# ==============================================================================
# STEP 1: CREATE CONSISTENT OCEAN MASK AND AREA WEIGHTING
# ==============================================================================

step1_start <- log_checkpoint("Creating consistent ocean mask and area weighting")

# First, determine the actual grid structure from one file
sample_file <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)[1]
log_file_processing(sample_file, processing_stage = "Loading reference grid")

sample_data <- readRDS(sample_file)

# Get unique lat-lon combinations from the data
ocean_grid <- sample_data %>%
  dplyr::select(Lon, Lat) %>%
  distinct() %>%
  arrange(Lat, Lon)

log_info("Ocean grid points found: {nrow(ocean_grid)}")
log_info("Latitude range: {paste(range(ocean_grid$Lat), collapse = ' to ')}")
log_info("Longitude range: {paste(range(ocean_grid$Lon), collapse = ' to ')}")

# Validate spatial extent
spatial_valid <- log_validation(
  "Spatial extent validation",
  abs(min(ocean_grid$Lon) + 179.5) < 0.1 && abs(max(ocean_grid$Lon) - 179.5) < 0.1,
  "Longitude range matches expected global coverage"
)

# Calculate areas for each grid cell using proper method
# For 1-degree grid cells, area varies with latitude
calculate_grid_area <- function(lat, lon, resolution = 1) {
  # Earth radius in km
  R <- 6371
  
  # Convert resolution to radians
  res_rad <- resolution * pi / 180
  
  # Calculate area of grid cell at given latitude
  lat_rad <- lat * pi / 180
  
  # Area = R^2 * delta_lon * (sin(lat + dlat/2) - sin(lat - dlat/2))
  lat_min <- lat_rad - res_rad/2
  lat_max <- lat_rad + res_rad/2
  
  area_km2 <- R^2 * res_rad * (sin(lat_max) - sin(lat_min))
  
  return(area_km2)
}

# Calculate area for each ocean grid cell
ocean_grid_with_areas <- ocean_grid %>%
  mutate(area_km2 = calculate_grid_area(Lat, Lon))

total_ocean_area <- sum(ocean_grid_with_areas$area_km2)
cat("Total ocean area:", round(total_ocean_area / 1e6, 1), "million km²\n")

# Clean up sample data
rm(sample_data)
gc()

# ==============================================================================
# STEP 2: DEFINE SPECIES GROUPS AND PROCESSING FUNCTION
# ==============================================================================

# Define species groups
zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                        "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")
all_species <- c(zooplankton_species, fish_species)

# Get all biomass files
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)
cat("STEP 2: Found", length(biomass_files), "biomass projection files\n")

# ==============================================================================
# STEP 3: CORRECTED PROCESSING FUNCTION
# ==============================================================================

process_biomass_corrected <- function(filepath, spatial_sample_fraction = 0.1) {
  
  filename <- basename(filepath)
  cat("Processing:", filename, "\n")
  
  tryCatch({
    # Load data
    data <- readRDS(filepath)
    cat("  Original data size:", nrow(data), "rows\n")
    
    # Sample spatially if needed for memory management
    if(spatial_sample_fraction < 1.0) {
      n_sample <- round(nrow(data) * spatial_sample_fraction)
      cat("  Sampling", n_sample, "spatial points for memory management\n")
      data <- data %>% slice_sample(n = n_sample)
    }
    
    # Extract metadata from filename
    model <- str_extract(filename, "(?<=withZooMSS_)[^_]+")
    scenario <- str_extract(filename, "(?<=_)[^_]+(?=_Control)")
    
    # Join with ocean grid areas (this maintains consistent area weighting)
    data_with_area <- data %>%
      inner_join(ocean_grid_with_areas, by = c("Lon", "Lat"))
    
    n_matched <- nrow(data_with_area)
    cat("  Matched", n_matched, "points with ocean grid\n")
    
    # Calculate area-weighted annual means using consistent methodology
    annual_means <- data_with_area %>%
      group_by(Year) %>%
      summarise(
        # Proper area-weighted means: sum(biomass * area) / sum(area)
        across(all_of(all_species), 
               ~sum(.x * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(.x)], na.rm = TRUE),
               .names = "{.col}_weighted"),
        
        # Simple means for comparison
        across(all_of(all_species), 
               ~mean(.x, na.rm = TRUE), 
               .names = "{.col}_simple"),
        
        # Calculate effective area (area of cells with data)
        effective_area_km2 = sum(area_km2, na.rm = TRUE),
        total_ocean_area_km2 = !!total_ocean_area,  # Consistent reference area
        n_cells = n(),
        .groups = 'drop'
      ) %>%
      # Add metadata
      mutate(
        filename = filename,
        model = model,
        scenario = scenario,
        
        # Calculate aggregate groups using weighted values
        Zooplankton_Total = rowSums(dplyr::select(., paste0(zooplankton_species, "_weighted")), na.rm = TRUE),
        Fish_Total = rowSums(dplyr::select(., paste0(fish_species, "_weighted")), na.rm = TRUE),
        TCB = Zooplankton_Total + Fish_Total
      ) %>%
      # Pivot to long format for detailed analysis
      pivot_longer(cols = ends_with("_weighted"), 
                   names_to = "species", 
                   values_to = "biomass_weighted") %>%
      mutate(species = str_remove(species, "_weighted"))
    
    cat("  Extracted", nrow(annual_means), "time series points\n")
    cat("  Year range:", min(annual_means$Year), "to", max(annual_means$Year), "\n")
    cat("  Effective area:", round(annual_means$effective_area_km2[1]/1e6, 1), "million km²\n")
    cat("  Reference ocean area:", round(annual_means$total_ocean_area_km2[1]/1e6, 1), "million km²\n")
    
    # Clean up
    rm(data, data_with_area)
    gc()
    
    return(annual_means)
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ==============================================================================
# STEP 4: PROCESS FILES WITH CORRECTED AREA WEIGHTING
# ==============================================================================

cat("\nSTEP 4: Processing files with corrected area weighting...\n")

# Get file info for processing order
file_info <- data.frame(
  filepath = biomass_files,
  filename = basename(biomass_files),
  size_mb = sapply(biomass_files, function(f) round(file.size(f) / 1024^2, 1))
) %>%
  arrange(size_mb) %>%
  mutate(
    model = str_extract(filename, "(?<=withZooMSS_)[^_]+"),
    scenario = str_extract(filename, "(?<=_)[^_]+(?=_Control)"),
    size_category = case_when(
      size_mb < 2000 ~ "small",
      size_mb < 4000 ~ "medium", 
      size_mb >= 4000 ~ "large"
    )
  )

cat("Processing order by file size:\n")
print(file_info %>% dplyr::select(model, scenario, size_mb, size_category))

# Process files with appropriate sampling (same as before for memory management)
all_corrected_timeseries <- list()

for(i in 1:nrow(file_info)) {
  
  filepath <- file_info$filepath[i]
  category <- file_info$size_category[i]
  
  # Adjust sampling based on file size (for memory management only)
  sample_fraction <- case_when(
    category == "small" ~ 0.15,
    category == "medium" ~ 0.08,
    category == "large" ~ 0.02
  )
  
  cat(sprintf("\n[%d/%d] ", i, nrow(file_info)))
  
  corrected_timeseries <- process_biomass_corrected(filepath, sample_fraction)
  
  if(!is.null(corrected_timeseries)) {
    all_corrected_timeseries[[i]] <- corrected_timeseries
  }
  
  # Save intermediate results every 3 files
  if(i %% 3 == 0) {
    if(length(all_corrected_timeseries) > 0) {
      intermediate_data <- bind_rows(all_corrected_timeseries)
      saveRDS(intermediate_data, paste0("Output/intermediate_corrected_timeseries_", i, "_files.rds"))
      cat("  Intermediate results saved\n")
    }
  }
  
  gc()
}

# Combine all results
if(length(all_corrected_timeseries) > 0) {
  combined_corrected_timeseries <- bind_rows(all_corrected_timeseries)
  saveRDS(combined_corrected_timeseries, "Output/combined_corrected_biomass_timeseries.rds")
  cat("\nCombined corrected time series saved\n")
  cat("Total time series points:", nrow(combined_corrected_timeseries), "\n")
} else {
  stop("No valid time series data was processed!")
}

# ==============================================================================
# STEP 5: VALIDATION AND COMPARISON
# ==============================================================================

cat("\nSTEP 5: Validation of corrected area weighting...\n")

# Check area consistency across scenarios
area_check <- combined_corrected_timeseries %>%
  group_by(model, scenario) %>%
  summarise(
    n_years = n_distinct(Year),
    avg_effective_area = mean(effective_area_km2, na.rm = TRUE) / 1e6,
    reference_area = first(total_ocean_area_km2) / 1e6,
    area_fraction = avg_effective_area / (reference_area),
    .groups = 'drop'
  )

cat("Area consistency check:\n")
print(area_check)

# The effective area should now reflect the spatial sampling fraction
# but the reference area should be consistent across all scenarios

cat("\nArea weighting validation complete!\n")
cat("Key improvements:\n")
cat("- Consistent reference ocean area across all scenarios\n")
cat("- Proper area weighting based on latitude\n") 
cat("- Area calculations independent of spatial sampling\n")

# Save validation results
write_csv(area_check, paste0("Output/area_weighting_validation.csv"))

cat("\n=== CORRECTED AREA WEIGHTING COMPLETE ===\n")
