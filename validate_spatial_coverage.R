# ==============================================================================
# SPATIAL COVERAGE VALIDATION
# ==============================================================================
# Purpose: Validate spatial coverage of corrected area-weighted data
# ==============================================================================

library(tidyverse)
library(raster)
library(viridis)
library(maps)
library(ggplot2)

cat("=== SPATIAL COVERAGE VALIDATION ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create output directory
figure_dir <- "Figures/Spatial_Coverage_Validation/"
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

# ==============================================================================
# STEP 1: LOAD ORIGINAL SPATIAL DATA FOR VALIDATION
# ==============================================================================

cat("STEP 1: Loading original spatial biomass data for validation...\n")

# Get list of original biomass projection files
biomass_files <- list.files("Output/Biomass_projections/", pattern = "*.rds", full.names = TRUE)
cat("Found", length(biomass_files), "biomass projection files\n")

# Function to extract spatial info from filename
extract_file_info <- function(filepath) {
  filename <- basename(filepath)
  # Extract model and scenario from filename
  parts <- strsplit(filename, "_")[[1]]
  model <- parts[5]  # e.g., "cesm2-waccm"
  scenario <- parts[6]  # e.g., "historical"
  return(list(model = model, scenario = scenario, filepath = filepath))
}

# Get file information
file_info <- map_dfr(biomass_files, extract_file_info)

cat("Files by model and scenario:\n")
print(table(file_info$model, file_info$scenario))

# ==============================================================================
# STEP 2: EXAMINE SPATIAL COVERAGE BY SCENARIO
# ==============================================================================

cat("\nSTEP 2: Examining spatial coverage by scenario...\n")

# Get unique spatial points for each scenario
spatial_coverage <- corrected_data %>%
  group_by(model, scenario) %>%
  summarise(
    n_unique_locations = n_distinct(paste(Lon, Lat)),
    lon_range = paste(round(range(Lon), 1), collapse = " to "),
    lat_range = paste(round(range(Lat), 1), collapse = " to "),
    n_years = n_distinct(Year),
    total_points = n(),
    .groups = 'drop'
  ) %>%
  arrange(model, scenario)

cat("Spatial coverage summary:\n")
print(spatial_coverage)

# ==============================================================================
# STEP 3: CREATE SPATIAL COVERAGE MAPS
# ==============================================================================

cat("\nSTEP 3: Creating spatial coverage maps...\n")

# Get world map data for context
world_map <- map_data("world")

# Function to create spatial coverage map for a scenario
create_coverage_map <- function(model_name, scenario_name, year_sample = NULL) {
  
  cat("Creating map for", model_name, scenario_name, "\n")
  
  # Filter data for specific model/scenario
  scenario_data <- corrected_data %>%
    filter(model == model_name, scenario == scenario_name)
  
  if(nrow(scenario_data) == 0) {
    cat("  No data found for", model_name, scenario_name, "\n")
    return(NULL)
  }
  
  # Sample a specific year if requested, otherwise use all years
  if(!is.null(year_sample)) {
    scenario_data <- scenario_data %>% filter(Year == year_sample)
  }
  
  # Get unique spatial points
  spatial_points <- scenario_data %>%
    dplyr::select(Lon, Lat) %>%
    distinct()
  
  cat("  Plotting", nrow(spatial_points), "unique locations\n")
  
  # Create map
  p <- ggplot() +
    # Add world map background
    geom_polygon(data = world_map, 
                 aes(x = long, y = lat, group = group), 
                 fill = "grey90", color = "white", size = 0.1) +
    # Add data points
    geom_point(data = spatial_points, 
               aes(x = Lon, y = Lat), 
               color = "red", size = 0.1, alpha = 0.6) +
    # Map styling
    coord_quickmap() +
    scale_x_continuous(breaks = seq(-180, 180, 60)) +
    scale_y_continuous(breaks = seq(-90, 90, 30)) +
    labs(
      title = paste("Spatial Coverage:", model_name, scenario_name),
      subtitle = paste("Total locations:", nrow(spatial_points)),
      x = "Longitude", 
      y = "Latitude"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text = element_text(size = 8)
    )
  
  return(p)
}

# Create maps for each model-scenario combination
models <- unique(corrected_data$model)
scenarios <- c("historical", "ssp585")  # Focus on key scenarios

coverage_maps <- list()

for(model in models) {
  for(scenario in scenarios) {
    map_key <- paste(model, scenario, sep = "_")
    coverage_maps[[map_key]] <- create_coverage_map(model, scenario)
  }
}

# Remove NULL maps
coverage_maps <- coverage_maps[!sapply(coverage_maps, is.null)]

# Save individual maps
for(map_name in names(coverage_maps)) {
  filename <- paste0(figure_dir, "spatial_coverage_", map_name, ".png")
  ggsave(filename, coverage_maps[[map_name]], width = 12, height = 8, dpi = 300)
}

# ==============================================================================
# STEP 4: COMPARE SPATIAL COVERAGE ACROSS SCENARIOS
# ==============================================================================

cat("\nSTEP 4: Comparing spatial coverage across scenarios...\n")

# Create comparison data
coverage_comparison <- corrected_data %>%
  group_by(model, scenario, Lon, Lat) %>%
  summarise(n_years = n_distinct(Year), .groups = 'drop') %>%
  group_by(model, scenario) %>%
  summarise(
    total_locations = n(),
    avg_years_per_location = mean(n_years),
    .groups = 'drop'
  )

cat("Coverage comparison:\n")
print(coverage_comparison)

# Create a comparison plot
p_comparison <- coverage_comparison %>%
  ggplot(aes(x = scenario, y = total_locations, fill = model)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(
    title = "Spatial Coverage Comparison Across Scenarios",
    subtitle = paste("Data source:", data_source),
    x = "Scenario",
    y = "Number of Unique Locations",
    fill = "Model"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(paste0(figure_dir, "spatial_coverage_comparison.png"), 
       p_comparison, width = 12, height = 8, dpi = 300)

# ==============================================================================
# STEP 5: CHECK FOR COMPLETE OCEAN COVERAGE
# ==============================================================================

cat("\nSTEP 5: Checking for complete ocean coverage...\n")

# Load one complete biomass file to see the full spatial extent
sample_files <- list.files("Output/Biomass_projections/", pattern = "*.rds", full.names = TRUE)
if(length(sample_files) > 0) {
  
  cat("Loading sample file to check full ocean grid...\n")
  sample_full_data <- readRDS(sample_files[1])
  
  # Get unique spatial points from full data
  full_ocean_grid <- sample_full_data %>%
    dplyr::select(Lon, Lat) %>%
    distinct() %>%
    arrange(Lat, Lon)
  
  cat("Full ocean grid points:", nrow(full_ocean_grid), "\n")
  
  # Get spatial points from processed data (one scenario)
  processed_grid <- corrected_data %>%
    filter(model == models[1], scenario == "historical") %>%
    dplyr::select(Lon, Lat) %>%
    distinct() %>%
    arrange(Lat, Lon)
  
  cat("Processed grid points (historical):", nrow(processed_grid), "\n")
  
  # Calculate coverage percentage
  coverage_pct <- nrow(processed_grid) / nrow(full_ocean_grid) * 100
  cat("Spatial coverage percentage:", round(coverage_pct, 1), "%\n")
  
  # Create coverage diagnostic map
  full_ocean_grid$data_type <- "Full Ocean Grid"
  processed_grid$data_type <- "Processed Data"
  
  combined_grid <- bind_rows(full_ocean_grid, processed_grid)
  
  p_diagnostic <- ggplot() +
    geom_polygon(data = world_map, 
                 aes(x = long, y = lat, group = group), 
                 fill = "grey90", color = "white", size = 0.1) +
    geom_point(data = combined_grid, 
               aes(x = Lon, y = Lat, color = data_type), 
               size = 0.1, alpha = 0.7) +
    scale_color_manual(values = c("Full Ocean Grid" = "blue", "Processed Data" = "red")) +
    coord_quickmap() +
    labs(
      title = "Ocean Grid Coverage Diagnostic",
      subtitle = paste("Coverage:", round(coverage_pct, 1), "% of full ocean grid"),
      x = "Longitude", y = "Latitude",
      color = "Data Type"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  
  ggsave(paste0(figure_dir, "ocean_grid_coverage_diagnostic.png"), 
         p_diagnostic, width = 14, height = 10, dpi = 300)
  
  # Clean up
  rm(sample_full_data, full_ocean_grid)
  gc()
}

# ==============================================================================
# STEP 6: SUMMARY REPORT
# ==============================================================================

cat("\nSTEP 6: Summary report...\n")

summary_report <- list(
  data_source = data_source,
  total_data_points = nrow(corrected_data),
  models = unique(corrected_data$model),
  scenarios = unique(corrected_data$scenario),
  spatial_coverage = spatial_coverage,
  coverage_comparison = coverage_comparison
)

# Save summary report
saveRDS(summary_report, paste0("Output/spatial_coverage_validation_report.rds"))
write_csv(spatial_coverage, paste0(figure_dir, "spatial_coverage_summary.csv"))
write_csv(coverage_comparison, paste0(figure_dir, "coverage_comparison.csv"))

cat("\nSPATIAL COVERAGE VALIDATION COMPLETE!\n")
cat("Maps and reports saved to:", figure_dir, "\n")
cat("\nKey findings:\n")
cat("- Data source:", data_source, "\n")
cat("- Models:", paste(unique(corrected_data$model), collapse = ", "), "\n")
cat("- Scenarios:", paste(unique(corrected_data$scenario), collapse = ", "), "\n")

if(exists("coverage_pct")) {
  cat("- Spatial coverage: ~", round(coverage_pct, 1), "% of full ocean grid\n")
}

cat("\n=== VALIDATION COMPLETE ===\n")
