# ==============================================================================
# COMPREHENSIVE BIOMASS ANALYSIS WITH AREA WEIGHTING
# ==============================================================================
# Purpose: Enhanced biomass time series analysis with proper area weighting,
#          spatial analysis, and comprehensive plotting inspired by ZooMSS_CC
# ==============================================================================

library(tidyverse)
library(raster)
library(patchwork)
library(scales)
library(viridis)
library(RColorBrewer)

# Set directories
output_dir <- "Output/Biomass_projections/"
figure_dir <- "Figures/Biomass_Enhanced/"

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== COMPREHENSIVE BIOMASS ANALYSIS WITH AREA WEIGHTING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: SETUP AREA WEIGHTING
# ==============================================================================

cat("STEP 1: Setting up area weighting...\n")

# Create global raster for area calculation (1-degree resolution to match data)
global_raster <- raster(nrows = 180, ncols = 360, 
                       xmn = -180, xmx = 180, ymn = -90, ymx = 90)

# Calculate cell areas in km²
cell_areas <- raster::area(global_raster) 

# Convert to dataframe with coordinates
area_df <- as.data.frame(global_raster, xy = TRUE, na.rm = FALSE) %>%
  dplyr::select(-layer) %>%
  bind_cols(as.data.frame(cell_areas)) %>%
  rename(area_km2 = layer, Lon = x, Lat = y)

cat("Area weighting setup complete. Grid cells:", nrow(area_df), "\n")

# ==============================================================================
# STEP 2: DEFINE SPECIES GROUPS AND FILE PROCESSING
# ==============================================================================

# Define species groups for analysis
zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                        "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")

fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")

all_species <- c(zooplankton_species, fish_species)

# Get all biomass files (now including new UKESM files)
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)

cat("STEP 2: Found", length(biomass_files), "biomass projection files\n")

# ==============================================================================
# STEP 3: MEMORY-EFFICIENT AREA-WEIGHTED PROCESSING FUNCTION
# ==============================================================================

process_biomass_with_weighting <- function(filepath, spatial_sample_fraction = 0.1) {
  
  filename <- basename(filepath)
  cat("Processing:", filename, "\n")
  
  tryCatch({
    # Load data
    data <- readRDS(filepath)
    
    cat("  Original data size:", nrow(data), "rows\n")
    
    # Sample spatially if needed
    if(spatial_sample_fraction < 1.0) {
      n_sample <- round(nrow(data) * spatial_sample_fraction)
      cat("  Sampling", n_sample, "spatial points\n")
      data <- data %>% slice_sample(n = n_sample)
    }
    
    # Extract metadata from filename
    model <- str_extract(filename, "(?<=withZooMSS_)[^_]+")
    scenario <- str_extract(filename, "(?<=_)[^_]+(?=_Control)")
    
    # Add area weighting
    data_with_area <- data %>%
      left_join(area_df, by = c("Lon", "Lat")) %>%
      filter(!is.na(area_km2))  # Remove cells without area data
    
    # Calculate area-weighted annual means
    annual_means <- data_with_area %>%
      group_by(Year) %>%
      summarise(
        # Area-weighted means for each species
        across(all_of(all_species), 
               ~sum(.x * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(.x)], na.rm = TRUE)),
        
        # Also calculate simple means for comparison
        across(all_of(all_species), 
               ~mean(.x, na.rm = TRUE), 
               .names = "{.col}_simple"),
        
        # Calculate total ocean area sampled
        total_area_km2 = sum(area_km2, na.rm = TRUE),
        n_cells = n(),
        .groups = 'drop'
      ) %>%
      # Add metadata
      mutate(
        filename = filename,
        model = model,
        scenario = scenario,
        
        # Calculate aggregate groups
        Zooplankton_Total = rowSums(dplyr::select(., all_of(zooplankton_species)), na.rm = TRUE),
        Fish_Total = rowSums(dplyr::select(., all_of(fish_species)), na.rm = TRUE),
        TCB = Zooplankton_Total + Fish_Total
      ) %>%
      # Convert to long format for detailed analysis
      pivot_longer(cols = all_of(all_species), 
                   names_to = "species", 
                   values_to = "biomass_weighted")
    
    cat("  Extracted", nrow(annual_means), "time series points\n")
    cat("  Year range:", min(annual_means$Year), "to", max(annual_means$Year), "\n")
    cat("  Total area sampled:", round(annual_means$total_area_km2[1]/1e6, 1), "million km²\n")
    
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
# STEP 4: PROCESS ALL FILES WITH AREA WEIGHTING
# ==============================================================================

cat("\nSTEP 4: Processing all files with area weighting...\n")

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

# Process files with appropriate sampling
all_weighted_timeseries <- list()

for(i in 1:nrow(file_info)) {
  
  filepath <- file_info$filepath[i]
  category <- file_info$size_category[i]
  
  # Adjust sampling based on file size
  sample_fraction <- case_when(
    category == "small" ~ 0.15,
    category == "medium" ~ 0.08,
    category == "large" ~ 0.02
  )
  
  cat(sprintf("\n[%d/%d] ", i, nrow(file_info)))
  
  weighted_timeseries <- process_biomass_with_weighting(filepath, sample_fraction)
  
  if(!is.null(weighted_timeseries)) {
    all_weighted_timeseries[[i]] <- weighted_timeseries
  }
  
  # Save intermediate results every 3 files
  if(i %% 3 == 0) {
    if(length(all_weighted_timeseries) > 0) {
      intermediate_data <- bind_rows(all_weighted_timeseries)
      saveRDS(intermediate_data, paste0("Output/intermediate_weighted_timeseries_", i, "_files.rds"))
      cat("  Intermediate results saved\n")
    }
  }
  
  gc()
}

# Combine all results
if(length(all_weighted_timeseries) > 0) {
  combined_weighted_timeseries <- bind_rows(all_weighted_timeseries)
  saveRDS(combined_weighted_timeseries, "Output/combined_weighted_biomass_timeseries.rds")
  cat("\nCombined weighted time series saved\n")
} else {
  stop("No valid time series data was processed!")
}

# ==============================================================================
# STEP 5: CREATE AGGREGATE TIME SERIES FOR PLOTTING
# ==============================================================================

cat("\nSTEP 5: Creating aggregate time series...\n")

# Create summary data for aggregate groups
aggregate_timeseries <- combined_weighted_timeseries %>%
  group_by(Year, scenario, model) %>%
  summarise(
    Zooplankton_Total = first(Zooplankton_Total),
    Fish_Total = first(Fish_Total),
    TCB = first(TCB),
    total_area_km2 = first(total_area_km2),
    .groups = 'drop'
  ) %>%
  # Calculate percentage changes relative to historical baseline
  group_by(model, scenario) %>%
  arrange(Year) %>%
  mutate(
    # Use first 20 years as baseline for each scenario
    year_min = min(Year),
    year_max = max(Year),
    Zoop_baseline = mean(Zooplankton_Total[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    Fish_baseline = mean(Fish_Total[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    TCB_baseline = mean(TCB[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    
    Zoop_Change = (Zooplankton_Total - Zoop_baseline) / Zoop_baseline * 100,
    Fish_Change = (Fish_Total - Fish_baseline) / Fish_baseline * 100,
    TCB_Change = (TCB - TCB_baseline) / TCB_baseline * 100
  ) %>%
  ungroup()

cat("Aggregate time series created with", nrow(aggregate_timeseries), "data points\n")

# ==============================================================================
# STEP 6: ENHANCED PLOTTING
# ==============================================================================

cat("\nSTEP 6: Creating enhanced plots...\n")

# Define color schemes
scenario_colors <- c(
  "historical" = "#2E2E2E",
  "picontrol" = "#808080", 
  "ssp126" = "#1f77b4",
  "ssp534-over" = "#ff7f0e",
  "ssp585" = "#d62728"
)

model_shapes <- c("cesm2-waccm" = 16, "ipsl-cm6a-lr" = 17, "ukesm1-0-ll" = 18)

# Plot 1: Total Biomass Time Series by Group
p1 <- aggregate_timeseries %>%
  filter(scenario != "picontrol") %>%
  dplyr::select(Year, scenario, model, Zooplankton_Total, Fish_Total) %>%
  pivot_longer(cols = c(Zooplankton_Total, Fish_Total), 
               names_to = "Group", values_to = "Biomass") %>%
  mutate(Group = str_replace(Group, "_Total", "")) %>%
  ggplot(aes(x = Year, y = Biomass, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.8) +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Total Biomass Projections: Zooplankton vs Fish",
    subtitle = "Area-weighted global means",
    x = "Year",
    y = "Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "total_biomass_zooplankton_vs_fish.png"), 
       p1, width = 14, height = 8, dpi = 300)

# Plot 2: Percentage Changes Over Time
p2 <- aggregate_timeseries %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")) %>%
  dplyr::select(Year, scenario, model, Zoop_Change, Fish_Change, TCB_Change) %>%
  pivot_longer(cols = c(Zoop_Change, Fish_Change, TCB_Change), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = case_when(
    Group == "Zoop_Change" ~ "Zooplankton",
    Group == "Fish_Change" ~ "Fish", 
    Group == "TCB_Change" ~ "Total Consumer Biomass"
  )) %>%
  ggplot(aes(x = Year, y = Change, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Biomass Changes Relative to Early Period Baseline",
    subtitle = "Percentage change from first 20 years of each scenario",
    x = "Year",
    y = "Change (%)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "biomass_percentage_changes.png"), 
       p2, width = 14, height = 8, dpi = 300)

# Plot 3: Individual Species Time Series (Top 6 species)
top_species <- combined_weighted_timeseries %>%
  group_by(species) %>%
  summarise(mean_biomass = mean(biomass_weighted, na.rm = TRUE), .groups = 'drop') %>%
  top_n(6, mean_biomass) %>%
  pull(species)

p3 <- combined_weighted_timeseries %>%
  filter(species %in% top_species,
         scenario %in% c("historical", "ssp126", "ssp585")) %>%
  ggplot(aes(x = Year, y = biomass_weighted, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.6) +
  facet_wrap(~species, scales = "free_y", ncol = 3) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Top 6 Species Biomass Projections",
    subtitle = "Area-weighted global means for most abundant species",
    x = "Year",
    y = "Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "top_species_biomass_projections.png"), 
       p3, width = 16, height = 10, dpi = 300)

# Plot 4: Long-term Trajectory Comparison (focus on 2100-2300)
p4 <- aggregate_timeseries %>%
  filter(Year >= 2100,
         scenario %in% c("ssp126", "ssp585", "ssp534-over")) %>%
  ggplot(aes(x = Year, y = TCB, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 1.0) +
  facet_wrap(~model) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Long-term Marine Biomass Trajectories (2100-2300)",
    subtitle = "Total Consumer Biomass by scenario and model", 
    x = "Year",
    y = "Total Consumer Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "longterm_biomass_trajectories_2100_2300.png"), 
       p4, width = 14, height = 8, dpi = 300)

# Save summary statistics
summary_stats <- aggregate_timeseries %>%
  group_by(model, scenario) %>%
  summarise(
    n_years = n_distinct(Year),
    mean_zoop_biomass = mean(Zooplankton_Total, na.rm = TRUE),
    mean_fish_biomass = mean(Fish_Total, na.rm = TRUE),
    mean_total_biomass = mean(TCB, na.rm = TRUE),
    final_zoop_change = last(Zoop_Change[!is.na(Zoop_Change)]),
    final_fish_change = last(Fish_Change[!is.na(Fish_Change)]),
    .groups = 'drop'
  )

write_csv(summary_stats, paste0(figure_dir, "enhanced_biomass_summary_statistics.csv"))

cat("Enhanced plotting complete!\n")
cat("Plots saved to:", figure_dir, "\n")
cat("Summary stats:", nrow(summary_stats), "scenario-model combinations\n")

print(summary_stats)

cat("\n=== ENHANCED ANALYSIS COMPLETE ===\n")
