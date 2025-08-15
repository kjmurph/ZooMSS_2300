# ==============================================================================
# SPATIAL BIOMASS CHANGE ANALYSIS (2300 vs HISTORICAL)
# ==============================================================================
# Purpose: Spatial analysis of biomass changes, particularly fish biomass
#          comparing 2300 projections to historical baseline
# ==============================================================================

library(tidyverse)
library(raster)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

# Set directories
output_dir <- "Output/Biomass_projections/"
figure_dir <- "Figures/Spatial_Analysis/"

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== SPATIAL BIOMASS CHANGE ANALYSIS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: IDENTIFY MATCHING HISTORICAL AND FUTURE SCENARIOS
# ==============================================================================

cat("STEP 1: Identifying matching scenario pairs...\n")

# Get all biomass files
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)

# Extract file metadata
file_metadata <- data.frame(
  filepath = biomass_files,
  filename = basename(biomass_files)
) %>%
  mutate(
    model = str_extract(filename, "(?<=withZooMSS_)[^_]+"),
    scenario = str_extract(filename, "(?<=_)[^_]+(?=_Control)"),
    size_mb = sapply(filepath, function(f) round(file.size(f) / 1024^2, 1))
  ) %>%
  filter(!is.na(model), !is.na(scenario))

cat("Available models:", paste(unique(file_metadata$model), collapse = ", "), "\n")
cat("Available scenarios:", paste(unique(file_metadata$scenario), collapse = ", "), "\n")

# Focus on scenarios with strong contrasts
target_scenarios <- c("historical", "ssp126", "ssp585")
available_combinations <- file_metadata %>%
  filter(scenario %in% target_scenarios) %>%
  group_by(model) %>%
  summarise(scenarios = list(scenario), n_scenarios = n(), .groups = 'drop') %>%
  filter(n_scenarios >= 2)

cat("Models with multiple scenarios:\n")
print(available_combinations)

# ==============================================================================
# STEP 2: SPATIAL PROCESSING FUNCTIONS
# ==============================================================================

extract_spatial_biomass <- function(filepath, target_years = NULL, sample_fraction = 0.05) {
  
  filename <- basename(filepath)
  cat("Loading:", filename, "\n")
  
  # Extract metadata
  model <- str_extract(filename, "(?<=withZooMSS_)[^_]+")
  scenario <- str_extract(filename, "(?<=_)[^_]+(?=_Control)")
  
  # Load data
  data <- readRDS(filepath)
  cat("  Data size:", nrow(data), "rows\n")
  
  # Filter years if specified
  if(!is.null(target_years)) {
    data <- data %>% filter(Year %in% target_years)
    cat("  Filtered to years:", paste(range(data$Year), collapse = "-"), "\n")
  }
  
  # Sample spatially for memory management
  if(sample_fraction < 1.0) {
    n_sample <- round(nrow(data) * sample_fraction)
    data <- data %>% slice_sample(n = n_sample)
    cat("  Sampled to", nrow(data), "spatial points\n")
  }
  
  # Calculate fish and zooplankton totals
  fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")
  zoop_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                   "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
  
  spatial_data <- data %>%
    mutate(
      Fish_Total = rowSums(select(., all_of(fish_species)), na.rm = TRUE),
      Zoop_Total = rowSums(select(., all_of(zoop_species)), na.rm = TRUE),
      TCB = Fish_Total + Zoop_Total,
      model = model,
      scenario = scenario
    ) %>%
    select(Lon, Lat, Year, model, scenario, Fish_Total, Zoop_Total, TCB, 
           all_of(fish_species), all_of(zoop_species))
  
  return(spatial_data)
}

calculate_spatial_changes <- function(historical_data, future_data, reference_period = 1990:2009, 
                                    future_period = 2280:2299) {
  
  cat("Calculating spatial changes...\n")
  cat("  Reference period:", paste(range(reference_period), collapse = "-"), "\n")
  cat("  Future period:", paste(range(future_period), collapse = "-"), "\n")
  
  # Calculate baseline means for each grid cell
  baseline_means <- historical_data %>%
    filter(Year %in% reference_period) %>%
    group_by(Lon, Lat) %>%
    summarise(
      Fish_Historical = mean(Fish_Total, na.rm = TRUE),
      Zoop_Historical = mean(Zoop_Total, na.rm = TRUE),
      TCB_Historical = mean(TCB, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate future means for each grid cell
  future_means <- future_data %>%
    filter(Year %in% future_period) %>%
    group_by(Lon, Lat) %>%
    summarise(
      Fish_Future = mean(Fish_Total, na.rm = TRUE),
      Zoop_Future = mean(Zoop_Total, na.rm = TRUE),
      TCB_Future = mean(TCB, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate changes
  spatial_changes <- baseline_means %>%
    inner_join(future_means, by = c("Lon", "Lat")) %>%
    mutate(
      Fish_Change = Fish_Future - Fish_Historical,
      Fish_Change_Pct = (Fish_Future - Fish_Historical) / Fish_Historical * 100,
      Zoop_Change = Zoop_Future - Zoop_Historical,
      Zoop_Change_Pct = (Zoop_Future - Zoop_Historical) / Zoop_Historical * 100,
      TCB_Change = TCB_Future - TCB_Historical,
      TCB_Change_Pct = (TCB_Future - TCB_Historical) / TCB_Historical * 100
    ) %>%
    # Filter out extreme outliers for visualization
    filter(
      abs(Fish_Change_Pct) < 500,
      abs(Zoop_Change_Pct) < 500,
      abs(TCB_Change_Pct) < 500
    )
  
  cat("  Spatial changes calculated for", nrow(spatial_changes), "grid cells\n")
  
  return(spatial_changes)
}

# ==============================================================================
# STEP 3: PROCESS SPATIAL DATA FOR KEY MODEL COMPARISONS  
# ==============================================================================

cat("\nSTEP 3: Processing spatial data for comparisons...\n")

# Process CESM2-WACCM historical vs SSP5-8.5 (if available)
cesm_files <- file_metadata %>% filter(model == "cesm2-waccm")

if("historical" %in% cesm_files$scenario && "ssp585" %in% cesm_files$scenario) {
  
  cat("\nProcessing CESM2-WACCM historical vs SSP5-8.5...\n")
  
  # Load historical data
  cesm_hist_file <- cesm_files %>% filter(scenario == "historical") %>% pull(filepath)
  cesm_historical <- extract_spatial_biomass(cesm_hist_file, target_years = 1990:2014)
  
  # Load SSP5-8.5 data  
  cesm_585_file <- cesm_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  cesm_ssp585 <- extract_spatial_biomass(cesm_585_file, target_years = 2280:2299)
  
  # Calculate changes
  cesm_changes <- calculate_spatial_changes(cesm_historical, cesm_ssp585)
  cesm_changes$model <- "CESM2-WACCM"
  cesm_changes$comparison <- "Historical vs SSP5-8.5"
  
  # Clean up memory
  rm(cesm_historical, cesm_ssp585)
  gc()
}

# Process IPSL-CM6A-LR if available
ipsl_files <- file_metadata %>% filter(model == "ipsl-cm6a-lr")

if("historical" %in% ipsl_files$scenario && "ssp585" %in% ipsl_files$scenario) {
  
  cat("\nProcessing IPSL-CM6A-LR historical vs SSP5-8.5...\n")
  
  # Load historical data
  ipsl_hist_file <- ipsl_files %>% filter(scenario == "historical") %>% pull(filepath)
  ipsl_historical <- extract_spatial_biomass(ipsl_hist_file, target_years = 1990:2014)
  
  # Load SSP5-8.5 data
  ipsl_585_file <- ipsl_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  ipsl_ssp585 <- extract_spatial_biomass(ipsl_585_file, target_years = 2280:2299)
  
  # Calculate changes
  ipsl_changes <- calculate_spatial_changes(ipsl_historical, ipsl_ssp585)
  ipsl_changes$model <- "IPSL-CM6A-LR"
  ipsl_changes$comparison <- "Historical vs SSP5-8.5"
  
  rm(ipsl_historical, ipsl_ssp585)
  gc()
}

# Process UKESM1-0-LL if available
ukesm_files <- file_metadata %>% filter(model == "ukesm1-0-ll")

if("historical" %in% ukesm_files$scenario && "ssp585" %in% ukesm_files$scenario) {
  
  cat("\nProcessing UKESM1-0-LL historical vs SSP5-8.5...\n")
  
  # Load historical data
  ukesm_hist_file <- ukesm_files %>% filter(scenario == "historical") %>% pull(filepath)
  ukesm_historical <- extract_spatial_biomass(ukesm_hist_file, target_years = 1990:2014)
  
  # Load SSP5-8.5 data
  ukesm_585_file <- ukesm_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  ukesm_ssp585 <- extract_spatial_biomass(ukesm_585_file, target_years = 2280:2299)
  
  # Calculate changes
  ukesm_changes <- calculate_spatial_changes(ukesm_historical, ukesm_ssp585)
  ukesm_changes$model <- "UKESM1-0-LL"
  ukesm_changes$comparison <- "Historical vs SSP5-8.5"
  
  rm(ukesm_historical, ukesm_ssp585)
  gc()
}

# ==============================================================================
# STEP 4: COMBINE AND SAVE SPATIAL CHANGES
# ==============================================================================

# Combine all available spatial changes
all_spatial_changes <- list()

if(exists("cesm_changes")) all_spatial_changes[["cesm"]] <- cesm_changes
if(exists("ipsl_changes")) all_spatial_changes[["ipsl"]] <- ipsl_changes
if(exists("ukesm_changes")) all_spatial_changes[["ukesm"]] <- ukesm_changes

if(length(all_spatial_changes) > 0) {
  combined_spatial_changes <- bind_rows(all_spatial_changes)
  saveRDS(combined_spatial_changes, "Output/spatial_biomass_changes_2300.rds")
  cat("\nSpatial changes saved for", length(all_spatial_changes), "models\n")
} else {
  stop("No spatial change data was successfully processed!")
}

# ==============================================================================
# STEP 5: CREATE SPATIAL CHANGE MAPS
# ==============================================================================

cat("\nSTEP 5: Creating spatial change maps...\n")

# Helper function for spatial plots
create_spatial_plot <- function(data, variable, title, legend_title, 
                               color_scale = scale_fill_gradient2()) {
  
  ggplot(data, aes(x = Lon, y = Lat, fill = !!sym(variable))) +
    geom_tile() +
    color_scale +
    coord_quickmap() +
    labs(
      title = title,
      x = "Longitude", 
      y = "Latitude",
      fill = legend_title
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
}

# Fish biomass change maps by model
fish_change_plots <- list()

for(model_name in unique(combined_spatial_changes$model)) {
  
  model_data <- combined_spatial_changes %>% filter(model == model_name)
  
  p <- create_spatial_plot(
    model_data, 
    "Fish_Change_Pct",
    paste("Fish Biomass Change (%) -", model_name),
    "Change (%)",
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-150, 150),
      na.value = "grey90"
    )
  )
  
  fish_change_plots[[model_name]] <- p
}

# Combine fish change plots
if(length(fish_change_plots) > 1) {
  combined_fish_plot <- wrap_plots(fish_change_plots, ncol = 1)
} else {
  combined_fish_plot <- fish_change_plots[[1]]
}

ggsave(paste0(figure_dir, "fish_biomass_spatial_changes_2300.png"), 
       combined_fish_plot, width = 14, height = 4 * length(fish_change_plots), dpi = 300)

# Total consumer biomass change maps
tcb_change_plots <- list()

for(model_name in unique(combined_spatial_changes$model)) {
  
  model_data <- combined_spatial_changes %>% filter(model == model_name)
  
  p <- create_spatial_plot(
    model_data, 
    "TCB_Change_Pct",
    paste("Total Consumer Biomass Change (%) -", model_name),
    "Change (%)",
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-100, 100),
      na.value = "grey90"
    )
  )
  
  tcb_change_plots[[model_name]] <- p
}

if(length(tcb_change_plots) > 1) {
  combined_tcb_plot <- wrap_plots(tcb_change_plots, ncol = 1)
} else {
  combined_tcb_plot <- tcb_change_plots[[1]]
}

ggsave(paste0(figure_dir, "total_biomass_spatial_changes_2300.png"), 
       combined_tcb_plot, width = 14, height = 4 * length(tcb_change_plots), dpi = 300)

# ==============================================================================
# STEP 6: SUMMARY STATISTICS AND REGIONAL ANALYSIS
# ==============================================================================

cat("\nSTEP 6: Calculating summary statistics...\n")

# Calculate global and regional summaries
spatial_summary <- combined_spatial_changes %>%
  group_by(model) %>%
  summarise(
    n_cells = n(),
    
    # Fish changes
    mean_fish_change = mean(Fish_Change_Pct, na.rm = TRUE),
    median_fish_change = median(Fish_Change_Pct, na.rm = TRUE),
    fish_decrease_pct = sum(Fish_Change_Pct < 0, na.rm = TRUE) / n() * 100,
    fish_increase_pct = sum(Fish_Change_Pct > 0, na.rm = TRUE) / n() * 100,
    
    # Zooplankton changes  
    mean_zoop_change = mean(Zoop_Change_Pct, na.rm = TRUE),
    median_zoop_change = median(Zoop_Change_Pct, na.rm = TRUE),
    zoop_decrease_pct = sum(Zoop_Change_Pct < 0, na.rm = TRUE) / n() * 100,
    zoop_increase_pct = sum(Zoop_Change_Pct > 0, na.rm = TRUE) / n() * 100,
    
    # Total consumer biomass
    mean_tcb_change = mean(TCB_Change_Pct, na.rm = TRUE),
    median_tcb_change = median(TCB_Change_Pct, na.rm = TRUE),
    tcb_decrease_pct = sum(TCB_Change_Pct < 0, na.rm = TRUE) / n() * 100,
    tcb_increase_pct = sum(TCB_Change_Pct > 0, na.rm = TRUE) / n() * 100,
    
    .groups = 'drop'
  )

# Regional analysis (by latitude bands)
regional_summary <- combined_spatial_changes %>%
  mutate(
    region = case_when(
      Lat >= 60 ~ "Arctic (>60°N)",
      Lat >= 30 ~ "Northern Temperate (30-60°N)",
      Lat >= -30 ~ "Tropical (30°S-30°N)",
      Lat >= -60 ~ "Southern Temperate (60-30°S)",
      TRUE ~ "Antarctic (<60°S)"
    )
  ) %>%
  group_by(model, region) %>%
  summarise(
    n_cells = n(),
    mean_fish_change = mean(Fish_Change_Pct, na.rm = TRUE),
    mean_zoop_change = mean(Zoop_Change_Pct, na.rm = TRUE),
    mean_tcb_change = mean(TCB_Change_Pct, na.rm = TRUE),
    .groups = 'drop'
  )

# Save results
write_csv(spatial_summary, paste0(figure_dir, "spatial_change_global_summary.csv"))
write_csv(regional_summary, paste0(figure_dir, "spatial_change_regional_summary.csv"))

cat("Spatial analysis complete!\n")
cat("Global summary:\n")
print(spatial_summary)
cat("\nRegional summary:\n")
print(regional_summary)

cat("\n=== SPATIAL ANALYSIS COMPLETE ===\n")
