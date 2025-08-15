# ==============================================================================
# SIMPLIFIED SPATIAL ANALYSIS - FISH BIOMASS CHANGES BY 2300
# ==============================================================================
# Purpose: Focus on fish biomass spatial changes (2300 vs historical)
# ==============================================================================

library(tidyverse)
library(raster)
library(viridis)

# Set directories  
output_dir <- "Output/Biomass_projections/"
figure_dir <- "Figures/Spatial_Analysis/"

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== SIMPLIFIED SPATIAL FISH BIOMASS ANALYSIS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Define fish species
fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")

# Get biomass files
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)

# Extract file info
file_info <- data.frame(
  filepath = biomass_files,
  filename = basename(biomass_files)
) %>%
  mutate(
    model = str_extract(filename, "(?<=withZooMSS_)[^_]+"),
    scenario = str_extract(filename, "(?<=_)[^_]+(?=_Control)"),
    size_mb = sapply(filepath, function(f) round(file.size(f) / 1024^2, 1))
  ) %>%
  filter(!is.na(model), !is.na(scenario))

# Function to extract fish biomass for specific years
extract_fish_data <- function(filepath, target_years) {
  
  filename <- basename(filepath)
  model <- str_extract(filename, "(?<=withZooMSS_)[^_]+")
  scenario <- str_extract(filename, "(?<=_)[^_]+(?=_Control)")
  
  cat("Loading:", model, scenario, "for years", paste(range(target_years), collapse = "-"), "\n")
  
  # Load and filter data
  data <- readRDS(filepath) %>%
    filter(Year %in% target_years) %>%
    slice_sample(n = min(50000, nrow(.)))  # Sample for memory management
  
  cat("  Sampled", nrow(data), "spatial points\n")
  
  # Calculate fish total
  fish_data <- data %>%
    mutate(Fish_Total = rowSums(dplyr::select(., all_of(fish_species)), na.rm = TRUE)) %>%
    dplyr::select(Lon, Lat, Year, Fish_Total) %>%
    group_by(Lon, Lat) %>%
    summarise(Fish_Mean = mean(Fish_Total, na.rm = TRUE), .groups = 'drop') %>%
    mutate(model = model, scenario = scenario)
  
  return(fish_data)
}

# Process CESM2-WACCM if available
cesm_files <- file_info %>% filter(model == "cesm2-waccm")

if("historical" %in% cesm_files$scenario && "ssp585" %in% cesm_files$scenario) {
  
  cat("\nProcessing CESM2-WACCM fish biomass changes...\n")
  
  # Historical baseline (1990-2009)
  hist_file <- cesm_files %>% filter(scenario == "historical") %>% pull(filepath)
  cesm_hist <- extract_fish_data(hist_file, 1990:2009)
  
  # Future period (2280-2299) 
  ssp585_file <- cesm_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  cesm_future <- extract_fish_data(ssp585_file, 2280:2299)
  
  # Calculate changes
  cesm_changes <- cesm_hist %>%
    inner_join(cesm_future, by = c("Lon", "Lat"), suffix = c("_hist", "_future")) %>%
    mutate(
      Fish_Change = Fish_Mean_future - Fish_Mean_hist,
      Fish_Change_Pct = (Fish_Mean_future - Fish_Mean_hist) / Fish_Mean_hist * 100,
      model = "CESM2-WACCM"
    ) %>%
    # Filter extreme outliers
    filter(abs(Fish_Change_Pct) < 300)
  
  cat("  Calculated changes for", nrow(cesm_changes), "grid cells\n")
  
  # Create map
  p_cesm <- ggplot(cesm_changes, aes(x = Lon, y = Lat, fill = Fish_Change_Pct)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-100, 100),
      name = "Change (%)"
    ) +
    coord_quickmap() +
    labs(
      title = "Fish Biomass Change by 2300 - CESM2-WACCM",
      subtitle = "SSP5-8.5 (2280-2299) vs Historical (1990-2009)",
      x = "Longitude", y = "Latitude"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(figure_dir, "fish_change_cesm2_waccm.png"), 
         p_cesm, width = 12, height = 8, dpi = 300)
  
  # Save data
  saveRDS(cesm_changes, paste0("Output/cesm_fish_spatial_changes.rds"))
  
  # Summary stats
  cesm_summary <- cesm_changes %>%
    summarise(
      n_cells = n(),
      mean_change = mean(Fish_Change_Pct, na.rm = TRUE),
      median_change = median(Fish_Change_Pct, na.rm = TRUE),
      decrease_pct = sum(Fish_Change_Pct < 0, na.rm = TRUE) / n() * 100,
      increase_pct = sum(Fish_Change_Pct > 0, na.rm = TRUE) / n() * 100
    )
  
  cat("CESM2-WACCM Fish Biomass Summary:\n")
  cat("  Mean change:", round(cesm_summary$mean_change, 1), "%\n")
  cat("  Areas with decreases:", round(cesm_summary$decrease_pct, 1), "%\n") 
  cat("  Areas with increases:", round(cesm_summary$increase_pct, 1), "%\n")
  
  rm(cesm_hist, cesm_future)
  gc()
}

# Process IPSL-CM6A-LR if available
ipsl_files <- file_info %>% filter(model == "ipsl-cm6a-lr")

if("historical" %in% ipsl_files$scenario && "ssp585" %in% ipsl_files$scenario) {
  
  cat("\nProcessing IPSL-CM6A-LR fish biomass changes...\n")
  
  # Historical baseline
  hist_file <- ipsl_files %>% filter(scenario == "historical") %>% pull(filepath)
  ipsl_hist <- extract_fish_data(hist_file, 1990:2009)
  
  # Future period
  ssp585_file <- ipsl_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  ipsl_future <- extract_fish_data(ssp585_file, 2280:2299)
  
  # Calculate changes
  ipsl_changes <- ipsl_hist %>%
    inner_join(ipsl_future, by = c("Lon", "Lat"), suffix = c("_hist", "_future")) %>%
    mutate(
      Fish_Change = Fish_Mean_future - Fish_Mean_hist,
      Fish_Change_Pct = (Fish_Mean_future - Fish_Mean_hist) / Fish_Mean_hist * 100,
      model = "IPSL-CM6A-LR"
    ) %>%
    filter(abs(Fish_Change_Pct) < 300)
  
  cat("  Calculated changes for", nrow(ipsl_changes), "grid cells\n")
  
  # Create map
  p_ipsl <- ggplot(ipsl_changes, aes(x = Lon, y = Lat, fill = Fish_Change_Pct)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-100, 100),
      name = "Change (%)"
    ) +
    coord_quickmap() +
    labs(
      title = "Fish Biomass Change by 2300 - IPSL-CM6A-LR",
      subtitle = "SSP5-8.5 (2280-2299) vs Historical (1990-2009)",
      x = "Longitude", y = "Latitude"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(figure_dir, "fish_change_ipsl_cm6a_lr.png"), 
         p_ipsl, width = 12, height = 8, dpi = 300)
  
  saveRDS(ipsl_changes, paste0("Output/ipsl_fish_spatial_changes.rds"))
  
  # Summary stats
  ipsl_summary <- ipsl_changes %>%
    summarise(
      n_cells = n(),
      mean_change = mean(Fish_Change_Pct, na.rm = TRUE),
      median_change = median(Fish_Change_Pct, na.rm = TRUE),
      decrease_pct = sum(Fish_Change_Pct < 0, na.rm = TRUE) / n() * 100,
      increase_pct = sum(Fish_Change_Pct > 0, na.rm = TRUE) / n() * 100
    )
  
  cat("IPSL-CM6A-LR Fish Biomass Summary:\n")
  cat("  Mean change:", round(ipsl_summary$mean_change, 1), "%\n")
  cat("  Areas with decreases:", round(ipsl_summary$decrease_pct, 1), "%\n")
  cat("  Areas with increases:", round(ipsl_summary$increase_pct, 1), "%\n")
  
  rm(ipsl_hist, ipsl_future)
  gc()
}

# Process UKESM1-0-LL if available
ukesm_files <- file_info %>% filter(model == "ukesm1-0-ll")

if("historical" %in% ukesm_files$scenario && "ssp585" %in% ukesm_files$scenario) {
  
  cat("\nProcessing UKESM1-0-LL fish biomass changes...\n")
  
  # Historical baseline
  hist_file <- ukesm_files %>% filter(scenario == "historical") %>% pull(filepath)
  ukesm_hist <- extract_fish_data(hist_file, 1990:2009)
  
  # Future period
  ssp585_file <- ukesm_files %>% filter(scenario == "ssp585") %>% pull(filepath)
  ukesm_future <- extract_fish_data(ssp585_file, 2280:2299)
  
  # Calculate changes
  ukesm_changes <- ukesm_hist %>%
    inner_join(ukesm_future, by = c("Lon", "Lat"), suffix = c("_hist", "_future")) %>%
    mutate(
      Fish_Change = Fish_Mean_future - Fish_Mean_hist,
      Fish_Change_Pct = (Fish_Mean_future - Fish_Mean_hist) / Fish_Mean_hist * 100,
      model = "UKESM1-0-LL"
    ) %>%
    filter(abs(Fish_Change_Pct) < 300)
  
  cat("  Calculated changes for", nrow(ukesm_changes), "grid cells\n")
  
  # Create map
  p_ukesm <- ggplot(ukesm_changes, aes(x = Lon, y = Lat, fill = Fish_Change_Pct)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-100, 100),
      name = "Change (%)"
    ) +
    coord_quickmap() +
    labs(
      title = "Fish Biomass Change by 2300 - UKESM1-0-LL",
      subtitle = "SSP5-8.5 (2280-2299) vs Historical (1990-2009)",
      x = "Longitude", y = "Latitude"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(figure_dir, "fish_change_ukesm1_0_ll.png"), 
         p_ukesm, width = 12, height = 8, dpi = 300)
  
  saveRDS(ukesm_changes, paste0("Output/ukesm_fish_spatial_changes.rds"))
  
  # Summary stats
  ukesm_summary <- ukesm_changes %>%
    summarise(
      n_cells = n(),
      mean_change = mean(Fish_Change_Pct, na.rm = TRUE),
      median_change = median(Fish_Change_Pct, na.rm = TRUE),
      decrease_pct = sum(Fish_Change_Pct < 0, na.rm = TRUE) / n() * 100,
      increase_pct = sum(Fish_Change_Pct > 0, na.rm = TRUE) / n() * 100
    )
  
  cat("UKESM1-0-LL Fish Biomass Summary:\n")
  cat("  Mean change:", round(ukesm_summary$mean_change, 1), "%\n")
  cat("  Areas with decreases:", round(ukesm_summary$decrease_pct, 1), "%\n")
  cat("  Areas with increases:", round(ukesm_summary$increase_pct, 1), "%\n")
  
  rm(ukesm_hist, ukesm_future)
  gc()
}

cat("\n=== SPATIAL FISH ANALYSIS COMPLETE ===\n")
