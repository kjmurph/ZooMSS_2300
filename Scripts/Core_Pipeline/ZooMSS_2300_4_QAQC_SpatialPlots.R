# ==============================================================================
# SPATIAL BIOMASS PLOTTING - QAQC VERSION
# ==============================================================================
# Purpose: Create spatial plots from QAQC biomass projections
# Version: QAQC - Quality Check Run
# Date: 2025-11-07
# ==============================================================================

library(tidyverse)
library(viridis)
library(scales)
library(maps)
library(RColorBrewer)
library(patchwork)

# Set directories for QAQC run
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300")
figure_dir <- file.path(base_dir, "Figures", "QAQC_Spatial_Biomass_2300")

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
  cat("Created QAQC figure directory:", figure_dir, "\n")
}

cat("==============================================================================\n")
cat("SPATIAL BIOMASS PLOTTING - QUALITY CHECK RUN\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Input directory:", input_dir, "\n")
cat("Output directory:", figure_dir, "\n\n")

# ==============================================================================
# LOAD AND PREPARE SPATIAL DATA
# ==============================================================================

# Get list of biomass files from QAQC run
biomass_files <- list.files(input_dir, pattern = "ZooMSS_Biomass_2300_.*\\.rds$", full.names = TRUE)
cat("Found", length(biomass_files), "QAQC biomass files for spatial analysis\n\n")

# Select files for comprehensive multi-model spatial analysis
selected_files <- c(
  # Historical scenarios (needed for baseline)
  grep("cesm2-waccm_historical", biomass_files, value = TRUE),
  grep("ipsl-cm6a-lr_historical", biomass_files, value = TRUE),
  grep("ukesm1-0-ll_historical", biomass_files, value = TRUE),
  
  # SSP1-2.6 scenarios (all models)
  grep("cesm2-waccm_ssp126", biomass_files, value = TRUE),
  grep("ipsl-cm6a-lr_ssp126", biomass_files, value = TRUE),
  grep("ukesm1-0-ll_ssp126", biomass_files, value = TRUE),
  
  # SSP5-8.5 scenarios (all models) 
  grep("cesm2-waccm_ssp585", biomass_files, value = TRUE),
  grep("ipsl-cm6a-lr_ssp585", biomass_files, value = TRUE),
  grep("ukesm1-0-ll_ssp585", biomass_files, value = TRUE),
  
  # SSP5-3.4-overshoot scenarios (all models)
  grep("cesm2-waccm_ssp534-over", biomass_files, value = TRUE),
  grep("ipsl-cm6a-lr_ssp534-over", biomass_files, value = TRUE),
  grep("ukesm1-0-ll_ssp534-over", biomass_files, value = TRUE)
)

# Remove any NA values
selected_files <- selected_files[!is.na(selected_files)]
cat("Selected", length(selected_files), "representative files for QAQC spatial analysis:\n")
for (f in selected_files) {
  cat("  -", basename(f), "\n")
}
cat("\n")

# ==============================================================================
# FUNCTION TO LOAD AND PROCESS SPATIAL DATA
# ==============================================================================

load_spatial_biomass <- function(file_path, time_slice = NULL) {
  cat("Loading:", basename(file_path), "\n")
  
  # Extract metadata from filename
  # Format: ZooMSS_Biomass_2300_MODEL_SCENARIO.rds
  filename <- basename(file_path)
  parts <- str_remove(filename, "ZooMSS_Biomass_2300_") %>%
    str_remove("\\.rds$") %>%
    str_split("_", n = 2) %>%
    .[[1]]
  
  model <- parts[1]
  scenario <- parts[2]
  
  cat("  Model:", model, "| Scenario:", scenario, "\n")
  
  # Load data
  data <- readRDS(file_path)
  
  # Add metadata
  data$model <- model
  data$scenario <- scenario
  data$file_source <- filename
  
  # Filter to specific time slice if requested
  if (!is.null(time_slice)) {
    if (time_slice == "recent") {
      # Use 2090-2099 for recent projections
      data <- data %>% filter(Date >= 2090 & Date <= 2099)
    } else if (time_slice == "future") {
      # Use 2290-2299 for far future
      data <- data %>% filter(Date >= 2290 & Date <= 2299)
    } else if (time_slice == "historical") {
      # Use 1990-1999 for historical reference
      data <- data %>% filter(Date >= 1990 & Date <= 1999)
    }
  }
  
  cat("  Loaded", nrow(data), "rows for", time_slice, "period\n")
  return(data)
}

# ==============================================================================
# LOAD DATA FOR DIFFERENT TIME PERIODS
# ==============================================================================

cat("Loading spatial data for different time periods...\n\n")

# Load historical baseline (1990-1999)
historical_data <- map_dfr(selected_files, ~{
  tryCatch({
    load_spatial_biomass(.x, "historical")
  }, error = function(e) {
    cat("Error loading", basename(.x), "for historical period:", e$message, "\n")
    return(NULL)
  })
})

# Load recent projections (2090-2099)
recent_data <- map_dfr(selected_files, ~{
  tryCatch({
    load_spatial_biomass(.x, "recent")
  }, error = function(e) {
    cat("Error loading", basename(.x), "for recent period:", e$message, "\n")
    return(NULL)
  })
})

# Load far future projections (2290-2299)
future_data <- map_dfr(selected_files, ~{
  tryCatch({
    load_spatial_biomass(.x, "future")
  }, error = function(e) {
    cat("Error loading", basename(.x), "for future period:", e$message, "\n")
    return(NULL)
  })
})

cat("\nData loading summary:\n")
cat("  Historical (1990-1999):", nrow(historical_data), "rows\n")
cat("  Recent (2090-2099):", nrow(recent_data), "rows\n") 
cat("  Future (2290-2299):", nrow(future_data), "rows\n\n")

# ==============================================================================
# CALCULATE SPATIAL MEANS AND CHANGES
# ==============================================================================

cat("Calculating spatial means and changes...\n")

# Function to calculate biomass totals
calculate_totals <- function(data) {
  zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                          "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
  fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")
  
  data %>%
    mutate(
      Zooplankton_Total = Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                         CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish,
      Fish_Total = Fish_Small + Fish_Med + Fish_Large,
      TCB = Zooplankton_Total + Fish_Total
    ) %>%
    group_by(Lon, Lat, model, scenario) %>%
    summarise(
      Zooplankton_Total = mean(Zooplankton_Total, na.rm = TRUE),
      Fish_Total = mean(Fish_Total, na.rm = TRUE),
      TCB = mean(TCB, na.rm = TRUE),
      n_years = n_distinct(Date),
      .groups = 'drop'
    )
}

# Calculate spatial means for each period
historical_spatial <- calculate_totals(historical_data)
recent_spatial <- calculate_totals(recent_data) 
future_spatial <- calculate_totals(future_data)

# Calculate changes (recent vs historical)
recent_changes <- recent_spatial %>%
  left_join(
    historical_spatial %>% filter(scenario == "historical") %>% select(-scenario),
    by = c("Lon", "Lat", "model"), 
    suffix = c("_recent", "_historical")
  ) %>%
  filter(!is.na(TCB_historical) & !is.na(TCB_recent)) %>%
  mutate(
    Zoop_Change = (Zooplankton_Total_recent - Zooplankton_Total_historical) / Zooplankton_Total_historical * 100,
    Fish_Change = (Fish_Total_recent - Fish_Total_historical) / Fish_Total_historical * 100,
    TCB_Change = (TCB_recent - TCB_historical) / TCB_historical * 100
  ) %>%
  select(Lon, Lat, model, scenario, Zoop_Change, Fish_Change, TCB_Change)

# Calculate changes (future vs historical)
future_changes <- future_spatial %>%
  left_join(
    historical_spatial %>% filter(scenario == "historical") %>% select(-scenario),
    by = c("Lon", "Lat", "model"),
    suffix = c("_future", "_historical")
  ) %>%
  filter(!is.na(TCB_historical) & !is.na(TCB_future)) %>%
  mutate(
    Zoop_Change = (Zooplankton_Total_future - Zooplankton_Total_historical) / Zooplankton_Total_historical * 100,
    Fish_Change = (Fish_Total_future - Fish_Total_historical) / Fish_Total_historical * 100,
    TCB_Change = (TCB_future - TCB_historical) / TCB_historical * 100
  ) %>%
  select(Lon, Lat, model, scenario, Zoop_Change, Fish_Change, TCB_Change)

cat("Spatial calculations complete!\n\n")

# ==============================================================================
# SPATIAL PLOTTING FUNCTION
# ==============================================================================

create_spatial_plot <- function(data, variable, title, subtitle = "", 
                               color_scale = "RdBu", symmetric = FALSE,
                               limits = NULL) {
  
  world_map <- map_data("world")
  
  cat("Creating plot:", title, "\n")
  cat("  Data points:", nrow(data), "\n")
  cat("  Variable range:", round(min(data[[variable]], na.rm=TRUE), 2), "to", 
      round(max(data[[variable]], na.rm=TRUE), 2), "\n")
  
  # Create base plot
  p <- ggplot() +
    geom_tile(data = data, aes(x = Lon, y = Lat, fill = !!sym(variable))) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "gray20", color = "white", linewidth = 0.15, alpha = 0.8) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-85, 85)) +
    labs(
      title = paste(title, "- QAQC"),
      subtitle = subtitle,
      x = "", 
      y = "",
      fill = if(variable %in% c("TCB_Change", "Zoop_Change", "Fish_Change")) "Change (%)" else "Biomass",
      caption = "QAQC Run - November 2025"
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
      plot.caption = element_text(size = 9, hjust = 1, color = "gray50", margin = margin(t = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.margin = margin(t = 15),
      plot.margin = margin(10, 15, 10, 15)
    )
  
  # Apply color scales
  if (symmetric && is.null(limits)) {
    if (variable %in% c("TCB_Change", "Zoop_Change", "Fish_Change")) {
      limits <- c(-100, 100)
    }
  }
  
  if (color_scale == "RdBu") {
    p <- p + scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = limits, na.value = "gray90",
      oob = scales::squish,
      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
    )
  } else if (color_scale == "viridis") {
    p <- p + scale_fill_viridis_c(
      limits = limits, na.value = "gray90", oob = scales::squish,
      option = "plasma",
      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
    )
  }
  
  return(p)
}

# ==============================================================================
# CREATE QAQC SPATIAL PLOTS
# ==============================================================================

cat("Creating QAQC spatial plots...\n\n")

# Plot 1: Historical Baseline Total Consumer Biomass
cat("Plot 1: Historical baseline\n")
p1 <- historical_spatial %>%
  filter(scenario == "historical") %>%
  create_spatial_plot("TCB", 
                     "Historical Total Consumer Biomass (1990-1999)",
                     "Baseline marine biomass distribution",
                     color_scale = "viridis")

ggsave(file.path(figure_dir, "QAQC_historical_total_biomass_spatial.png"), 
       p1, width = 12, height = 8, dpi = 300)

# Plot 2: Recent Changes (2090-2099) - SSP5-8.5
cat("Plot 2: Recent changes SSP5-8.5\n")
p2 <- recent_changes %>%
  filter(scenario == "ssp585") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2090s (SSP5-8.5)",
                     "Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE)

ggsave(file.path(figure_dir, "QAQC_recent_biomass_change_ssp585_spatial.png"),
       p2, width = 12, height = 8, dpi = 300)

# Plot 3: Future Changes (2290-2299) - SSP5-8.5 Multi-model
cat("Plot 3: Future changes SSP5-8.5 multi-model\n")
p3 <- future_changes %>%
  filter(scenario == "ssp585") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2290s (SSP5-8.5)",
                     "Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE) +
  facet_wrap(~model, ncol = 2)

ggsave(file.path(figure_dir, "QAQC_future_biomass_change_multimodel_ssp585_spatial.png"),
       p3, width = 16, height = 12, dpi = 300)

# Plot 4: Future Changes - SSP1-2.6 Multi-model
cat("Plot 4: Future changes SSP1-2.6 multi-model\n")
p4 <- future_changes %>%
  filter(scenario == "ssp126") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2290s (SSP1-2.6)",
                     "Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE) +
  facet_wrap(~model, ncol = 2)

ggsave(file.path(figure_dir, "QAQC_future_biomass_change_multimodel_ssp126_spatial.png"),
       p4, width = 16, height = 12, dpi = 300)

# Plot 5: Future Changes - Overshoot Multi-model
cat("Plot 5: Future changes SSP5-3.4-overshoot multi-model\n")
p5 <- future_changes %>%
  filter(scenario == "ssp534-over") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2290s (SSP5-3.4-Overshoot)",
                     "Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE) +
  facet_wrap(~model, ncol = 2)

ggsave(file.path(figure_dir, "QAQC_future_biomass_change_multimodel_ssp534over_spatial.png"),
       p5, width = 16, height = 12, dpi = 300)

# Plot 6: Zooplankton vs Fish changes (2090s, SSP5-8.5)
cat("Plot 6: Zooplankton vs Fish changes\n")
zoop_fish_changes <- recent_changes %>%
  filter(scenario == "ssp585") %>%
  select(Lon, Lat, model, Zoop_Change, Fish_Change) %>%
  pivot_longer(cols = c(Zoop_Change, Fish_Change), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = ifelse(Group == "Zoop_Change", "Zooplankton", "Fish"))

p6 <- create_spatial_plot(zoop_fish_changes, "Change",
                         "Zooplankton vs Fish Changes by 2090s (SSP5-8.5)",
                         "Percentage change from 1990-1999 baseline",
                         color_scale = "RdBu", symmetric = TRUE) +
  facet_wrap(~Group, ncol = 2)

ggsave(file.path(figure_dir, "QAQC_zoop_fish_changes_comparison_spatial.png"),
       p6, width = 16, height = 8, dpi = 300)

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

cat("\nCalculating summary statistics...\n")

regional_stats <- bind_rows(
  recent_changes %>% mutate(period = "2090s"),
  future_changes %>% mutate(period = "2290s")
) %>%
  group_by(period, model, scenario) %>%
  summarise(
    n_cells = n(),
    mean_tcb_change = mean(TCB_Change, na.rm = TRUE),
    median_tcb_change = median(TCB_Change, na.rm = TRUE),
    sd_tcb_change = sd(TCB_Change, na.rm = TRUE),
    p10_tcb_change = quantile(TCB_Change, 0.1, na.rm = TRUE),
    p90_tcb_change = quantile(TCB_Change, 0.9, na.rm = TRUE),
    .groups = 'drop'
  )

# Save summary statistics
write_csv(regional_stats, file.path(figure_dir, "QAQC_spatial_summary_statistics.csv"))

cat("\n==============================================================================\n")
cat("QAQC SPATIAL PLOTTING COMPLETE!\n")
cat("==============================================================================\n")
cat("Plots saved to:", figure_dir, "\n")
cat("Summary statistics saved to: QAQC_spatial_summary_statistics.csv\n")
cat("\nSummary:\n")
print(regional_stats %>% select(period, model, scenario, mean_tcb_change, median_tcb_change))
cat("==============================================================================\n")
