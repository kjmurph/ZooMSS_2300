# ==============================================================================
# SPATIAL BIOMASS PLOTTING 
# ==============================================================================
# Purpose: Create spatial plots showing biomass distributions and changes
# Version: Updated for QAQC run
# ==============================================================================

library(tidyverse)
library(raster)
library(ncdf4)
library(viridis)
library(scales)
library(maps)
library(RColorBrewer)
library(patchwork)
library(sf)

# Set directories for QAQC run
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300")
figure_dir <- file.path(base_dir, "Figures", "QAQC_Spatial_Biomass_2300")

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== SPATIAL BIOMASS PLOTTING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# LOAD AND PREPARE SPATIAL DATA
# ==============================================================================

# Get list of biomass files from QAQC run
biomass_files <- list.files(input_dir, pattern = "ZooMSS_Biomass_2300_.*\\.rds$", full.names = TRUE)
cat("Found", length(biomass_files), "biomass files for spatial analysis\n")

# Load comprehensive set for complete multi-model spatial analysis
selected_files <- c(
  # Historical scenarios (needed for baseline)
  grep("cesm2-waccm_historical", biomass_files, value = TRUE)[1],
  grep("ipsl-cm6a-lr_historical", biomass_files, value = TRUE)[1],
  grep("ukesm1-0-ll_historical", biomass_files, value = TRUE)[1],
  
  # SSP1-2.6 scenarios (all models)
  grep("cesm2-waccm_ssp126", biomass_files, value = TRUE)[1],
  grep("ipsl-cm6a-lr_ssp126", biomass_files, value = TRUE)[1],
  grep("ukesm1-0-ll_ssp126", biomass_files, value = TRUE)[1],
  
  # SSP5-8.5 scenarios (all models) 
  grep("cesm2-waccm_ssp585", biomass_files, value = TRUE)[1],
  grep("ipsl-cm6a-lr_ssp585", biomass_files, value = TRUE)[1],
  grep("ukesm1-0-ll_ssp585", biomass_files, value = TRUE)[1],
  
  # piControl scenarios (all models)
  grep("cesm2-waccm_picontrol", biomass_files, value = TRUE)[1],
  grep("ipsl-cm6a-lr_picontrol", biomass_files, value = TRUE)[1],
  grep("ukesm1-0-ll_picontrol", biomass_files, value = TRUE)[1]
)

# Remove any NA values
selected_files <- selected_files[!is.na(selected_files)]
cat("Selected", length(selected_files), "representative files for spatial analysis\n")
print(basename(selected_files))

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
  
  cat("  Detected - Model:", model, "Scenario:", scenario, "\n")
  
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

cat("\nLoading spatial data for different time periods...\n")

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

cat("Loaded historical data:", nrow(historical_data), "rows\n")
cat("Loaded recent data:", nrow(recent_data), "rows\n") 
cat("Loaded future data:", nrow(future_data), "rows\n")

# ==============================================================================
# CALCULATE SPATIAL MEANS AND CHANGES
# ==============================================================================

cat("\nCalculating spatial means and changes...\n")

# Function to calculate biomass totals
calculate_totals <- function(data) {
  # Define species groups
  zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                          "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
  fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")
  
  data %>%
    # Calculate totals for each row first
    mutate(
      Zooplankton_Total = Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                         CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish,
      Fish_Total = Fish_Small + Fish_Med + Fish_Large,
      TCB = Zooplankton_Total + Fish_Total
    ) %>%
    # Then group and summarize
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

# Calculate changes
cat("Calculating spatial changes...\n")

# Recent vs Historical (match by model only, not scenario)
recent_changes <- recent_spatial %>%
  # Join with historical data from the same model but historical scenario
  left_join(
    historical_spatial %>% filter(scenario == "historical") %>% dplyr::select(-scenario),
    by = c("Lon", "Lat", "model"), 
    suffix = c("_recent", "_historical")
  ) %>%
  filter(!is.na(TCB_historical) & !is.na(TCB_recent)) %>%
  mutate(
    Zoop_Change = (Zooplankton_Total_recent - Zooplankton_Total_historical) / Zooplankton_Total_historical * 100,
    Fish_Change = (Fish_Total_recent - Fish_Total_historical) / Fish_Total_historical * 100,
    TCB_Change = (TCB_recent - TCB_historical) / TCB_historical * 100
  ) %>%
  dplyr::select(Lon, Lat, model, scenario, Zoop_Change, Fish_Change, TCB_Change)

# Future vs Historical (match by model only, not scenario)  
future_changes <- future_spatial %>%
  # Join with historical data from the same model but historical scenario
  left_join(
    historical_spatial %>% filter(scenario == "historical") %>% dplyr::select(-scenario),
    by = c("Lon", "Lat", "model"),
    suffix = c("_future", "_historical")
  ) %>%
  filter(!is.na(TCB_historical) & !is.na(TCB_future)) %>%
  mutate(
    Zoop_Change = (Zooplankton_Total_future - Zooplankton_Total_historical) / Zooplankton_Total_historical * 100,
    Fish_Change = (Fish_Total_future - Fish_Total_historical) / Fish_Total_historical * 100,
    TCB_Change = (TCB_future - TCB_historical) / TCB_historical * 100
  ) %>%
  dplyr::select(Lon, Lat, model, scenario, Zoop_Change, Fish_Change, TCB_Change)

# ==============================================================================
# SPATIAL PLOTTING FUNCTIONS
# ==============================================================================

# Function to create world map with data
create_spatial_plot <- function(data, variable, title, subtitle = "", 
                               color_scale = "RdYlBu", symmetric = FALSE,
                               limits = NULL) {
  
  # Get world map with enhanced coastline
  world_map <- map_data("world")
  
  # Keep all data points for complete spatial coverage
  # No subsampling - display the full 1-degree global ocean grid
  
  cat("Plotting", nrow(data), "grid cells for", variable, "\n")
  cat("Data range:", min(data[[variable]], na.rm=TRUE), "to", max(data[[variable]], na.rm=TRUE), "\n")
  
  # Create base plot with enhanced Tittensor-style theme
  p <- ggplot() +
    # Add data tiles (fill grid cells with color scale)
    geom_tile(data = data, aes(x = Lon, y = Lat, fill = !!sym(variable))) +
    # Add enhanced world map on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "gray20", color = "white", linewidth = 0.15, alpha = 0.8) +
    # Use standard coordinate system (Robinson projection causes sf errors)
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-85, 85)) +
    # Enhanced themes and labels
    labs(
      title = title, 
      subtitle = subtitle, 
      x = "", 
      y = "",
      fill = if(variable %in% c("TCB_Change", "Zoop_Change", "Fish_Change")) "Change (%)" else "Biomass"
    ) +
    theme_void() +
    theme(
      # Panel and plot styling
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Text styling following Tittensor et al. conventions
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold", 
                               margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30",
                                  margin = margin(b = 15)),
      
      # Enhanced legend styling
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.margin = margin(t = 15),
      legend.box.margin = margin(t = 10),
      
      # Plot margins
      plot.margin = margin(10, 15, 10, 15),
      
      # Remove axis elements for cleaner map appearance
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
  
  # Apply enhanced color scales following Tittensor et al. conventions
  if (symmetric && is.null(limits)) {
    # For percentage change variables, use ±100% limits (following archived project approach)
    if (variable %in% c("TCB_Change", "Zoop_Change", "Fish_Change")) {
      limits <- c(-100, 100)
    } else {
      max_abs <- max(abs(data[[variable]]), na.rm = TRUE)
      limits <- c(-max_abs, max_abs)
    }
  }
  
  if (color_scale == "RdYlBu") {
    p <- p + scale_fill_distiller(
      palette = "RdYlBu", 
      direction = 1, 
      limits = limits, 
      na.value = "gray90",
      oob = scales::squish,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    )
  } else if (color_scale == "viridis") {
    p <- p + scale_fill_viridis_c(
      limits = limits, 
      na.value = "gray90",
      oob = scales::squish,
      option = "plasma",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    )
  } else if (color_scale == "RdBu") {
    # Blue for increases, Red for decreases (swapped from default)
    p <- p + scale_fill_gradient2(
      low = "#b2182b",     # Red for decreases
      mid = "white", 
      high = "#2166ac",    # Blue for increases
      midpoint = 0, 
      limits = limits, 
      na.value = "gray90",
      oob = scales::squish,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    )
  } else if (color_scale == "colorblind") {
    # Colorblind-friendly palette: Orange for decreases, Blue for increases
    p <- p + scale_fill_gradient2(
      low = "#e66101",     # Orange for decreases (colorblind-friendly)
      mid = "white", 
      high = "#5e3c99",    # Purple-blue for increases (colorblind-friendly)
      midpoint = 0, 
      limits = limits, 
      na.value = "gray90",
      oob = scales::squish,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    )
  }
  
  return(p)
}

# ==============================================================================
# CREATE SPATIAL PLOTS
# ==============================================================================

cat("\nCreating spatial plots...\n")

# Plot 1: Historical Baseline Total Consumer Biomass
p1 <- historical_spatial %>%
  filter(scenario == "historical") %>%
  create_spatial_plot("TCB", 
                     "Historical Total Consumer Biomass (1990-1999)",
                     "Baseline marine biomass distribution",
                     color_scale = "viridis")

ggsave(paste0(figure_dir, "historical_total_biomass_spatial.png"), 
       p1, width = 12, height = 8, dpi = 300)

# Plot 2: Recent Changes (2090-2099 vs 1990-1999) - IPSL Model Only
p2 <- recent_changes %>%
  filter(scenario == "ssp585" & model == "ipsl-cm6a-lr") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2090s (SSP5-8.5)",
                     "IPSL-CM6A-LR Model | Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE)

ggsave(paste0(figure_dir, "recent_biomass_change_IPSL_ssp585_spatial.png"),
       p2, width = 12, height = 8, dpi = 300)

# Plot 2b: Colorblind-friendly version of recent changes - IPSL Model Only
p2b <- recent_changes %>%
  filter(scenario == "ssp585" & model == "ipsl-cm6a-lr") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2090s (SSP5-8.5)",
                     "IPSL-CM6A-LR Model | Percentage change from 1990-1999 baseline | Colorblind-friendly",
                     color_scale = "colorblind", symmetric = TRUE)

ggsave(paste0(figure_dir, "recent_biomass_change_IPSL_ssp585_colorblind_spatial.png"),
       p2b, width = 12, height = 8, dpi = 300)

# Plot 3: Future Changes (2290-2299 vs 1990-1999)
p3 <- future_changes %>%
  filter(scenario == "ssp585") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2290s (SSP5-8.5)", 
                     "Percentage change from 1990-1999 baseline",
                     color_scale = "RdBu", symmetric = TRUE)

ggsave(paste0(figure_dir, "future_biomass_change_ssp585_spatial.png"),
       p3, width = 12, height = 8, dpi = 300)

# Plot 3b: Colorblind-friendly version of future changes
p3b <- future_changes %>%
  filter(scenario == "ssp585") %>%
  create_spatial_plot("TCB_Change",
                     "Total Consumer Biomass Change by 2290s (SSP5-8.5)", 
                     "Percentage change from 1990-1999 baseline | Colorblind-friendly",
                     color_scale = "colorblind", symmetric = TRUE)

ggsave(paste0(figure_dir, "future_biomass_change_ssp585_colorblind_spatial.png"),
       p3b, width = 12, height = 8, dpi = 300)

# Plot 3c: Multi-model comparison for SSP5-8.5 future changes (2290-2299)
if (length(unique(future_changes$model)) > 1) {
  p3c <- future_changes %>%
    filter(scenario == "ssp585") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2290s: Multi-Model Comparison (SSP5-8.5)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "future_biomass_change_multimodel_ssp585_spatial.png"),
         p3c, width = 16, height = 12, dpi = 300)
}

# Plot 3d: Multi-model comparison for SSP1-2.6 future changes (2290-2299)
if (length(unique(future_changes$model)) > 1) {
  p3d <- future_changes %>%
    filter(scenario == "ssp126") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2290s: Multi-Model Comparison (SSP1-2.6)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "future_biomass_change_multimodel_ssp126_spatial.png"),
         p3d, width = 16, height = 12, dpi = 300)
}

# Plot 3e: Multi-model comparison for piControl future changes (if available)
if (length(unique(future_changes$model)) > 1 && "picontrol" %in% future_changes$scenario) {
  p3e <- future_changes %>%
    filter(scenario == "picontrol") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2090s: Multi-Model Comparison (piControl)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "future_biomass_change_multimodel_picontrol_spatial.png"),
         p3e, width = 16, height = 12, dpi = 300)
}

# Plot 4: Multi-model comparison for SSP5-8.5 recent changes
if (length(unique(recent_changes$model)) > 1) {
  p4 <- recent_changes %>%
    filter(scenario == "ssp585") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2090s: Multi-Model Comparison (SSP5-8.5)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "recent_biomass_change_multimodel_ssp585_spatial.png"),
         p4, width = 16, height = 12, dpi = 300)
}

# Plot 4b: Multi-model comparison for SSP1-2.6 recent changes
if (length(unique(recent_changes$model)) > 1) {
  p4b <- recent_changes %>%
    filter(scenario == "ssp126") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2090s: Multi-Model Comparison (SSP1-2.6)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "recent_biomass_change_multimodel_ssp126_spatial.png"),
         p4b, width = 16, height = 12, dpi = 300)
}

# Plot 4c: Multi-model comparison for piControl recent changes (if available)
if (length(unique(recent_changes$model)) > 1 && "picontrol" %in% recent_changes$scenario) {
  p4c <- recent_changes %>%
    filter(scenario == "picontrol") %>%
    create_spatial_plot("TCB_Change",
                       "TCB Change by 2090s: Multi-Model Comparison (piControl)",
                       "Percentage change from 1990-1999 baseline",
                       color_scale = "RdBu", symmetric = TRUE) +
    facet_wrap(~model, ncol = 2)
  
  ggsave(paste0(figure_dir, "recent_biomass_change_multimodel_picontrol_spatial.png"),
         p4c, width = 16, height = 12, dpi = 300)
}

# Plot 5: Zooplankton vs Fish changes comparison
zoop_fish_changes <- recent_changes %>%
  filter(scenario == "ssp585") %>%
  dplyr::select(Lon, Lat, model, Zoop_Change, Fish_Change) %>%
  pivot_longer(cols = c(Zoop_Change, Fish_Change), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = ifelse(Group == "Zoop_Change", "Zooplankton", "Fish"))

p5 <- create_spatial_plot(zoop_fish_changes, "Change",
                         "Zooplankton vs Fish Changes by 2090s (SSP5-8.5)",
                         "Percentage change from 1990-1999 baseline",
                         color_scale = "RdBu", symmetric = TRUE) +
  facet_wrap(~Group, ncol = 2)

ggsave(paste0(figure_dir, "zoop_fish_changes_comparison_spatial.png"),
       p5, width = 16, height = 8, dpi = 300)

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

cat("\nCalculating summary statistics...\n")

# Calculate regional statistics
regional_stats <- list(
  recent_changes = recent_changes,
  future_changes = future_changes
) %>%
  map_dfr(~{
    .x %>%
      group_by(model, scenario) %>%
      summarise(
        n_cells = n(),
        mean_tcb_change = mean(TCB_Change, na.rm = TRUE),
        median_tcb_change = median(TCB_Change, na.rm = TRUE),
        p10_tcb_change = quantile(TCB_Change, 0.1, na.rm = TRUE),
        p90_tcb_change = quantile(TCB_Change, 0.9, na.rm = TRUE),
        .groups = 'drop'
      )
  }, .id = "period")

# Also calculate historical baseline statistics
historical_stats <- historical_spatial %>%
  group_by(model, scenario) %>%
  summarise(
    n_cells = n(),
    mean_tcb = mean(TCB, na.rm = TRUE),
    median_tcb = median(TCB, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(period = "historical")

# Combine all statistics
all_stats <- bind_rows(
  historical_stats %>% rename(mean_tcb_change = mean_tcb, median_tcb_change = median_tcb) %>%
                      mutate(p10_tcb_change = NA, p90_tcb_change = NA),
  regional_stats
)

# Save summary statistics
write_csv(all_stats, paste0(figure_dir, "spatial_summary_statistics.csv"))

cat("Spatial plotting complete!\n")
cat("Plots saved to:", figure_dir, "\n")
cat("Summary statistics saved to spatial_summary_statistics.csv\n")

print(all_stats)

cat("\n=== SPATIAL PLOTTING COMPLETE ===\n")
