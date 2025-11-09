# ================================================================
# ZooMSS 2300 - FishMIP Spatial Comparison Maps
# ================================================================
# Creates spatial maps comparing:
# - Baseline: 1990-1999 average
# - Late 21st century: 2091-2100 average
# - Late 23rd century: 2291-2300 average
# Shows absolute values and % change from baseline

library(tidyverse)
library(patchwork)
library(viridis)
library(scales)

# Setup paths
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format_submission_version")
figure_dir <- file.path(base_dir, "Figures", "FishMIP_Outputs", "Spatial_Comparisons")

# Create output directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("==============================================================================\n")
cat("ZooMSS 2300 - FishMIP Spatial Comparison Maps\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Define scenarios to plot (exclude picontrol)
scenarios_to_plot <- c("ssp126", "ssp585", "ssp534-over")
scenario_labels <- c(
  "ssp126" = "SSP1-2.6",
  "ssp585" = "SSP5-8.5",
  "ssp534-over" = "SSP5-3.4-OS"
)

# Define models
model_names <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
model_labels <- c(
  "cesm2-waccm" = "CESM2-WACCM",
  "ipsl-cm6a-lr" = "IPSL-CM6A-LR",
  "ukesm1-0-ll" = "UKESM1-0-LL"
)

# World map outline for reference
world_map <- map_data("world")

cat("=== Processing spatial data ===\n\n")

# Process each model-scenario combination
for (model in model_names) {
  for (scenario in scenarios_to_plot) {
    
    cat(sprintf("Processing: %s - %s\n", model_labels[model], scenario_labels[scenario]))
    
    # Load historical and scenario data
    hist_file <- file.path(input_dir, sprintf("ZooMSS_FishMIP_2300_%s_historical.rds", model))
    scen_file <- file.path(input_dir, sprintf("ZooMSS_FishMIP_2300_%s_%s.rds", model, scenario))
    
    if (!file.exists(hist_file) || !file.exists(scen_file)) {
      cat("  ⚠ Files not found, skipping...\n")
      next
    }
    
    hist_data <- readRDS(hist_file)
    scen_data <- readRDS(scen_file)
    
    # Calculate baseline (1990-1999)
    baseline <- hist_data %>%
      filter(Date >= 1990, Date <= 1999) %>%
      group_by(Lon, Lat) %>%
      summarise(
        tcb_baseline = mean(tcb, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate late 21st century (2091-2100)
    late_21st <- scen_data %>%
      filter(Date >= 2091, Date <= 2100) %>%
      group_by(Lon, Lat) %>%
      summarise(
        tcb_2100 = mean(tcb, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate late 23rd century (2291-2300)
    late_23rd <- scen_data %>%
      filter(Date >= 2291, Date <= 2300) %>%
      group_by(Lon, Lat) %>%
      summarise(
        tcb_2300 = mean(tcb, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Combine data
    spatial_data <- baseline %>%
      left_join(late_21st, by = c("Lon", "Lat")) %>%
      left_join(late_23rd, by = c("Lon", "Lat")) %>%
      mutate(
        change_2100 = ((tcb_2100 - tcb_baseline) / tcb_baseline) * 100,
        change_2300 = ((tcb_2300 - tcb_baseline) / tcb_baseline) * 100
      )
    
    # Create plots - 3x3 grid (3 scenarios as columns, 3 rows: baseline, % change 2100, % change 2300)
    
    ## Panel A: Baseline (1990-1999) - Absolute values
    p_baseline <- ggplot(spatial_data, aes(x = Lon, y = Lat, fill = tcb_baseline)) +
      geom_tile() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                   fill = "gray20", color = "gray30", linewidth = 0.2, inherit.aes = FALSE) +
      scale_fill_viridis_c(
        option = "plasma",
        trans = "log10",
        limits = c(1, 1000),
        breaks = c(1, 10, 100, 1000),
        labels = c("1", "10", "100", "1000"),
        name = "TCB\n(g/m²)",
        na.value = "gray50"
      ) +
      coord_quickmap() +
      labs(title = "Baseline (1990-1999)") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 8)
      )
    
    ## Panel B: Late 21st century (2091-2100) - % Change
    # FLIPPED COLORS: Blue for increases, Red for decreases
    p_2100_change <- ggplot(spatial_data, aes(x = Lon, y = Lat, fill = change_2100)) +
      geom_tile() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                   fill = "gray20", color = "gray30", linewidth = 0.2, inherit.aes = FALSE) +
      scale_fill_gradientn(
        colors = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                   "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"),
        limits = c(-100, 100),
        breaks = seq(-100, 100, 25),
        name = "Change\n(%)",
        na.value = "gray50",
        oob = squish
      ) +
      coord_quickmap() +
      labs(title = "% Change by 2091-2100") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 8)
      )
    
    ## Panel C: Late 23rd century (2291-2300) - % Change
    # FLIPPED COLORS: Blue for increases, Red for decreases
    p_2300_change <- ggplot(spatial_data, aes(x = Lon, y = Lat, fill = change_2300)) +
      geom_tile() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                   fill = "gray20", color = "gray30", linewidth = 0.2, inherit.aes = FALSE) +
      scale_fill_gradientn(
        colors = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                   "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"),
        limits = c(-100, 100),
        breaks = seq(-100, 100, 25),
        name = "Change\n(%)",
        na.value = "gray50",
        oob = squish
      ) +
      coord_quickmap() +
      labs(title = "% Change by 2291-2300") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 8)
      )
    
    # Combine into 3-row layout: baseline, % change 2100, % change 2300
    combined_plot <- p_baseline / p_2100_change / p_2300_change +
      plot_annotation(
        title = sprintf("%s - %s: Total Consumer Biomass (TCB)",
                       model_labels[model], scenario_labels[scenario]),
        subtitle = "Top: Baseline (1990-1999) | Middle: % change by 2100 | Bottom: % change by 2300 (Blue=increase, Red=decrease)",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5)
        )
      )
    
    # Save figure
    filename <- sprintf("FishMIP_Spatial_%s_%s.png", model, scenario)
    ggsave(
      filename = file.path(figure_dir, filename),
      plot = combined_plot,
      width = 10,      # Single column width
      height = 18,     # Taller for 3 rows
      dpi = 300,
      bg = "white"
    )
    
    cat(sprintf("  ✓ Saved: %s\n", filename))
  }
}

cat("\n==============================================================================\n")
cat("Spatial comparison maps complete!\n")
cat("==============================================================================\n")
cat("Output directory:", figure_dir, "\n")
cat("Files created: 9 spatial comparison maps (3 models × 3 scenarios)\n")
cat("==============================================================================\n")
