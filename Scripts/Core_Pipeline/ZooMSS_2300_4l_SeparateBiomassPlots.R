# ================================================================
# ZooMSS 2300 - Separate Biomass Plots by Group and Model
# ================================================================
# Creates separate percentage change plots for Fish, TCB, and Zooplankton
# Each plot shows the three ESMs in separate panels
# Version: Updated for QAQC run

# Load required libraries
library(tidyverse)
library(patchwork)

# Setup paths for QAQC run
base_dir <- getwd()
figure_dir <- file.path(base_dir, "Figures", "QAQC_Spatial_Biomass_2300")
input_dir <- file.path(base_dir, "Output")

cat("=== SEPARATE BIOMASS PLOTS BY GROUP AND MODEL ===\n")
cat("Date:", Sys.time(), "\n\n")

# Define scenario colors (consistent with previous plots)
scenario_colors <- c(
  "historical" = "#2E86C1",
  "ssp126" = "#28B463", 
  "ssp585" = "#E74C3C",
  "ssp534-over" = "#F39C12",
  "picontrol" = "#8E44AD"
)

# Define model names and labels
model_names <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
model_labels <- c("CESM2-WACCM", "IPSL-CM6A-LR", "UKESM1-0-LL")
names(model_labels) <- model_names

# Load and process data function
load_and_aggregate_data <- function() {
  
  # Load the combined corrected biomass timeseries from QAQC run
  cat("Loading QAQC combined corrected biomass timeseries...\n")
  all_data <- readRDS(file.path(input_dir, "QAQC_combined_corrected_biomass_timeseries.rds"))
  
  cat("Successfully loaded biomass data\n")
  cat("Dimensions:", nrow(all_data), "x", ncol(all_data), "\n")
  cat("Models:", paste(unique(all_data$model), collapse = ", "), "\n")
  cat("Scenarios:", paste(unique(all_data$scenario), collapse = ", "), "\n")
  cat("Date range:", min(all_data$Date, na.rm = TRUE), "to", max(all_data$Date, na.rm = TRUE), "\n\n")
  
  # The data already has the aggregated biomass totals we need
  # Just select the unique combinations (remove the species rows since we have totals)
  spatial_means <- all_data %>%
    select(Date, model, scenario, Zooplankton_Total, Fish_Total, TCB) %>%
    distinct() %>%
    filter(!is.na(Zooplankton_Total), !is.na(Fish_Total), !is.na(TCB))
  
  cat("After filtering unique combinations:", nrow(spatial_means), "rows\n")
  
  return(spatial_means)
}

# Create baseline-corrected data
create_baseline_data <- function(spatial_means) {
  
  # Calculate historical 1990-1999 baseline for each model and biomass group
  historical_baseline <- spatial_means %>%
    filter(scenario == "historical", Date >= 1990, Date <= 1999) %>%
    group_by(model) %>%
    summarise(
      Zoop_hist_baseline = mean(Zooplankton_Total, na.rm = TRUE),
      Fish_hist_baseline = mean(Fish_Total, na.rm = TRUE),
      TCB_hist_baseline = mean(TCB, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Join with spatial means and calculate percentage changes
  baseline_data <- spatial_means %>%
    left_join(historical_baseline, by = "model") %>%
    mutate(
      Zoop_Change_1990s = (Zooplankton_Total - Zoop_hist_baseline) / Zoop_hist_baseline * 100,
      Fish_Change_1990s = (Fish_Total - Fish_hist_baseline) / Fish_hist_baseline * 100,
      TCB_Change_1990s = (TCB - TCB_hist_baseline) / TCB_hist_baseline * 100
    )
  
  return(baseline_data)
}

# Create plotting function for each biomass group
create_biomass_plot <- function(data, biomass_group, group_label, y_limits = NULL) {
  
  # Select the appropriate change variable
  change_var <- paste0(str_replace(group_label, " ", "_"), "_Change_1990s")
  if (group_label == "Total Consumer Biomass") change_var <- "TCB_Change_1990s"
  if (group_label == "Zooplankton") change_var <- "Zoop_Change_1990s"
  if (group_label == "Fish") change_var <- "Fish_Change_1990s"
  
  plot_data <- data %>%
    filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
           Date >= 1970,
           model %in% model_names) %>%
    select(Date, scenario, model, all_of(change_var)) %>%
    rename(Change = all_of(change_var)) %>%
    mutate(
      model_label = model_labels[model],
      model_label = factor(model_label, levels = model_labels)
    )
  
  # Create the plot
  p <- plot_data %>%
    ggplot(aes(x = Date, y = Change, color = scenario)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "black") +
    geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
    annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "blue") +
    facet_wrap(~model_label, ncol = 3) +
    scale_color_manual(values = scenario_colors, name = "Scenario") +
    scale_x_continuous(breaks = seq(1980, 2300, 40), minor_breaks = seq(1980, 2300, 20)) +
    labs(
      title = paste(group_label, "Change Relative to Historical 1990-1999 Baseline - QAQC"),
      subtitle = "Percentage change from historical simulation 1990-1999 reference period by Earth System Model",
      x = "Year",
      y = "Change (%)",
      color = "Scenario",
      caption = "QAQC Run - November 2025"
    ) +
    theme_bw() +
    theme(
      # Main plot styling
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
      
      # Facet styling
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(size = 11, face = "bold", color = "black"),
      
      # Axis styling
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      
      # Legend styling
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),
      
      # Grid styling
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
      
      # Plot margins
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # Apply y-limits if specified
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  # Add baseline annotation
  p <- p + 
    annotate("text", x = 1994.5, y = Inf, label = "Baseline\n1990-1999", 
             vjust = 1.2, hjust = 0.5, size = 3, color = "darkblue", fontface = "bold")
  
  return(p)
}

# Main execution
cat("Loading and processing data...\n")
spatial_means <- load_and_aggregate_data()

cat("Creating baseline-corrected data...\n")
baseline_data <- create_baseline_data(spatial_means)

cat("Creating individual biomass plots...\n")

# Calculate reasonable y-limits for each group
y_limits_zoop <- range(baseline_data$Zoop_Change_1990s[baseline_data$Date >= 1970 & 
                       baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)
y_limits_fish <- range(baseline_data$Fish_Change_1990s[baseline_data$Date >= 1970 & 
                       baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)
y_limits_tcb <- range(baseline_data$TCB_Change_1990s[baseline_data$Date >= 1970 & 
                      baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)

# Add some padding to the limits
add_padding <- function(limits, padding = 0.1) {
  range_size <- diff(limits)
  c(limits[1] - range_size * padding, limits[2] + range_size * padding)
}

y_limits_zoop <- add_padding(y_limits_zoop)
y_limits_fish <- add_padding(y_limits_fish)
y_limits_tcb <- add_padding(y_limits_tcb)

# 1. Zooplankton Plot
cat("Creating zooplankton plot...\n")
zoop_plot <- create_biomass_plot(baseline_data, "zooplankton", "Zooplankton", y_limits_zoop)
ggsave(file.path(figure_dir, "QAQC_zooplankton_percentage_change_by_model.png"),
       zoop_plot, width = 16, height = 8, dpi = 300, bg = "white")

# 2. Fish Plot  
cat("Creating fish plot...\n")
fish_plot <- create_biomass_plot(baseline_data, "fish", "Fish", y_limits_fish)
ggsave(file.path(figure_dir, "QAQC_fish_percentage_change_by_model.png"),
       fish_plot, width = 16, height = 8, dpi = 300, bg = "white")

# 3. Total Consumer Biomass Plot
cat("Creating TCB plot...\n")
tcb_plot <- create_biomass_plot(baseline_data, "tcb", "Total Consumer Biomass", y_limits_tcb)
ggsave(file.path(figure_dir, "QAQC_tcb_percentage_change_by_model.png"),
       tcb_plot, width = 16, height = 8, dpi = 300, bg = "white")

# Print summary information
cat("\n=== PLOTS CREATED SUCCESSFULLY ===\n")
cat("Files saved to:", figure_dir, "\n")
cat("- QAQC_zooplankton_percentage_change_by_model.png\n")
cat("- QAQC_fish_percentage_change_by_model.png\n")
cat("- QAQC_tcb_percentage_change_by_model.png\n\n")

cat("Data ranges:\n")
cat("Zooplankton change range:", round(y_limits_zoop, 1), "%\n")
cat("Fish change range:", round(y_limits_fish, 1), "%\n")
cat("TCB change range:", round(y_limits_tcb, 1), "%\n\n")

cat("Analysis complete!\n")
