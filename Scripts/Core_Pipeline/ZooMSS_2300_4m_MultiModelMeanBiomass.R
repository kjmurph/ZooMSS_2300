# ================================================================
# ZooMSS 2300 - Multi-Model Mean Biomass Plots
# ================================================================
# Creates multi-model ensemble mean plots for Fish, TCB, and Zooplankton
# Shows ensemble mean with uncertainty bands across the three ESMs
# Based on the separate biomass plots structure

# Load required libraries
library(tidyverse)
library(patchwork)

# Setup paths
base_dir <- "c:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
figure_dir <- paste0(base_dir, "Figures/Biomass_Enhanced/")
input_dir <- paste0(base_dir, "Input/")

cat("=== MULTI-MODEL MEAN BIOMASS PLOTS ===\n")
cat("Date:", Sys.time(), "\n\n")

# Define scenario colors (consistent with previous plots)
scenario_colors <- c(
  "historical" = "#2E86C1",
  "ssp126" = "#28B463", 
  "ssp585" = "#E74C3C",
  "ssp534-over" = "#F39C12",
  "picontrol" = "#8E44AD"
)

# Define model names
model_names <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")

# Load and process data function
load_and_aggregate_data <- function() {
  
  # Load the combined weighted biomass timeseries
  cat("Loading combined weighted biomass timeseries...\n")
  all_data <- readRDS("Output/combined_weighted_biomass_timeseries.rds")
  
  cat("Successfully loaded biomass data\n")
  cat("Dimensions:", nrow(all_data), "x", ncol(all_data), "\n")
  cat("Models:", paste(unique(all_data$model), collapse = ", "), "\n")
  cat("Scenarios:", paste(unique(all_data$scenario), collapse = ", "), "\n")
  cat("Year range:", min(all_data$Year, na.rm = TRUE), "to", max(all_data$Year, na.rm = TRUE), "\n\n")
  
  # The data already has the aggregated biomass totals we need
  # Just select the unique combinations (remove the species rows since we have totals)
  spatial_means <- all_data %>%
    select(Year, model, scenario, Zooplankton_Total, Fish_Total, TCB) %>%
    distinct() %>%
    filter(!is.na(Zooplankton_Total), !is.na(Fish_Total), !is.na(TCB))
  
  cat("After filtering unique combinations:", nrow(spatial_means), "rows\n")
  
  return(spatial_means)
}

# Create baseline-corrected data
create_baseline_data <- function(spatial_means) {
  
  # Calculate historical 1990-1999 baseline for each model and biomass group
  historical_baseline <- spatial_means %>%
    filter(scenario == "historical", Year >= 1990, Year <= 1999) %>%
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

# Create multi-model ensemble statistics
create_ensemble_statistics <- function(baseline_data) {
  
  cat("Calculating multi-model ensemble statistics...\n")
  
  # Calculate ensemble statistics for each year and scenario
  ensemble_stats <- baseline_data %>%
    filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
           Year >= 1970,
           model %in% model_names) %>%
    group_by(Year, scenario) %>%
    summarise(
      # Zooplankton statistics
      Zoop_Mean = mean(Zoop_Change_1990s, na.rm = TRUE),
      Zoop_SD = sd(Zoop_Change_1990s, na.rm = TRUE),
      Zoop_Min = min(Zoop_Change_1990s, na.rm = TRUE),
      Zoop_Max = max(Zoop_Change_1990s, na.rm = TRUE),
      Zoop_Q25 = quantile(Zoop_Change_1990s, 0.25, na.rm = TRUE),
      Zoop_Q75 = quantile(Zoop_Change_1990s, 0.75, na.rm = TRUE),
      
      # Fish statistics
      Fish_Mean = mean(Fish_Change_1990s, na.rm = TRUE),
      Fish_SD = sd(Fish_Change_1990s, na.rm = TRUE),
      Fish_Min = min(Fish_Change_1990s, na.rm = TRUE),
      Fish_Max = max(Fish_Change_1990s, na.rm = TRUE),
      Fish_Q25 = quantile(Fish_Change_1990s, 0.25, na.rm = TRUE),
      Fish_Q75 = quantile(Fish_Change_1990s, 0.75, na.rm = TRUE),
      
      # TCB statistics
      TCB_Mean = mean(TCB_Change_1990s, na.rm = TRUE),
      TCB_SD = sd(TCB_Change_1990s, na.rm = TRUE),
      TCB_Min = min(TCB_Change_1990s, na.rm = TRUE),
      TCB_Max = max(TCB_Change_1990s, na.rm = TRUE),
      TCB_Q25 = quantile(TCB_Change_1990s, 0.25, na.rm = TRUE),
      TCB_Q75 = quantile(TCB_Change_1990s, 0.75, na.rm = TRUE),
      
      # Number of models contributing
      n_models = n(),
      .groups = 'drop'
    )
  
  cat("Ensemble statistics calculated for", nrow(ensemble_stats), "Year/Scenario combinations\n")
  cat("Models per combination:", unique(ensemble_stats$n_models), "\n")
  
  return(ensemble_stats)
}

# Create multi-model mean plotting function
create_multimodel_plot <- function(ensemble_stats, biomass_group, group_label, y_limits = NULL) {
  
  # Select the appropriate variables based on biomass group
  if (group_label == "Zooplankton") {
    mean_var <- "Zoop_Mean"
    q25_var <- "Zoop_Q25"
    q75_var <- "Zoop_Q75"
    min_var <- "Zoop_Min"
    max_var <- "Zoop_Max"
  } else if (group_label == "Fish") {
    mean_var <- "Fish_Mean"
    q25_var <- "Fish_Q25"
    q75_var <- "Fish_Q75"
    min_var <- "Fish_Min"
    max_var <- "Fish_Max"
  } else if (group_label == "Total Consumer Biomass") {
    mean_var <- "TCB_Mean"
    q25_var <- "TCB_Q25"
    q75_var <- "TCB_Q75"
    min_var <- "TCB_Min"
    max_var <- "TCB_Max"
  }
  
  # Prepare data for plotting
  plot_data <- ensemble_stats %>%
    select(Year, scenario, all_of(c(mean_var, q25_var, q75_var, min_var, max_var))) %>%
    rename(
      Mean = all_of(mean_var),
      Q25 = all_of(q25_var),
      Q75 = all_of(q75_var),
      Min = all_of(min_var),
      Max = all_of(max_var)
    )
  
  # Create the plot
  p <- plot_data %>%
    ggplot(aes(x = Year, color = scenario, fill = scenario)) +
    # Add uncertainty bands (IQR)
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.3, color = NA) +
    # Add range bands (min/max) - more transparent
    geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.1, color = NA) +
    # Add ensemble mean line
    geom_line(aes(y = Mean), linewidth = 1.5, alpha = 0.9) +
    # Add reference lines
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "black") +
    geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
    # Add baseline shading
    annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "blue") +
    # Styling
    scale_color_manual(values = scenario_colors, name = "Scenario") +
    scale_fill_manual(values = scenario_colors, name = "Scenario") +
    scale_x_continuous(breaks = seq(1980, 2300, 40), minor_breaks = seq(1980, 2300, 20)) +
    labs(
      title = paste("Multi-Model Ensemble Mean:", group_label, "Change"),
      subtitle = "Ensemble mean ± Inter-Quartile Range and full model range across three Earth System Models\nPercentage change relative to historical 1990-1999 baseline",
      x = "Year",
      y = "Change (%)",
      color = "Scenario",
      fill = "Scenario"
    ) +
    theme_bw() +
    theme(
      # Main plot styling
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
      
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

cat("Creating ensemble statistics...\n")
ensemble_stats <- create_ensemble_statistics(baseline_data)

cat("Creating multi-model mean plots...\n")

# Calculate reasonable y-limits for each group
y_limits_zoop <- range(c(ensemble_stats$Zoop_Min, ensemble_stats$Zoop_Max), na.rm = TRUE)
y_limits_fish <- range(c(ensemble_stats$Fish_Min, ensemble_stats$Fish_Max), na.rm = TRUE)
y_limits_tcb <- range(c(ensemble_stats$TCB_Min, ensemble_stats$TCB_Max), na.rm = TRUE)

# Add some padding to the limits
add_padding <- function(limits, padding = 0.1) {
  range_size <- diff(limits)
  c(limits[1] - range_size * padding, limits[2] + range_size * padding)
}

y_limits_zoop <- add_padding(y_limits_zoop)
y_limits_fish <- add_padding(y_limits_fish)
y_limits_tcb <- add_padding(y_limits_tcb)

# 1. Zooplankton Multi-Model Mean Plot
cat("Creating zooplankton multi-model mean plot...\n")
zoop_ensemble_plot <- create_multimodel_plot(ensemble_stats, "zooplankton", "Zooplankton", y_limits_zoop)
ggsave(paste0(figure_dir, "zooplankton_multimodel_mean.png"),
       zoop_ensemble_plot, width = 14, height = 8, dpi = 300, bg = "white")

# 2. Fish Multi-Model Mean Plot  
cat("Creating fish multi-model mean plot...\n")
fish_ensemble_plot <- create_multimodel_plot(ensemble_stats, "fish", "Fish", y_limits_fish)
ggsave(paste0(figure_dir, "fish_multimodel_mean.png"),
       fish_ensemble_plot, width = 14, height = 8, dpi = 300, bg = "white")

# 3. Total Consumer Biomass Multi-Model Mean Plot
cat("Creating TCB multi-model mean plot...\n")
tcb_ensemble_plot <- create_multimodel_plot(ensemble_stats, "tcb", "Total Consumer Biomass", y_limits_tcb)
ggsave(paste0(figure_dir, "tcb_multimodel_mean.png"),
       tcb_ensemble_plot, width = 14, height = 8, dpi = 300, bg = "white")

# Create a combined plot showing all three biomass groups
cat("Creating combined multi-model mean plot...\n")
combined_plot <- (zoop_ensemble_plot / fish_ensemble_plot / tcb_ensemble_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(paste0(figure_dir, "all_biomass_multimodel_mean_combined.png"),
       combined_plot, width = 14, height = 18, dpi = 300, bg = "white")

# Save ensemble statistics for reference
write.csv(ensemble_stats, paste0(figure_dir, "ensemble_biomass_statistics.csv"), row.names = FALSE)

# Print summary information
cat("\n=== MULTI-MODEL MEAN PLOTS CREATED SUCCESSFULLY ===\n")
cat("Files saved:\n")
cat("- zooplankton_multimodel_mean.png\n")
cat("- fish_multimodel_mean.png\n")
cat("- tcb_multimodel_mean.png\n")
cat("- all_biomass_multimodel_mean_combined.png\n")
cat("- ensemble_biomass_statistics.csv\n\n")

cat("Plot features:\n")
cat("- Thick line: Multi-model ensemble mean\n")
cat("- Dark shaded band: Inter-quartile range (Q25-Q75)\n")
cat("- Light shaded band: Full model range (Min-Max)\n")
cat("- Blue shaded area: 1990-1999 baseline period\n\n")

cat("Ensemble statistics summary:\n")
ensemble_summary <- ensemble_stats %>%
  group_by(scenario) %>%
  summarise(
    Year_range = paste(min(Year), "to", max(Year)),
    Zoop_mean_range = paste(round(min(Zoop_Mean, na.rm=TRUE), 1), "to", round(max(Zoop_Mean, na.rm=TRUE), 1), "%"),
    Fish_mean_range = paste(round(min(Fish_Mean, na.rm=TRUE), 1), "to", round(max(Fish_Mean, na.rm=TRUE), 1), "%"),
    TCB_mean_range = paste(round(min(TCB_Mean, na.rm=TRUE), 1), "to", round(max(TCB_Mean, na.rm=TRUE), 1), "%"),
    .groups = 'drop'
  )

print(ensemble_summary)

cat("\nAnalysis complete!\n")
