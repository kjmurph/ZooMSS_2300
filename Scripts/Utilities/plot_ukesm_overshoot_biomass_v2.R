# ================================================================
# ZooMSS 2300 - Updated UKESM Overshoot Biomass Plots (v2)
# ================================================================
# Creates updated biomass timeseries plots with complete UKESM overshoot (2040-2300)
# All figures saved with _v2 suffix for version control
# Based on ZooMSS_2300_4l_SeparateBiomassPlots.R

library(tidyverse)
library(patchwork)

# Setup paths
base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
figure_dir <- paste0(base_dir, "Figures/Biomass_Enhanced/")
input_dir <- paste0(base_dir, "Output/")

# Create output directory if needed
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== UPDATED UKESM OVERSHOOT BIOMASS PLOTS (v2) ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Force initial garbage collection
gc()
cat("Initial memory usage:", round(sum(gc()[,2]), 1), "MB\n\n")

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

# ==============================================================================
# LOAD AND PROCESS DATA
# ==============================================================================

cat("STEP 1: Loading individual model-scenario files...\n")

# Function to load a single model-scenario file with memory optimization
load_scenario <- function(model, scenario) {
  filename <- paste0("ClimateChange_2300_", model, "_", scenario, ".rds")
  filepath <- file.path(input_dir, filename)
  
  if (!file.exists(filepath)) {
    cat("  Warning: File not found:", filename, "\n")
    return(NULL)
  }
  
  data <- readRDS(filepath)
  
  # Immediately reduce to only what we need
  # Only keep years >= 1970 for plotting, and essential columns
  data_reduced <- data %>%
    filter(Date >= 1970) %>%
    select(Lon, Lat, Date, pico_biom, nano_biom, micro_biom) %>%
    mutate(Model = model, Experiment = scenario)
  
  # Free memory
  rm(data)
  gc()
  
  cat("  Loaded:", filename, "- Years:", min(data_reduced$Date, na.rm=TRUE), 
      "to", max(data_reduced$Date, na.rm=TRUE), "- Rows:", nrow(data_reduced), "\n")
  
  return(data_reduced)
}

# Load all model-scenario combinations
scenarios <- c("historical", "ssp126", "ssp585", "ssp534-over")  # Removed picontrol to save memory

all_data_list <- list()

for (model in model_names) {
  for (scenario in scenarios) {
    data <- load_scenario(model, scenario)
    if (!is.null(data)) {
      all_data_list[[paste(model, scenario, sep="_")]] <- data
    }
    # Force garbage collection after each file
    gc()
  }
}

# Combine all data
cat("\nCombining all datasets...\n")
all_data <- bind_rows(all_data_list)

# Free memory
rm(all_data_list)
gc()

cat("  Combined data: ", nrow(all_data), "rows\n")
cat("  Models:", paste(unique(all_data$Model), collapse=", "), "\n")
cat("  Scenarios:", paste(unique(all_data$Experiment), collapse=", "), "\n")
cat("  Year range:", min(all_data$Date, na.rm=TRUE), "to", max(all_data$Date, na.rm=TRUE), "\n\n")

# ==============================================================================
# CALCULATE BIOMASS TOTALS
# ==============================================================================

cat("STEP 2: Calculating biomass totals...\n")

# Calculate total biomass per grid cell per year
biomass_data <- all_data %>%
  mutate(
    # Total Consumer Biomass (sum of all three groups)
    TCB = pico_biom + nano_biom + micro_biom,
    # Zooplankton = nano + micro (picoplankton are primary producers)
    Zooplankton_Total = nano_biom + micro_biom,
    # Fish = micro only (assuming microplankton represent fish food web)
    Fish_Total = micro_biom
  )

# Free memory
rm(all_data)
gc()

cat("  Calculated TCB, Zooplankton, and Fish totals\n\n")

# ==============================================================================
# CALCULATE AREA-WEIGHTED GLOBAL MEANS
# ==============================================================================

cat("STEP 3: Calculating area-weighted global means...\n")

# Calculate grid cell areas (1-degree grid)
# Area = cos(latitude) * (111.32 km)^2
biomass_data <- biomass_data %>%
  mutate(
    lat_rad = Lat * pi / 180,
    cell_area = cos(lat_rad) * (111.32^2)  # km^2
  )

# Calculate area-weighted global means by year, model, and scenario
global_means <- biomass_data %>%
  group_by(Date, Model, Experiment) %>%
  summarise(
    # Area-weighted means
    TCB_global = sum(TCB * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    Zooplankton_global = sum(Zooplankton_Total * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    Fish_global = sum(Fish_Total * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    Pico_global = sum(pico_biom * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    Nano_global = sum(nano_biom * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    Micro_global = sum(micro_biom * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE),
    n_cells = n(),
    .groups = 'drop'
  ) %>%
  rename(Year = Date, model = Model, scenario = Experiment)

# Free memory from large biomass_data
rm(biomass_data)
gc()

cat("  Global means calculated for", nrow(global_means), "year-model-scenario combinations\n")
cat("  Year range:", min(global_means$Year), "to", max(global_means$Year), "\n")
cat("  Memory after aggregation:", round(sum(gc()[,2]), 1), "MB\n\n")

# ==============================================================================
# CALCULATE BASELINE-CORRECTED CHANGES
# ==============================================================================

cat("STEP 4: Calculating percentage changes from 1990s baseline...\n")

# Calculate historical 1990-1999 baseline for each model
historical_baseline <- global_means %>%
  filter(scenario == "historical", Year >= 1990, Year <= 1999) %>%
  group_by(model) %>%
  summarise(
    TCB_hist_baseline = mean(TCB_global, na.rm = TRUE),
    Zoop_hist_baseline = mean(Zooplankton_global, na.rm = TRUE),
    Fish_hist_baseline = mean(Fish_global, na.rm = TRUE),
    Pico_hist_baseline = mean(Pico_global, na.rm = TRUE),
    Nano_hist_baseline = mean(Nano_global, na.rm = TRUE),
    Micro_hist_baseline = mean(Micro_global, na.rm = TRUE),
    .groups = 'drop'
  )

cat("  Historical baselines (1990-1999):\n")
print(historical_baseline)
cat("\n")

# Join with global means and calculate percentage changes
baseline_data <- global_means %>%
  left_join(historical_baseline, by = "model") %>%
  mutate(
    TCB_Change_1990s = (TCB_global - TCB_hist_baseline) / TCB_hist_baseline * 100,
    Zoop_Change_1990s = (Zooplankton_global - Zoop_hist_baseline) / Zoop_hist_baseline * 100,
    Fish_Change_1990s = (Fish_global - Fish_hist_baseline) / Fish_hist_baseline * 100,
    Pico_Change_1990s = (Pico_global - Pico_hist_baseline) / Pico_hist_baseline * 100,
    Nano_Change_1990s = (Nano_global - Nano_hist_baseline) / Nano_hist_baseline * 100,
    Micro_Change_1990s = (Micro_global - Micro_hist_baseline) / Micro_hist_baseline * 100
  )

cat("  Percentage changes calculated\n\n")

# ==============================================================================
# PLOTTING FUNCTIONS
# ==============================================================================

# Create plotting function for each biomass group
create_biomass_plot <- function(data, change_var, group_label, y_limits = NULL) {
  
  plot_data <- data %>%
    filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
           Year >= 1970,
           model %in% model_names) %>%
    select(Year, scenario, model, all_of(change_var)) %>%
    rename(Change = all_of(change_var)) %>%
    mutate(
      model_label = model_labels[model],
      model_label = factor(model_label, levels = model_labels)
    ) %>%
    filter(!is.na(Change))
  
  # Find UKESM overshoot year range for annotation
  ukesm_overshoot_years <- plot_data %>%
    filter(model == "ukesm1-0-ll", scenario == "ssp534-over") %>%
    pull(Year) %>%
    range()
  
  cat("  ", group_label, "- UKESM overshoot years:", ukesm_overshoot_years[1], "to", ukesm_overshoot_years[2], "\n")
  
  # Create the plot
  p <- plot_data %>%
    ggplot(aes(x = Year, y = Change, color = scenario)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "black") +
    geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
    annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "blue") +
    facet_wrap(~model_label, ncol = 3) +
    scale_color_manual(values = scenario_colors, name = "Scenario",
                      labels = c("historical" = "Historical",
                                "ssp126" = "SSP1-2.6",
                                "ssp585" = "SSP5-8.5",
                                "ssp534-over" = "SSP5-3.4-OS (Overshoot)")) +
    scale_x_continuous(breaks = seq(1980, 2300, 40), minor_breaks = seq(1980, 2300, 20)) +
    labs(
      title = paste(group_label, "Change Relative to Historical 1990-1999 Baseline"),
      subtitle = "Updated with complete UKESM overshoot scenario (2040-2300) - Version 2",
      x = "Year",
      y = "Change (%)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(size = 11, face = "bold", color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
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

# ==============================================================================
# CREATE INDIVIDUAL PLOTS
# ==============================================================================

cat("STEP 5: Creating individual biomass plots with v2 naming...\n\n")

# Calculate y-limits with padding
add_padding <- function(limits, padding = 0.1) {
  range_size <- diff(limits)
  c(limits[1] - range_size * padding, limits[2] + range_size * padding)
}

# Filter data for limit calculation
plot_filter_data <- baseline_data %>%
  filter(Year >= 1970, scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"))

y_limits_tcb <- add_padding(range(plot_filter_data$TCB_Change_1990s, na.rm = TRUE))
y_limits_zoop <- add_padding(range(plot_filter_data$Zoop_Change_1990s, na.rm = TRUE))
y_limits_fish <- add_padding(range(plot_filter_data$Fish_Change_1990s, na.rm = TRUE))
y_limits_pico <- add_padding(range(plot_filter_data$Pico_Change_1990s, na.rm = TRUE))
y_limits_nano <- add_padding(range(plot_filter_data$Nano_Change_1990s, na.rm = TRUE))
y_limits_micro <- add_padding(range(plot_filter_data$Micro_Change_1990s, na.rm = TRUE))

# 1. Total Consumer Biomass (TCB)
cat("Creating Total Consumer Biomass plot (v2)...\n")
tcb_plot <- create_biomass_plot(baseline_data, "TCB_Change_1990s", 
                                "Total Consumer Biomass", y_limits_tcb)
ggsave(paste0(figure_dir, "tcb_percentage_change_by_model_v2.png"),
       tcb_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(tcb_plot); gc()

# 2. Zooplankton
cat("Creating Zooplankton plot (v2)...\n")
zoop_plot <- create_biomass_plot(baseline_data, "Zoop_Change_1990s",
                                 "Zooplankton", y_limits_zoop)
ggsave(paste0(figure_dir, "zooplankton_percentage_change_by_model_v2.png"),
       zoop_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(zoop_plot); gc()

# 3. Fish
cat("Creating Fish plot (v2)...\n")
fish_plot <- create_biomass_plot(baseline_data, "Fish_Change_1990s",
                                 "Fish", y_limits_fish)
ggsave(paste0(figure_dir, "fish_percentage_change_by_model_v2.png"),
       fish_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(fish_plot); gc()

# 4. Picoplankton
cat("Creating Picoplankton plot (v2)...\n")
pico_plot <- create_biomass_plot(baseline_data, "Pico_Change_1990s",
                                 "Picoplankton", y_limits_pico)
ggsave(paste0(figure_dir, "picoplankton_percentage_change_by_model_v2.png"),
       pico_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(pico_plot); gc()

# 5. Nanoplankton
cat("Creating Nanoplankton plot (v2)...\n")
nano_plot <- create_biomass_plot(baseline_data, "Nano_Change_1990s",
                                 "Nanoplankton", y_limits_nano)
ggsave(paste0(figure_dir, "nanoplankton_percentage_change_by_model_v2.png"),
       nano_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(nano_plot); gc()

# 6. Microplankton
cat("Creating Microplankton plot (v2)...\n")
micro_plot <- create_biomass_plot(baseline_data, "Micro_Change_1990s",
                                  "Microplankton", y_limits_micro)
ggsave(paste0(figure_dir, "microplankton_percentage_change_by_model_v2.png"),
       micro_plot, width = 16, height = 8, dpi = 300, bg = "white")
rm(micro_plot); gc()

cat("\nMemory after all plots:", round(sum(gc()[,2]), 1), "MB\n")

cat("\n")

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

cat("STEP 6: Summary statistics for UKESM overshoot scenario...\n\n")

ukesm_overshoot_summary <- baseline_data %>%
  filter(model == "ukesm1-0-ll", scenario == "ssp534-over") %>%
  summarise(
    Years = paste(min(Year), "to", max(Year)),
    N_years = n(),
    TCB_2100 = TCB_Change_1990s[Year == 2100],
    TCB_2200 = TCB_Change_1990s[Year == 2200],
    TCB_2300 = TCB_Change_1990s[Year == 2300],
    TCB_max_change = max(TCB_Change_1990s, na.rm = TRUE),
    TCB_max_year = Year[which.max(TCB_Change_1990s)],
    Zoop_2300 = Zoop_Change_1990s[Year == 2300],
    Fish_2300 = Fish_Change_1990s[Year == 2300]
  )

cat("UKESM Overshoot (ssp534-over) Summary:\n")
print(ukesm_overshoot_summary)
cat("\n")

# Check for recovery (declining change after peak)
ukesm_recovery <- baseline_data %>%
  filter(model == "ukesm1-0-ll", scenario == "ssp534-over", Year >= 2100) %>%
  arrange(Year) %>%
  select(Year, TCB_Change_1990s, Zoop_Change_1990s, Fish_Change_1990s)

cat("UKESM Overshoot post-2100 trajectory (every 20 years):\n")
print(ukesm_recovery %>% filter(Year %% 20 == 0))
cat("\n")

# ==============================================================================
# SAVE PROCESSED DATA
# ==============================================================================

cat("STEP 7: Saving processed data...\n")

# Save the baseline-corrected data for further analysis
saveRDS(baseline_data, paste0(input_dir, "ukesm_overshoot_baseline_data_v2.rds"))
cat("  Saved: ukesm_overshoot_baseline_data_v2.rds\n")

# Save global means
saveRDS(global_means, paste0(input_dir, "ukesm_overshoot_global_means_v2.rds"))
cat("  Saved: ukesm_overshoot_global_means_v2.rds\n\n")

# ==============================================================================
# COMPLETION SUMMARY
# ==============================================================================

cat("=============================================================================\n")
cat("UKESM OVERSHOOT BIOMASS PLOTS (v2) COMPLETE\n")
cat("=============================================================================\n\n")

cat("✓ Created 6 biomass timeseries plots:\n")
cat("  1. tcb_percentage_change_by_model_v2.png\n")
cat("  2. zooplankton_percentage_change_by_model_v2.png\n")
cat("  3. fish_percentage_change_by_model_v2.png\n")
cat("  4. picoplankton_percentage_change_by_model_v2.png\n")
cat("  5. nanoplankton_percentage_change_by_model_v2.png\n")
cat("  6. microplankton_percentage_change_by_model_v2.png\n\n")

cat("All figures saved to:", figure_dir, "\n")
cat("All figures use '_v2' suffix for version control\n\n")

cat("Key improvements in v2:\n")
cat("  - Complete UKESM overshoot data (2040-2300)\n")
cat("  - Updated subtitle noting version 2\n")
cat("  - Extended x-axis to 2300\n")
cat("  - Proper scenario labeling (SSP5-3.4-OS)\n\n")

cat("Next steps:\n")
cat("  1. Review v2 figures manually\n")
cat("  2. Compare with original versions (without _v2 suffix)\n")
cat("  3. If satisfied, rename v2 to replace originals\n")
cat("  4. Generate spatial plots for key years (2100, 2200, 2300)\n\n")

cat("=============================================================================\n")
