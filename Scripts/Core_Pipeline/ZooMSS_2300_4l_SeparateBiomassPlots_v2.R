# ================================================================
# ZooMSS 2300 - Separate Biomass Plots by Group and Model (v2)
# ================================================================
# Creates updated percentage change plots for Fish, TCB, and Zooplankton
# This version explicitly replaces the UKESM ssp534-over series with the
# combined 2015–2300 file and writes plots with a v2 suffix and title.

suppressPackageStartupMessages({
  library(tidyverse)
})

cat("=== SEPARATE BIOMASS PLOTS BY GROUP AND MODEL (v2) ===\n")
cat("Date:", Sys.time(), "\n\n")

# Paths
repo_root <- getwd()  # assume running from repo root
fig_dir <- file.path(repo_root, "Figures", "Biomass_Enhanced")
proj_dir <- file.path(repo_root, "Output", "Biomass_projections")
combined_ts_file <- file.path(repo_root, "Output", "combined_weighted_biomass_timeseries.rds")
ukesm_combined_file <- file.path(proj_dir, "Biomass_ClimateChange_Compiled_withZooMSS_ukesm1-0-ll_ssp534-over_2015-2300_Control.rds")

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Scenario colors (consistent with others)
scenario_colors <- c(
  "historical" = "#2E86C1",
  "ssp126" = "#28B463",
  "ssp585" = "#E74C3C",
  "ssp534-over" = "#F39C12",
  "picontrol" = "#8E44AD"
)

# Model labels
model_names <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
model_labels <- c("CESM2-WACCM", "IPSL-CM6A-LR", "UKESM1-0-LL")
names(model_labels) <- model_names

# Species groupings
zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods",
                         "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")

# Area calculation for 1-degree grid cells (km^2)
calculate_grid_area <- function(lat_deg, resolution = 1) {
  R <- 6371
  res_rad <- resolution * pi / 180
  lat_rad <- lat_deg * pi / 180
  lat_min <- lat_rad - res_rad/2
  lat_max <- lat_rad + res_rad/2
  area_km2 <- R^2 * res_rad * (sin(lat_max) - sin(lat_min))
  return(area_km2)
}

# Load original aggregated time series (used for baseline + other models)
stopifnot(file.exists(combined_ts_file))
all_data <- readRDS(combined_ts_file)
cat("Loaded:", combined_ts_file, "\n")
cat("Models:", paste(unique(all_data$model), collapse = ", "), "\n")
cat("Scenarios:", paste(unique(all_data$scenario), collapse = ", "), "\n")

# Build spatial means from the aggregated dataset
spatial_means <- all_data %>%
  select(Year, model, scenario, Zooplankton_Total, Fish_Total, TCB) %>%
  distinct() %>%
  filter(!is.na(Zooplankton_Total), !is.na(Fish_Total), !is.na(TCB))

# Compute replacement UKESM ssp534-over series directly from the combined file
stopifnot(file.exists(ukesm_combined_file))
ukesm_grid <- readRDS(ukesm_combined_file)
cat("Loaded UKESM combined grid-level file:\n - ", ukesm_combined_file, "\n", sep = "")

# Ensure species columns exist
missing_zoop <- setdiff(zooplankton_species, names(ukesm_grid))
missing_fish <- setdiff(fish_species, names(ukesm_grid))
if (length(missing_zoop) > 0 || length(missing_fish) > 0) {
  stop("Combined UKESM file missing expected species columns: ",
       paste(c(missing_zoop, missing_fish), collapse = ", "))
}

# Compute per-cell aggregates and area-weighted annual means
ukesm_grid <- ukesm_grid %>%
  mutate(
    Zooplankton_Total = rowSums(across(all_of(zooplankton_species)), na.rm = TRUE),
    Fish_Total = rowSums(across(all_of(fish_species)), na.rm = TRUE),
    TCB = Zooplankton_Total + Fish_Total,
    area_km2 = calculate_grid_area(Lat)
  )

ukesm_replacement <- ukesm_grid %>%
  group_by(Year) %>%
  summarise(
    Zooplankton_Total = sum(Zooplankton_Total * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(Zooplankton_Total)], na.rm = TRUE),
    Fish_Total        = sum(Fish_Total        * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(Fish_Total)], na.rm = TRUE),
    TCB               = sum(TCB               * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(TCB)], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    model = "ukesm1-0-ll",
    scenario = "ssp534-over"
  ) %>%
  select(Year, model, scenario, Zooplankton_Total, Fish_Total, TCB)

cat("Replacement UKESM ssp534-over years:", min(ukesm_replacement$Year), "-", max(ukesm_replacement$Year), "\n")

# Replace in spatial_means
spatial_means_v2 <- spatial_means %>%
  filter(!(model == "ukesm1-0-ll" & scenario == "ssp534-over")) %>%
  bind_rows(ukesm_replacement) %>%
  arrange(model, scenario, Year)

# Baseline: 1990-1999 historical per model
historical_baseline <- spatial_means_v2 %>%
  filter(scenario == "historical", Year >= 1990, Year <= 1999) %>%
  group_by(model) %>%
  summarise(
    Zoop_hist_baseline = mean(Zooplankton_Total, na.rm = TRUE),
    Fish_hist_baseline = mean(Fish_Total, na.rm = TRUE),
    TCB_hist_baseline  = mean(TCB, na.rm = TRUE),
    .groups = 'drop'
  )

baseline_data <- spatial_means_v2 %>%
  left_join(historical_baseline, by = "model") %>%
  mutate(
    Zoop_Change_1990s = (Zooplankton_Total - Zoop_hist_baseline) / Zoop_hist_baseline * 100,
    Fish_Change_1990s = (Fish_Total        - Fish_hist_baseline) / Fish_hist_baseline * 100,
    TCB_Change_1990s  = (TCB               - TCB_hist_baseline)  / TCB_hist_baseline  * 100
  )

# Helper to create plots
create_biomass_plot <- function(data, group_label, y_limits = NULL, suffix = "v2") {
  change_var <- switch(group_label,
    "Total Consumer Biomass" = "TCB_Change_1990s",
    "Zooplankton" = "Zoop_Change_1990s",
    "Fish" = "Fish_Change_1990s",
    stop("Unknown group label: ", group_label)
  )

  plot_data <- data %>%
    filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
           Year >= 1970,
           model %in% model_names) %>%
    select(Year, scenario, model, all_of(change_var)) %>%
    rename(Change = all_of(change_var)) %>%
    mutate(
      model_label = model_labels[model],
      model_label = factor(model_label, levels = model_labels)
    )

  p <- plot_data %>%
    ggplot(aes(x = Year, y = Change, color = scenario)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "black") +
    geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
    annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
    facet_wrap(~model_label, ncol = 3) +
    scale_color_manual(values = scenario_colors, name = "Scenario") +
    scale_x_continuous(breaks = seq(1980, 2300, 40), minor_breaks = seq(1980, 2300, 20)) +
    labs(
      title = paste(group_label, "Change Relative to Historical 1990-1999 Baseline (", suffix, ")"),
      subtitle = "Percentage change from historical simulation 1990-1999 reference period by Earth System Model",
      x = "Year",
      y = "Change (%)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
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
    ) +
    annotate("text", x = 1994.5, y = Inf, label = "Baseline\n1990-1999",
             vjust = 1.2, hjust = 0.5, size = 3, color = "darkblue", fontface = "bold")

  if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
  return(p)
}

cat("Creating plots (v2)...\n")

# Determine y-limits from v2 data
y_limits_zoop <- range(baseline_data$Zoop_Change_1990s[baseline_data$Year >= 1970 &
                       baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)
y_limits_fish <- range(baseline_data$Fish_Change_1990s[baseline_data$Year >= 1970 &
                       baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)
y_limits_tcb <- range(baseline_data$TCB_Change_1990s[baseline_data$Year >= 1970 &
                      baseline_data$scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over")], na.rm = TRUE)
add_padding <- function(lim, pad = 0.1) { sz <- diff(lim); c(lim[1] - sz*pad, lim[2] + sz*pad) }
y_limits_zoop <- add_padding(y_limits_zoop)
y_limits_fish <- add_padding(y_limits_fish)
y_limits_tcb  <- add_padding(y_limits_tcb)

# 1. Zooplankton
zoop_plot_v2 <- create_biomass_plot(baseline_data, "Zooplankton", y_limits_zoop, suffix = "v2")
ggsave(file.path(fig_dir, "zooplankton_percentage_change_by_model_v2.png"),
       zoop_plot_v2, width = 16, height = 8, dpi = 300, bg = "white")

# 2. Fish
fish_plot_v2 <- create_biomass_plot(baseline_data, "Fish", y_limits_fish, suffix = "v2")
ggsave(file.path(fig_dir, "fish_percentage_change_by_model_v2.png"),
       fish_plot_v2, width = 16, height = 8, dpi = 300, bg = "white")

# 3. TCB
tcb_plot_v2 <- create_biomass_plot(baseline_data, "Total Consumer Biomass", y_limits_tcb, suffix = "v2")
ggsave(file.path(fig_dir, "tcb_percentage_change_by_model_v2.png"),
       tcb_plot_v2, width = 16, height = 8, dpi = 300, bg = "white")

cat("\n=== v2 PLOTS CREATED SUCCESSFULLY ===\n")
cat("Files saved:\n")
cat("- ", file.path(fig_dir, "zooplankton_percentage_change_by_model_v2.png"), "\n", sep = "")
cat("- ", file.path(fig_dir, "fish_percentage_change_by_model_v2.png"), "\n", sep = "")
cat("- ", file.path(fig_dir, "tcb_percentage_change_by_model_v2.png"), "\n", sep = "")
