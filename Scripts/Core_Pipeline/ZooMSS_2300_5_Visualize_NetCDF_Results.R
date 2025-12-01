# ============================================================================
# ZooMSS 2300 - Visualize NetCDF Results
# Creates time series and spatial maps from ISIMIP-compliant NetCDF outputs
# ============================================================================

library(tidyverse)
library(ncdf4)
library(raster)
library(sf)
library(viridis)
library(patchwork)
library(scales)

cat("============================================================\n")
cat("ZooMSS 2300 - NetCDF Visualization\n")
cat("============================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ============================================================================
# Configuration
# ============================================================================

BASE_DIR <- "Output/FishMIP_NetCDF_v2_ISIMIP_compliant"
OUTPUT_DIR <- "Figures/NetCDF_Visualizations_2300"

# Create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
cat("Output directory:", OUTPUT_DIR, "\n\n")

# ESMs and scenarios
ESMS <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
ESM_LABELS <- c("cesm2-waccm" = "CESM2-WACCM", 
                "ipsl-cm6a-lr" = "IPSL-CM6A-LR", 
                "ukesm1-0-ll" = "UKESM1-0-LL")

SCENARIOS <- c("historical", "ssp126", "ssp534-over", "ssp585")
SCENARIO_COLORS <- c("historical" = "grey40",
                     "picontrol" = "grey60",
                     "ssp126" = "#2166AC",
                     "ssp534-over" = "#762A83", 
                     "ssp585" = "#B2182B")

SCENARIO_LABELS <- c("historical" = "Historical",
                     "picontrol" = "PI Control",
                     "ssp126" = "SSP1-2.6",
                     "ssp534-over" = "SSP5-3.4-OS",
                     "ssp585" = "SSP5-8.5")

# Variables to plot
VARIABLES <- c("tcb", "bp30cm", "bp30to90cm", "bp90cm")
VAR_LABELS <- c("tcb" = "Total Consumer Biomass",
                "bp30cm" = "Small Pelagics (<30cm)",
                "bp30to90cm" = "Medium Pelagics (30-90cm)",
                "bp90cm" = "Large Pelagics (>90cm)")
VAR_UNITS <- "g m⁻²"

# ============================================================================
# Helper Functions
# ============================================================================

# Read NetCDF and extract global mean time series
read_nc_timeseries <- function(nc_file, var_name) {
  if (!file.exists(nc_file)) {
    return(NULL)
  }
  
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))
  
  # Get data
  data <- ncvar_get(nc, var_name)
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  time <- ncvar_get(nc, "time")
  
  # Get time units and calculate years
  time_units <- ncatt_get(nc, "time", "units")$value
  # Extract reference year from "days since YYYY-01-01"
  ref_year <- as.numeric(gsub(".*since (\\d{4}).*", "\\1", time_units))
  years <- ref_year + time / 365.25
  
  # Get fill value
  fill_val <- ncatt_get(nc, var_name, "_FillValue")$value
  
  # Replace fill values with NA
  data[abs(data - fill_val) < 1e10] <- NA
  
  # Calculate area weights (cosine of latitude)
  lat_weights <- cos(lat * pi / 180)
  lat_weights_matrix <- matrix(rep(lat_weights, each = length(lon)), 
                                nrow = length(lon), ncol = length(lat))
  
  # Calculate global mean for each time step
  global_means <- numeric(length(time))
  for (t in seq_along(time)) {
    data_t <- data[, , t]
    valid_mask <- !is.na(data_t)
    if (sum(valid_mask) > 0) {
      weighted_sum <- sum(data_t[valid_mask] * lat_weights_matrix[valid_mask])
      weight_sum <- sum(lat_weights_matrix[valid_mask])
      global_means[t] <- weighted_sum / weight_sum
    } else {
      global_means[t] <- NA
    }
  }
  
  return(data.frame(
    Year = round(years),
    Value = global_means
  ))
}

# Read NetCDF and extract spatial data for a specific year
read_nc_spatial <- function(nc_file, var_name, target_year) {
  if (!file.exists(nc_file)) {
    return(NULL)
  }
  
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))
  
  # Get coordinates
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  time <- ncvar_get(nc, "time")
  
  # Get time units and calculate years
  time_units <- ncatt_get(nc, "time", "units")$value
  ref_year <- as.numeric(gsub(".*since (\\d{4}).*", "\\1", time_units))
  years <- round(ref_year + time / 365.25)
  
  # Find target year index
  year_idx <- which(years == target_year)
  if (length(year_idx) == 0) {
    return(NULL)
  }
  
  # Get data for target year
  data <- ncvar_get(nc, var_name, start = c(1, 1, year_idx[1]), count = c(-1, -1, 1))
  
  # Get fill value and replace
  fill_val <- ncatt_get(nc, var_name, "_FillValue")$value
  data[abs(data - fill_val) < 1e10] <- NA
  
  # Create data frame
  expand.grid(Lon = lon, Lat = lat) %>%
    mutate(Value = as.vector(data))
}

# Find all NetCDF files for a variable/esm/scenario combination
find_nc_files <- function(esm, scenario, variable) {
  # Map scenario to folder name
  if (scenario == "picontrol") {
    folders <- c("pre-industrial", "historical", "future")
  } else if (scenario == "historical") {
    folders <- "historical"
  } else {
    folders <- scenario
  }
  
  files <- character()
  for (folder in folders) {
    pattern <- paste0(".*_", variable, "_global_annual_.*\\.nc$")
    folder_path <- file.path(BASE_DIR, esm, folder)
    if (dir.exists(folder_path)) {
      folder_files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
      # For SSP scenarios, exclude picontrol files
      if (scenario != "picontrol") {
        folder_files <- folder_files[!grepl("picontrol", folder_files)]
      } else {
        folder_files <- folder_files[grepl("picontrol", folder_files)]
      }
      files <- c(files, folder_files)
    }
  }
  
  return(files)
}

# ============================================================================
# 1. GLOBAL TIME SERIES PLOTS
# ============================================================================

cat("=== Generating Global Time Series Plots ===\n\n")

# Collect all time series data
all_timeseries <- list()

for (var in VARIABLES) {
  cat("Processing variable:", var, "\n")
  
  var_data <- list()
  
  for (esm in ESMS) {
    for (scenario in c("historical", "picontrol", "ssp126", "ssp534-over", "ssp585")) {
      
      files <- find_nc_files(esm, scenario, var)
      
      if (length(files) > 0) {
        # Read and combine all files for this combination
        scenario_data <- map_dfr(files, function(f) {
          df <- read_nc_timeseries(f, var)
          if (!is.null(df)) {
            df$File <- basename(f)
          }
          return(df)
        })
        
        if (nrow(scenario_data) > 0) {
          scenario_data <- scenario_data %>%
            group_by(Year) %>%
            summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
            mutate(
              ESM = esm,
              Scenario = scenario,
              Variable = var
            )
          
          var_data[[paste(esm, scenario, sep = "_")]] <- scenario_data
        }
      }
    }
  }
  
  all_timeseries[[var]] <- bind_rows(var_data)
}

# Combine all data
timeseries_df <- bind_rows(all_timeseries)

# Save timeseries data
saveRDS(timeseries_df, file.path(OUTPUT_DIR, "global_timeseries_data.rds"))
cat("\nSaved time series data to:", file.path(OUTPUT_DIR, "global_timeseries_data.rds"), "\n\n")

# ============================================================================
# Create Time Series Plots
# ============================================================================

# Plot 1: TCB by ESM (faceted by scenario)
cat("Creating TCB time series plot...\n")

tcb_data <- timeseries_df %>%
  filter(Variable == "tcb", Scenario != "picontrol") %>%
  mutate(
    ESM_Label = ESM_LABELS[ESM],
    Scenario_Label = SCENARIO_LABELS[Scenario]
  )

p1 <- ggplot(tcb_data, aes(x = Year, y = Value, color = Scenario, linetype = ESM_Label)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  scale_color_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_x_continuous(breaks = seq(1850, 2300, 50)) +
  labs(
    title = "Global Total Consumer Biomass (1850-2300)",
    subtitle = "ZooMSS FishMIP Projections",
    x = "Year",
    y = expression("Total Consumer Biomass (g m"^-2*")"),
    color = "Scenario",
    linetype = "Earth System Model"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

ggsave(file.path(OUTPUT_DIR, "01_TCB_timeseries_all.png"), p1, 
       width = 12, height = 7, dpi = 300)
cat("  Saved: 01_TCB_timeseries_all.png\n")

# Plot 2: TCB faceted by ESM
p2 <- ggplot(tcb_data, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ ESM_Label, ncol = 1, scales = "free_y") +
  scale_color_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_x_continuous(breaks = seq(1850, 2300, 50)) +
  labs(
    title = "Global Total Consumer Biomass by Earth System Model",
    subtitle = "ZooMSS FishMIP Projections (1850-2300)",
    x = "Year",
    y = expression("Total Consumer Biomass (g m"^-2*")"),
    color = "Scenario"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "02_TCB_timeseries_by_ESM.png"), p2, 
       width = 10, height = 10, dpi = 300)
cat("  Saved: 02_TCB_timeseries_by_ESM.png\n")

# Plot 3: All size classes for one ESM (IPSL example)
cat("Creating size class time series plot...\n")

size_data <- timeseries_df %>%
  filter(ESM == "ipsl-cm6a-lr", Scenario %in% c("historical", "ssp126", "ssp585")) %>%
  mutate(
    Variable_Label = VAR_LABELS[Variable],
    Scenario_Label = SCENARIO_LABELS[Scenario]
  )

p3 <- ggplot(size_data, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Variable_Label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_x_continuous(breaks = seq(1850, 2300, 100)) +
  labs(
    title = "Biomass by Size Class - IPSL-CM6A-LR",
    subtitle = "ZooMSS FishMIP Projections (1850-2300)",
    x = "Year",
    y = expression("Biomass Density (g m"^-2*")"),
    color = "Scenario"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "03_Size_classes_IPSL.png"), p3, 
       width = 10, height = 8, dpi = 300)
cat("  Saved: 03_Size_classes_IPSL.png\n")

# Plot 4: Percentage change from historical baseline (1985-2014)
cat("Creating percentage change plot...\n")

baseline_period <- 1985:2014

# Calculate baseline from HISTORICAL scenario only, then join to future scenarios
historical_baselines <- timeseries_df %>%
  filter(Variable == "tcb", Scenario == "historical", Year %in% baseline_period) %>%
  group_by(ESM) %>%
  summarise(Baseline = mean(Value, na.rm = TRUE), .groups = "drop")

pct_change_data <- timeseries_df %>%
  filter(Variable == "tcb", Scenario %in% c("ssp126", "ssp534-over", "ssp585")) %>%
  left_join(historical_baselines, by = "ESM") %>%
  mutate(
    Pct_Change = (Value - Baseline) / Baseline * 100,
    ESM_Label = ESM_LABELS[ESM],
    Scenario_Label = SCENARIO_LABELS[Scenario]
  )

p4 <- ggplot(pct_change_data %>% filter(Scenario != "historical"), 
             aes(x = Year, y = Pct_Change, color = Scenario, linetype = ESM_Label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_x_continuous(breaks = seq(2020, 2300, 40)) +
  labs(
    title = "Projected Change in Total Consumer Biomass",
    subtitle = "Relative to 1985-2014 baseline",
    x = "Year",
    y = "Change from baseline (%)",
    color = "Scenario",
    linetype = "Earth System Model"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "04_TCB_percentage_change.png"), p4, 
       width = 12, height = 7, dpi = 300)
cat("  Saved: 04_TCB_percentage_change.png\n")

# Plot 5: Multi-model mean with uncertainty ribbon
cat("Creating multi-model mean plot...\n")

mmm_data <- timeseries_df %>%
  filter(Variable == "tcb", Scenario %in% c("historical", "ssp126", "ssp585")) %>%
  group_by(Year, Scenario) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Scenario_Label = SCENARIO_LABELS[Scenario])

p5 <- ggplot(mmm_data, aes(x = Year, y = Mean, color = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_fill_manual(values = SCENARIO_COLORS, labels = SCENARIO_LABELS) +
  scale_x_continuous(breaks = seq(1850, 2300, 50)) +
  labs(
    title = "Multi-Model Mean Total Consumer Biomass",
    subtitle = "Shaded area shows range across 3 Earth System Models",
    x = "Year",
    y = expression("Total Consumer Biomass (g m"^-2*")"),
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "05_TCB_multimodel_mean.png"), p5, 
       width = 12, height = 7, dpi = 300)
cat("  Saved: 05_TCB_multimodel_mean.png\n")

# ============================================================================
# 2. SPATIAL MAPS
# ============================================================================

cat("\n=== Generating Spatial Maps ===\n\n")

# Get world map
world <- map_data("world")

# Target years for maps
map_years <- c(2000, 2100, 2200, 2300)

# Function to create spatial map
create_spatial_map <- function(data, title, subtitle = NULL) {
  ggplot() +
    geom_tile(data = data %>% filter(!is.na(Value)), 
              aes(x = Lon, y = Lat, fill = Value)) +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "grey70", color = "grey40", linewidth = 0.1) +
    scale_fill_viridis_c(option = "viridis", name = expression("g m"^-2),
                         trans = "log10",
                         labels = scales::label_number()) +
    coord_fixed(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
    labs(title = title, subtitle = subtitle, x = "Longitude", y = "Latitude") +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      panel.grid = element_blank()
    )
}

# Create maps for each ESM and scenario at key years
for (esm in ESMS) {
  cat("Creating maps for", esm, "...\n")
  
  for (scenario in c("ssp126", "ssp585")) {
    
    # Find files for this scenario
    files <- find_nc_files(esm, scenario, "tcb")
    
    # Get 2100 and 2300 maps
    for (target_year in c(2100, 2300)) {
      
      # Find the right file
      for (f in files) {
        spatial_data <- read_nc_spatial(f, "tcb", target_year)
        if (!is.null(spatial_data) && nrow(spatial_data) > 0) {
          break
        }
      }
      
      if (!is.null(spatial_data) && nrow(spatial_data) > 0) {
        p <- create_spatial_map(
          spatial_data,
          paste0("Total Consumer Biomass - ", ESM_LABELS[esm]),
          paste0(SCENARIO_LABELS[scenario], " (", target_year, ")")
        )
        
        fname <- paste0("Map_TCB_", esm, "_", scenario, "_", target_year, ".png")
        ggsave(file.path(OUTPUT_DIR, fname), p, width = 10, height = 5, dpi = 300)
        cat("  Saved:", fname, "\n")
      }
    }
  }
}

# Create difference maps (2300 vs 2000 baseline)
cat("\nCreating difference maps...\n")

for (esm in ESMS) {
  cat("Creating difference maps for", esm, "...\n")
  
  # Get historical 2000 data as baseline
  hist_files <- find_nc_files(esm, "historical", "tcb")
  baseline_data <- NULL
  for (f in hist_files) {
    baseline_data <- read_nc_spatial(f, "tcb", 2000)
    if (!is.null(baseline_data)) break
  }
  
  if (is.null(baseline_data)) {
    cat("  Warning: Could not find baseline data for", esm, "\n")
    next
  }
  
  for (scenario in c("ssp126", "ssp585")) {
    
    # Get 2300 data
    files <- find_nc_files(esm, scenario, "tcb")
    future_data <- NULL
    for (f in files) {
      future_data <- read_nc_spatial(f, "tcb", 2300)
      if (!is.null(future_data)) break
    }
    
    if (!is.null(future_data)) {
      # Calculate difference
      diff_data <- baseline_data %>%
        left_join(future_data %>% rename(Future = Value), 
                  by = c("Lon", "Lat")) %>%
        mutate(
          Diff = Future - Value,
          Pct_Diff = (Future - Value) / Value * 100
        )
      
      # Create difference map
      p_diff <- ggplot() +
        geom_tile(data = diff_data %>% filter(!is.na(Pct_Diff)), 
                  aes(x = Lon, y = Lat, fill = Pct_Diff)) +
        geom_polygon(data = world, aes(x = long, y = lat, group = group),
                     fill = "grey70", color = "grey40", linewidth = 0.1) +
        scale_fill_gradient2(
          low = "#B2182B", mid = "white", high = "#2166AC",
          midpoint = 0, 
          name = "Change (%)",
          limits = c(-100, 100),
          oob = scales::squish
        ) +
        coord_fixed(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
        labs(
          title = paste0("Change in Total Consumer Biomass - ", ESM_LABELS[esm]),
          subtitle = paste0(SCENARIO_LABELS[scenario], ": 2300 vs 2000 baseline"),
          x = "Longitude", y = "Latitude"
        ) +
        theme_bw(base_size = 10) +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          panel.grid = element_blank()
        )
      
      fname <- paste0("Map_TCB_Diff_", esm, "_", scenario, "_2300vs2000.png")
      ggsave(file.path(OUTPUT_DIR, fname), p_diff, width = 10, height = 5, dpi = 300)
      cat("  Saved:", fname, "\n")
    }
  }
}

# ============================================================================
# 3. SUMMARY PANEL PLOTS
# ============================================================================

cat("\n=== Creating Summary Panel Plots ===\n\n")

# Panel 1: All ESMs and scenarios in one view
cat("Creating summary panels...\n")

summary_data <- timeseries_df %>%
  filter(Variable == "tcb", Scenario %in% c("historical", "ssp126", "ssp534-over", "ssp585")) %>%
  mutate(
    ESM_Label = ESM_LABELS[ESM],
    Scenario_Label = factor(SCENARIO_LABELS[Scenario], 
                            levels = c("Historical", "SSP1-2.6", "SSP5-3.4-OS", "SSP5-8.5"))
  )

p_panel <- ggplot(summary_data, aes(x = Year, y = Value, color = Scenario_Label)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ ESM_Label, ncol = 1) +
  scale_color_manual(values = c("Historical" = "grey40", 
                                "SSP1-2.6" = "#2166AC",
                                "SSP5-3.4-OS" = "#762A83",
                                "SSP5-8.5" = "#B2182B")) +
  scale_x_continuous(breaks = seq(1850, 2300, 50)) +
  labs(
    title = "ZooMSS Global Total Consumer Biomass Projections (1850-2300)",
    subtitle = "FishMIP ISIMIP3b Protocol",
    x = "Year",
    y = expression("Total Consumer Biomass (g m"^-2*")"),
    color = "Scenario"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "06_Summary_all_ESMs_scenarios.png"), p_panel, 
       width = 12, height = 10, dpi = 300)
cat("  Saved: 06_Summary_all_ESMs_scenarios.png\n")

# Panel 2: End-century values summary
cat("Creating end-century summary...\n")

end_values <- timeseries_df %>%
  filter(Variable == "tcb", 
         Year %in% c(2100, 2200, 2300),
         Scenario %in% c("ssp126", "ssp585")) %>%
  mutate(
    ESM_Label = ESM_LABELS[ESM],
    Scenario_Label = SCENARIO_LABELS[Scenario],
    Year = factor(Year)
  )

p_bars <- ggplot(end_values, aes(x = ESM_Label, y = Value, fill = Scenario_Label)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ Year, nrow = 1) +
  scale_fill_manual(values = c("SSP1-2.6" = "#2166AC", "SSP5-8.5" = "#B2182B")) +
  labs(
    title = "Total Consumer Biomass at Key Time Points",
    subtitle = "Comparison across Earth System Models and Scenarios",
    x = "Earth System Model",
    y = expression("Total Consumer Biomass (g m"^-2*")"),
    fill = "Scenario"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(file.path(OUTPUT_DIR, "07_End_century_comparison.png"), p_bars, 
       width = 12, height = 6, dpi = 300)
cat("  Saved: 07_End_century_comparison.png\n")

# ============================================================================
# Summary Statistics
# ============================================================================

cat("\n=== Summary Statistics ===\n\n")

# Calculate historical baselines from the historical scenario
hist_baselines <- timeseries_df %>%
  filter(Variable == "tcb", Scenario == "historical", Year %in% 1985:2014) %>%
  group_by(ESM) %>%
  summarise(Baseline = mean(Value, na.rm = TRUE), .groups = "drop")

# Get future values
future_values <- timeseries_df %>%
  filter(Variable == "tcb", Scenario %in% c("ssp126", "ssp585"), Year %in% c(2100, 2300)) %>%
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "Year_") %>%
  left_join(hist_baselines, by = "ESM") %>%
  mutate(
    `Change 2100 (%)` = (Year_2100 - Baseline) / Baseline * 100,
    `Change 2300 (%)` = (Year_2300 - Baseline) / Baseline * 100
  ) %>%
  rename(
    `Historical Mean (1985-2014)` = Baseline,
    `2100 Value` = Year_2100,
    `2300 Value` = Year_2300
  ) %>%
  dplyr::select(ESM, Scenario, `Historical Mean (1985-2014)`, `2100 Value`, `2300 Value`, 
         `Change 2100 (%)`, `Change 2300 (%)`)

print(future_values)

# Save statistics
write_csv(future_values, file.path(OUTPUT_DIR, "summary_statistics.csv"))
cat("\nSaved summary statistics to:", file.path(OUTPUT_DIR, "summary_statistics.csv"), "\n")

# ============================================================================
# Complete
# ============================================================================

cat("\n============================================================\n")
cat("Visualization Complete!\n")
cat("============================================================\n")
cat("\nOutput files saved to:", OUTPUT_DIR, "\n")
cat("\nGenerated plots:\n")
list.files(OUTPUT_DIR, pattern = "\\.png$") %>% 
  walk(~ cat("  -", .x, "\n"))
cat("\n")
