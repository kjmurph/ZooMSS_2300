# ==============================================================================
# MULTI-MODEL ENSEMBLE SPATIAL ANALYSIS
# ==============================================================================
# Purpose: Create ensemble statistics (mean, median, variability) for SSP scenarios
# Version: Updated for QAQC run
# ==============================================================================

library(tidyverse)
library(raster)
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
}

cat("=== MULTI-MODEL ENSEMBLE SPATIAL ANALYSIS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# ENHANCED SPATIAL PLOTTING FUNCTION
# ==============================================================================

create_ensemble_plot <- function(data, variable, title, subtitle = "", 
                                color_scale = "RdBu", symmetric = TRUE,
                                limits = NULL) {
  
  # Get world map with enhanced coastline
  world_map <- map_data("world")
  
  cat("Plotting", nrow(data), "grid cells for", variable, "\n")
  cat("Data range:", min(data[[variable]], na.rm=TRUE), "to", max(data[[variable]], na.rm=TRUE), "\n")
  
  # Create base plot with enhanced Tittensor-style theme
  p <- ggplot() +
    # Add data tiles (fill grid cells with color scale)
    geom_tile(data = data, aes(x = Lon, y = Lat, fill = !!sym(variable))) +
    # Add enhanced world map on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "gray20", color = "white", linewidth = 0.15, alpha = 0.8) +
    # Use standard coordinate system
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-85, 85)) +
    # Enhanced themes and labels
    labs(
      title = title, 
      subtitle = subtitle, 
      x = "", 
      y = "",
      fill = if(variable %in% c("mean_change", "median_change")) "Change (%)" else 
             if(variable == "sd_change") "Std Dev (%)" else 
             if(variable == "iqr_change") "IQR (%)" else
             if(variable == "cv_change") "Coeff Var" else 
             if(variable == "agreement_pct") "Agreement (%)" else
             if(variable == "range_change") "Range (%)" else "Value"
    ) +
    theme_void() +
    theme(
      # Panel and plot styling
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Text styling following Tittensor et al. conventions
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold", 
                               margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30",
                                  margin = margin(b = 10)),
      
      # Enhanced legend styling
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.margin = margin(t = 10),
      legend.box.margin = margin(t = 5),
      
      # Plot margins
      plot.margin = margin(5, 10, 5, 10),
      
      # Remove axis elements for cleaner map appearance
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
  
  # Apply color scales based on variable type
  if (symmetric && is.null(limits)) {
    # For percentage change variables, use ±100% limits
    if (variable %in% c("mean_change", "median_change")) {
      limits <- c(-100, 100)
    } else if (variable %in% c("sd_change", "iqr_change", "range_change")) {
      # For variability metrics, use 0 to max
      max_val <- max(abs(data[[variable]]), na.rm = TRUE)
      limits <- c(0, max_val)
      symmetric <- FALSE
    } else if (variable == "cv_change") {
      # For coefficient of variation, use 0 to reasonable max
      max_val <- min(quantile(data[[variable]], 0.95, na.rm = TRUE), 2)
      limits <- c(0, max_val)
      symmetric <- FALSE
    } else if (variable == "agreement_pct") {
      # For agreement percentage, use 33% to 100%
      limits <- c(33, 100)
      symmetric <- FALSE
    }
  }
  
  if (color_scale == "RdBu" && symmetric) {
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
  } else if (color_scale == "viridis" || !symmetric) {
    # For variability metrics, use single-direction scale
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
  } else {
    # Default RdYlBu
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
  }
  
  return(p)
}

# ==============================================================================
# LOAD AND PROCESS DATA FOR ENSEMBLE ANALYSIS
# ==============================================================================

# Get list of biomass files for SSP scenarios
biomass_files <- list.files(input_dir, pattern = "ZooMSS_Biomass_2300.*\\.rds$", full.names = TRUE)

# Select files for ensemble analysis (SSP1-2.6 and SSP5-8.5)
ssp126_files <- grep("ssp126", biomass_files, value = TRUE)
ssp585_files <- grep("ssp585", biomass_files, value = TRUE)
historical_files <- grep("historical", biomass_files, value = TRUE)

cat("Found files for ensemble analysis:\n")
cat("SSP1-2.6:", length(ssp126_files), "files\n")
cat("SSP5-8.5:", length(ssp585_files), "files\n")
cat("Historical:", length(historical_files), "files\n")

# Function to load and process data for ensemble
load_ensemble_data <- function(file_path, time_slice) {
  filename <- basename(file_path)
  
  # Extract model from filename
  if (grepl("cesm2-waccm", filename)) {
    model <- "cesm2-waccm"
  } else if (grepl("ipsl-cm6a-lr", filename)) {
    model <- "ipsl-cm6a-lr"  
  } else if (grepl("ukesm1-0-ll", filename)) {
    model <- "ukesm1-0-ll"
  } else {
    model <- "unknown"
  }
  
  # Extract scenario
  if (grepl("historical", filename)) {
    scenario <- "historical"
  } else if (grepl("ssp126", filename)) {
    scenario <- "ssp126"
  } else if (grepl("ssp585", filename)) {
    scenario <- "ssp585"
  } else {
    scenario <- "unknown"
  }
  
  cat("Loading:", model, scenario, "for", time_slice, "\n")
  
  # Load data
  data <- readRDS(file_path)
  
  # Add metadata
  data$model <- model
  data$scenario <- scenario
  
  # Filter to specific time slice
  if (time_slice == "recent") {
    data <- data %>% filter(Date >= 2090 & Date <= 2099)
  } else if (time_slice == "future") {
    data <- data %>% filter(Date >= 2290 & Date <= 2299)
  } else if (time_slice == "historical") {
    data <- data %>% filter(Date >= 1990 & Date <= 1999)
  }
  
  # Calculate spatial means for each grid cell
  spatial_means <- data %>%
    group_by(Lon, Lat, model, scenario) %>%
    summarise(
      Zooplankton_Total = mean(Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                              CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish, na.rm = TRUE),
      Fish_Total = mean(Fish_Small + Fish_Med + Fish_Large, na.rm = TRUE),
      TCB = mean(Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish +
                Fish_Small + Fish_Med + Fish_Large, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("  Processed", nrow(spatial_means), "grid cells\n")
  return(spatial_means)
}

# Load historical baseline for all models
cat("\nLoading historical baseline data...\n")
historical_data <- map_dfr(historical_files, ~load_ensemble_data(.x, "historical"))

# Load SSP scenario data
cat("\nLoading SSP1-2.6 recent data (2090s)...\n")
ssp126_recent <- map_dfr(ssp126_files, ~load_ensemble_data(.x, "recent"))

cat("\nLoading SSP1-2.6 future data (2290s)...\n")
ssp126_future <- map_dfr(ssp126_files, ~load_ensemble_data(.x, "future"))

cat("\nLoading SSP5-8.5 recent data (2090s)...\n")
ssp585_recent <- map_dfr(ssp585_files, ~load_ensemble_data(.x, "recent"))

cat("\nLoading SSP5-8.5 future data (2290s)...\n")
ssp585_future <- map_dfr(ssp585_files, ~load_ensemble_data(.x, "future"))

# ==============================================================================
# CALCULATE ENSEMBLE STATISTICS
# ==============================================================================

cat("\nCalculating ensemble statistics...\n")

# Function to calculate ensemble statistics for biomass changes
calculate_ensemble_stats <- function(scenario_data, historical_baseline, period_name) {
  
  cat("Processing", period_name, "ensemble statistics...\n")
  
  # Calculate changes relative to historical baseline
  changes <- scenario_data %>%
    left_join(
      historical_baseline %>% 
        filter(scenario == "historical") %>%
        dplyr::select(Lon, Lat, model, TCB_historical = TCB),
      by = c("Lon", "Lat", "model")
    ) %>%
    filter(!is.na(TCB_historical) & !is.na(TCB)) %>%
    mutate(
      TCB_Change = (TCB - TCB_historical) / TCB_historical * 100
    ) %>%
    dplyr::select(Lon, Lat, model, scenario, TCB_Change)
  
  # Calculate ensemble statistics by grid cell
  ensemble_stats <- changes %>%
    group_by(Lon, Lat, scenario) %>%
    summarise(
      n_models = n(),
      mean_change = mean(TCB_Change, na.rm = TRUE),
      median_change = median(TCB_Change, na.rm = TRUE),
      sd_change = sd(TCB_Change, na.rm = TRUE),
      min_change = min(TCB_Change, na.rm = TRUE),
      max_change = max(TCB_Change, na.rm = TRUE),
      q25_change = quantile(TCB_Change, 0.25, na.rm = TRUE),
      q75_change = quantile(TCB_Change, 0.75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Only keep grid cells with data from all 3 models
    filter(n_models == 3) %>%
    # Calculate additional variability metrics
    mutate(
      # Coefficient of variation (normalized variability)
      cv_change = ifelse(abs(mean_change) > 1, abs(sd_change / mean_change), NA),
      cv_change = ifelse(is.infinite(cv_change) | cv_change > 3, NA, cv_change),
      
      # Inter-quartile range (robust measure of spread)
      iqr_change = q75_change - q25_change,
      
      # Range (max - min)
      range_change = max_change - min_change,
      
      period = period_name
    ) %>%
    # Calculate model agreement on direction separately
    left_join(
      changes %>%
        group_by(Lon, Lat, scenario) %>%
        summarise(
          # Count models with positive/negative/neutral changes
          pos_models = sum(TCB_Change > 5, na.rm = TRUE),   # > +5% increase
          neg_models = sum(TCB_Change < -5, na.rm = TRUE),  # > -5% decrease
          neutral_models = sum(abs(TCB_Change) <= 5, na.rm = TRUE), # within ±5%
          n_models_agreement = n(),
          .groups = 'drop'
        ) %>%
        mutate(
          # Agreement percentage (models agreeing on major direction)
          agreement_pct = pmax(pos_models, neg_models) / n_models_agreement * 100,
          
          # Uncertainty category based on IQR
          uncertainty_category = "Medium"  # Default, will be updated based on IQR
        ),
      by = c("Lon", "Lat", "scenario")
    ) %>%
    # Update uncertainty category based on IQR  
    mutate(
      uncertainty_category = case_when(
        iqr_change <= 10 ~ "Low",
        iqr_change <= 30 ~ "Medium", 
        iqr_change <= 60 ~ "High",
        TRUE ~ "Very High"
      )
    )
  
  cat("  Calculated ensemble stats for", nrow(ensemble_stats), "grid cells\n")
  return(ensemble_stats)
}

# Calculate ensemble statistics for each scenario and time period
ssp126_recent_ensemble <- calculate_ensemble_stats(ssp126_recent, historical_data, "recent")
ssp126_future_ensemble <- calculate_ensemble_stats(ssp126_future, historical_data, "future")
ssp585_recent_ensemble <- calculate_ensemble_stats(ssp585_recent, historical_data, "recent")
ssp585_future_ensemble <- calculate_ensemble_stats(ssp585_future, historical_data, "future")

# ==============================================================================
# CREATE ENSEMBLE PLOTS
# ==============================================================================

cat("\nCreating ensemble plots...\n")

# Function to create 3-panel ensemble plot
create_ensemble_comparison <- function(ensemble_data, scenario_name, period_name) {
  
  # Panel 1: Multi-model mean
  p1 <- create_ensemble_plot(
    ensemble_data, "mean_change",
    "Multi-Model Mean",
    paste("TCB change:", scenario_name, "|", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  # Panel 2: Multi-model median  
  p2 <- create_ensemble_plot(
    ensemble_data, "median_change",
    "Multi-Model Median", 
    paste("TCB change:", scenario_name, "|", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  # Panel 3: Inter-model spread (Inter-Quartile Range - more interpretable)
  p3 <- create_ensemble_plot(
    ensemble_data, "iqr_change",
    "Inter-Model Spread (IQR)",
    paste("IQR spread:", scenario_name, "|", period_name),
    color_scale = "viridis", symmetric = FALSE
  )
  
  # Combine panels
  combined <- (p1 | p2 | p3) +
    plot_annotation(
      title = paste("Multi-Model Ensemble Analysis:", scenario_name, "Scenario"),
      subtitle = paste("Total Consumer Biomass change by", period_name, "vs 1990-1999 baseline"),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))
      )
    )
  
  return(combined)
}

# New function for alternative uncertainty metrics
create_uncertainty_comparison <- function(ensemble_data, scenario_name, period_name) {
  
  # Panel 1: Model Agreement (% of models agreeing on direction)
  p1 <- create_ensemble_plot(
    ensemble_data, "agreement_pct",
    "Model Agreement",
    paste("% Agreement on direction:", scenario_name, "|", period_name),
    color_scale = "viridis", symmetric = FALSE, limits = c(33, 100)
  )
  
  # Panel 2: Range (Max - Min)
  p2 <- create_ensemble_plot(
    ensemble_data, "range_change", 
    "Model Range",
    paste("Max-Min range:", scenario_name, "|", period_name),
    color_scale = "plasma", symmetric = FALSE
  )
  
  # Panel 3: Coefficient of Variation (normalized uncertainty)
  p3 <- ensemble_data %>%
    filter(!is.na(cv_change) & cv_change <= 2) %>%  # Filter extreme values
    ggplot(aes(x = Lon, y = Lat)) +
    geom_tile(aes(fill = cv_change)) +
    scale_fill_viridis_c(
      option = "inferno", na.value = "grey90",
      name = "CV",
      trans = "sqrt"
    ) +
    coord_fixed(xlim = c(-180, 180), ylim = c(-90, 90)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.8, "cm"),
      panel.grid = element_blank()
    ) +
    labs(
      title = "Coefficient of Variation",
      subtitle = paste("Normalized uncertainty:", scenario_name, "|", period_name)
    )
  
  # Combine panels
  combined <- (p1 | p2 | p3) +
    plot_annotation(
      title = paste("Uncertainty Metrics:", scenario_name, "Scenario"),
      subtitle = paste("Alternative measures of inter-model uncertainty by", period_name),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))
      )
    )
  
  return(combined)
}

# New function for custom ensemble comparison (mean, CV, agreement)
create_custom_ensemble_comparison <- function(ssp126_data, ssp585_data, period_name) {
  
  # SSP1-2.6 Mean
  p1 <- create_ensemble_plot(
    ssp126_data, "mean_change",
    "SSP1-2.6: Multi-Model Mean",
    paste("Mean TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  # SSP1-2.6 CV
  p2 <- ssp126_data %>%
    filter(!is.na(cv_change) & cv_change <= 2) %>%
    ggplot(aes(x = Lon, y = Lat)) +
    geom_tile(aes(fill = cv_change)) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
                 fill = "gray20", color = "white", linewidth = 0.15, alpha = 0.8) +
    scale_fill_viridis_c(
      option = "inferno", na.value = "grey90",
      name = "CV",
      trans = "sqrt",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-85, 85)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold", margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.margin = margin(t = 10),
      plot.margin = margin(5, 10, 5, 10)
    ) +
    labs(
      title = "SSP1-2.6: Coefficient of Variation",
      subtitle = paste("Normalized uncertainty by", period_name)
    )
  
  # SSP1-2.6 Agreement
  p3 <- create_ensemble_plot(
    ssp126_data, "agreement_pct",
    "SSP1-2.6: Model Agreement",
    paste("% models agreeing on direction by", period_name),
    color_scale = "viridis", symmetric = FALSE, limits = c(33, 100)
  )
  
  # SSP5-8.5 Mean
  p4 <- create_ensemble_plot(
    ssp585_data, "mean_change",
    "SSP5-8.5: Multi-Model Mean",
    paste("Mean TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  # SSP5-8.5 CV
  p5 <- ssp585_data %>%
    filter(!is.na(cv_change) & cv_change <= 2) %>%
    ggplot(aes(x = Lon, y = Lat)) +
    geom_tile(aes(fill = cv_change)) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
                 fill = "gray20", color = "white", linewidth = 0.15, alpha = 0.8) +
    scale_fill_viridis_c(
      option = "inferno", na.value = "grey90",
      name = "CV",
      trans = "sqrt",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-85, 85)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold", margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.margin = margin(t = 10),
      plot.margin = margin(5, 10, 5, 10)
    ) +
    labs(
      title = "SSP5-8.5: Coefficient of Variation",
      subtitle = paste("Normalized uncertainty by", period_name)
    )
  
  # SSP5-8.5 Agreement
  p6 <- create_ensemble_plot(
    ssp585_data, "agreement_pct",
    "SSP5-8.5: Model Agreement",
    paste("% models agreeing on direction by", period_name),
    color_scale = "viridis", symmetric = FALSE, limits = c(33, 100)
  )
  
  # Combine in 2x3 layout
  combined <- (p1 | p2 | p3) / (p4 | p5 | p6) +
    plot_annotation(
      title = paste("Multi-Model Ensemble Comparison:", period_name),
      subtitle = "Mean Change, Uncertainty (CV), and Model Agreement",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))
      )
    )
  
  return(combined)
}

# Create ensemble plots for each scenario and time period

# SSP1-2.6 Recent (2090s)
cat("Creating SSP1-2.6 recent ensemble plot...\n")
ssp126_recent_plot <- create_ensemble_comparison(ssp126_recent_ensemble, "SSP1-2.6", "2090s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_ssp126_recent_comparison.png"),
       ssp126_recent_plot, width = 18, height = 8, dpi = 300, bg = "white")

# SSP1-2.6 Future (2290s)
cat("Creating SSP1-2.6 future ensemble plot...\n")
ssp126_future_plot <- create_ensemble_comparison(ssp126_future_ensemble, "SSP1-2.6", "2290s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_ssp126_future_comparison.png"),
       ssp126_future_plot, width = 18, height = 8, dpi = 300, bg = "white")

# SSP5-8.5 Recent (2090s)
cat("Creating SSP5-8.5 recent ensemble plot...\n")
ssp585_recent_plot <- create_ensemble_comparison(ssp585_recent_ensemble, "SSP5-8.5", "2090s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_ssp585_recent_comparison.png"),
       ssp585_recent_plot, width = 18, height = 8, dpi = 300, bg = "white")

# SSP5-8.5 Future (2290s)
cat("Creating SSP5-8.5 future ensemble plot...\n")
ssp585_future_plot <- create_ensemble_comparison(ssp585_future_ensemble, "SSP5-8.5", "2290s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_ssp585_future_comparison.png"),
       ssp585_future_plot, width = 18, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# CREATE COMBINED SCENARIO COMPARISON
# ==============================================================================

cat("\nCreating combined scenario comparison...\n")

# Create a 2x3 comparison showing both scenarios side by side
create_scenario_comparison <- function(ssp126_data, ssp585_data, period_name) {
  
  # SSP1-2.6 plots
  p1_126 <- create_ensemble_plot(
    ssp126_data, "mean_change",
    "SSP1-2.6: Multi-Model Mean",
    paste("TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  p2_126 <- create_ensemble_plot(
    ssp126_data, "median_change",
    "SSP1-2.6: Multi-Model Median",
    paste("TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  p3_126 <- create_ensemble_plot(
    ssp126_data, "sd_change",
    "SSP1-2.6: Inter-Model Variability",
    paste("Standard deviation by", period_name),
    color_scale = "viridis", symmetric = FALSE
  )
  
  # SSP5-8.5 plots
  p1_585 <- create_ensemble_plot(
    ssp585_data, "mean_change",
    "SSP5-8.5: Multi-Model Mean",
    paste("TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  p2_585 <- create_ensemble_plot(
    ssp585_data, "median_change",
    "SSP5-8.5: Multi-Model Median",
    paste("TCB change by", period_name),
    color_scale = "RdBu", symmetric = TRUE
  )
  
  p3_585 <- create_ensemble_plot(
    ssp585_data, "sd_change",
    "SSP5-8.5: Inter-Model Variability",
    paste("Standard deviation by", period_name),
    color_scale = "viridis", symmetric = FALSE
  )
  
  # Combine in 2x3 layout
  combined <- (p1_126 | p1_585) / (p2_126 | p2_585) / (p3_126 | p3_585) +
    plot_annotation(
      title = paste("Multi-Model Ensemble Comparison:", period_name),
      subtitle = "Total Consumer Biomass changes vs 1990-1999 baseline",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20))
      )
    )
  
  return(combined)
}

# Create combined scenario comparisons
recent_comparison <- create_scenario_comparison(ssp126_recent_ensemble, ssp585_recent_ensemble, "2090s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_scenarios_recent_comparison.png"),
       recent_comparison, width = 16, height = 18, dpi = 300, bg = "white")

future_comparison <- create_scenario_comparison(ssp126_future_ensemble, ssp585_future_ensemble, "2290s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_scenarios_future_comparison.png"),
       future_comparison, width = 16, height = 18, dpi = 300, bg = "white")

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

cat("\nCalculating summary statistics...\n")

# Combine all ensemble data for summary
all_ensemble_stats <- bind_rows(
  ssp126_recent_ensemble %>% mutate(scenario = "ssp126", period = "recent"),
  ssp126_future_ensemble %>% mutate(scenario = "ssp126", period = "future"),
  ssp585_recent_ensemble %>% mutate(scenario = "ssp585", period = "recent"),
  ssp585_future_ensemble %>% mutate(scenario = "ssp585", period = "future")
)

# Calculate global summary statistics
global_summary <- all_ensemble_stats %>%
  group_by(scenario, period) %>%
  summarise(
    n_cells = n(),
    global_mean_change = mean(mean_change, na.rm = TRUE),
    global_median_change = mean(median_change, na.rm = TRUE),
    mean_variability = mean(sd_change, na.rm = TRUE),
    median_variability = median(sd_change, na.rm = TRUE),
    high_agreement_cells = sum(sd_change < 10, na.rm = TRUE),
    low_agreement_cells = sum(sd_change > 30, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    pct_high_agreement = round(high_agreement_cells / n_cells * 100, 1),
    pct_low_agreement = round(low_agreement_cells / n_cells * 100, 1)
  )

write.csv(global_summary, paste0(figure_dir, "/QAQC_ensemble_summary_statistics.csv"), row.names = FALSE)

cat("\n=== MULTI-MODEL ENSEMBLE ANALYSIS COMPLETE ===\n")
cat("Individual scenario plots saved:\n")
cat("- QAQC_ensemble_ssp126_recent_comparison.png\n")
cat("- QAQC_ensemble_ssp126_future_comparison.png\n")
cat("- QAQC_ensemble_ssp585_recent_comparison.png\n")
cat("- QAQC_ensemble_ssp585_future_comparison.png\n")
cat("\nCombined scenario plots saved:\n")
cat("- QAQC_ensemble_scenarios_recent_comparison.png\n")
cat("- QAQC_ensemble_scenarios_future_comparison.png\n")

# Create enhanced uncertainty plots with alternative metrics
cat("\nCreating enhanced uncertainty plots...\n")

# SSP1-2.6 Future uncertainty
ssp126_future_uncertainty <- create_uncertainty_comparison(ssp126_future_ensemble, "SSP1-2.6", "2290s")
ggsave(paste0(figure_dir, "/QAQC_uncertainty_ssp126_future.png"),
       ssp126_future_uncertainty, width = 18, height = 8, dpi = 300, bg = "white")

# SSP5-8.5 Future uncertainty
ssp585_future_uncertainty <- create_uncertainty_comparison(ssp585_future_ensemble, "SSP5-8.5", "2290s")
ggsave(paste0(figure_dir, "/QAQC_uncertainty_ssp585_future.png"),
       ssp585_future_uncertainty, width = 18, height = 8, dpi = 300, bg = "white")

cat("\nEnhanced uncertainty plots saved:\n")
cat("- QAQC_uncertainty_ssp126_future.png\n")
cat("- QAQC_uncertainty_ssp585_future.png\n")

# Create custom ensemble comparison (mean, CV, agreement only)
cat("\nCreating custom ensemble comparison (mean, CV, agreement)...\n")
custom_future_comparison <- create_custom_ensemble_comparison(ssp126_future_ensemble, ssp585_future_ensemble, "2290s")
ggsave(paste0(figure_dir, "/QAQC_ensemble_custom_future_comparison.png"),
       custom_future_comparison, width = 18, height = 12, dpi = 300, bg = "white")

cat("\nCustom ensemble plot saved:\n")
cat("- QAQC_ensemble_custom_future_comparison.png (mean, CV, agreement)\n")
cat("\nSummary statistics saved to: QAQC_ensemble_summary_statistics.csv\n")

print(global_summary)
