# ==============================================================================
# COMBINE UKESM OVERSHOOT 2101-2300 SST + CHLOROPHYLL DATA
# ==============================================================================
# Purpose: Combine processed SST and Chl into RDS format for pipeline
# Author: ZooMSS_2300 Analysis Team
# Date: October 15, 2025
# ==============================================================================

library(tidyverse)
library(ncdf4)
library(raster)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Use forward slashes consistently
base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
tos_dir <- file.path(base_dir, "Input/tos/")
chl_dir <- file.path(base_dir, "Input/phyc/")  # Phyc files are here
output_dir <- file.path(base_dir, "Input/2300_processed/")

cat("=== COMBINING UKESM OVERSHOOT 2101-2300 DATA ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOCATE PROCESSED FILES
# ==============================================================================

cat("STEP 1: Locating processed annual files...\n")

# Find SST annual file - specifically the 2101-2300 one
# Pattern: ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_annual_2101_2300.nc
tos_file <- list.files(tos_dir, 
                       pattern = "ukesm.*ssp534.*tos.*annual_2101_2300\\.nc$",
                       full.names = TRUE)[1]

if (is.na(tos_file) || length(tos_file) == 0) {
  stop("ERROR: SST annual file (2101-2300) not found! Run process_ukesm_overshoot_sst.R first.")
}

cat("  SST file:", basename(tos_file), "\n")

# Find phyc annual file (we'll convert to chl)
# Pattern: ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc-top_60arcmin_global_annual_2101_2300.nc
phyc_file <- list.files(chl_dir,
                       pattern = "ukesm.*ssp534.*phyc-top.*annual_2101_2300\\.nc$",
                       full.names = TRUE)[1]

if (is.na(phyc_file) || length(phyc_file) == 0) {
  stop("ERROR: Phyc annual file (2101-2300) not found! Run extract_ukesm_overshoot_toplayer.R first.")
}

cat("  Phyc file:", basename(phyc_file), "\n")
cat("  Will convert phyc → chlorophyll inline\n")

# ==============================================================================
# STEP 2: LOAD SST DATA
# ==============================================================================

cat("\nSTEP 2: Loading SST data...\n")

nc_tos <- nc_open(tos_file)
tos_data <- ncvar_get(nc_tos, "tos")
lon_vals <- ncvar_get(nc_tos, "lon")
lat_vals <- ncvar_get(nc_tos, "lat")
time_vals <- ncvar_get(nc_tos, "time")
nc_close(nc_tos)

cat("  Dimensions:", paste(dim(tos_data), collapse = " × "), "\n")
cat("  SST range:", paste(range(tos_data, na.rm = TRUE), collapse = " to "), "°C\n")

# Convert from Kelvin to Celsius if needed
if (min(tos_data, na.rm = TRUE) > 200) {
  cat("  Converting from Kelvin to Celsius...\n")
  tos_data <- tos_data - 273.15
  cat("  New SST range:", paste(range(tos_data, na.rm = TRUE), collapse = " to "), "°C\n")
}

# ==============================================================================
# STEP 3: LOAD CHLOROPHYLL DATA (FROM PHYC)
# ==============================================================================

cat("\nSTEP 3: Loading Chlorophyll data (from phyc)...\n")

nc_phyc <- nc_open(phyc_file)
phyc_data <- ncvar_get(nc_phyc, "phyc")
nc_close(nc_phyc)

# Convert phyc to chlorophyll (C:Chl = 50 g:g)
# phyc [mol/m³] * 12 [g C/mol] / 50 [g C/g Chl] * 1000 [mg/g] = Chl [mg/m³]
chl_data <- phyc_data * 12 / 50 * 1000

cat("  Phyc dimensions:", paste(dim(phyc_data), collapse = " × "), "\n")
cat("  Phyc range:", paste(range(phyc_data, na.rm = TRUE), collapse = " to "), "mol/m³\n")
cat("  Converted Chl range:", paste(range(chl_data, na.rm = TRUE), collapse = " to "), "mg/m³\n")

# ==============================================================================
# STEP 4: VERIFY DIMENSIONS MATCH
# ==============================================================================

cat("\nSTEP 4: Verifying dimensions match...\n")

if (!identical(dim(tos_data), dim(chl_data))) {
  stop("ERROR: SST and Chlorophyll dimensions do not match!")
}

cat("  ✓ Dimensions match:", paste(dim(tos_data), collapse = " × "), "\n")

n_lon <- dim(tos_data)[1]
n_lat <- dim(tos_data)[2]
n_time <- dim(tos_data)[3]

cat("  Grid:", n_lon, "×", n_lat, "\n")
cat("  Time steps:", n_time, "years\n")

# ==============================================================================
# STEP 5: CONVERT TO DATAFRAME (MATCHING PIPELINE FORMAT)
# ==============================================================================

cat("\nSTEP 5: Converting to dataframe format...\n")

# Create coordinate grids
lon_grid <- rep(lon_vals, times = n_lat * n_time)
lat_grid <- rep(rep(lat_vals, each = n_lon), times = n_time)
year_grid <- rep(2101:(2101 + n_time - 1), each = n_lon * n_lat)

# Flatten data arrays
tos_vector <- as.vector(tos_data)
chl_vector <- as.vector(chl_data)

# Create dataframe
cat("  Creating combined dataframe...\n")
combined_data <- tibble(
  Lon = lon_grid,
  Lat = lat_grid,
  Year = year_grid,
  SST = tos_vector,
  Chl = chl_vector
) %>%
  filter(!is.na(SST), !is.na(Chl)) %>%  # Remove NA values
  mutate(
    Model = "ukesm1-0-ll",
    Experiment = "ssp534-over",
    Chl_log10 = log10(Chl),
    SST = round(SST, digits = 1),
    Chl_log10 = round(Chl_log10, digits = 2)
  )

cat("  Records after removing NAs:", nrow(combined_data), "\n")
cat("  Years covered:", min(combined_data$Year), "to", max(combined_data$Year), "\n")

# ==============================================================================
# STEP 6: DATA QUALITY CHECKS
# ==============================================================================

cat("\nSTEP 6: Data quality checks...\n")

# Check for reasonable ranges
sst_range <- range(combined_data$SST)
chl_range <- range(combined_data$Chl)

cat("  SST range:", paste(sst_range, collapse = " to "), "°C\n")
cat("  Chl range:", paste(chl_range, collapse = " to "), "mg/m³\n")
cat("  Chl_log10 range:", paste(range(combined_data$Chl_log10), collapse = " to "), "\n")

# Check for unrealistic values
if (sst_range[1] < -10 || sst_range[2] > 40) {
  warning("⚠️ SST values outside typical ocean range!")
}

if (chl_range[1] < 0 || chl_range[2] > 100) {
  warning("⚠️ Chlorophyll values outside typical range!")
}

# Summary statistics by year
year_summary <- combined_data %>%
  group_by(Year) %>%
  summarise(
    n_points = n(),
    mean_SST = mean(SST, na.rm = TRUE),
    mean_Chl = mean(Chl, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n  Points per year - range:", paste(range(year_summary$n_points), collapse = " to "), "\n")
cat("  Mean SST across years:", round(mean(year_summary$mean_SST), 2), "°C\n")
cat("  Mean Chl across years:", round(mean(year_summary$mean_Chl), 3), "mg/m³\n")

# ==============================================================================
# STEP 7: SAVE TO RDS FORMAT
# ==============================================================================

cat("\nSTEP 7: Saving to RDS format...\n")

# Create output filename matching existing convention
output_filename <- "2300_ukesm1-0-ll_ssp534-over_2101-2300.rds"
output_path <- file.path(output_dir, output_filename)

# Create output directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("  Created output directory\n")
}

# Save
saveRDS(combined_data, output_path)

file_size_mb <- file.size(output_path) / (1024^2)
cat("  ✓ Saved to:", basename(output_path), "\n")
cat("  Size:", round(file_size_mb, 1), "MB\n")

# ==============================================================================
# STEP 8: VALIDATION VISUALIZATION
# ==============================================================================

cat("\nSTEP 8: Creating validation visualization...\n")

library(ggplot2)
library(patchwork)

# Create output figure directory
fig_dir <- file.path(base_dir, "Figures/UKESM_Validation")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# Sample data for visualization (every 10th year)
vis_data <- combined_data %>%
  filter(Year %% 10 == 1 | Year == max(Year))

# Plot SST-Chl relationship
p1 <- ggplot(vis_data, aes(x = SST, y = Chl_log10)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(trans = "log10", name = "Count") +
  labs(
    title = "UKESM Overshoot 2101-2300: SST-Chlorophyll Space",
    x = "Sea Surface Temperature (°C)",
    y = "log10(Chlorophyll-a) [mg/m³]"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

# Time series
ts_data <- combined_data %>%
  group_by(Year) %>%
  summarise(
    mean_SST = mean(SST, na.rm = TRUE),
    mean_Chl = mean(Chl, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(ts_data, aes(x = Year, y = mean_SST)) +
  geom_line(color = "red", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(
    title = "Mean SST Trend",
    x = "Year",
    y = "Mean SST (°C)"
  ) +
  theme_bw()

p3 <- ggplot(ts_data, aes(x = Year, y = mean_Chl)) +
  geom_line(color = "green", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(
    title = "Mean Chlorophyll Trend",
    x = "Year",
    y = "Mean Chl (mg/m³)"
  ) +
  theme_bw()

combined_plot <- (p2 / p3) | p1

ggsave(
  file.path(fig_dir, "ukesm_overshoot_2101-2300_combined_data.png"),
  combined_plot,
  width = 14, height = 8, dpi = 300
)

cat("  ✓ Saved validation plot\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n=============================================================================\n")
cat("DATA COMBINATION COMPLETE\n")
cat("=============================================================================\n\n")

cat("OUTPUT FILE:\n")
cat("  ", output_path, "\n")
cat("  Size:", round(file_size_mb, 1), "MB\n")
cat("  Records:", format(nrow(combined_data), big.mark = ","), "\n")
cat("  Years: 2101 - 2300 (200 years)\n")
cat("  Model: ukesm1-0-ll\n")
cat("  Scenario: ssp534-over\n\n")

cat("DATA SUMMARY:\n")
cat("  SST range:", paste(round(sst_range, 2), collapse = " to "), "°C\n")
cat("  Chl range:", paste(round(chl_range, 3), collapse = " to "), "mg/m³\n")
cat("  Spatial coverage:", length(unique(paste(combined_data$Lon, combined_data$Lat))), "unique locations\n\n")

cat("✓ Data ready for validation!\n\n")

cat("NEXT STEPS:\n")
cat("  1. Run validation script: validate_ukesm_overshoot_coverage.R\n")
cat("  2. Check if environmental matrix covers all SST-Chl combinations\n")
cat("  3. If gaps exist, generate new ZooMSS predictions\n")
cat("  4. Integrate into complete UKESM overshoot analysis (2015-2300)\n")

cat("\n=============================================================================\n")

# Save processing metadata
metadata <- list(
  processing_date = Sys.time(),
  output_file = output_path,
  source_files = list(
    tos = basename(tos_file),
    chl = basename(chl_file)
  ),
  n_records = nrow(combined_data),
  year_range = range(combined_data$Year),
  sst_range = sst_range,
  chl_range = chl_range,
  spatial_coverage = length(unique(paste(combined_data$Lon, combined_data$Lat)))
)

saveRDS(metadata, file.path(output_dir, "ukesm_overshoot_2101-2300_metadata.rds"))
cat("Metadata saved!\n")
