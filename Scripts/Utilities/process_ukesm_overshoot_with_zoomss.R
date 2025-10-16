# ==============================================================================
# UKESM OVERSHOOT 2101-2300 PIPELINE PROCESSING
# ==============================================================================
# Purpose: Process complete UKESM overshoot scenario (2101-2300) with
#          ZooMSS predictions using nearest-neighbor matching
# Author: ZooMSS_2300 Analysis Team  
# Date: October 15, 2025
# ==============================================================================

library(tidyverse)

# Install FNN if not available
if (!require("FNN", character.only = TRUE)) {
  cat("Installing FNN package...\n")
  install.packages("FNN", repos = "https://cloud.r-project.org")
  library(FNN)
}

# ==============================================================================
# CONFIGURATION
# ==============================================================================

base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
cat("=== UKESM OVERSHOOT 2101-2300 PIPELINE PROCESSING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOAD ENVIRONMENTAL MATRIX AND INPUT DATA
# ==============================================================================

cat("STEP 1: Loading environmental matrix and input data...\n")

# Load environmental matrix with ZooMSS predictions
enviro_matrix_file <- file.path(base_dir, "Enviro_Matrix/all_sst_chl_combinations_2300_wPhyto.rds")
enviro_matrix <- readRDS(enviro_matrix_file)

# Standardize column names and add log10(chlo)
if ("sst" %in% names(enviro_matrix)) enviro_matrix <- enviro_matrix %>% rename(SST = sst)
if ("chlo" %in% names(enviro_matrix)) {
  enviro_matrix <- enviro_matrix %>% 
    rename(Chl = chlo) %>%
    mutate(Chl_log10 = log10(Chl))
}

cat("  Environmental matrix loaded:", nrow(enviro_matrix), "combinations\n")
cat("    SST range:", paste(range(enviro_matrix$SST), collapse = " to "), "°C\n")
cat("    Chl_log10 range:", paste(range(enviro_matrix$Chl_log10), collapse = " to "), "\n\n")

# Load UKESM overshoot 2101-2300 processed data
ukesm_input_file <- file.path(base_dir, "Input/2300_processed/2300_ukesm1-0-ll_ssp534-over_2101-2300.rds")
ukesm_data <- readRDS(ukesm_input_file)

cat("  UKESM input data loaded:", nrow(ukesm_data), "rows\n")
cat("    Years:", min(ukesm_data$Year), "to", max(ukesm_data$Year), "\n")
cat("    Locations:", length(unique(paste(ukesm_data$Lon, ukesm_data$Lat))), "\n\n")

# ==============================================================================
# STEP 2: PREPARE DATA FOR NEAREST NEIGHBOR MATCHING
# ==============================================================================

cat("STEP 2: Preparing data for nearest-neighbor matching...\n")

# Round environmental matrix to match UKESM precision (SST: 0.1°C, Chl_log10: 0.01)
enviro_rounded <- enviro_matrix %>%
  mutate(
    SST_match = round(SST, digits = 1),
    Chl_log10_match = round(Chl_log10, digits = 2)
  )

# Ensure UKESM data is also rounded (should already be, but being explicit)
ukesm_data <- ukesm_data %>%
  mutate(
    SST_match = round(SST, digits = 1),
    Chl_log10_match = round(Chl_log10, digits = 2)
  )

cat("  Environmental matrix prepared for matching\n")
cat("  UKESM data prepared for matching\n\n")

# ==============================================================================
# STEP 3: NEAREST NEIGHBOR MATCHING
# ==============================================================================

cat("STEP 3: Matching UKESM data to environmental matrix...\n")
cat("  Using nearest-neighbor for any missing combinations (127 expected)\n\n")

# Extract unique SST-Chl combinations from environmental matrix
enviro_coords <- enviro_rounded %>%
  distinct(SST_match, Chl_log10_match, .keep_all = TRUE) %>%
  arrange(SST_match, Chl_log10_match)

# For each row in UKESM data, find nearest neighbor in environmental matrix
# Using data.table for efficiency

cat("  Building lookup matrix...\n")

# Create matrix of coordinates for fast nearest neighbor search
enviro_coords_matrix <- as.matrix(enviro_coords[, c("SST_match", "Chl_log10_match")])
ukesm_coords_matrix <- as.matrix(ukesm_data[, c("SST_match", "Chl_log10_match")])

cat("  Performing nearest neighbor search (this may take a few minutes)...\n")
cat("    UKESM points:", nrow(ukesm_coords_matrix), "\n")
cat("    Environmental points:", nrow(enviro_coords_matrix), "\n")

# Find nearest neighbors
# k=1 means find the single nearest neighbor
nn_result <- FNN::get.knnx(
  data = enviro_coords_matrix,
  query = ukesm_coords_matrix, 
  k = 1
)

cat("  ✓ Nearest neighbor matching complete\n\n")

# ==============================================================================
# STEP 4: JOIN WITH ENVIRONMENTAL MATRIX
# ==============================================================================

cat("STEP 4: Joining with environmental matrix to get ZooMSS predictions...\n")

# Get indices of matched environmental matrix rows
matched_indices <- nn_result$nn.index[,1]

# Extract matched environmental data
matched_enviro <- enviro_coords[matched_indices, ]

# Check how many exact matches vs nearest neighbor approximations
exact_matches <- sum(
  ukesm_data$SST_match == matched_enviro$SST_match &
  ukesm_data$Chl_log10_match == matched_enviro$Chl_log10_match
)

approx_matches <- nrow(ukesm_data) - exact_matches

cat("  Exact matches:", exact_matches, sprintf("(%.2f%%)", 100 * exact_matches / nrow(ukesm_data)), "\n")
cat("  Nearest-neighbor approximations:", approx_matches, sprintf("(%.2f%%)", 100 * approx_matches / nrow(ukesm_data)), "\n\n")

# Add ZooMSS predictions to UKESM data
cat("  Adding ZooMSS predictions to UKESM data...\n")

# Select prediction columns from environmental matrix
prediction_cols <- c("pico_biom", "nano_biom", "micro_biom", "phyto_slope", "phyto_int", "phyto_max")

# Add predictions to UKESM data
ukesm_with_predictions <- bind_cols(
  ukesm_data,
  matched_enviro[, prediction_cols]
)

cat("  ✓ Predictions added\n\n")

# ==============================================================================
# STEP 5: CALCULATE DERIVED VARIABLES
# ==============================================================================

cat("STEP 5: Calculating derived variables...\n")

ukesm_with_predictions <- ukesm_with_predictions %>%
  mutate(
    # Rename Date column to match existing format (Year is used as Date in output)
    Date = Year,
    # Calculate phytoplankton biomass (log10)
    Phy_log10 = phyto_slope * SST + phyto_int,
    # Convert to linear scale
    phy = 10^Phy_log10
  ) %>%
  # Remove temporary matching columns
  select(-SST_match, -Chl_log10_match, -Year)

cat("  ✓ Derived variables calculated\n\n")

# ==============================================================================
# STEP 6: COMBINE WITH EXISTING 2040-2100 DATA
# ==============================================================================

cat("STEP 6: Combining with existing 2040-2100 data...\n")

# Load existing 2040-2100 data
existing_file <- file.path(base_dir, "Output/ClimateChange_2300_ukesm1-0-ll_ssp534-over.rds")
existing_data <- readRDS(existing_file)

cat("  Existing data (2040-2100):", nrow(existing_data), "rows\n")
cat("    Years:", min(existing_data$Date), "to", max(existing_data$Date), "\n")

cat("  New data (2101-2300):", nrow(ukesm_with_predictions), "rows\n")
cat("    Years:", min(ukesm_with_predictions$Date), "to", max(ukesm_with_predictions$Date), "\n\n")

# Ensure column order matches
common_cols <- intersect(names(existing_data), names(ukesm_with_predictions))

cat("  Columns in common:", length(common_cols), "\n")
cat("  Columns in existing but not new:", paste(setdiff(names(existing_data), names(ukesm_with_predictions)), collapse=", "), "\n")
cat("  Columns in new but not existing:", paste(setdiff(names(ukesm_with_predictions), names(existing_data)), collapse=", "), "\n\n")

# Select common columns and combine
existing_subset <- existing_data[, common_cols]
new_subset <- ukesm_with_predictions[, common_cols]

# Combine datasets
combined_data <- bind_rows(existing_subset, new_subset) %>%
  arrange(Lon, Lat, Date)

cat("  ✓ Data combined\n")
cat("    Total rows:", nrow(combined_data), "\n")
cat("    Years:", min(combined_data$Date), "to", max(combined_data$Date), "\n")
cat("    Unique years:", length(unique(combined_data$Date)), "\n\n")

# ==============================================================================
# STEP 7: SAVE OUTPUT
# ==============================================================================

cat("STEP 7: Saving output files...\n")

# Save complete dataset (2040-2300)
output_file_complete <- file.path(base_dir, "Output/ClimateChange_2300_ukesm1-0-ll_ssp534-over_COMPLETE.rds")
saveRDS(combined_data, output_file_complete)
cat("  ✓ Saved complete dataset:", basename(output_file_complete), "\n")
cat("    Size:", file.info(output_file_complete)$size / 1024^2, "MB\n")

# Also save just the 2101-2300 segment
output_file_2101_2300 <- file.path(base_dir, "Output/ClimateChange_2300_ukesm1-0-ll_ssp534-over_2101-2300.rds")
saveRDS(new_subset, output_file_2101_2300)
cat("  ✓ Saved 2101-2300 segment:", basename(output_file_2101_2300), "\n")
cat("    Size:", file.info(output_file_2101_2300)$size / 1024^2, "MB\n\n")

# ==============================================================================
# STEP 8: VALIDATION AND SUMMARY
# ==============================================================================

cat("STEP 8: Validation and summary...\n\n")

# Check for NAs
na_summary <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

cat("  Missing values per column:\n")
print(na_summary)
cat("\n")

# Summary statistics for biomass
biomass_summary <- combined_data %>%
  summarise(
    pico_mean = mean(pico_biom, na.rm = TRUE),
    pico_range = paste(range(pico_biom, na.rm = TRUE), collapse = " to "),
    nano_mean = mean(nano_biom, na.rm = TRUE),
    nano_range = paste(range(nano_biom, na.rm = TRUE), collapse = " to "),
    micro_mean = mean(micro_biom, na.rm = TRUE),
    micro_range = paste(range(micro_biom, na.rm = TRUE), collapse = " to ")
  )

cat("  Biomass summary:\n")
cat("    Picoplankton: mean =", sprintf("%.4f", biomass_summary$pico_mean), 
    ", range =", biomass_summary$pico_range, "\n")
cat("    Nanoplankton: mean =", sprintf("%.4f", biomass_summary$nano_mean),
    ", range =", biomass_summary$nano_range, "\n")
cat("    Microplankton: mean =", sprintf("%.4f", biomass_summary$micro_mean),
    ", range =", biomass_summary$micro_range, "\n\n")

# Temporal coverage check
temporal_check <- combined_data %>%
  group_by(Date) %>%
  summarise(n_locations = n(), .groups = 'drop')

cat("  Temporal coverage:\n")
cat("    Years with data:", nrow(temporal_check), "\n")
cat("    Expected years: 261 (2040-2300)\n")
cat("    Locations per year (should be constant):", unique(temporal_check$n_locations), "\n\n")

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("=============================================================================\n")
cat("UKESM OVERSHOOT PROCESSING COMPLETE\n")
cat("=============================================================================\n\n")

cat("✓ SUCCESS: UKESM overshoot scenario processed successfully\n\n")

cat("Output files created:\n")
cat("  1. Complete dataset (2040-2300):\n")
cat("     ", output_file_complete, "\n")
cat("     ", nrow(combined_data), "rows,", length(unique(combined_data$Date)), "years\n\n")
cat("  2. New segment (2101-2300):\n")
cat("     ", output_file_2101_2300, "\n")
cat("     ", nrow(new_subset), "rows,", length(unique(new_subset$Date)), "years\n\n")

cat("Next steps:\n")
cat("  1. Replace the existing file with the complete version:\n")
cat("     File.rename('", output_file_complete, "',\n")
cat("                 '", existing_file, "')\n\n")
cat("  2. Update compiled datasets if needed\n")
cat("  3. Generate visualizations and analysis plots\n\n")

cat("=============================================================================\n")
