# ==============================================================================
# COMPARE UKESM OVERSHOOT AGAINST CLIMATE CHANGE ENVIRONMENTAL MATRIX
# ==============================================================================
# Purpose: Check if the larger Climate Change project environmental matrix
#          provides better coverage of UKESM overshoot combinations
# Author: ZooMSS_2300 Analysis Team
# Date: October 15, 2025
# ==============================================================================

library(tidyverse)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
cat("=== UKESM OVERSHOOT vs CLIMATE CHANGE MATRIX COMPARISON ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Load UKESM overshoot data
ukesm_file <- file.path(base_dir, "Input/2300_processed/2300_ukesm1-0-ll_ssp534-over_2101-2300.rds")
ukesm_data <- readRDS(ukesm_file)
cat("  UKESM overshoot:", nrow(ukesm_data), "rows,", 
    min(ukesm_data$Year), "to", max(ukesm_data$Year), "\n")

# Load environmental matrices
enviro_2300 <- readRDS(file.path(base_dir, "Enviro_Matrix/all_sst_chl_combinations_2300_wPhyto.rds"))
enviro_cc <- readRDS(file.path(base_dir, "Enviro_Matrix/ClimateChange_Compiled_Distinct.rds"))

# Standardize column names to uppercase and convert chlo to log10
if ("sst" %in% names(enviro_2300)) enviro_2300 <- enviro_2300 %>% rename(SST = sst)
if ("chlo" %in% names(enviro_2300)) enviro_2300 <- enviro_2300 %>% rename(Chl = chlo) %>% mutate(Chl_log10 = log10(Chl))
if ("sst" %in% names(enviro_cc)) enviro_cc <- enviro_cc %>% rename(SST = sst)
if ("chlo" %in% names(enviro_cc)) enviro_cc <- enviro_cc %>% rename(Chl = chlo) %>% mutate(Chl_log10 = log10(Chl))

cat("  2300 Matrix:", nrow(enviro_2300), "combinations\n")
cat("  Climate Change Matrix:", nrow(enviro_cc), "combinations\n\n")

# ==============================================================================
# EXTRACT UNIQUE COMBINATIONS
# ==============================================================================

cat("Extracting unique SST-Chl combinations...\n")

# UKESM combinations (already rounded but let's be explicit)
ukesm_combinations <- ukesm_data %>%
  dplyr::select(SST, Chl_log10) %>%
  mutate(
    SST = round(SST, digits = 1),
    Chl_log10 = round(Chl_log10, digits = 2)
  ) %>%
  distinct() %>%
  arrange(SST, Chl_log10)

cat("  UKESM overshoot:", nrow(ukesm_combinations), "unique combinations\n")
cat("    SST range:", paste(range(ukesm_combinations$SST), collapse = " to "), "°C\n")
cat("    Chl_log10 range:", paste(range(ukesm_combinations$Chl_log10), collapse = " to "), "\n\n")

# 2300 matrix combinations
# Round to match UKESM precision (SST: 0.1°C, Chl_log10: 0.01)
enviro_2300_combos <- enviro_2300 %>%
  mutate(
    SST_rounded = round(SST, digits = 1),
    Chl_log10_rounded = round(Chl_log10, digits = 2)
  ) %>%
  dplyr::select(SST_rounded, Chl_log10_rounded) %>%
  distinct() %>%
  rename(SST = SST_rounded, Chl_log10 = Chl_log10_rounded)

# Climate Change matrix combinations
enviro_cc_combos <- enviro_cc %>%
  mutate(
    SST_rounded = round(SST, digits = 1),
    Chl_log10_rounded = round(Chl_log10, digits = 2)
  ) %>%
  dplyr::select(SST_rounded, Chl_log10_rounded) %>%
  distinct() %>%
  rename(SST = SST_rounded, Chl_log10 = Chl_log10_rounded)

cat("  2300 Matrix (unique after rounding):", nrow(enviro_2300_combos), "combinations\n")
cat("  Climate Change Matrix (unique after rounding):", nrow(enviro_cc_combos), "combinations\n\n")

# ==============================================================================
# COVERAGE ANALYSIS
# ==============================================================================

cat("=== COVERAGE ANALYSIS ===\n\n")

# Coverage with 2300 matrix
missing_2300 <- ukesm_combinations %>%
  anti_join(enviro_2300_combos, by = c("SST", "Chl_log10"))

coverage_2300 <- (nrow(ukesm_combinations) - nrow(missing_2300)) / nrow(ukesm_combinations) * 100

cat("2300 Matrix Coverage:\n")
cat("  Covered:", nrow(ukesm_combinations) - nrow(missing_2300), "combinations\n")
cat("  Missing:", nrow(missing_2300), "combinations\n")
cat("  Coverage:", sprintf("%.2f%%", coverage_2300), "\n\n")

# Coverage with Climate Change matrix
missing_cc <- ukesm_combinations %>%
  anti_join(enviro_cc_combos, by = c("SST", "Chl_log10"))

coverage_cc <- (nrow(ukesm_combinations) - nrow(missing_cc)) / nrow(ukesm_combinations) * 100

cat("Climate Change Matrix Coverage:\n")
cat("  Covered:", nrow(ukesm_combinations) - nrow(missing_cc), "combinations\n")
cat("  Missing:", nrow(missing_cc), "combinations\n")
cat("  Coverage:", sprintf("%.2f%%", coverage_cc), "\n\n")

# ==============================================================================
# IMPROVEMENT ANALYSIS
# ==============================================================================

cat("=== IMPROVEMENT ANALYSIS ===\n\n")

improvement <- nrow(missing_2300) - nrow(missing_cc)
improvement_pct <- (improvement / nrow(missing_2300)) * 100

if (improvement > 0) {
  cat("✓ Climate Change Matrix provides BETTER coverage!\n")
  cat("  Additional combinations covered:", improvement, "\n")
  cat("  Improvement:", sprintf("%.1f%%", improvement_pct), "of previously missing\n")
  cat("  Remaining gaps reduced from", nrow(missing_2300), "to", nrow(missing_cc), "\n\n")
  
  # Which combinations are now covered?
  newly_covered <- missing_2300 %>%
    anti_join(missing_cc, by = c("SST", "Chl_log10"))
  
  cat("  Newly covered combinations:\n")
  cat("    Count:", nrow(newly_covered), "\n")
  if (nrow(newly_covered) > 0) {
    cat("    SST range:", paste(range(newly_covered$SST), collapse = " to "), "°C\n")
    cat("    Chl_log10 range:", paste(range(newly_covered$Chl_log10), collapse = " to "), "\n\n")
    
    cat("  Sample of newly covered (first 20):\n")
    print(head(newly_covered, 20))
  }
  
} else if (improvement == 0) {
  cat("→ Both matrices provide identical coverage\n")
  cat("  No improvement from using Climate Change matrix\n\n")
} else {
  cat("✗ Climate Change Matrix provides WORSE coverage (unexpected!)\n")
  cat("  This shouldn't happen - investigating...\n\n")
}

# ==============================================================================
# REMAINING GAPS ANALYSIS
# ==============================================================================

if (nrow(missing_cc) > 0) {
  cat("\n=== REMAINING GAPS (Climate Change Matrix) ===\n\n")
  cat("  Still missing:", nrow(missing_cc), "combinations\n")
  cat("  SST range:", paste(range(missing_cc$SST), collapse = " to "), "°C\n")
  cat("  Chl_log10 range:", paste(range(missing_cc$Chl_log10), collapse = " to "), "\n\n")
  
  cat("  Sample of remaining missing (first 20):\n")
  print(head(missing_cc, 20))
  
  # Save remaining missing combinations
  output_file <- file.path(base_dir, "Output/ukesm_overshoot_missing_with_cc_matrix.csv")
  write_csv(missing_cc, output_file)
  cat("\n  ✓ Saved remaining missing combinations to:\n")
  cat("    ", output_file, "\n")
} else {
  cat("\n=== COMPLETE COVERAGE ACHIEVED! ===\n\n")
  cat("✓ The Climate Change environmental matrix provides 100% coverage!\n")
  cat("  No additional ZooMSS simulations needed.\n")
  cat("  You can proceed directly with processing using the Climate Change matrix.\n")
}

# ==============================================================================
# MATRIX COMPARISON
# ==============================================================================

cat("\n=== MATRIX CHARACTERISTICS ===\n\n")

cat("2300 Matrix:\n")
cat("  SST range:", paste(range(enviro_2300$SST), collapse = " to "), "°C\n")
cat("  Chl range:", paste(range(enviro_2300$Chl), collapse = " to "), "mg/m³\n")
cat("  Chl_log10 range:", paste(range(enviro_2300$Chl_log10), collapse = " to "), "\n\n")

cat("Climate Change Matrix:\n")
cat("  SST range:", paste(range(enviro_cc$SST), collapse = " to "), "°C\n")
cat("  Chl range:", paste(range(enviro_cc$Chl), collapse = " to "), "mg/m³\n")
cat("  Chl_log10 range:", paste(range(enviro_cc$Chl_log10), collapse = " to "), "\n\n")

# ==============================================================================
# RECOMMENDATION
# ==============================================================================

cat("=== RECOMMENDATION ===\n\n")

if (nrow(missing_cc) == 0) {
  cat("★ Use Climate Change Matrix for processing!\n")
  cat("  - Provides 100% coverage\n")
  cat("  - No additional simulations needed\n")
  cat("  - Ready to proceed with nearest-neighbor matching\n\n")
} else if (improvement > 0) {
  cat("★ Use Climate Change Matrix for processing!\n")
  cat("  - Better coverage than 2300 matrix (", sprintf("%.2f%% vs %.2f%%", coverage_cc, coverage_2300), ")\n")
  cat("  - Reduces missing combinations from", nrow(missing_2300), "to", nrow(missing_cc), "\n")
  cat("  - Remaining gaps (", nrow(missing_cc), ") can use nearest-neighbor interpolation\n\n")
} else {
  cat("→ Continue with 2300 Matrix\n")
  cat("  - Climate Change matrix doesn't improve coverage\n")
  cat("  - Use nearest-neighbor interpolation for", nrow(missing_2300), "missing combinations\n\n")
}

cat("=============================================================================\n")
cat("COMPARISON COMPLETE\n")
cat("=============================================================================\n")
