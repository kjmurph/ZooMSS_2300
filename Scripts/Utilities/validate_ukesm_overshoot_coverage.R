# ==============================================================================
# UKESM OVERSHOOT SCENARIO VALIDATION
# ==============================================================================
# Purpose: Validate that existing environmental matrix covers all SST-Chl
#          combinations in the complete UKESM overshoot scenario (to 2300)
# Author: ZooMSS_2300 Analysis Team
# Date: October 15, 2025
# ==============================================================================

library(tidyverse)
library(raster)
library(viridis)
library(patchwork)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

base_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/"
input_dir <- file.path(base_dir, "Input")
enviro_matrix_dir <- file.path(base_dir, "Enviro_Matrix")
output_dir <- file.path(base_dir, "Output")
figure_dir <- file.path(base_dir, "Figures/UKESM_Validation")

# Create output directories
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("\n=== UKESM OVERSHOOT SCENARIO VALIDATION ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOAD EXISTING ENVIRONMENTAL MATRICES
# ==============================================================================

cat("STEP 1: Loading existing environmental matrices...\n")

# Load the combined environmental matrix (original + novel)
enviro_matrix_all <- readRDS(file.path(enviro_matrix_dir, "all_sst_chl_combinations_2300_wPhyto.rds"))
cat("  - Loaded 'all_sst_chl_combinations_2300_wPhyto.rds'\n")
cat("    Dimensions:", nrow(enviro_matrix_all), "rows,", ncol(enviro_matrix_all), "columns\n")

# Load the novel combinations only
enviro_matrix_novel <- readRDS(file.path(enviro_matrix_dir, "novel_sst_chl_combinations_2300_wPhyto.rds"))
cat("  - Loaded 'novel_sst_chl_combinations_2300_wPhyto.rds'\n")
cat("    Dimensions:", nrow(enviro_matrix_novel), "rows,", ncol(enviro_matrix_novel), "columns\n")

# Load the original climate change matrix
enviro_matrix_original <- readRDS(file.path(enviro_matrix_dir, "ClimateChange_Compiled_Distinct.rds"))
cat("  - Loaded 'ClimateChange_Compiled_Distinct.rds'\n")
cat("    Dimensions:", nrow(enviro_matrix_original), "rows,", ncol(enviro_matrix_original), "columns\n")

# Standardize column names for comparison
if ("sst" %in% names(enviro_matrix_all)) {
  enviro_matrix_all <- enviro_matrix_all %>% rename(SST = sst)
}
if ("chlo" %in% names(enviro_matrix_all)) {
  enviro_matrix_all <- enviro_matrix_all %>% rename(Chl = chlo)
}

# Add Chl_log10 if not present
if (!"Chl_log10" %in% names(enviro_matrix_all) && "Chl" %in% names(enviro_matrix_all)) {
  enviro_matrix_all <- enviro_matrix_all %>%
    mutate(Chl_log10 = log10(Chl))
}

cat("\n  Environmental matrix summary:\n")
cat("    SST range:", paste(range(enviro_matrix_all$SST, na.rm = TRUE), collapse = " to "), "°C\n")
cat("    Chl range:", paste(range(enviro_matrix_all$Chl, na.rm = TRUE), collapse = " to "), "mg/m³\n")
cat("    Chl_log10 range:", paste(range(enviro_matrix_all$Chl_log10, na.rm = TRUE), collapse = " to "), "\n")

# ==============================================================================
# STEP 2: LOAD UKESM OVERSHOOT DATA (2101-2300)
# ==============================================================================

cat("\nSTEP 2: Loading UKESM overshoot processed data (2101-2300)...\n")

# Load the processed RDS file directly
processed_file <- file.path(base_dir, "Input/2300_processed/2300_ukesm1-0-ll_ssp534-over_2101-2300.rds")

if (!file.exists(processed_file)) {
  stop("ERROR: Processed UKESM overshoot file not found! Run combine_ukesm_overshoot_data.R first.\nExpected: ", processed_file)
}

cat("  Loading:", basename(processed_file), "\n")
ukesm_overshoot <- readRDS(processed_file)

cat("  Loaded:", nrow(ukesm_overshoot), "rows\n")
cat("  Years:", min(ukesm_overshoot$Year), "to", max(ukesm_overshoot$Year), "\n")
cat("  SST range:", paste(range(ukesm_overshoot$SST, na.rm = TRUE), collapse = " to "), "°C\n")
cat("  Chl range:", paste(range(ukesm_overshoot$Chl, na.rm = TRUE), collapse = " to "), "mg/m³\n")

# ==============================================================================
# STEP 3: EXTRACT UNIQUE SST-CHL COMBINATIONS
# ==============================================================================

cat("\nSTEP 3: Extracting unique SST-Chl combinations...\n")

# Get unique combinations from UKESM overshoot
ukesm_combinations <- ukesm_overshoot %>%
  dplyr::select(SST, Chl_log10) %>%
  distinct() %>%
  arrange(SST, Chl_log10)

cat("  Unique SST-Chl combinations in UKESM overshoot:", nrow(ukesm_combinations), "\n")
cat("    SST range:", paste(range(ukesm_combinations$SST), collapse = " to "), "°C\n")
cat("    Chl_log10 range:", paste(range(ukesm_combinations$Chl_log10), collapse = " to "), "\n")

# Get unique combinations from existing environmental matrix
existing_combinations <- enviro_matrix_all %>%
  mutate(
    SST_rounded = round(SST, digits = 1),
    Chl_log10_rounded = round(Chl_log10, digits = 2)
  ) %>%
  dplyr::select(SST_rounded, Chl_log10_rounded) %>%
  distinct() %>%
  rename(SST = SST_rounded, Chl_log10 = Chl_log10_rounded)

cat("  Unique combinations in existing matrix:", nrow(existing_combinations), "\n")

# ==============================================================================
# STEP 4: IDENTIFY GAPS (MISSING COMBINATIONS)
# ==============================================================================

cat("\nSTEP 4: Identifying missing combinations...\n")

# Find combinations in UKESM that are NOT in existing matrix
missing_combinations <- ukesm_combinations %>%
  anti_join(existing_combinations, by = c("SST", "Chl_log10"))

n_missing <- nrow(missing_combinations)
cat("\n  ⚠️  Missing combinations:", n_missing, "\n")

if (n_missing > 0) {
  cat("\n  Missing SST-Chl combinations summary:\n")
  cat("    SST range:", paste(range(missing_combinations$SST), collapse = " to "), "°C\n")
  cat("    Chl_log10 range:", paste(range(missing_combinations$Chl_log10), collapse = " to "), "\n")
  
  # Show sample of missing combinations
  cat("\n  Sample of missing combinations (first 20):\n")
  print(head(missing_combinations, 20))
  
  # Save missing combinations
  missing_file <- file.path(output_dir, "ukesm_overshoot_missing_combinations.csv")
  write_csv(missing_combinations, missing_file)
  cat("\n  ✓ Saved missing combinations to:", missing_file, "\n")
  
} else {
  cat("\n  ✓ All UKESM overshoot combinations are covered by existing environmental matrix!\n")
}

# ==============================================================================
# STEP 5: COVERAGE ANALYSIS
# ==============================================================================

cat("\nSTEP 5: Coverage analysis...\n")

coverage_pct <- (nrow(ukesm_combinations) - n_missing) / nrow(ukesm_combinations) * 100
cat("  Coverage:", round(coverage_pct, 2), "%\n")
cat("  Covered combinations:", nrow(ukesm_combinations) - n_missing, "\n")
cat("  Missing combinations:", n_missing, "\n")

# Identify extreme values
extreme_sst_high <- ukesm_combinations %>% 
  filter(SST > max(existing_combinations$SST))
extreme_sst_low <- ukesm_combinations %>% 
  filter(SST < min(existing_combinations$SST))
extreme_chl_high <- ukesm_combinations %>% 
  filter(Chl_log10 > max(existing_combinations$Chl_log10))
extreme_chl_low <- ukesm_combinations %>% 
  filter(Chl_log10 < min(existing_combinations$Chl_log10))

cat("\n  Extreme value analysis:\n")
cat("    SST higher than existing maximum:", nrow(extreme_sst_high), "combinations\n")
if (nrow(extreme_sst_high) > 0) {
  cat("      New SST max:", max(extreme_sst_high$SST), "°C (existing max:",
      max(existing_combinations$SST), "°C)\n")
}
cat("    SST lower than existing minimum:", nrow(extreme_sst_low), "combinations\n")
if (nrow(extreme_sst_low) > 0) {
  cat("      New SST min:", min(extreme_sst_low$SST), "°C (existing min:",
      min(existing_combinations$SST), "°C)\n")
}
cat("    Chl higher than existing maximum:", nrow(extreme_chl_high), "combinations\n")
if (nrow(extreme_chl_high) > 0) {
  cat("      New Chl_log10 max:", max(extreme_chl_high$Chl_log10), "(existing max:",
      max(existing_combinations$Chl_log10), ")\n")
}
cat("    Chl lower than existing minimum:", nrow(extreme_chl_low), "combinations\n")
if (nrow(extreme_chl_low) > 0) {
  cat("      New Chl_log10 min:", min(extreme_chl_low$Chl_log10), "(existing min:",
      min(existing_combinations$Chl_log10), ")\n")
}

# ==============================================================================
# STEP 6: VISUALIZATION
# ==============================================================================

cat("\nSTEP 6: Creating visualizations...\n")

# Prepare data for plotting
ukesm_plot_data <- ukesm_combinations %>%
  mutate(source = "UKESM Overshoot")

existing_plot_data <- existing_combinations %>%
  mutate(source = "Existing Matrix")

# Combine for overlay plot
combined_plot_data <- bind_rows(ukesm_plot_data, existing_plot_data)

# Plot 1: Coverage comparison
p1 <- ggplot() +
  geom_hex(data = existing_plot_data, 
           aes(x = SST, y = Chl_log10), 
           bins = 50, alpha = 0.7) +
  scale_fill_viridis(name = "Count\n(Existing)", trans = "log10") +
  geom_point(data = missing_combinations, 
             aes(x = SST, y = Chl_log10), 
             color = "red", size = 1, alpha = 0.6) +
  labs(
    title = "UKESM Overshoot Coverage Analysis",
    subtitle = paste0("Red points = Missing combinations (n=", n_missing, ")"),
    x = "Sea Surface Temperature (°C)",
    y = "log10(Chlorophyll-a) [mg/m³]"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave(file.path(figure_dir, "ukesm_overshoot_coverage_analysis.png"), 
       p1, width = 10, height = 7, dpi = 300)
cat("  ✓ Saved: ukesm_overshoot_coverage_analysis.png\n")

# Plot 2: Density comparison
p2 <- ggplot(combined_plot_data, aes(x = SST, y = Chl_log10, color = source)) +
  geom_density_2d(linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("UKESM Overshoot" = "red", 
                                "Existing Matrix" = "blue"),
                    name = "Data Source") +
  labs(
    title = "Environmental Space Comparison",
    subtitle = "Density contours: UKESM Overshoot vs Existing Matrix",
    x = "Sea Surface Temperature (°C)",
    y = "log10(Chlorophyll-a) [mg/m³]"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(figure_dir, "ukesm_overshoot_density_comparison.png"), 
       p2, width = 10, height = 7, dpi = 300)
cat("  ✓ Saved: ukesm_overshoot_density_comparison.png\n")

# Plot 3: Hexbin comparison (side by side)
p3a <- ggplot(existing_plot_data, aes(x = SST, y = Chl_log10)) +
  geom_hex(bins = 50) +
  scale_fill_viridis(name = "Count", trans = "log10") +
  labs(title = "Existing Matrix", x = "SST (°C)", y = "log10(Chl)") +
  theme_bw()

p3b <- ggplot(ukesm_plot_data, aes(x = SST, y = Chl_log10)) +
  geom_hex(bins = 50) +
  scale_fill_viridis(name = "Count", trans = "log10") +
  labs(title = "UKESM Overshoot", x = "SST (°C)", y = "log10(Chl)") +
  theme_bw()

p3 <- p3a + p3b + 
  plot_annotation(
    title = "Environmental Space Distribution Comparison",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(figure_dir, "ukesm_overshoot_hexbin_comparison.png"), 
       p3, width = 14, height = 6, dpi = 300)
cat("  ✓ Saved: ukesm_overshoot_hexbin_comparison.png\n")

# ==============================================================================
# STEP 7: GENERATE VALIDATION REPORT
# ==============================================================================

cat("\nSTEP 7: Generating validation report...\n")

validation_report <- list(
  analysis_date = Sys.time(),
  ukesm_data = list(
    total_records = nrow(ukesm_overshoot),
    unique_combinations = nrow(ukesm_combinations),
    sst_range = range(ukesm_combinations$SST),
    chl_log10_range = range(ukesm_combinations$Chl_log10)
  ),
  existing_matrix = list(
    total_combinations = nrow(existing_combinations),
    sst_range = range(existing_combinations$SST),
    chl_log10_range = range(existing_combinations$Chl_log10)
  ),
  coverage = list(
    coverage_pct = coverage_pct,
    covered_combinations = nrow(ukesm_combinations) - n_missing,
    missing_combinations = n_missing,
    missing_data = missing_combinations
  ),
  extreme_values = list(
    extreme_sst_high = nrow(extreme_sst_high),
    extreme_sst_low = nrow(extreme_sst_low),
    extreme_chl_high = nrow(extreme_chl_high),
    extreme_chl_low = nrow(extreme_chl_low)
  )
)

# Save report
report_file <- file.path(output_dir, "ukesm_overshoot_validation_report.rds")
saveRDS(validation_report, report_file)
cat("  ✓ Saved validation report to:", report_file, "\n")

# Save human-readable summary
summary_file <- file.path(output_dir, "ukesm_overshoot_validation_summary.txt")
sink(summary_file)
cat("=============================================================================\n")
cat("UKESM OVERSHOOT SCENARIO VALIDATION SUMMARY\n")
cat("=============================================================================\n")
cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("UKESM OVERSHOOT DATA:\n")
cat("  Total records:", nrow(ukesm_overshoot), "\n")
cat("  Unique SST-Chl combinations:", nrow(ukesm_combinations), "\n")
cat("  SST range:", paste(range(ukesm_combinations$SST), collapse = " to "), "°C\n")
cat("  Chl_log10 range:", paste(range(ukesm_combinations$Chl_log10), collapse = " to "), "\n\n")

cat("EXISTING ENVIRONMENTAL MATRIX:\n")
cat("  Total combinations:", nrow(existing_combinations), "\n")
cat("  SST range:", paste(range(existing_combinations$SST), collapse = " to "), "°C\n")
cat("  Chl_log10 range:", paste(range(existing_combinations$Chl_log10), collapse = " to "), "\n\n")

cat("COVERAGE ANALYSIS:\n")
cat("  Coverage:", round(coverage_pct, 2), "%\n")
cat("  Covered combinations:", nrow(ukesm_combinations) - n_missing, "\n")
cat("  Missing combinations:", n_missing, "\n\n")

if (n_missing > 0) {
  cat("⚠️  ACTION REQUIRED:\n")
  cat("  The existing environmental matrix does NOT cover all UKESM overshoot combinations.\n")
  cat("  You need to:\n")
  cat("    1. Review missing combinations in: ukesm_overshoot_missing_combinations.csv\n")
  cat("    2. Generate ZooMSS predictions for missing combinations\n")
  cat("    3. Update the environmental matrix\n\n")
} else {
  cat("✓ VALIDATION PASSED:\n")
  cat("  All UKESM overshoot combinations are covered by the existing matrix.\n")
  cat("  No additional ZooMSS simulations required.\n\n")
}

cat("EXTREME VALUES:\n")
cat("  SST exceeding existing maximum:", nrow(extreme_sst_high), "combinations\n")
cat("  SST below existing minimum:", nrow(extreme_sst_low), "combinations\n")
cat("  Chl exceeding existing maximum:", nrow(extreme_chl_high), "combinations\n")
cat("  Chl below existing minimum:", nrow(extreme_chl_low), "combinations\n\n")

cat("OUTPUT FILES:\n")
cat("  - Validation report (RDS):", report_file, "\n")
cat("  - Missing combinations (CSV):", file.path(output_dir, "ukesm_overshoot_missing_combinations.csv"), "\n")
cat("  - Figures:", figure_dir, "\n")
cat("=============================================================================\n")
sink()

cat("  ✓ Saved summary to:", summary_file, "\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n=============================================================================\n")
cat("VALIDATION COMPLETE\n")
cat("=============================================================================\n\n")

if (n_missing > 0) {
  cat("⚠️  WARNING: Environmental matrix requires updating!\n\n")
  cat("Missing combinations:", n_missing, "\n")
  cat("Coverage:", round(coverage_pct, 2), "%\n\n")
  cat("Next steps:\n")
  cat("  1. Review missing combinations in Output/ukesm_overshoot_missing_combinations.csv\n")
  cat("  2. Run ZooMSS simulations for missing environmental conditions\n")
  cat("  3. Update environmental matrices in Enviro_Matrix/\n")
  cat("  4. Re-process UKESM overshoot scenario with complete coverage\n\n")
} else {
  cat("✓ SUCCESS: All combinations covered!\n\n")
  cat("The existing environmental matrix contains all SST-Chlorophyll combinations\n")
  cat("present in the complete UKESM overshoot scenario data.\n\n")
  cat("You can proceed with re-processing the UKESM overshoot scenario using the\n")
  cat("existing environmental matrix without additional ZooMSS simulations.\n\n")
}

cat("Results saved to:\n")
cat("  - Summary:", summary_file, "\n")
cat("  - Report:", report_file, "\n")
if (n_missing > 0) {
  cat("  - Missing combinations:", file.path(output_dir, "ukesm_overshoot_missing_combinations.csv"), "\n")
}
cat("  - Figures:", figure_dir, "\n")
cat("\n=============================================================================\n")
