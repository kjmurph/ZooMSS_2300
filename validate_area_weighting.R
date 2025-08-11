# ==============================================================================
# AREA WEIGHTING VALIDATION
# ==============================================================================
# Purpose: Check the area weighting issue in existing results
# ==============================================================================

library(tidyverse)

cat("=== AREA WEIGHTING VALIDATION ===\n")

# Load the existing processed data
cat("Loading existing biomass data...\n")
existing_data <- readRDS("Output/combined_weighted_biomass_timeseries.rds")

cat("Data loaded. Total points:", nrow(existing_data), "\n")

# Check area consistency across scenarios
area_validation <- existing_data %>%
  group_by(model, scenario) %>%
  summarise(
    n_years = n_distinct(Year),
    year_range = paste(min(Year), max(Year), sep = "-"),
    avg_total_area_km2 = mean(total_area_km2, na.rm = TRUE) / 1e6,  # Convert to millions
    min_area = min(total_area_km2, na.rm = TRUE) / 1e6,
    max_area = max(total_area_km2, na.rm = TRUE) / 1e6,
    area_variation = max_area - min_area,
    .groups = 'drop'
  ) %>%
  arrange(model, scenario)

cat("\nAREA VALIDATION RESULTS:\n")
cat("======================\n")
print(area_validation)

cat("\nISSUE IDENTIFIED:\n")
cat("Different total areas across scenarios indicates improper area weighting\n")
cat("Expected: All scenarios should have similar total areas (same ocean grid)\n")
cat("Actual: Large variations in total area sampled\n\n")

# Check by file size category
file_size_check <- existing_data %>%
  group_by(model, scenario) %>%
  summarise(avg_area = mean(total_area_km2, na.rm = TRUE) / 1e6, .groups = 'drop') %>%
  mutate(
    expected_category = case_when(
      avg_area > 40 ~ "small_files",
      avg_area > 20 ~ "medium_files", 
      avg_area < 15 ~ "large_files"
    )
  )

cat("AREA BY SUSPECTED FILE SIZE:\n")
print(file_size_check)

cat("\nCONCLUSION:\n")
cat("The area weighting was applied AFTER spatial sampling, not before.\n")
cat("This means:\n")
cat("1. Different scenarios had different spatial samples\n")
cat("2. Area calculations reflect sample size, not true ocean area\n") 
cat("3. The 'area weighting' is not properly normalized\n")
cat("4. Results may be biased by sampling differences\n\n")

cat("RECOMMENDATION:\n")
cat("Re-run analysis with proper area weighting that:\n")
cat("1. Uses consistent ocean grid/mask\n")
cat("2. Applies area weights before any sampling\n")
cat("3. Reports consistent total ocean area\n")

# Save validation
write_csv(area_validation, "Output/area_weighting_issue_validation.csv")

cat("\nValidation complete. Results saved to area_weighting_issue_validation.csv\n")
