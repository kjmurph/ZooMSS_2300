library(tidyverse)

# Set base directory
base_dir <- "~/R Projects/ZooMSS_2300/"

cat("Creating full environmental matrix for 2300 scenarios...\n")

## Step 1: Load the 16,542 new combinations (in HPC simulation order)
enviro_unique_2300 <- read_rds(file.path(base_dir, "Enviro_Matrix/novel_sst_chl_combinations_2300_wPhyto.rds")) # Adjust path as needed

cat("Loaded", nrow(enviro_unique_2300), "new combinations\n")

# Ensure it has the required columns
required_cols <- c("sst", "chlo", "phyto_slope", "phyto_int", "phyto_max")
if(!all(required_cols %in% names(enviro_unique_2300))) {
  stop("Missing required columns in enviro_unique_2300. Expected: ", paste(required_cols, collapse = ", "))
}

## Step 2: Load the 98,896 unique combinations for 2300 scenarios
# Use your simple dataframe instead of the large climate file
all_2300_combinations <- read_rds(file.path(base_dir, "Enviro_Matrix/all_sst_chl_combinations_2300_wPhyto.rds")) # Adjust filename as needed

cat("Loaded", nrow(all_2300_combinations), "unique combinations for 2300 scenarios\n")

# Ensure column names match expected format
if("SST" %in% names(all_2300_combinations)) {
  all_2300_combinations <- all_2300_combinations %>% rename(sst = SST)
}

# Verify required columns
required_cols <- c("sst", "chlo", "phyto_slope", "phyto_int", "phyto_max")
if(!all(required_cols %in% names(all_2300_combinations))) {
  stop("Missing required columns in unique combinations file. Expected: ", paste(required_cols, collapse = ", "))
}

# Sort for consistency
all_2300_combinations <- all_2300_combinations %>%
  arrange(sst, chlo)

## Step 3: Find the remaining combinations (not in the new 16,542)
cat("Finding remaining combinations not covered by new simulations...\n")

# Use anti_join to find combinations NOT in the new set
remaining_combinations <- all_2300_combinations %>%
  anti_join(enviro_unique_2300, by = required_cols)

cat("Remaining combinations to fill with existing simulations:", nrow(remaining_combinations), "\n")

# Verify the math
total_expected <- nrow(enviro_unique_2300) + nrow(remaining_combinations)
cat("Verification: 16,542 + ", nrow(remaining_combinations), " = ", total_expected, "\n")

if(total_expected != nrow(all_2300_combinations)) {
  warning("Total doesn't match! Expected: ", nrow(all_2300_combinations), ", Got: ", total_expected)
}

## Step 4: Create the full environmental matrix with correct ordering
cat("Creating full environmental matrix...\n")

# New combinations FIRST (rows 1-16,542), then remaining combinations
enviro_matrix_2300_full <- bind_rows(
  enviro_unique_2300 %>% mutate(source = "new_simulation"),
  remaining_combinations %>% mutate(source = "existing_match")
) %>%
  mutate(
    row_id = row_number(),
    FID = row_number()
  ) %>%
  select(FID, sst, chlo, phyto_slope, phyto_int, phyto_max, source, row_id)

cat("Full environmental matrix created with", nrow(enviro_matrix_2300_full), "combinations\n")

## Step 5: Summary and verification
cat("\nSummary:\n")
summary_table <- enviro_matrix_2300_full %>%
  group_by(source) %>%
  summarise(
    count = n(),
    first_row = min(row_id),
    last_row = max(row_id),
    .groups = 'drop'
  )
print(summary_table)

# Check that new simulations are in rows 1-16,542
new_sim_rows <- enviro_matrix_2300_full %>%
  filter(source == "new_simulation") %>%
  pull(row_id)

if(all(new_sim_rows == 1:nrow(enviro_unique_2300))) {
  cat("✓ New simulations correctly placed in rows 1-", nrow(enviro_unique_2300), "\n")
} else {
  warning("❌ New simulations not in expected positions!")
}

## Step 6: Save the full matrix
output_file <- file.path(base_dir, "Enviro_Matrix/enviro_matrix_2300_full.rds")
saveRDS(enviro_matrix_2300_full, output_file)

cat("\nFull environmental matrix saved to:", output_file, "\n")

# # Also save a CSV for inspection
# csv_file <- file.path(base_dir, "Input/enviro_matrix_2300_full.csv")
# write_csv(enviro_matrix_2300_full, csv_file)
# cat("CSV version saved to:", csv_file, "\n")

## Step 7: Create a clean version without helper columns for the main script
enviro_matrix_clean <- enviro_matrix_2300_full %>%
  select(FID, sst, chlo, phyto_slope, phyto_int, phyto_max)

saveRDS(enviro_matrix_clean, file.path(base_dir, "Enviro_Matrix/enviro_matrix_2300_full_clean.rds"))

cat("\nClean version (for main script) saved as: enviro_matrix_2300_full_clean.rds\n")

cat("\n=== Summary ===\n")
cat("• Rows 1-16,542: NEW simulations (use HPC results directly)\n")
cat("• Rows 16,543-", nrow(enviro_matrix_2300_full), ": Existing combinations (use nearest neighbor matching)\n")
cat("• Ready for compilation script!\n")