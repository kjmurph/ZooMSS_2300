# ZooMSS 2300 Data Integration Workflow
# Combine novel simulations with existing repo results for 2300 scenarios

library(dplyr)
library(purrr)
library(readr)

# Set paths
base_dir <- "~/R Projects/ZooMSS_2300/"
input_dir <- paste0(base_dir, "Input/")
processed_2300_dir <- paste0(input_dir, "2300_processed/")
enviro_dir <- paste0(base_dir, "Enviro_Matrix/")

# ============================================================================
# STEP 1: Load reference data
# ============================================================================

print("Loading reference data...")

# Load original compiled data (for variable structure reference)
climate_compiled <- readRDS(paste0(input_dir, "ClimateChange_Compiled.rds"))

# Load existing simulation results
res_control <- readRDS(paste0(input_dir, "res_Control.RDS"))

# Load novel combinations
novel_combinations <- readRDS(paste0(enviro_dir, "novel_sst_chl_combinations_2300_wPhyto.rds"))

print(paste("Original compiled data:", nrow(climate_compiled), "rows"))
print(paste("Novel combinations:", nrow(novel_combinations), "rows"))

# ============================================================================
# STEP 2: Load ZooMSS functions and create function to add missing variables
# ============================================================================

# Source the ZooMSS functions
source("fZooMSS_Xtras.R")  # Adjust path if needed

add_missing_variables <- function(data) {
  # Add missing variables using proper ZooMSS calculations

  data_updated <- data %>%
    mutate(
      # Rename Chl to chlo to match original structure
      chlo = Chl,
      Date = Year,

      # Calculate basic derived variables
      Chl_log10 = log10(chlo)
    )

  # Apply ZooMSS phytoplankton parameter calculations
  data_updated <- fZooMSS_CalculatePhytoParam(data_updated)

  # Calculate phy as sum of all phytoplankton biomass components
  data_final <- data_updated %>%
    mutate(
      phy = pico_biom + nano_biom + micro_biom,  # Total phytoplankton biomass
      Phy_log10 = log10(phy)
    ) %>%
    # Reorder columns to match original structure
    select(Lon, Lat, Date, SST, chlo, Model, Experiment,
           Chl_log10, Phy_log10, phy, pico_biom, nano_biom, micro_biom,
           phyto_slope, phyto_int, phyto_max, Year)

  return(data_final)
}

# ============================================================================
# STEP 3: Process all 2300 files to add missing variables
# ============================================================================

print("Processing 2300 files to add missing variables...")

# Get list of all .rds files in 2300_processed directory
rds_files_2300 <- list.files(processed_2300_dir, pattern = "\\.rds$", full.names = TRUE)

# Process each file
process_2300_file <- function(file_path) {
  file_name <- basename(file_path)
  print(paste("Processing:", file_name))

  # Read data
  data <- readRDS(file_path)

  # Add missing variables
  data_updated <- add_missing_variables(data)

  # Save updated file (backup original first)
  backup_path <- gsub("\\.rds$", "_backup.rds", file_path)
  if (!file.exists(backup_path)) {
    file.copy(file_path, backup_path)
  }

  # Save updated file
  saveRDS(data_updated, file_path)

  return(paste("Processed:", file_name, "- Rows:", nrow(data_updated)))
}

# Process all files
processing_results <- map_chr(rds_files_2300, process_2300_file)
print(processing_results)

# ============================================================================
# STEP 4: Create comprehensive chl-sst mapping system
# ============================================================================

print("Creating chl-sst mapping system...")

# Function to create unique chl-sst combinations from a dataset
create_chl_sst_key <- function(data, round_digits = 6) {
  data %>%
    mutate(
      sst_rounded = round(SST, round_digits),
      chl_rounded = round(chlo, round_digits),
      chl_sst_key = paste(sst_rounded, chl_rounded, sep = "_")
    ) %>%
    select(sst_rounded, chl_rounded, chl_sst_key)
}

# Create mapping from original climate_compiled data
original_chl_sst <- climate_compiled %>%
  create_chl_sst_key() %>%
  distinct() %>%
  mutate(original_index = row_number())

print(paste("Original unique chl-sst combinations:", nrow(original_chl_sst)))

# ============================================================================
# STEP 5: Analyze 2300 data combinations
# ============================================================================

print("Analyzing 2300 data combinations...")

# Combine all 2300 processed files to get all unique combinations
all_2300_combinations <- map_dfr(rds_files_2300, function(file_path) {
  data <- readRDS(file_path)
  create_chl_sst_key(data) %>%
    distinct() %>%
    mutate(source_file = basename(file_path))
})

# Get unique combinations across all 2300 files
unique_2300_combinations <- all_2300_combinations %>%
  distinct(chl_sst_key, .keep_all = TRUE) %>%
  mutate(combo_2300_index = row_number())

print(paste("Total unique 2300 combinations:", nrow(unique_2300_combinations)))

# ============================================================================
# STEP 6: Create matching matrix
# ============================================================================

print("Creating matching matrix...")

# Match 2300 combinations with original combinations
matching_matrix <- unique_2300_combinations %>%
  left_join(original_chl_sst, by = "chl_sst_key") %>%
  mutate(
    is_novel = is.na(original_index),
    match_type = case_when(
      is_novel ~ "novel",
      !is_novel ~ "existing"
    )
  )

# Summary of matches
match_summary <- matching_matrix %>%
  count(match_type) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Matching summary:")
print(match_summary)

# ============================================================================
# STEP 7: Create simulation result mapping
# ============================================================================

print("Creating simulation result mapping...")

# Function to map simulation results
create_simulation_mapping <- function(matching_matrix, novel_sim_results, existing_sim_results) {

  # Create mapping for existing combinations
  existing_mapping <- matching_matrix %>%
    filter(match_type == "existing") %>%
    mutate(sim_result_index = original_index)

  # Create mapping for novel combinations
  novel_mapping <- matching_matrix %>%
    filter(match_type == "novel") %>%
    mutate(
      novel_sequence = row_number(),
      sim_result_index = nrow(existing_sim_results) + novel_sequence
    )

  # Combine mappings
  complete_mapping <- bind_rows(existing_mapping, novel_mapping) %>%
    arrange(combo_2300_index)

  return(complete_mapping)
}

# Create the mapping (assuming novel simulation results exist)
# simulation_mapping <- create_simulation_mapping(matching_matrix, novel_sim_results, res_control)

# ============================================================================
# STEP 8: Validation and summary functions
# ============================================================================

# Function to validate the mapping
validate_mapping <- function(mapping_df) {
  validation_results <- list(
    total_combinations = nrow(mapping_df),
    novel_combinations = sum(mapping_df$match_type == "novel"),
    existing_combinations = sum(mapping_df$match_type == "existing"),
    missing_mappings = sum(is.na(mapping_df$sim_result_index)),
    duplicate_keys = mapping_df %>% count(chl_sst_key) %>% filter(n > 1) %>% nrow()
  )

  return(validation_results)
}

# Function to create summary report
create_summary_report <- function() {

  cat("\n=== ZOOMSS 2300 INTEGRATION SUMMARY ===\n")
  cat("Date:", Sys.time(), "\n\n")

  cat("FILES PROCESSED:\n")
  cat("- Original compiled data:", nrow(climate_compiled), "rows\n")
  cat("- Novel combinations file:", nrow(novel_combinations), "rows\n")
  cat("- 2300 processed files:", length(rds_files_2300), "files\n\n")

  cat("COMBINATION ANALYSIS:\n")
  print(match_summary)
  cat("\n")

  cat("VALIDATION:\n")
  validation <- validate_mapping(matching_matrix)
  cat("- Total combinations:", validation$total_combinations, "\n")
  cat("- Novel combinations:", validation$novel_combinations, "\n")
  cat("- Existing combinations:", validation$existing_combinations, "\n")
  cat("- Missing mappings:", validation$missing_mappings, "\n")
  cat("- Duplicate keys:", validation$duplicate_keys, "\n\n")

  cat("NEXT STEPS:\n")
  cat("1. Verify novel simulation results are available\n")
  cat("2. Run simulation mapping creation\n")
  cat("3. Compile final 2300 scenario results\n")
  cat("4. Validate against expected totals\n")
}

# ============================================================================
# STEP 9: Save intermediate results
# ============================================================================

print("Saving intermediate results...")

# Save mapping matrix
saveRDS(matching_matrix, paste0(enviro_dir, "chl_sst_matching_matrix.rds"))

# Save unique 2300 combinations
saveRDS(unique_2300_combinations, paste0(enviro_dir, "unique_2300_combinations.rds"))

# Save summary
create_summary_report()

# ============================================================================
# STEP 10: Helper functions for next steps
# ============================================================================

# Function to compile final 2300 scenario (to be run after simulation mapping)
compile_2300_scenario <- function(esm_scenario_file, simulation_mapping, novel_sim_results, existing_sim_results) {

  # Load ESM scenario data
  scenario_data <- readRDS(esm_scenario_file)

  # Create chl-sst keys for scenario data
  scenario_with_keys <- scenario_data %>%
    create_chl_sst_key() %>%
    left_join(simulation_mapping %>% select(chl_sst_key, sim_result_index, match_type),
              by = "chl_sst_key")

  # Add simulation results based on mapping
  # This would need to be implemented based on the structure of your simulation results

  return(scenario_with_keys)
}

print("\n=== WORKFLOW COMPLETE ===")
print("Check the summary above and proceed with simulation result integration.")
print("Use the saved matching matrix and unique combinations for the next steps.")