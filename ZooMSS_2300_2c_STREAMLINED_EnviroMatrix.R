# ==============================================================================
# STREAMLINED SST-CHLOROPHYLL COMBINATION ANALYSIS
# ==============================================================================
# Purpose: Single script to efficiently determine unique SST-Chl combinations
#          from 2300 ESM data and create comprehensive mapping to ZooMSS results
# ==============================================================================

library(tidyverse)
library(data.table)

# Source ZooMSS functions
source("fZooMSS_Xtras.R")

# Set directories
base_dir <- "~/R Projects/ZooMSS_2300/"
input_dir <- paste0(base_dir, "Input/")
processed_2300_dir <- paste0(input_dir, "2300_processed/")
enviro_dir <- paste0(base_dir, "Enviro_Matrix/")

cat("=== STREAMLINED ZOOMSS 2300 ENVIRONMENTAL MATRIX WORKFLOW ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOAD REFERENCE DATA
# ==============================================================================

cat("STEP 1: Loading reference data...\n")

# Load existing Climate Change combinations (for comparison)
climate_change_enviro <- readRDS(paste0(enviro_dir, "ClimateChange_Compiled_Distinct.rds"))
original_combinations <- climate_change_enviro %>%
  select(sst, chlo) %>%
  filter(!is.na(sst), !is.na(chlo)) %>%
  distinct() %>%
  mutate(source = "original")

cat("- Original Climate Change combinations:", nrow(original_combinations), "\n")

# Load ZooMSS results for validation
res_control <- readRDS(paste0(input_dir, "res_Control.RDS"))
cat("- Original ZooMSS results available:", length(res_control), "\n")

# ==============================================================================
# STEP 2: EXTRACT ALL UNIQUE COMBINATIONS FROM 2300 DATA (EFFICIENT VERSION)
# ==============================================================================

cat("\nSTEP 2: Extracting unique combinations from 2300 data...\n")

# Function to efficiently process all 2300 files
extract_unique_2300_combinations <- function(folder_path) {
  
  # Get all .rds files
  rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  cat("- Processing", length(rds_files), "files...\n")
  
  # Use data.table for memory efficiency
  all_combinations <- data.table()
  
  for (i in seq_along(rds_files)) {
    file_path <- rds_files[i]
    file_name <- basename(file_path)
    
    if (i %% 3 == 1) cat("  Processing file", i, "of", length(rds_files), "\n")
    
    # Load and extract unique combinations from current file
    current_data <- readRDS(file_path)
    
    # Extract just SST and Chl, convert to standard format
    current_combinations <- current_data %>%
      select(SST, Chl_log10) %>%
      mutate(
        sst = round(SST, 1),  # Round to 0.1°C precision
        chlo = round(10^Chl_log10, 3)  # Convert back and round to mg/m³
      ) %>%
      select(sst, chlo) %>%
      filter(!is.na(sst), !is.na(chlo)) %>%
      distinct()
    
    # Convert to data.table and combine
    current_dt <- as.data.table(current_combinations)
    all_combinations <- rbindlist(list(all_combinations, current_dt))
  }
  
  # Get unique combinations across all files
  unique_combinations <- all_combinations %>%
    distinct() %>%
    arrange(sst, chlo)
  
  return(unique_combinations)
}

# Extract all unique combinations from 2300 data
all_2300_combinations <- extract_unique_2300_combinations(processed_2300_dir)
cat("- Total unique 2300 combinations:", nrow(all_2300_combinations), "\n")

# ==============================================================================
# STEP 3: IDENTIFY NOVEL COMBINATIONS
# ==============================================================================

cat("\nSTEP 3: Identifying novel combinations...\n")

# Use anti_join to find combinations in 2300 but not in original
novel_combinations <- all_2300_combinations %>%
  anti_join(original_combinations, by = c("sst", "chlo")) %>%
  mutate(source = "novel")

# Apply reasonable filters
novel_combinations_filtered <- novel_combinations %>%
  filter(
    chlo > 0,        # Remove zero/negative chlorophyll
    chlo <= 20,      # Remove extremely high chlorophyll (likely errors)
    sst >= -5,       # Remove extremely low temperatures
    sst <= 35        # Remove extremely high temperatures
  )

cat("- Novel combinations (unfiltered):", nrow(novel_combinations), "\n")
cat("- Novel combinations (filtered):", nrow(novel_combinations_filtered), "\n")

# Calculate reusable combinations
reusable_combinations <- all_2300_combinations %>%
  inner_join(original_combinations, by = c("sst", "chlo")) %>%
  mutate(source = "reusable")

cat("- Reusable combinations:", nrow(reusable_combinations), "\n")

# ==============================================================================
# STEP 4: ADD PHYTOPLANKTON PARAMETERS
# ==============================================================================

cat("\nSTEP 4: Calculating phytoplankton parameters...\n")

# Function to safely add phytoplankton parameters
add_phyto_params_safe <- function(data) {
  result <- tryCatch({
    fZooMSS_CalculatePhytoParam(data)
  }, error = function(e) {
    cat("Warning: Error in phytoplankton calculation, adding placeholder columns\n")
    data %>%
      mutate(
        pico_biom = NA,
        nano_biom = NA, 
        micro_biom = NA,
        phyto_slope = NA,
        phyto_int = NA,
        phyto_max = NA
      )
  })
  return(result)
}

# Add phytoplankton parameters to novel combinations
novel_combinations_with_phyto <- add_phyto_params_safe(novel_combinations_filtered)

# Add phytoplankton parameters to all 2300 combinations
all_2300_combinations_with_phyto <- add_phyto_params_safe(all_2300_combinations)

cat("- Phytoplankton parameters calculated\n")

# ==============================================================================
# STEP 5: CREATE COMPREHENSIVE MAPPING SYSTEM
# ==============================================================================

cat("\nSTEP 5: Creating mapping system...\n")

# Create master environmental matrix combining original and novel
master_enviro_matrix <- bind_rows(
  # Original combinations (map to existing ZooMSS results)
  original_combinations %>%
    mutate(
      enviro_id = row_number(),
      zoomss_result_index = row_number(),  # Maps to res_Control indices
      result_source = "res_Control"
    ),
  
  # Novel combinations (map to new ZooMSS results) 
  novel_combinations_with_phyto %>%
    mutate(
      enviro_id = nrow(original_combinations) + row_number(),
      zoomss_result_index = length(res_control) + row_number(),  # Maps to res_ZooMSS_2300 indices
      result_source = "res_ZooMSS_2300"
    )
) %>%
  # Add lookup key for efficient matching
  mutate(
    sst_chl_key = paste(round(sst, 1), round(chlo, 3), sep = "_")
  )

cat("- Master environmental matrix created:", nrow(master_enviro_matrix), "combinations\n")
cat("- Original combinations:", sum(master_enviro_matrix$source == "original"), "\n") 
cat("- Novel combinations:", sum(master_enviro_matrix$source == "novel"), "\n")

# ==============================================================================
# STEP 6: VALIDATION
# ==============================================================================

cat("\nSTEP 6: Validation...\n")

# Check for duplicates
duplicate_keys <- master_enviro_matrix %>%
  count(sst_chl_key) %>%
  filter(n > 1)

cat("- Duplicate keys found:", nrow(duplicate_keys), "\n")

# Check data ranges
sst_range <- range(master_enviro_matrix$sst, na.rm = TRUE)
chlo_range <- range(master_enviro_matrix$chlo, na.rm = TRUE)

cat("- SST range:", sst_range[1], "to", sst_range[2], "°C\n")
cat("- Chlorophyll range:", chlo_range[1], "to", chlo_range[2], "mg/m³\n")

# Validate against expected ZooMSS result lengths
if (file.exists(paste0(input_dir, "res_ZooMSS_2300.RDS"))) {
  res_novel <- readRDS(paste0(input_dir, "res_ZooMSS_2300.RDS"))
  
  expected_novel <- sum(master_enviro_matrix$source == "novel")
  actual_novel <- length(res_novel)
  
  cat("- Expected novel ZooMSS results:", expected_novel, "\n")
  cat("- Actual novel ZooMSS results:", actual_novel, "\n")
  cat("- Match:", expected_novel == actual_novel, "\n")
} else {
  cat("- res_ZooMSS_2300.RDS not found for validation\n")
}

# ==============================================================================
# STEP 7: SAVE OUTPUTS (CLEAN NAMING)
# ==============================================================================

cat("\nSTEP 7: Saving final outputs...\n")

# Clean up old redundant files (backup first)
backup_dir <- paste0(enviro_dir, "backup_", format(Sys.Date(), "%Y%m%d"), "/")
if (!dir.exists(backup_dir)) {
  dir.create(backup_dir, recursive = TRUE)
}

# List files to backup
old_files <- c(
  "all_2300_sst_chl_combinations_complete.rds",
  "all_sst_chl_combinations_2300_wPhyto.rds",
  "novel_sst_chl_combinations_2300_vs_climate_change.rds",
  "novel_sst_chl_combinations_2300_filtered.rds"
)

for (file in old_files) {
  file_path <- paste0(enviro_dir, file)
  if (file.exists(file_path)) {
    file.copy(file_path, paste0(backup_dir, file))
    cat("- Backed up:", file, "\n")
  }
}

# Save new streamlined outputs
saveRDS(master_enviro_matrix, 
        paste0(enviro_dir, "master_enviro_matrix_2300.rds"))

saveRDS(novel_combinations_with_phyto,
        paste0(enviro_dir, "novel_sst_chl_combinations_2300_wPhyto.rds"))

saveRDS(all_2300_combinations_with_phyto,
        paste0(enviro_dir, "all_sst_chl_combinations_2300_wPhyto.rds"))

# Save summary for documentation
summary_info <- list(
  processing_date = Sys.time(),
  total_combinations = nrow(master_enviro_matrix),
  original_combinations = sum(master_enviro_matrix$source == "original"),
  novel_combinations = sum(master_enviro_matrix$source == "novel"),
  sst_range = sst_range,
  chlo_range = chlo_range,
  files_processed = length(list.files(processed_2300_dir, pattern = "\\.rds$"))
)

saveRDS(summary_info, paste0(enviro_dir, "processing_summary.rds"))

cat("- Master environmental matrix saved\n")
cat("- Novel combinations with phyto parameters saved\n") 
cat("- All 2300 combinations with phyto parameters saved\n")
cat("- Processing summary saved\n")

# ==============================================================================
# STEP 8: CREATE VISUALIZATION
# ==============================================================================

cat("\nSTEP 8: Creating visualization...\n")

# Plot comparison of original vs novel combinations
p_comparison <- ggplot() +
  geom_point(data = original_combinations, 
             aes(x = sst, y = log10(chlo)), 
             color = "blue", alpha = 0.6, size = 0.5) +
  geom_point(data = novel_combinations_filtered,
             aes(x = sst, y = log10(chlo)),
             color = "red", alpha = 0.6, size = 0.5) +
  labs(
    title = "SST-Chlorophyll Combinations: Original vs Novel (2300)",
    subtitle = "Blue = Original (reusable), Red = Novel (new ZooMSS runs needed)",
    x = "Sea Surface Temperature (°C)",
    y = "log₁₀(Chlorophyll mg/m³)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Figures/SST_Chl_Original_vs_Novel_2300.png", p_comparison, 
       width = 12, height = 8, dpi = 300)

cat("- Comparison plot saved\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n=== FINAL SUMMARY ===\n")
cat("Total environmental combinations for 2300 scenarios:", nrow(master_enviro_matrix), "\n")
cat("Combinations reusing existing ZooMSS results:", sum(master_enviro_matrix$source == "original"), "\n")
cat("Combinations requiring new ZooMSS results:", sum(master_enviro_matrix$source == "novel"), "\n")
cat("Computational efficiency: ", 
    round(sum(master_enviro_matrix$source == "original") / nrow(master_enviro_matrix) * 100, 1), 
    "% reuse\n")

cat("\nKey output files:\n")
cat("- master_enviro_matrix_2300.rds: Complete mapping system\n")
cat("- novel_sst_chl_combinations_2300_wPhyto.rds: Novel combinations only\n")
cat("- all_sst_chl_combinations_2300_wPhyto.rds: All 2300 combinations\n")
cat("- processing_summary.rds: Metadata and statistics\n")

cat("\nOld redundant files backed up to:", backup_dir, "\n")
cat("Workflow complete!\n")
