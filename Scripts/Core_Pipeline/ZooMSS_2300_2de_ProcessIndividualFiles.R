# ==============================================================================
# ZooMSS_2300: Process Individual Model-Scenario Files
# ==============================================================================
# 
# Purpose: Apply phytoplankton parameter calculations to each model-scenario file
#          Combines steps 2d and 2e without creating large combined file
#
# Input:  Input/2300_processed/2300_*.rds (from step 2b)
# Output: Output/ClimateChange_2300_*_*.rds (ready for step 3d)
#
# ==============================================================================

library(tidyverse)
source("Scripts/Utilities/fZooMSS_Xtras.R")

cat("==============================================================================\n")
cat("ZooMSS 2300 - Process Model-Scenario Files\n")
cat("==============================================================================\n\n")

# Set directories
base_dir <- getwd()
input_dir <- file.path(base_dir, "Input/2300_processed/")
output_dir <- file.path(base_dir, "Output/")

# Create output directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get list of all input .rds files
input_files <- list.files(input_dir, pattern = "^2300_.*\\.rds$", full.names = TRUE)

cat("Found", length(input_files), "model-scenario files to process\n\n")

if (length(input_files) == 0) {
  stop("No input files found in ", input_dir)
}

# Process each file individually
for(i in 1:length(input_files)) {
  
  file_path <- input_files[i]
  file_name <- basename(file_path)
  
  cat("==============================================================================\n")
  cat("Processing file", i, "of", length(input_files), "\n")
  cat("Input:", file_name, "\n")
  
  # Read the file
  df <- readRDS(file_path)
  
  cat("  Rows:", nrow(df), "\n")
  cat("  Columns:", paste(names(df), collapse=", "), "\n")
  
  # Print structure for first file
  if(i == 1) {
    cat("\n  Original structure:\n")
    print(glimpse(df))
  }
  
  # Step 1: Rename columns to match expected names
  cat("\n  Step 1: Renaming columns...\n")
  
  if("Chl" %in% names(df)) {
    df <- df %>% rename(chlo = Chl)
    cat("    ✓ Renamed Chl to chlo\n")
  }
  
  if("Year" %in% names(df)) {
    df <- df %>% rename(Date = Year)
    cat("    ✓ Renamed Year to Date\n")
  }
  
  # Ensure required columns exist
  if(!"chlo" %in% names(df)) {
    stop("Column 'chlo' not found in file: ", file_name)
  }
  
  # Step 2: Calculate phytoplankton parameters
  cat("\n  Step 2: Calculating phytoplankton parameters...\n")
  cat("    (pico_biom, nano_biom, micro_biom, phyto_slope, phyto_int, phyto_max)\n")
  
  df_processed <- fZooMSS_CalculatePhytoParam(df)
  
  cat("    ✓ Phytoplankton parameters calculated\n")
  
  # Step 3: Add derived columns
  cat("\n  Step 3: Adding derived columns...\n")
  
  df_processed <- df_processed %>%
    mutate(
      phy = pico_biom + nano_biom + micro_biom,
      Phy_log10 = log10(phy)
    )
  
  cat("    ✓ Added phy (total phytoplankton biomass)\n")
  cat("    ✓ Added Phy_log10\n")
  
  # Step 4: Reorder columns for consistency
  cat("\n  Step 4: Reordering columns...\n")
  
  df_processed <- df_processed %>%
    select(Lon, Lat, Date, SST, chlo, Model, Experiment, Chl_log10,
           Phy_log10, phy, pico_biom, nano_biom, micro_biom,
           phyto_slope, phyto_int, phyto_max)
  
  cat("    ✓ Columns reordered\n")
  
  # Step 5: Create output filename
  # Extract model and experiment from input filename
  # Input format: "2300_model_experiment.rds"
  # Output format: "ClimateChange_2300_model_experiment.rds"
  
  parts <- str_split(file_name, "_")[[1]]
  if (length(parts) >= 3) {
    # Remove "2300" prefix and ".rds" suffix
    model_exp <- str_remove(file_name, "^2300_")
    model_exp <- str_remove(model_exp, "\\.rds$")
    output_filename <- paste0("ClimateChange_2300_", model_exp, ".rds")
  } else {
    # Fallback
    output_filename <- str_replace(file_name, "^2300_", "ClimateChange_2300_")
  }
  
  output_path <- file.path(output_dir, output_filename)
  
  # Step 6: Save processed file
  cat("\n  Step 6: Saving processed file...\n")
  cat("  Output:", output_filename, "\n")
  
  saveRDS(df_processed, output_path)
  
  output_size_mb <- file.size(output_path) / 1024^2
  cat("    ✓ Saved (", round(output_size_mb, 1), "MB )\n")
  
  # Print summary for this file
  cat("\n  Summary:\n")
  cat("    Model:", unique(df_processed$Model), "\n")
  cat("    Experiment:", unique(df_processed$Experiment), "\n")
  cat("    Years:", min(df_processed$Date), "to", max(df_processed$Date), "\n")
  cat("    Total years:", length(unique(df_processed$Date)), "\n")
  cat("    Rows:", nrow(df_processed), "\n")
  
  # Print structure for first file after processing
  if(i == 1) {
    cat("\n  Processed structure:\n")
    print(glimpse(df_processed))
  }
  
  # Clean up
  rm(df, df_processed)
  gc()
  
  cat("\n")
}

# Create summary of all processed files
cat("==============================================================================\n")
cat("Processing Complete!\n")
cat("==============================================================================\n\n")

output_files <- list.files(output_dir, pattern = "^ClimateChange_2300_.*\\.rds$", full.names = TRUE)

file_summary <- data.frame(
  filename = basename(output_files),
  size_mb = sapply(output_files, function(f) file.size(f) / 1024^2),
  stringsAsFactors = FALSE
) %>%
  arrange(filename)

cat("Created", nrow(file_summary), "output files:\n\n")
print(file_summary %>% mutate(size_mb = round(size_mb, 1)))

cat("\nTotal size:", round(sum(file_summary$size_mb) / 1024, 2), "GB\n")
cat("Average file size:", round(mean(file_summary$size_mb), 1), "MB\n")

cat("\n==============================================================================\n")
cat("Files ready for Step 3d (Experiments)\n")
cat("==============================================================================\n")

cat("\nOutput files are in:", output_dir, "\n")
cat("Format: ClimateChange_2300_<model>_<experiment>.rds\n\n")

cat("Each file contains:\n")
cat("  - Lon, Lat, Date (year), SST, chlo\n")
cat("  - Model, Experiment metadata\n")
cat("  - Chl_log10, Phy_log10\n")
cat("  - Phytoplankton size classes: pico_biom, nano_biom, micro_biom\n")
cat("  - Total phytoplankton: phy\n")
cat("  - Phytoplankton parameters: phyto_slope, phyto_int, phyto_max\n\n")

cat("==============================================================================\n")
