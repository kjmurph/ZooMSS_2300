library(tidyverse)
source("Scripts/Utilities/fZooMSS_Xtras.R")

# Set your base directory and input path
base_dir <- getwd()  # Use current working directory
input_dir <- file.path(base_dir, "Input/2300_processed/")

# Get list of all .rds files in the directory
rds_files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)

print(paste("Found", length(rds_files), "RDS files to process:"))
print(basename(rds_files))

# Initialize list to store processed dataframes
processed_data_list <- list()

# Process each file
for(i in 1:length(rds_files)) {

  file_path <- rds_files[i]
  file_name <- basename(file_path)

  cat("\nProcessing file", i, "of", length(rds_files), ":", file_name, "\n")

  # Read the file
  df <- readRDS(file_path)

  # Print original structure for first file
  if(i == 1) {
    cat("Original structure of first file:\n")
    print(glimpse(df))
  }

  # Rename Chl to chlo to match expected column name
  if("Chl" %in% names(df)) {
    df <- df %>% rename(chlo = Chl)
  }

  # Rename Year to Date to match original structure
  if("Year" %in% names(df)) {
    df <- df %>% rename(Date = Year)
  }

  # Ensure chlo column exists before calculating phyto parameters
  if(!"chlo" %in% names(df)) {
    stop(paste("Column 'chlo' not found in file:", file_name))
  }

  # Apply phytoplankton parameter calculations
  cat("Calculating phytoplankton parameters...\n")
  df_processed <- fZooMSS_CalculatePhytoParam(df)

  # Add additional columns to match original structure
  # Calculate total phytoplankton biomass (phy)
  df_processed <- df_processed %>%
    mutate(
      phy = pico_biom + nano_biom + micro_biom,
      Phy_log10 = log10(phy)
    )

  # Reorder columns to match original structure
  df_processed <- df_processed %>%
    select(Lon, Lat, Date, SST, chlo, Model, Experiment, Chl_log10,
           Phy_log10, phy, pico_biom, nano_biom, micro_biom,
           phyto_slope, phyto_int, phyto_max)

  # Store processed dataframe
  processed_data_list[[i]] <- df_processed

  cat("Processed", nrow(df_processed), "rows\n")

  # Print structure for first file after processing
  if(i == 1) {
    cat("Structure after processing:\n")
    print(glimpse(df_processed))
  }
}

# Save each processed file back to temp location first
cat("\nSaving processed files individually...\n")
temp_dir <- file.path(base_dir, "Output/temp_compiled/")
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

for(i in 1:length(processed_data_list)) {
  temp_file <- file.path(temp_dir, paste0("processed_", i, ".rds"))
  saveRDS(processed_data_list[[i]], temp_file)
  cat("Saved temp file", i, "of", length(processed_data_list), "\n")
}

# Clear memory before combining
rm(processed_data_list)
gc()

# Combine files incrementally to avoid memory issues
cat("\nCombining all processed files incrementally...\n")
output_file <- file.path(base_dir, "Output/ClimateChange_2300_Compiled.rds")

# Initialize with first file
combined_data <- readRDS(file.path(temp_dir, "processed_1.rds"))
total_rows <- nrow(combined_data)
cat("Loaded file 1:", total_rows, "rows\n")

# Add remaining files one at a time
for(i in 2:length(rds_files)) {
  temp_file <- file.path(temp_dir, paste0("processed_", i, ".rds"))
  next_chunk <- readRDS(temp_file)
  combined_data <- bind_rows(combined_data, next_chunk)
  rm(next_chunk)
  gc()
  
  total_rows <- nrow(combined_data)
  cat("Added file", i, "- Total rows now:", total_rows, "\n")
}

# Print summary of combined data
cat("\nSummary of combined data:\n")
print(glimpse(combined_data))

# Summary by Model and Experiment
cat("\nData distribution by Model and Experiment:\n")
summary_table <- combined_data %>%
  group_by(Model, Experiment) %>%
  summarise(
    rows = n(),
    years = n_distinct(Date),
    .groups = 'drop'
  )
print(summary_table)

# Save the combined dataset
saveRDS(combined_data, output_file)

cat("\nCombined dataset saved to:", output_file, "\n")
cat("Total rows in combined dataset:", nrow(combined_data), "\n")

# Clean up temp files and memory
cat("\nCleaning up temporary files...\n")
unlink(temp_dir, recursive = TRUE)
rm(combined_data)
gc()

cat("\nProcessing complete!\n")