library(tidyverse)
source("fZooMSS_Xtras.R")

# Set your base directory and input path
base_dir <- "~/R Projects/ZooMSS_2300/"
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

# Combine all processed dataframes
cat("\nCombining all processed files...\n")
combined_data <- bind_rows(processed_data_list)

# Print summary of combined data
cat("\nSummary of combined data:\n")
print(glimpse(combined_data))

# Check for any missing values
cat("\nChecking for missing values:\n")
missing_summary <- combined_data %>%
  summarise_all(~sum(is.na(.)))
print(missing_summary)

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
output_file <- file.path(base_dir, "Output/ClimateChange_2300_Compiled.rds")
saveRDS(combined_data, output_file)

cat("\nCombined dataset saved to:", output_file, "\n")
cat("Total rows in combined dataset:", nrow(combined_data), "\n")

# Clean up memory
rm(processed_data_list)
gc()

cat("\nProcessing complete!\n")