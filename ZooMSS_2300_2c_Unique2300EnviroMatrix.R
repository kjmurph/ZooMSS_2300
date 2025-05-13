library(tidyverse)
library(data.table)

## Test example with one model/experiment combo
df_cesm_hist <- readRDS("~/R Projects/ZooMSS_2300/Input/2300_processed/2300_cesm2-waccm_historical.rds") # "~/Nextcloud/MME2Data/ZooMSS_2300/Inputs/2300_processed/..."

glimpse(df_cesm_hist)

# Get unique combinations of SST and Chl
unique_combinations <- unique(df_cesm_hist[, c("SST", "Chl_log10")])

# Print statistics
cat("Original data count:", nrow(df_cesm_hist), "\n")
cat("Unique combinations count:", nrow(unique_combinations), "\n")
cat("Data reduction:", round((nrow(df_cesm_hist) - nrow(unique_combinations)) / nrow(df_cesm_hist) * 100, 2), "%\n")

process_rds_files <- function(folder_path) {
  # Get list of all .rds files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Create empty master data.table
  master_dt <- data.table(SST = numeric(), Chl_log10 = numeric())

  # Track progress
  total_files <- length(all_files)
  cat("Processing", total_files, ".rds files...\n")

  # Process each file
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)

    # Progress update
    cat(sprintf("[%d/%d] Processing file: %s\n", i, total_files, file_name))

    # Read current RDS file
    current_df <- readRDS(file_path)

    # Convert to data.table if it's not already
    if (!is.data.table(current_df)) {
      current_dt <- as.data.table(current_df[, c("SST", "Chl_log10")])
    } else {
      current_dt <- current_df[, .(SST, Chl_log10)]
    }

    # Get unique combinations from current file
    current_unique <- unique(current_dt)

    # If master is empty, initialize it with first file's unique values
    if (nrow(master_dt) == 0) {
      master_dt <- current_unique
      cat(sprintf("  Added %d unique combinations to master data.table\n", nrow(current_unique)))
    } else {
      # Find new combinations efficiently using data.table syntax
      setkey(master_dt, SST, Chl_log10)
      setkey(current_unique, SST, Chl_log10)

      # Find rows in current_unique that aren't in master_dt
      new_combinations <- current_unique[!master_dt]

      # Add new combinations to master
      if (nrow(new_combinations) > 0) {
        master_dt <- rbindlist(list(master_dt, new_combinations))
        cat(sprintf("  Added %d new unique combinations to master data.table\n", nrow(new_combinations)))
      } else {
        cat("  No new unique combinations found in this file\n")
      }
    }

    # Report current size of master dataframe
    cat(sprintf("  Master data.table now contains %d unique combinations\n", nrow(master_dt)))
  }

  cat("Processing complete!\n")
  cat("Final master data.table contains", nrow(master_dt), "unique SST and Chl_log10 combinations\n")

  # Return the master data.table
  return(master_dt)
}


folder_path <- "~/R Projects/ZooMSS_2300/Input/2300_processed/" # "~/Nextcloud/MME2Data/ZooMSS_2300/Inputs/2300_processed/..."
master_dt <- process_rds_files(folder_path)

# Save results
saveRDS(master_dt, "Enviro_Matrix/enviro_matrix_2300_all_unique_sst_chl_log10_combinations.rds")

enviro_data <- master_dt %>%
  mutate(chlo = 10^Chl_log10) %>%
  dplyr::select(c(SST, chlo)) %>%
  arrange(desc(chlo), desc(SST)) %>%
  filter(is.na(chlo)==FALSE) %>%
  filter(is.na(SST)==FALSE) %>%
  rename(sst = SST)


# Read in distinct SST-chl combinations from Climate Change repo
ClimateChange_Compiled_Distinct <- readRDS("Enviro_Matrix/ClimateChange_Compiled_Distinct.RDS")


# Create a dataframe of unique combinations from enviro_data
enviro_2300_unique <- unique(enviro_data[, c("sst", "chlo")])

# Create a dataframe of unique combinations from ClimateChange_Compiled_Distinct
climate_change_unique <- unique(ClimateChange_Compiled_Distinct[, c("sst", "chlo")])

# Find rows in enviro_2300_unique that are not in climate_change_unique
enviro_2300_distinct <- anti_join(enviro_2300_unique, climate_change_unique, by = c("sst", "chlo"))

enviro_2300_distinct_trim <- enviro_2300_distinct %>%
  filter(!chlo>10)

# plot the unique 2300
ggplot(data = enviro_2300_distinct, mapping = aes(x = sst, y = log10(chlo))) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

saveRDS(enviro_2300_distinct, "Enviro_Matrix/enviro_matrix_unique_to_2300.RDS")

source("fZooMSS_Xtras.R")

enviro_wPhyto <- fZooMSS_CalculatePhytoParam(enviro_2300_distinct)



