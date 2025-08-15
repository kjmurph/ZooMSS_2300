library(tidyverse)
library(data.table)

# ==============================================================================
# SST-CHLOROPHYLL COMBINATION ANALYSIS WORKFLOW
# ==============================================================================
# Purpose: Extract unique SST-chlorophyll combinations from 2300 simulation data
#          and identify combinations that are novel compared to existing
#          Climate Change project data
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: INITIAL DATA EXPLORATION
# ------------------------------------------------------------------------------
# Load a single file to understand data structure and get initial statistics

# RDM storage for Inputs folder
#~/Nextcloud/MME1Data/ZooMSS_2300/Inputs/...

# Load sample file for exploration
df_cesm_hist <- readRDS("~/R Projects/ZooMSS_2300/Input/2300_processed/2300_cesm2-waccm_historical.rds")

glimpse(df_cesm_hist)

# Analyze data density to understand potential for reduction
sst_chl_combinations_sample <- unique(df_cesm_hist[, c("SST", "Chl_log10")])

# Print data reduction statistics
cat("=== Initial Data Analysis ===\n")
cat("Original data points:", nrow(df_cesm_hist), "\n")
cat("Unique SST-Chl combinations:", nrow(sst_chl_combinations_sample), "\n")
cat("Data reduction potential:", round((nrow(df_cesm_hist) - nrow(sst_chl_combinations_sample)) / nrow(df_cesm_hist) * 100, 2), "%\n")

# ------------------------------------------------------------------------------
# SECTION 2: EXTRACT ALL UNIQUE SST-CHLOROPHYLL COMBINATIONS FROM 2300 DATA
# ------------------------------------------------------------------------------

process_simulation_files <- function(folder_path) {
  # Get all .rds files in the specified folder
  rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Initialize master data.table to store all unique combinations
  master_sst_chl_combinations <- data.table(SST = numeric(), Chl_log10 = numeric())

  # Progress tracking
  total_files <- length(rds_files)
  cat("\n=== Processing", total_files, "simulation files ===\n")

  # Process each simulation file
  for (i in seq_along(rds_files)) {
    file_path <- rds_files[i]
    file_name <- basename(file_path)

    # Progress update
    cat(sprintf("[%d/%d] Processing: %s\n", i, total_files, file_name))

    # Load current simulation data
    current_simulation <- readRDS(file_path)

    # Convert to data.table and select only SST and Chl_log10 columns
    current_dt <- if (!is.data.table(current_simulation)) {
      as.data.table(current_simulation[, c("SST", "Chl_log10")])
    } else {
      current_simulation[, .(SST, Chl_log10)]
    }

    # Extract unique combinations from current file
    unique_combinations_current <- unique(current_dt)

    # Handle first file initialization
    if (nrow(master_sst_chl_combinations) == 0) {
      master_sst_chl_combinations <- unique_combinations_current
      cat(sprintf("  → Added %d combinations to master collection\n", nrow(unique_combinations_current)))
    } else {
      # Use data.table's efficient anti-join to find new combinations
      setkey(master_sst_chl_combinations, SST, Chl_log10)
      setkey(unique_combinations_current, SST, Chl_log10)

      # Find combinations in current file not yet in master collection
      new_combinations <- unique_combinations_current[!master_sst_chl_combinations]

      # Add new combinations if any exist
      if (nrow(new_combinations) > 0) {
        master_sst_chl_combinations <- rbindlist(list(master_sst_chl_combinations, new_combinations))
        cat(sprintf("  → Added %d NEW combinations\n", nrow(new_combinations)))
      } else {
        cat("  → No new combinations found\n")
      }
    }

    cat(sprintf("  → Total unique combinations: %d\n", nrow(master_sst_chl_combinations)))
  }

  cat("\n=== File Processing Complete ===\n")
  cat("Final count of unique SST-Chlorophyll combinations:", nrow(master_sst_chl_combinations), "\n")

  return(master_sst_chl_combinations)
}

# Process all 2300 simulation files
folder_path <- "~/R Projects/ZooMSS_2300/Input/2300_processed/"
all_2300_sst_chl_combinations <- process_simulation_files(folder_path)

# Save complete set of 2300 combinations for future reference
saveRDS(all_2300_sst_chl_combinations,
        "Enviro_Matrix/all_2300_sst_chl_combinations_complete.rds")

# ------------------------------------------------------------------------------
# SECTION 3: PREPARE DATA FOR COMPARISON
# ------------------------------------------------------------------------------

# Transform data for analysis:
# 1. Convert log10(chlorophyll) back to chlorophyll
# 2. Rename columns for consistency
# 3. Remove any missing values
# 4. Sort for easier inspection

sst_chl_2300_prepared <- all_2300_sst_chl_combinations %>%
  mutate(chlo = 10^Chl_log10) %>%          # Convert from log10 back to original scale
  select(SST, chlo) %>%                     # Select and rename columns
  rename(sst = SST) %>%                     # Standardize column names
  filter(!is.na(chlo), !is.na(sst)) %>%    # Remove any missing values
  arrange(desc(chlo), desc(sst)) %>%        # Sort by chlorophyll (desc), then SST (desc)
  distinct()                                # Ensure uniqueness after transformations

cat("=== Data Preparation Summary ===\n")
cat("2300 combinations after preparation:", nrow(sst_chl_2300_prepared), "\n")

# ------------------------------------------------------------------------------
# SECTION 4: IDENTIFY NOVEL COMBINATIONS VS CLIMATE CHANGE DATA
# ------------------------------------------------------------------------------

# Load existing Climate Change project data for comparison
climate_change_sst_chl <- readRDS("Enviro_Matrix/ClimateChange_Compiled_Distinct.RDS")

# Ensure both datasets have identical column structures
climate_change_prepared <- climate_change_sst_chl %>%
  select(sst, chlo) %>%
  filter(!is.na(chlo), !is.na(sst)) %>%
  distinct()

# Find combinations that exist in 2300 data but NOT in Climate Change data
novel_2300_combinations <- anti_join(sst_chl_2300_prepared,
                                     climate_change_prepared,
                                     by = c("sst", "chlo"))

# Apply chlorophyll filter to remove extreme values (optional)
novel_2300_combinations_filtered <- novel_2300_combinations %>%
  filter(chlo <= 10)  # Remove chlorophyll values > 10

cat("=== Comparison Results ===\n")
cat("Climate Change combinations:", nrow(climate_change_prepared), "\n")
cat("2300 combinations:", nrow(sst_chl_2300_prepared), "\n")
cat("Novel 2300 combinations (not in Climate Change):", nrow(novel_2300_combinations), "\n")
cat("Novel 2300 combinations (filtered):", nrow(novel_2300_combinations_filtered), "\n")

# ------------------------------------------------------------------------------
# SECTION 5: VISUALIZATION AND OUTPUT
# ------------------------------------------------------------------------------

# Create visualization of novel combinations
p_novel_combinations <- ggplot(data = novel_2300_combinations,
                               mapping = aes(x = sst, y = log10(chlo))) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", name = "Count") +
  labs(
    title = "Novel SST-Chlorophyll Combinations in 2300 Simulations",
    subtitle = "Combinations not present in Climate Change dataset",
    x = "Sea Surface Temperature (°C)",
    y = "log₁₀(Chlorophyll)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

print(p_novel_combinations)

ggsave("Figures/Novel_SSTChl_2300.png", p_novel_combinations, width = 12, height = 8, dpi = 300)

# Save results with descriptive names
saveRDS(novel_2300_combinations,
        "Enviro_Matrix/novel_sst_chl_combinations_2300_vs_climate_change.rds")

saveRDS(novel_2300_combinations_filtered,
        "Enviro_Matrix/novel_sst_chl_combinations_2300_filtered.rds")


# ------------------------------------------------------------------------------
# SECTION 6: CALCULATE PHYTOPLANKTON PARAMETERS (if needed)
# ------------------------------------------------------------------------------

# Load additional functions if required
source("fZooMSS_Xtras.R")

# Calculate phytoplankton parameters for novel combinations

sst_chl_with_phyto_params <- fZooMSS_CalculatePhytoParam(novel_2300_combinations)

all_sst_chl_2300_with_phyto_params <- fZooMSS_CalculatePhytoParam(sst_chl_2300_prepared)

# Check against enviro matrix from enviro <- "~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS"
enviro_CMIP_Matrix_wPhyto <- readRDS("Enviro_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
glimpse(enviro_CMIP_Matrix_wPhyto)

enviro_CMIP_Matrix_wPhyto_filtered <- enviro_CMIP_Matrix_wPhyto %>%
  filter(chlo <= 10)  # Remove chlorophyll values > 10
# This is almost the same number of combinations from the 'climate_change_sst_chl' enviro matrix

# Save results with descriptive names
saveRDS(sst_chl_with_phyto_params,
        "Enviro_Matrix/novel_sst_chl_combinations_2300_wPhyto.rds")

# Save results with descriptive names
saveRDS(all_sst_chl_2300_with_phyto_params,
        "Enviro_Matrix/all_sst_chl_combinations_2300_wPhyto.rds")


# ==============================================================================
# WORKFLOW SUMMARY
# ==============================================================================
# 1. Extracted unique SST-Chlorophyll combinations from all 2300 simulation files
# 2. Prepared data by converting log10(chl) back to chlorophyll and standardizing
# 3. Compared with existing Climate Change project data
# 4. Identified novel combinations unique to 2300 simulations
# 5. Applied optional filtering and created visualization
# 6. Saved results with descriptive filenames
# ==============================================================================