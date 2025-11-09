library(tidyverse)
library(yaImpute)

# Load utilities
source(file.path(getwd(), "Scripts", "Utilities", "fZooMSS_Xtras.R"))

# Set base directory to current working directory
base_dir <- getwd()

cat("==============================================================================\n")
cat("ZooMSS 2300 Step 3d: Apply ZooMSS Biomass Predictions with FishMIP Formatting\n")
cat("==============================================================================\n")
cat("Updated to:\n")
cat("  1. Use overshoot scenarios as-is (2040-2300) without SSP585 prepending\n")
cat("  2. Calculate FishMIP protocol output variables (tcb, tcblog10, tpb, bp size bins)\n")
cat("==============================================================================\n\n")

#### FishMIP Protocol Constants ####
cat("Setting FishMIP protocol constants...\n")
MLD <- 60  # Mixed Layer Depth in meters for converting biomass to m-2

# Weight thresholds for size bins (from FishMIP protocol)
# ZooMSS gives weight, but FishMIP wants length-based bins
# Using relationship: Weight (g) = 0.01 * Length (cm)^3
# Therefore: for a given length L, the weight threshold is W = 0.01 * L^3
weight30 <- 0.01 * 30^3  # 30cm fish = 270g
weight90 <- 0.01 * 90^3  # 90cm fish = 7290g

cat("  Mixed Layer Depth:", MLD, "m\n")
cat("  Weight threshold 30cm:", weight30, "g\n")
cat("  Weight threshold 90cm:", weight90, "g\n\n")

# Define log10 weight bin limits (for tcblog10 variable)
# FishMIP protocol: 1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg, >100kg
w_lim <- 10^c(-1, 0, 1, 2, 3, 4, 5)
cat("Log10 weight bin limits (g):", paste(w_lim, collapse = ", "), "\n\n")

#### Load ZooMSS Matrix Data ####
cat("Loading environmental data...\n")
enviro_data_original <- read_rds(file.path(base_dir, "Enviro_Matrix", "ClimateChange_Compiled_Distinct.rds"))
enviro_data_novel <- read_rds(file.path(base_dir, "Enviro_Matrix", "novel_sst_chl_combinations_2300_wPhyto.rds"))

# Identify common columns
common_cols <- intersect(names(enviro_data_original), names(enviro_data_novel))
cat("Common columns:", paste(common_cols, collapse = ", "), "\n")

# For the analysis, we need at minimum sst and chlo
required_cols <- c("sst", "chlo")
if (!all(required_cols %in% common_cols)) {
  stop("Required columns (sst, chlo) not found in both datasets!")
}

# Combine environmental data using only common columns
enviro_data <- bind_rows(
  enviro_data_original %>%
    select(all_of(common_cols)) %>%
    mutate(source = "original"),
  enviro_data_novel %>%
    select(all_of(common_cols)) %>%
    mutate(source = "novel")
) %>%
  mutate(cellID = 1:n())  # This creates sequential IDs across both datasets

cat("\nCombined environmental data dimensions:", dim(enviro_data), "\n")
cat("Original cells:", sum(enviro_data$source == "original"), "\n")
cat("Novel cells:", sum(enviro_data$source == "novel"), "\n")

# Quick data check
cat("\nSST range - Original:", range(enviro_data$sst[enviro_data$source == "original"]), "\n")
cat("SST range - Novel:", range(enviro_data$sst[enviro_data$source == "novel"]), "\n")
cat("Chlo range - Original:", range(enviro_data$chlo[enviro_data$source == "original"]), "\n")
cat("Chlo range - Novel:", range(enviro_data$chlo[enviro_data$source == "novel"]), "\n\n")

#### Get list of ESM files ####
cat("Finding ESM climate files...\n")
esm_files <- list.files(file.path(base_dir, "Output"),
                        pattern = "^ClimateChange_2300_.*\\.rds$",
                        full.names = TRUE)

# Keep all 15 individual model-scenario files (now including picontrol)
esm_files <- esm_files[grepl("^ClimateChange_2300_(cesm2-waccm|ipsl-cm6a-lr|ukesm1-0-ll)_(historical|picontrol|ssp126|ssp534-over|ssp585)\\.rds$", 
                             basename(esm_files))]

cat("Found", length(esm_files), "ESM files to process:\n")
for (i in seq_along(esm_files)) {
  cat(sprintf("  %2d. %s\n", i, basename(esm_files[i])))
}
cat("\n")

#### Load ZooMSS Model Outputs ####
cat("=== Loading and combining ZooMSS model outputs ===\n")

minb <- 1
maxb <- 191 # Use all weight classes: 1e-12 g to 10,000 kg (for FishMIP bins up to >100kg)

# Load both sets of ZooMSS outputs
zoo_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "res_Control.RDS"))
zoo_novel <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "2300", "res_ZooMSS_2300.RDS"))

# Load model parameters
mdl_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "model_Control.RDS"))
mdl_novel <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "2300", "model_ZooMSS_2300.RDS"))

# Verify model compatibility
if (!identical(mdl_original$param$Groups$Species, mdl_novel$param$Groups$Species)) {
  warning("Model species groups differ between original and novel runs!")
}

cat("Original zoo structure: List of", length(zoo_original), "elements\n")
cat("Novel zoo structure: List of", length(zoo_novel), "elements\n")

# Concatenate the two lists
zoo_combined <- c(zoo_original, zoo_novel)

cat("Combined length:", length(zoo_combined), "\n")
cat("Expected length:", length(zoo_original) + length(zoo_novel), "\n\n")

# Use the original model parameters
mdl <- mdl_original
mdl2 <- mdl
mdl2$param$w <- mdl$param$w[minb:maxb]

# Get weight classes from model
w <- mdl$param$w[minb:maxb]

cat("Weight classes: from", min(w), "to", max(w), "g\n")
cat("Number of weight classes:", length(w), "\n\n")

#### Calculate Biomass with FishMIP Formatting ####
cat("=== Calculating biomass with FishMIP protocol formatting ===\n")

# Get weight classes from model
w <- mdl$param$w[minb:maxb]
cat("Weight classes: from", min(w), "to", max(w), "g\n")
cat("Number of weight classes:", length(w), "\n\n")

# CORRECT WORKFLOW (matches original FishMIP Phase 1 code):
# 1. Extract size range from abundance data
cat("Extracting size range from abundance data...\n")
Bio_size <- fZooMSS_ExtractSizeRange(zoo_combined, minb, maxb)

# 2. Convert abundance to biomass AND sum across species
cat("Converting abundance to biomass (multiply by weight) and summing across species...\n")
BioSum <- fZooMSS_SizeBiomass(Bio_size, mdl2)

cat("Calculating FishMIP variables for all cells...\n")

# Create logical vectors for weight bins (outside the loop)
bin0 <- w >= w_lim[1] & w < w_lim[2]
bin1 <- w >= w_lim[2] & w < w_lim[3]
bin2 <- w >= w_lim[3] & w < w_lim[4]
bin3 <- w >= w_lim[4] & w < w_lim[5]
bin4 <- w >= w_lim[5] & w < w_lim[6]
bin5 <- w >= w_lim[6] & w < w_lim[7]
bp30_bin <- w < weight30
bp30to90_bin <- w >= weight30 & w < weight90
bp90_bin <- w >= weight90

# Function to calculate FishMIP variables for a single cell
# Input: bio_vector is biomass per weight class (already summed across species, in g/m³)
calculate_fishmip_cell <- function(bio_vector) {
  # Multiply by MLD to convert from g/m³ to g/m²
  bio_m2 <- bio_vector * MLD
  
  list(
    tcb = sum(bio_m2, na.rm = TRUE),
    tcblog10_0 = sum(bio_m2[bin0], na.rm = TRUE),
    tcblog10_1 = sum(bio_m2[bin1], na.rm = TRUE),
    tcblog10_2 = sum(bio_m2[bin2], na.rm = TRUE),
    tcblog10_3 = sum(bio_m2[bin3], na.rm = TRUE),
    tcblog10_4 = sum(bio_m2[bin4], na.rm = TRUE),
    tcblog10_5 = sum(bio_m2[bin5], na.rm = TRUE),
    tpb = sum(bio_m2, na.rm = TRUE),  # ZooMSS is all pelagic
    bp30cm = sum(bio_m2[bp30_bin], na.rm = TRUE),
    bp30to90cm = sum(bio_m2[bp30to90_bin], na.rm = TRUE),
    bp90cm = sum(bio_m2[bp90_bin], na.rm = TRUE)
  )
}

# Apply function to all cells
FishMIP_list <- lapply(BioSum, calculate_fishmip_cell)

# Convert list to data frame
FishMIP_summary <- bind_rows(FishMIP_list) %>%
  mutate(cellID = 1:n()) %>%
  # Join with environmental data for matching
  left_join(select(enviro_data, cellID, chlo, sst, source), by = "cellID") %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

cat("FishMIP summary prepared for", nrow(FishMIP_summary), "cells\n")
cat("FishMIP variables:", paste(names(FishMIP_summary)[1:11], collapse = ", "), "\n\n")

# Also prepare species-level biomass for standard output
cat("Preparing standard species biomass output...\n")
# Use Bio_size (abundance × size range) and convert to species biomass
Bio <- fZooMSS_SpeciesBiomass(Bio_size, mdl2)

Bio_standard <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
  rename_with(~mdl$param$Groups$Species) %>%
  mutate(cellID = 1:n()) %>%
  left_join(select(enviro_data, cellID, chlo, sst, source), by = "cellID") %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

cat("Standard biomass data prepared with", nrow(Bio_standard), "cells and", ncol(Bio_standard), "columns\n\n")

#### Process Each ESM File ####
cat("=== Processing ESM files (overshoot scenarios use 2040-2300 only) ===\n\n")

# Create output directories
output_dir_standard <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300_submission_version")
output_dir_fishmip <- file.path(base_dir, "Output", "Step3d_FishMIP_Format_submission_version")

if (!dir.exists(output_dir_standard)) {
  dir.create(output_dir_standard, recursive = TRUE)
}
if (!dir.exists(output_dir_fishmip)) {
  dir.create(output_dir_fishmip, recursive = TRUE)
}

cat("Output directories:\n")
cat("  Standard:", output_dir_standard, "\n")
cat("  FishMIP:", output_dir_fishmip, "\n\n")

# Process each ESM file individually
for (i in seq_along(esm_files)) {
  file <- esm_files[i]
  
  # Extract model name and scenario from filename
  filename <- basename(file)
  parts <- str_match(filename, "ClimateChange_2300_(.+)_(.+)\\.rds")
  model_name <- parts[,2]
  scenario <- parts[,3]
  
  cat(sprintf("\n[%d/%d] Processing: %s - %s\n",
              i, length(esm_files), model_name, scenario))
  
  # Time the processing
  start_time <- Sys.time()
  
  # Read single ESM file
  nc <- read_rds(file)
  cat("  - File loaded, dimensions:", dim(nc), "\n")
  cat("  - Date range:", min(nc$Date, na.rm = TRUE), "to", max(nc$Date, na.rm = TRUE), "\n")
  
  # For overshoot scenarios, use only 2040-2300 data (no prepending)
  if (scenario == "ssp534-over") {
    original_rows <- nrow(nc)
    nc <- nc %>% filter(Date >= 2040)
    cat("  - Overshoot: filtered to 2040+, rows:", original_rows, "->", nrow(nc), "\n")
  }
  
  # Perform KNN matching with STANDARD biomass data (12 functional groups)
  cat("  - Running KNN matching for standard output...\n")
  out_standard <- ann(as.matrix(Bio_standard[,c("SST", "Chl_log10")]),
                     as.matrix(nc[,c("SST", "Chl_log10")]),
                     k = 1, verbose = FALSE)
  
  # Merge standard biomass
  nc_standard <- nc %>%
    mutate(cellID = out_standard$knnIndexDist[,1]) %>%
    left_join(select(Bio_standard, cellID, Flagellates:Fish_Large), by = "cellID") %>%
    filter(!is.na(SST))
  
  cat("  - Standard biomass merged:", nrow(nc_standard), "rows\n")
  
  # Perform KNN matching with FISHMIP biomass data
  cat("  - Running KNN matching for FishMIP format...\n")
  out_fishmip <- ann(as.matrix(FishMIP_summary[,c("SST", "Chl_log10")]),
                    as.matrix(nc[,c("SST", "Chl_log10")]),
                    k = 1, verbose = FALSE)
  
  # Merge FishMIP variables
  nc_fishmip <- nc %>%
    mutate(
      cellID = out_fishmip$knnIndexDist[,1],
      EuclideanDist = out_fishmip$knnIndexDist[,2]
    ) %>%
    left_join(select(FishMIP_summary, cellID, tcb:bp90cm, 
                    Chl_log10_ZooMSS = Chl_log10, SST_ZooMSS = SST), 
             by = "cellID") %>%
    filter(!is.na(SST))
  
  cat("  - FishMIP variables merged:", nrow(nc_fishmip), "rows\n")
  
  # Save standard output
  output_file_standard <- file.path(output_dir_standard,
                                   paste0("ZooMSS_Biomass_2300_",
                                          model_name, "_", scenario, ".rds"))
  write_rds(nc_standard, output_file_standard, compress = "gz")
  
  # Save FishMIP output
  output_file_fishmip <- file.path(output_dir_fishmip,
                                  paste0("ZooMSS_FishMIP_2300_",
                                         model_name, "_", scenario, ".rds"))
  write_rds(nc_fishmip, output_file_fishmip, compress = "gz")
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "secs")
  
  cat("  - Saved standard to:", basename(output_file_standard), "\n")
  cat("  - Saved FishMIP to:", basename(output_file_fishmip), "\n")
  cat("  - Processing time:", round(processing_time, 2), "seconds\n")
  
  # Clear memory
  rm(nc, nc_standard, nc_fishmip, out_standard, out_fishmip)
  gc(verbose = FALSE)
}

# Final cleanup
rm(Bio, Bio_size, BioSum, Bio_standard, FishMIP_list, FishMIP_summary,
   zoo_original, zoo_novel, zoo_combined, mdl, mdl2, enviro_data,
   enviro_data_original, enviro_data_novel)
gc(verbose = FALSE)

cat("\n==============================================================================\n")
cat("All processing complete!\n")
cat("==============================================================================\n")
cat("Standard output files (12 functional groups) saved to:\n")
cat("  ", output_dir_standard, "\n")
cat("\nFishMIP protocol output files (tcb, tcblog10, tpb, bp size bins) saved to:\n")
cat("  ", output_dir_fishmip, "\n")
cat("\nKey changes implemented:\n")
cat("  - Overshoot scenarios use 2040-2300 data only (no SSP585 prepending)\n")
cat("  - FishMIP protocol variables calculated and saved separately\n")
cat("  - Both standard and FishMIP outputs available for analysis\n")
cat("==============================================================================\n")
