library(tidyverse)
library(yaImpute)

# Load utilities
source(file.path(getwd(), "Scripts", "Utilities", "fZooMSS_Xtras.R"))

# Set base directory to current working directory
base_dir <- getwd()

#### Load ZooMSS Matrix Data ####
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
cat("Columns retained:", paste(names(enviro_data), collapse = ", "), "\n")

# Quick data check
cat("\nSST range - Original:", range(enviro_data$sst[enviro_data$source == "original"]), "\n")
cat("SST range - Novel:", range(enviro_data$sst[enviro_data$source == "novel"]), "\n")
cat("Chlo range - Original:", range(enviro_data$chlo[enviro_data$source == "original"]), "\n")
cat("Chlo range - Novel:", range(enviro_data$chlo[enviro_data$source == "novel"]), "\n")

#### Get list of ESM files ####
# Read processed files from Output folder (created by step 2de)
# Only select the 15 individual model-scenario files (exclude combined by-model files)
esm_files <- list.files(file.path(base_dir, "Output"),
                        pattern = "^ClimateChange_2300_.*\\.rds$",
                        full.names = TRUE)

# Keep only files with model AND scenario (3 parts after splitting by underscore)
# Exclude: _Compiled, combined by-model files, _CORRECTED, _OLD_, segment files
esm_files <- esm_files[grepl("^ClimateChange_2300_(cesm2-waccm|ipsl-cm6a-lr|ukesm1-0-ll)_(historical|picontrol|ssp126|ssp534-over|ssp585)\\.rds$", 
                             basename(esm_files))]

# Sort files to process SSP585 before overshoot (for FishMIP 2300 protocol)
# This ensures SSP585 KNN matches are saved before overshoot scenarios need them
esm_files <- esm_files[order(
  basename(esm_files),
  # Primary sort: scenario (ssp585 before ssp534-over)
  decreasing = FALSE
)]

# Move SSP585 files to the front to ensure they're processed first
ssp585_files <- esm_files[grepl("_ssp585\\.rds$", esm_files)]
overshoot_files <- esm_files[grepl("_ssp534-over\\.rds$", esm_files)]
other_files <- esm_files[!grepl("_(ssp585|ssp534-over)\\.rds$", esm_files)]
esm_files <- c(other_files, ssp585_files, overshoot_files)

cat("\nFound", length(esm_files), "ESM files to process:\n")
cat("(SSP585 scenarios will be processed before overshoot for KNN match reuse)\n")
for (i in seq_along(esm_files)) {
  cat(sprintf("  %2d. %s\n", i, basename(esm_files[i])))
}

minb <- 1
maxb <- 158 # Max weight of 100 kg

cat("\n=== Loading and combining ZooMSS model outputs ===\n")

# Load both sets of ZooMSS outputs
zoo_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "res_Control.RDS"))
zoo_novel <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "2300", "res_ZooMSS_2300.RDS"))

# Load model parameters (check if both use the same model structure)
mdl_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "model_Control.RDS"))
mdl_novel <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "2300", "model_ZooMSS_2300.RDS"))

# Verify model compatibility
if (!identical(mdl_original$param$Groups$Species, mdl_novel$param$Groups$Species)) {
  warning("Model species groups differ between original and novel runs!")
}

# Check structure compatibility
cat("\nOriginal zoo structure: List of", length(zoo_original), "elements\n")
cat("Novel zoo structure: List of", length(zoo_novel), "elements\n")

# Check dimensions of first element as example
cat("\nOriginal zoo[[1]] dimensions:", dim(zoo_original[[1]]), "\n")
cat("Novel zoo[[1]] dimensions:", dim(zoo_novel[[1]]), "\n")

# Simply concatenate the two lists
zoo_combined <- c(zoo_original, zoo_novel)

# Verify
cat("Original length:", length(zoo_original), "\n")
cat("Novel length:", length(zoo_novel), "\n")
cat("Combined length:", length(zoo_combined), "\n")
cat("Expected length:", length(zoo_original) + length(zoo_novel), "\n")

# Check a few elements to verify structure is preserved
cat("\nFirst element of original - dims:", dim(zoo_combined[[1]]), "\n")
cat("Last element of original - dims:", dim(zoo_combined[[length(zoo_original)]]), "\n")
cat("First element of novel - dims:", dim(zoo_combined[[length(zoo_original) + 1]]), "\n")
cat("Last element of combined - dims:", dim(zoo_combined[[length(zoo_combined)]]), "\n")

# Use the original model parameters (or verify they're the same)
mdl <- mdl_original
mdl2 <- mdl
mdl2$param$w <- mdl$param$w[minb:maxb]

# Calculate biomass for combined data
Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo_combined, minb, maxb), mdl2)

Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
  rename_with(~mdl$param$Groups$Species) %>%
  mutate(cellID = 1:n()) %>%  # This should match the environmental data cellIDs
  left_join(enviro_data %>% dplyr::select(cellID, chlo, sst, source), by = "cellID") %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

cat("Combined biomass data prepared. Dimensions:", dim(Bio_df), "\n")
cat("Cells with biomass from original:", sum(Bio_df$source == "original", na.rm = TRUE), "\n")
cat("Cells with biomass from novel:", sum(Bio_df$source == "novel", na.rm = TRUE), "\n")

# Create directory to store KNN match indices (for overshoot protocol)
knn_match_dir <- file.path(base_dir, "Output", "KNN_Matches")
if (!dir.exists(knn_match_dir)) {
  dir.create(knn_match_dir, recursive = TRUE)
  cat("Created KNN match directory:", knn_match_dir, "\n")
}

# Process each ESM file individually
for (i in seq_along(esm_files)) {
  file <- esm_files[i]

  # Extract model name and scenario from filename
  # Filename format: ClimateChange_2300_<model>_<scenario>.rds
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

  # Special handling for overshoot protocol (FishMIP 2300)
  # SSP585 2015-2039 should have identical biomass as overshoot 2015-2039
  if (scenario == "ssp534-over") {
    # For overshoot: reuse SSP585 KNN matches for 2015-2039
    knn_match_file <- file.path(knn_match_dir, 
                                paste0("KNN_matches_", model_name, "_ssp585_2015-2039.rds"))
    
    if (file.exists(knn_match_file)) {
      cat("  - Loading saved SSP585 KNN matches for 2015-2039 period\n")
      
      # Split data by period
      nc_init <- nc %>% filter(Date >= 2015, Date <= 2039)  # Initialization period
      nc_future <- nc %>% filter(Date >= 2040)  # Future overshoot period
      
      # Load saved SSP585 matches for initialization period
      ssp585_matches <- read_rds(knn_match_file)
      cat("  - Loaded", nrow(ssp585_matches), "saved match indices\n")
      
      # Apply saved matches to initialization period
      nc2_init <- nc_init %>%
        left_join(ssp585_matches, by = c("Lon", "Lat", "Date")) %>%
        left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
        filter(!is.na(SST))
      
      cat("  - Applied saved matches to", nrow(nc2_init), "initialization rows\n")
      
      # Compute new KNN matches for future period only
      if (nrow(nc_future) > 0) {
        out_future <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
                         as.matrix(nc_future[,c("SST", "Chl_log10")]),
                         k = 1, verbose = FALSE)
        
        nc2_future <- nc_future %>%
          mutate(cellID = out_future$knnIndexDist[,1]) %>%
          left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
          filter(!is.na(SST))
        
        cat("  - Computed new matches for", nrow(nc2_future), "future rows\n")
        
        # Combine both periods
        nc2 <- bind_rows(nc2_init, nc2_future)
        rm(out_future, nc_init, nc_future, nc2_init, nc2_future, ssp585_matches)
      } else {
        nc2 <- nc2_init
        rm(nc_init, nc_future, nc2_init, ssp585_matches)
      }
      
      cat("  - Combined initialization + future periods\n")
      
    } else {
      # No saved matches available - fall back to standard KNN
      warning("  - SSP585 KNN matches not found, using standard KNN matching")
      out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
                 as.matrix(nc[,c("SST", "Chl_log10")]),
                 k = 1, verbose = FALSE)
      
      nc2 <- nc %>%
        mutate(cellID = out$knnIndexDist[,1]) %>%
        left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
        filter(!is.na(SST))
      
      rm(out)
    }
    
  } else {
    # Standard KNN matching for all non-overshoot scenarios
    out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
               as.matrix(nc[,c("SST", "Chl_log10")]),
               k = 1, verbose = FALSE)
    
    nc2 <- nc %>%
      mutate(cellID = out$knnIndexDist[,1]) %>%
      left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
      filter(!is.na(SST))
    
    # If this is SSP585, save the KNN matches for 2015-2039 (for overshoot reuse)
    if (scenario == "ssp585") {
      ssp585_2015_2039 <- nc2 %>% 
        filter(Date >= 2015, Date <= 2039) %>%
        select(Lon, Lat, Date, cellID)
      
      knn_match_file <- file.path(knn_match_dir, 
                                  paste0("KNN_matches_", model_name, "_ssp585_2015-2039.rds"))
      write_rds(ssp585_2015_2039, knn_match_file)
      cat("  - Saved SSP585 2015-2039 KNN matches (", nrow(ssp585_2015_2039), 
          "rows) for overshoot reuse\n")
      rm(ssp585_2015_2039)
    }
    
    rm(out)
  }

  cat("  - Merged data dimensions:", dim(nc2), "\n")

  # Create output directory with clear labeling for step 3d
  output_dir <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("  - Created output directory:", output_dir, "\n")
  }

  # Save output
  output_file <- file.path(output_dir,
                           paste0("ZooMSS_Biomass_2300_",
                                  model_name, "_", scenario, ".rds"))

  write_rds(nc2, output_file)

  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "secs")

  cat("  - Saved to:", basename(output_file), "\n")
  cat("  - Processing time:", round(processing_time, 2), "seconds\n")

  # Clear memory
  rm(nc, nc2, out)
  gc(verbose = FALSE)
}

# Final cleanup
rm(Bio, Bio_df, zoo_combined, mdl, mdl2)
gc(verbose = FALSE)

cat("\n==============================================================================\n")
cat("All processing complete!\n")
cat("==============================================================================\n")
cat("Output files saved to:\n")
cat("  ", file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300"), "\n")
cat("\nFiles contain ZooMSS biomass projections merged with ESM climate data\n")
cat("Ready for analysis and visualization\n")
cat("==============================================================================\n")