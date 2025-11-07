library(tidyverse)
library(yaImpute)

# Resolve project root and utilities
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "Scripts/Utilities/fZooMSS_Xtras.R"))

# Set your base directory and input path
base_dir <- project_root

#### Load ZooMSS Matrix Data ####
enviro_data_original <- read_rds(file.path(base_dir,"Enviro_Matrix/","ClimateChange_Compiled_Distinct.rds"))

enviro_data_novel <- read_rds(file.path(base_dir,"Enviro_Matrix/", "novel_sst_chl_combinations_2300_wPhyto.rds"))

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
esm_files <- list.files(file.path(base_dir, "Input", "2300_processed"),
                        pattern = "\\.rds$",
                        full.names = TRUE)
# Optional filter to process a subset (set env var PROCESS_PATTERN or pass --args pattern)
args <- commandArgs(trailingOnly = TRUE)
pattern_arg <- if (length(args) > 0) args[1] else Sys.getenv("PROCESS_PATTERN", unset = "")
if (nzchar(pattern_arg)) {
  cat("\nFiltering esm_files with pattern:", pattern_arg, "\n")
  esm_files <- esm_files[grepl(pattern_arg, basename(esm_files))]
  cat("Files to process:", length(esm_files), "\n")
}

minb <- 1
maxb <- 158 # Max weight of 100 kg

cat("\n=== Loading and combining ZooMSS model outputs ===\n")

# Load both sets of ZooMSS outputs
zoo_original <- read_rds(file.path(base_dir, "Input", "res_Control.RDS"))
zoo_novel <- read_rds(file.path(base_dir, "Input", "res_ZooMSS_2300.RDS"))

# Load model parameters (check if both use the same model structure)
mdl_original <- read_rds(file.path(base_dir, "Input", "model_Control.RDS"))
mdl_novel <- read_rds(file.path(base_dir, "Input", "model_ZooMSS_2300.RDS"))

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

# Process each ESM file individually
for (i in seq_along(esm_files)) {
  file <- esm_files[i]

  # Extract model name and scenario from filename
  filename <- basename(file)
  parts <- str_match(filename, "2300_(.+)_(.+)\\.rds")
  model_name <- parts[,2]
  scenario <- parts[,3]

  cat(sprintf("\n[%d/%d] Processing: %s - %s\n",
              i, length(esm_files), model_name, scenario))

  # Time the processing
  start_time <- Sys.time()

  # Read single ESM file
  nc <- read_rds(file)
  cat("  - File loaded, dimensions:", dim(nc), "\n")

  # Perform KNN matching
  out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  # Merge and process
  nc2 <- nc %>%
    mutate(cellID = out$knnIndexDist[,1]) %>%
    left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
    filter(!is.na(SST))

  cat("  - Merged data dimensions:", dim(nc2), "\n")

  # Create output directory if it doesn't exist
  output_dir <- file.path(base_dir, "Output", "Biomass_projections")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save output
  output_file <- file.path(output_dir,
                           paste0("Biomass_ClimateChange_Compiled_withZooMSS_",
                                  model_name, "_", scenario, "_Control.rds"))

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
rm(Bio, Bio_df, zoo, mdl, mdl2)
gc(verbose = FALSE)

cat("\nAll processing complete!\n")
cat("Output files saved to:", file.path(base_dir, "Output", "Biomass_projections"), "\n")