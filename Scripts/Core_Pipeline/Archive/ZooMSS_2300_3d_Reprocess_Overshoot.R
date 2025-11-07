library(tidyverse)
library(yaImpute)

# Load utilities
source(file.path(getwd(), "Scripts", "Utilities", "fZooMSS_Xtras.R"))

# Set base directory
base_dir <- getwd()

cat("==============================================================================\n")
cat("Reprocessing Overshoot Scenarios with Saved SSP585 KNN Matches\n")
cat("==============================================================================\n\n")

#### Load ZooMSS Matrix Data ####
cat("Loading environmental data...\n")
enviro_data_original <- read_rds(file.path(base_dir, "Enviro_Matrix", "ClimateChange_Compiled_Distinct.rds"))
enviro_data_novel <- read_rds(file.path(base_dir, "Enviro_Matrix", "novel_sst_chl_combinations_2300_wPhyto.rds"))

# Combine environmental data
common_cols <- intersect(names(enviro_data_original), names(enviro_data_novel))
enviro_data <- bind_rows(
  enviro_data_original %>%
    select(all_of(common_cols)) %>%
    mutate(source = "original"),
  enviro_data_novel %>%
    select(all_of(common_cols)) %>%
    mutate(source = "novel")
) %>%
  mutate(cellID = 1:n())

cat("Combined environmental data:", nrow(enviro_data), "cells\n\n")

#### Load and combine ZooMSS model outputs ####
cat("Loading ZooMSS model outputs...\n")
zoo_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "res_Control.RDS"))
zoo_novel <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "2300", "res_ZooMSS_2300.RDS"))
zoo_combined <- c(zoo_original, zoo_novel)

mdl_original <- read_rds(file.path(base_dir, "Input", "ZooMSS_enviro_matrix_results", "ClimateChange", "model_Control.RDS"))
mdl <- mdl_original
mdl2 <- mdl
minb <- 1
maxb <- 158
mdl2$param$w <- mdl$param$w[minb:maxb]

cat("Calculating biomass...\n")
Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo_combined, minb, maxb), mdl2)

Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
  rename_with(~mdl$param$Groups$Species) %>%
  mutate(cellID = 1:n()) %>%
  left_join(enviro_data %>% dplyr::select(cellID, chlo, sst, source), by = "cellID") %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

cat("Biomass data prepared:", nrow(Bio_df), "cells\n\n")

#### Get overshoot files ####
overshoot_files <- list.files(file.path(base_dir, "Output"),
                              pattern = "^ClimateChange_2300_(cesm2-waccm|ipsl-cm6a-lr|ukesm1-0-ll)_ssp534-over\\.rds$",
                              full.names = TRUE)

cat("Found", length(overshoot_files), "overshoot files to reprocess:\n")
for (i in seq_along(overshoot_files)) {
  cat(sprintf("  %d. %s\n", i, basename(overshoot_files[i])))
}
cat("\n")

knn_match_dir <- file.path(base_dir, "Output", "KNN_Matches")
output_dir <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300")

#### Reprocess each overshoot file ####
for (i in seq_along(overshoot_files)) {
  file <- overshoot_files[i]
  
  # Extract model name
  filename <- basename(file)
  parts <- str_match(filename, "ClimateChange_2300_(.+)_(.+)\\.rds")
  model_name <- parts[,2]
  scenario <- parts[,3]
  
  cat(sprintf("\n[%d/%d] Reprocessing: %s - %s\n",
              i, length(overshoot_files), model_name, scenario))
  
  start_time <- Sys.time()
  
  # Read overshoot file
  nc <- read_rds(file)
  cat("  - File loaded, dimensions:", dim(nc), "\n")
  
  # Load saved SSP585 KNN matches
  knn_match_file <- file.path(knn_match_dir, 
                              paste0("KNN_matches_", model_name, "_ssp585_2015-2039.rds"))
  
  if (file.exists(knn_match_file)) {
    cat("  - Loading saved SSP585 KNN matches for 2015-2039 period\n")
    
    # Split data by period
    nc_init <- nc %>% filter(Date >= 2015, Date <= 2039)
    nc_future <- nc %>% filter(Date >= 2040)
    
    # Load saved matches
    ssp585_matches <- read_rds(knn_match_file)
    cat("  - Loaded", nrow(ssp585_matches), "saved match indices\n")
    
    # Apply saved matches to initialization period
    nc2_init <- nc_init %>%
      left_join(ssp585_matches, by = c("Lon", "Lat", "Date")) %>%
      left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
      filter(!is.na(SST))
    
    cat("  - Applied saved matches to", nrow(nc2_init), "initialization rows\n")
    
    # Compute new KNN matches for future period
    if (nrow(nc_future) > 0) {
      out_future <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
                       as.matrix(nc_future[,c("SST", "Chl_log10")]),
                       k = 1, verbose = FALSE)
      
      nc2_future <- nc_future %>%
        mutate(cellID = out_future$knnIndexDist[,1]) %>%
        left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
        filter(!is.na(SST))
      
      cat("  - Computed new matches for", nrow(nc2_future), "future rows\n")
      
      # Combine periods
      nc2 <- bind_rows(nc2_init, nc2_future)
      rm(out_future)
    } else {
      nc2 <- nc2_init
    }
    
    cat("  - Combined data dimensions:", dim(nc2), "\n")
    
    # Save output
    output_file <- file.path(output_dir,
                            paste0("ZooMSS_Biomass_2300_",
                                   model_name, "_", scenario, ".rds"))
    
    write_rds(nc2, output_file)
    
    end_time <- Sys.time()
    processing_time <- difftime(end_time, start_time, units = "secs")
    
    cat("  - Saved to:", basename(output_file), "\n")
    cat("  - Processing time:", round(processing_time, 2), "seconds\n")
    
    rm(nc, nc_init, nc_future, nc2_init, nc2, ssp585_matches)
    gc(verbose = FALSE)
    
  } else {
    cat("  - ERROR: SSP585 KNN match file not found:", basename(knn_match_file), "\n")
  }
}

cat("\n==============================================================================\n")
cat("Overshoot reprocessing complete!\n")
cat("==============================================================================\n")
cat("Overshoot scenarios now use identical SSP585 biomass for 2015-2039\n")
cat("per FishMIP 2300 protocol\n")
cat("==============================================================================\n")
