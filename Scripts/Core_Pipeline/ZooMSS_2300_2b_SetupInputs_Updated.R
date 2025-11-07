# ==============================================================================
# ZooMSS_2300: Setup Inputs - Combine TOS and Chlorophyll Data
# ==============================================================================
# 
# Purpose: Combine TOS (temperature) and Chlorophyll data for each ESM-scenario
#          
# Special handling for SSP5-3.4-overshoot:
#   - Per FishMIP 2300 protocol, overshoot is initialized from SSP5-8.5
#   - Prepends 2015-2039 from SSP5-8.5 to overshoot 2040-2300
#   - For UKESM: Merges 2040-2100 and 2101-2300 overshoot files
#
# Input:  Input/raw/tos/*.nc and Input/converted/chl/*.nc
# Output: Input/2300_processed/*.rds (one file per model-scenario)
#
# ==============================================================================

library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

cat("==============================================================================\n")
cat("ZooMSS 2300 - Setup Inputs: Combine TOS and Chlorophyll\n")
cat("==============================================================================\n\n")

# Set directory paths
base_dir <- "Input/"  # Base input directory
out_dir <- "Input/2300_processed/"  # Output for processed files

# Model names
ModelArray <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
# Experiment names
ExpArray <- c("historical", "picontrol", "ssp126", "ssp534-over", "ssp585")

# Create output directory if it doesn't exist
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  cat("Created output directory:", out_dir, "\n\n")
}

# Helper function to process a single file pair
process_file_pair <- function(ftos, fchl, model_name, exp_name, year_start, year_end) {
  cat("    Processing:", basename(ftos), "\n")
  cat("    Years:", year_start, "to", year_end, "\n")
  
  # Get number of layers
  tos_stack <- stack(ftos)
  n_layers <- nlayers(tos_stack)
  cat("    Number of layers:", n_layers, "\n")
  
  # Free memory
  rm(tos_stack)
  gc()
  
  # Generate year sequence
  year_seq <- seq(from = year_start, to = year_end, length.out = n_layers)
  
  # Process in batches to save memory
  batch_size <- 5
  result_df <- NULL
  
  for(i in seq(1, n_layers, by = batch_size)) {
    batch_end <- min(i + batch_size - 1, n_layers)
    cat("      Processing layers", i, "to", batch_end, "\n")
    
    batch_years <- year_seq[i:batch_end]
    batch_df <- NULL
    
    for(j in i:batch_end) {
      layer_idx <- j - i + 1
      
      # Read individual layers
      tos_layer <- raster(ftos, band = j)
      chl_layer <- raster(fchl, band = j)
      
      # Convert to dataframe
      tos_df <- as.data.frame(tos_layer, xy = TRUE, na.rm = FALSE)
      names(tos_df) <- c("Lon", "Lat", "SST")
      
      chl_df <- as.data.frame(chl_layer, xy = TRUE, na.rm = FALSE)
      names(chl_df) <- c("Lon", "Lat", "Chl")
      
      # Combine
      layer_data <- tos_df %>%
        mutate(Chl = chl_df$Chl,
               Year = batch_years[layer_idx],
               Model = model_name,
               Experiment = exp_name)
      
      # Add to batch
      if(is.null(batch_df)) {
        batch_df <- layer_data
      } else {
        batch_df <- bind_rows(batch_df, layer_data)
      }
      
      # Clean up
      rm(tos_layer, chl_layer, tos_df, chl_df, layer_data)
      gc()
    }
    
    # Calculate log10 of Chl for the batch
    batch_df <- batch_df %>%
      mutate(SST = round(SST, digits = 1),
             Chl_log10 = log10(Chl),
             Chl_log10 = round(Chl_log10, digits = 2))
    
    # Append to result
    if(is.null(result_df)) {
      result_df <- batch_df
    } else {
      result_df <- bind_rows(result_df, batch_df)
    }
    
    # Free memory
    rm(batch_df)
    gc()
  }
  
  return(result_df)
}

# Process each model-experiment combination
for (m in 1:length(ModelArray)) {
  cat("\n==============================================================================\n")
  cat("Processing model:", ModelArray[m], "\n")
  cat("==============================================================================\n")

  for (e in 1:length(ExpArray)) {
    exp <- ExpArray[e]
    cat("\n  Experiment:", exp, "\n")
    cat("  ----------------------------------------------------------------------\n")

    # Create a unique identifier for this combination
    combo_id <- paste0(ModelArray[m], "_", exp)
    output_file <- paste0(out_dir, "2300_", combo_id, ".rds")

    # Skip if already processed
    if (file.exists(output_file)) {
      cat("  ✓ Output file already exists. Skipping.\n")
      next
    }

    # Set appropriate search patterns based on model and experiment
    if (ModelArray[m] == "ukesm1-0-ll") {
      if (exp == "historical") {
        tos_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*chla-top")
      } else if (exp == "picontrol") {
        tos_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*chla-top")
      } else {
        tos_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*chla-top")
      }
    } else {
      tos_pattern <- paste0(ModelArray[m], "_r1i1p1f1_", exp, ".*tos")
      chla_pattern <- paste0(ModelArray[m], "_r1i1p1f1_", exp, ".*chla-top")
    }

    # Special handling for SSP5-3.4-overshoot
    if (exp == "ssp534-over") {
      cat("  Special processing: SSP5-3.4-overshoot with SSP5-8.5 initialization\n")
      cat("  Per FishMIP 2300 protocol: Using SSP5-8.5 2015-2039 + overshoot 2040-2300\n\n")
      
      tryCatch({
        # Step 1: Get SSP5-8.5 data for 2015-2039
        cat("  Step 1: Loading SSP5-8.5 data (2015-2039 initialization period)\n")
        
        # Adjust pattern for SSP585
        if (ModelArray[m] == "ukesm1-0-ll") {
          tos_pattern_585 <- paste0(ModelArray[m], "_r4i1p1f2_ssp585.*tos")
          chla_pattern_585 <- paste0(ModelArray[m], "_r4i1p1f2_ssp585.*chla-top")
        } else {
          tos_pattern_585 <- paste0(ModelArray[m], "_r1i1p1f1_ssp585.*tos")
          chla_pattern_585 <- paste0(ModelArray[m], "_r1i1p1f1_ssp585.*chla-top")
        }
        
        ftos_585 <- list.files(paste0(base_dir, "raw/tos/"), pattern = tos_pattern_585, full.names = TRUE)
        fchl_585 <- list.files(paste0(base_dir, "converted/chl/"), pattern = chla_pattern_585, full.names = TRUE)
        
        if(length(ftos_585) == 0 || length(fchl_585) == 0) {
          cat("  ERROR: Could not find SSP5-8.5 files for initialization\n")
          next
        }
        
        # Process SSP585 data and filter for 2015-2039
        ssp585_data <- process_file_pair(ftos_585[1], fchl_585[1], ModelArray[m], "ssp585", 2015, 2299)
        ssp585_init <- ssp585_data %>% filter(Year >= 2015 & Year <= 2039)
        rm(ssp585_data)
        gc()
        
        cat("  ✓ Loaded", length(unique(ssp585_init$Year)), "years of SSP5-8.5 initialization data\n\n")
        
        # Step 2: Get overshoot data
        cat("  Step 2: Loading SSP5-3.4-overshoot data (2040-2300)\n")
        
        ftos <- list.files(paste0(base_dir, "raw/tos/"), pattern = tos_pattern, full.names = TRUE)
        fchl <- list.files(paste0(base_dir, "converted/chl/"), pattern = chla_pattern, full.names = TRUE)
        
        if(length(ftos) == 0 || length(fchl) == 0) {
          cat("  ERROR: No overshoot files found\n")
          next
        }
        
        cat("  Found", length(ftos), "TOS file(s) and", length(fchl), "CHL file(s)\n")
        
        # For UKESM, we may have two overshoot files (2040-2100 and 2101-2300)
        overshoot_data <- NULL
        
        for(i in 1:length(ftos)) {
          # Extract year range from filename
          year_info <- str_extract(basename(ftos[i]), "\\d{4}_\\d{4}")
          if(!is.na(year_info)) {
            years <- as.numeric(str_split(year_info, "_")[[1]])
            start_year <- years[1]
            end_year <- years[2]
          } else {
            cat("  WARNING: Could not extract year range from", basename(ftos[i]), "\n")
            next
          }
          
          # Process this overshoot file segment
          segment_data <- process_file_pair(ftos[i], fchl[i], ModelArray[m], exp, start_year, end_year)
          
          # Combine segments
          if(is.null(overshoot_data)) {
            overshoot_data <- segment_data
          } else {
            overshoot_data <- bind_rows(overshoot_data, segment_data)
          }
          
          rm(segment_data)
          gc()
        }
        
        cat("  ✓ Loaded", length(unique(overshoot_data$Year)), "years of overshoot data\n\n")
        
        # Step 3: Combine SSP585 initialization with overshoot
        cat("  Step 3: Combining SSP5-8.5 initialization (2015-2039) with overshoot (2040-2300)\n")
        
        # Update experiment label for SSP585 initialization period
        ssp585_init <- ssp585_init %>% mutate(Experiment = "ssp534-over")
        
        # Combine
        result_df <- bind_rows(ssp585_init, overshoot_data)
        
        # Sort by year
        result_df <- result_df %>% arrange(Year, Lon, Lat)
        
        rm(ssp585_init, overshoot_data)
        gc()
        
        cat("  ✓ Combined dataset: Years", min(result_df$Year), "to", max(result_df$Year), "\n")
        cat("    Total years:", length(unique(result_df$Year)), "\n")
        
        # Save the combined dataset
        write_rds(result_df, output_file)
        cat("  ✓ Data saved to:", output_file, "\n")
        
        rm(result_df)
        gc()
        
      }, error = function(e) {
        cat("  ERROR processing", combo_id, ":", conditionMessage(e), "\n")
        print(traceback())
      })
      
    } else {
      # Standard processing for non-overshoot scenarios
      cat("  Standard processing\n")
      
      tryCatch({
        # Search for files
        ftos <- list.files(paste0(base_dir, "raw/tos/"), pattern = tos_pattern, full.names = TRUE)
        fchl <- list.files(paste0(base_dir, "converted/chl/"), pattern = chla_pattern, full.names = TRUE)

        if(length(ftos) == 0) {
          cat("  No TOS files found. Skipping.\n")
          next
        }
        
        if(length(fchl) == 0) {
          cat("  No CHL files found. Skipping.\n")
          next
        }

        cat("  Found:", length(ftos), "TOS file(s),", length(fchl), "CHL file(s)\n")

        # Extract year information from filename
        year_info <- str_extract(basename(ftos[1]), "\\d{4}_\\d{4}")
        if(!is.na(year_info)) {
          years <- as.numeric(str_split(year_info, "_")[[1]])
          start_year <- years[1]
          end_year <- years[2]
        } else {
          cat("  WARNING: Could not extract year information. Using defaults.\n")
          start_year <- 1850
          end_year <- 2300
        }

        # Process the file pair
        result_df <- process_file_pair(ftos[1], fchl[1], ModelArray[m], exp, start_year, end_year)

        # Save
        write_rds(result_df, output_file)
        cat("  ✓ Data saved to:", output_file, "\n")

        rm(result_df)
        gc()

      }, error = function(e) {
        cat("  ERROR processing", combo_id, ":", conditionMessage(e), "\n")
        print(traceback())
      })
    }

    # Force garbage collection
    gc()
  }
}

cat("\n==============================================================================\n")
cat("Processing Complete!\n")
cat("==============================================================================\n")
cat("\nIndividual files saved for each model-experiment combination in:\n")
cat("  ", out_dir, "\n\n")

cat("Summary of SSP5-3.4-overshoot handling:\n")
cat("  - Per FishMIP 2300 protocol, overshoot initialized from SSP5-8.5\n")
cat("  - All models: 2015-2039 from SSP5-8.5 + 2040-2300 from overshoot\n")
cat("  - UKESM: Merged 2040-2100 and 2101-2300 overshoot segments\n")
cat("  - Total coverage: 2015-2300 (286 years)\n\n")

cat("To combine all files into a single compiled dataset, run:\n")
cat("  Rscript Scripts/Core_Pipeline/ZooMSS_2300_2d_CompileInputs.R\n\n")
cat("==============================================================================\n")
