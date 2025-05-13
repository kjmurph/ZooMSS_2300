---
  title: "R Notebook"
output: html_notebook
---


  ```{r}
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

```

```{r}
df_existing_matrix <- readRDS("enviro_CMIP_Matrix.RDS")
```



```{r}
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Base directory
base_dir <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/merged"
out_dir <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"

# Test with just one model and one experiment
ModelArray <- c("cesm2-waccm")
ExpArray <- c("historical")

# Create empty data frame to store results
df <- NULL

cat("=== TESTING SUBSET PROCESSING ===\n")
cat("Processing only", ModelArray[1], "with experiment", ExpArray[1], "\n\n")

for (m in 1:length(ModelArray)){
  cat("\nProcessing model:", ModelArray[m], "\n")

  # For the test, use consistent realization
  realization <- "r1i1p1f1"

  for (e in 1:length(ExpArray)){
    exp <- ExpArray[e]
    cat("  Processing experiment:", exp, "\n")

    # Search patterns with correct realizations
    tos_pattern <- paste0(ModelArray[m], "_", realization, "_", exp, ".*tos")
    chla_pattern <- paste0(ModelArray[m], "_", realization, "_", exp, ".*chla-top")

    cat("  Looking for tos files with pattern:", tos_pattern, "\n")
    cat("  Looking for chla files with pattern:", chla_pattern, "\n")

    # Search in the base directory for files matching the patterns
    ftos <- list.files(base_dir, pattern = tos_pattern, full.names = TRUE)
    fchl <- list.files(base_dir, pattern = chla_pattern, full.names = TRUE)

    cat("  tos files found:", length(ftos), "\n")
    if(length(ftos) > 0) {
      cat("    tos files:\n")
      print(basename(ftos))
    } else {
      cat("    No tos files found for this model/experiment combination.\n")
    }

    cat("  chla files found:", length(fchl), "\n")
    if(length(fchl) > 0) {
      cat("    chla files:\n")
      print(basename(fchl))
    } else {
      cat("    No chla files found for this model/experiment combination.\n")
    }

    # Skip this iteration if no files found
    if(length(ftos) == 0 || length(fchl) == 0) {
      cat("  Skipping this combination due to missing files.\n")
      next
    }

    # Process the files as before
    tryCatch({
      cat("\nReading tos raster stack...\n")
      tos_stack <- stack(ftos)
      cat("  tos stack info:", nlayers(tos_stack), "layers\n")

      cat("Converting tos stack to dataframe...\n")
      ttos <- as.data.frame(tos_stack, xy = TRUE, na.rm = FALSE)
      cat("  tos dataframe dimensions:", dim(ttos)[1], "rows,", dim(ttos)[2], "columns\n")

      cat("Pivoting tos dataframe...\n")
      ttos <- ttos %>%
        pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "SST")
      cat("  pivoted tos dataframe dimensions:", dim(ttos)[1], "rows,", dim(ttos)[2], "columns\n")

      cat("\nReading chla raster stack...\n")
      chl_stack <- stack(fchl)
      cat("  chla stack info:", nlayers(chl_stack), "layers\n")

      cat("Converting chla stack to dataframe...\n")
      tchl <- as.data.frame(chl_stack, xy = TRUE, na.rm = FALSE)
      cat("  chla dataframe dimensions:", dim(tchl)[1], "rows,", dim(tchl)[2], "columns\n")

      cat("Pivoting chla dataframe...\n")
      tchl <- tchl %>%
        pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "Chl") %>%
        dplyr::select(Chl)
      cat("  pivoted chla dataframe dimensions:", dim(tchl)[1], "rows,", dim(tchl)[2], "columns\n")

      cat("Combining dataframes...\n")
      temp <- bind_cols(ttos, tchl) %>%
        add_column(Model = ModelArray[m],
                   Experiment = exp)
      cat("  combined dataframe dimensions:", dim(temp)[1], "rows,", dim(temp)[2], "columns\n")

      if (is.null(df)){
        df <- temp
      } else {
        df <- bind_rows(df, temp)
      }
      cat("  Successfully processed data for", ModelArray[m], exp, "\n")

      # Sample the data to show a small preview
      cat("\nPreview of the processed data (first 10 rows):\n")
      print(head(temp, 10))

    }, error = function(e) {
      cat("  ERROR processing", ModelArray[m], exp, ":", conditionMessage(e), "\n")
    })

    rm(temp, ttos, tchl, ftos, fchl)
  }
}

if (!is.null(df)) {
  cat("\nTransforming final dataset...\n")
  df <- df %>%
    mutate(Date = str_replace(Date, "X", ""),
           Date = ymd(Date),
           Year = year(Date)) %>%
    rename(Lon = x, Lat = y) %>%
    mutate(SST = round(SST, digits = 1),
           Chl_log10 = log10(Chl),
           Chl_log10 = round(Chl_log10, digits = 2))

  cat("Final dataframe has", nrow(df), "rows and", ncol(df), "columns\n")
  cat("First few rows of the final dataframe:\n")
  print(head(df))

  # For testing, let's not save the file yet
  cat("\nTest completed successfully!\n")
  cat("To save the data, uncomment the write_rds line in the code.\n")
  # write_rds(df, paste0(out_dir,.Platform$file.sep,"ClimateChange_Compiled.rds"))
} else {
  cat("No data was processed. Please check file patterns and directory structure.\n")
}
```


```{r}
# Base directory
base_dir <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/merged"
out_dir <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"

# Updated model names to match actual file patterns
ModelArray <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
# Updated experiment names for ssp534
ExpArray <- c("historical", "picontrol", "ssp126", "ssp534-over", "ssp585")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(paste0(out_dir,.Platform$file.sep,"ClimateChange_Compiled.rds")))) {
  dir.create(dirname(paste0(out_dir,.Platform$file.sep,"ClimateChange_Compiled.rds")), recursive = TRUE)
}

# Process each model-experiment combination separately and save to separate files
for (m in 1:length(ModelArray)) {
  cat("\nProcessing model:", ModelArray[m], "\n")

  for (e in 1:length(ExpArray)) {
    exp <- ExpArray[e]
    cat("  Processing experiment:", exp, "\n")

    # Create a unique identifier for this combination
    combo_id <- paste0(ModelArray[m], "_", exp)
    output_file <- paste0(out_dir, .Platform$file.sep, "ClimateChange_", combo_id, ".rds")

    # Skip if already processed
    if (file.exists(output_file)) {
      cat("  Output file already exists for", combo_id, ". Skipping.\n")
      next
    }

    # Set appropriate search patterns based on model and experiment
    if (ModelArray[m] == "ukesm1-0-ll") {
      if (exp == "historical") {
        # Historical has different realizations for tos vs chla
        tos_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*chla-top")
      } else if (exp == "picontrol") {
        # Picontrol uses r1i1p1f2 for both
        tos_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r1i1p1f2_", exp, ".*chla-top")
      } else {
        # SSP scenarios use r4i1p1f2 for both
        tos_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*tos")
        chla_pattern <- paste0(ModelArray[m], "_r4i1p1f2_", exp, ".*chla-top")
      }
    } else {
      # Other models use r1i1p1f1 consistently
      tos_pattern <- paste0(ModelArray[m], "_r1i1p1f1_", exp, ".*tos")
      chla_pattern <- paste0(ModelArray[m], "_r1i1p1f1_", exp, ".*chla-top")
    }

    cat("  Looking for tos files with pattern:", tos_pattern, "\n")
    cat("  Looking for chla files with pattern:", chla_pattern, "\n")

    # Search in the base directory for files matching the patterns
    ftos <- list.files(base_dir, pattern = tos_pattern, full.names = TRUE)
    fchl <- list.files(base_dir, pattern = chla_pattern, full.names = TRUE)

    cat("  tos files found:", length(ftos), "\n")
    if(length(ftos) == 0) {
      cat("    No tos files found for this model/experiment combination.\n")
      next
    }

    cat("    First tos file:", basename(ftos[1]), "\n")

    # Extract year information from filename
    year_info <- str_extract(basename(ftos[1]), "\\d{4}_\\d{4}")
    if(!is.na(year_info)) {
      start_year <- as.numeric(str_split(year_info, "_")[[1]][1])
      end_year <- as.numeric(str_split(year_info, "_")[[1]][2])
      cat("    Years covered:", start_year, "to", end_year, "\n")
    } else {
      cat("    Couldn't extract year information from filename\n")
      start_year <- NA
      end_year <- NA
    }

    cat("  chla files found:", length(fchl), "\n")
    if(length(fchl) == 0) {
      cat("    No chla files found for this model/experiment combination.\n")
      next
    }
    cat("    First chla file:", basename(fchl[1]), "\n")

    # Process the files with memory optimization
    tryCatch({
      # Create a result dataframe to store all processed data
      result_df <- NULL

      # Get information about the rasters first
      tos_info <- raster(ftos)
      chl_info <- raster(fchl)

      # Get number of layers
      tos_stack <- stack(ftos)
      n_layers <- nlayers(tos_stack)
      cat("  Number of layers:", n_layers, "\n")

      # Free memory
      rm(tos_stack)
      gc()

      # Generate year sequence
      if(!is.na(start_year) && !is.na(end_year)) {
        year_seq <- seq(from = start_year, to = end_year, length.out = n_layers)
        cat("  Generated year sequence from", start_year, "to", end_year, "\n")
      } else {
        year_seq <- seq_len(n_layers)
        cat("  Using layer indices as years\n")
      }

      # Process in batches to save memory
      batch_size <- 5  # Adjust this number based on your system's memory

      for(i in seq(1, n_layers, by = batch_size)) {
        batch_end <- min(i + batch_size - 1, n_layers)
        cat("  Processing layers", i, "to", batch_end, "\n")

        # Read the batch of layers directly without stacking first
        batch_years <- year_seq[i:batch_end]
        batch_df <- NULL

        for(j in i:batch_end) {
          # Read one layer at a time
          cat("    Reading layer", j, "of", n_layers, "\n")

          # Calculate layer index
          layer_idx <- j - i + 1

          # Read individual layers
          tos_layer <- raster(ftos, band = j)
          chl_layer <- raster(fchl, band = j)

          # Convert to dataframe with coordinates
          tos_df <- as.data.frame(tos_layer, xy = TRUE, na.rm = FALSE)
          names(tos_df) <- c("Lon", "Lat", "SST")

          chl_df <- as.data.frame(chl_layer, xy = TRUE, na.rm = FALSE)
          names(chl_df) <- c("Lon", "Lat", "Chl")

          # Only keep coordinates from one dataframe
          chl_values <- chl_df$Chl

          # Combine with metadata
          layer_data <- tos_df %>%
            mutate(Chl = chl_values,
                   Year = batch_years[layer_idx],
                   Model = ModelArray[m],
                   Experiment = exp)

          # Add to batch dataframe
          if(is.null(batch_df)) {
            batch_df <- layer_data
          } else {
            batch_df <- bind_rows(batch_df, layer_data)
          }

          # Clean up
          rm(tos_layer, chl_layer, tos_df, chl_df, layer_data, chl_values)
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

        # Save intermediate results after each batch to avoid memory issues
        intermediate_file <- paste0(out_dir, .Platform$file.sep, "ClimateChange_", combo_id, "_temp.rds")
        write_rds(result_df, intermediate_file)
        cat("    Saved intermediate results to", intermediate_file, "\n")
      }

      # Save the final combined dataset for this model-experiment
      write_rds(result_df, output_file)
      cat("  Data saved to:", output_file, "\n")

      # Remove intermediate file
      intermediate_file <- paste0(out_dir, .Platform$file.sep, "ClimateChange_", combo_id, "_temp.rds")
      if(file.exists(intermediate_file)) {
        file.remove(intermediate_file)
      }

      # Clean up
      rm(result_df)
      gc()

    }, error = function(e) {
      cat("  ERROR processing", combo_id, ":", conditionMessage(e), "\n")
      cat("  Error details:", e$message, "\n")

      # Print traceback to debug
      cat("  Traceback:\n")
      print(traceback())
    })

    # Force garbage collection
    gc()
  }
}

cat("\nProcessing complete. Individual files have been saved for each model-experiment combination.\n")
cat("To combine all files into a single dataset, run the following code:\n\n")

cat('
# Combine all individual files
all_files <- list.files(paste0(out_dir), pattern = "ClimateChange_.*.rds", full.names = TRUE)
all_files <- all_files[!grepl("_temp.rds$", all_files)]  # Exclude any temporary files
combined_df <- NULL

for (file in all_files) {
  cat("Loading", basename(file), "...\n")
  temp_df <- readRDS(file)

  if (is.null(combined_df)) {
    combined_df <- temp_df
  } else {
    combined_df <- bind_rows(combined_df, temp_df)
  }

  # Clean up
  rm(temp_df)
  gc()
}

# Save the combined dataset
write_rds(combined_df, paste0(out_dir, .Platform$file.sep, "ClimateChange_Compiled.rds"))
cat("Combined dataset saved to:", paste0(out_dir, .Platform$file.sep, "ClimateChange_Compiled.rds"), "\n")
')
```

