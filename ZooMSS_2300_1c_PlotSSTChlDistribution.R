
# Comprehensive script to process RDS files and create SST vs Chl visualizations
# Combining memory-efficient processing with Jason Everett's visualization approach

library(data.table)
library(tidyverse)
library(viridis)
library(hexbin)

# Define the models and experiments we expect to find
expected_models <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
expected_experiments <- c("historical","picontrol", "ssp126", "ssp534-over", "ssp585")

process_rds_files <- function(folder_path, chunk_size = 500000) {
  # Get list of all .rds files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Create output directory
  dir.create("processed_data", showWarnings = FALSE)

  # Initialize a data frame to track models and experiments found in each file
  file_contents <- data.frame(
    file_name = character(),
    model = character(),
    experiment = character(),
    row_count = integer(),
    stringsAsFactors = FALSE
  )

  # Track progress
  total_files <- length(all_files)
  cat("Processing", total_files, ".rds files...\n")
  total_rows_processed <- 0

  # Track models and experiments found
  all_models <- character(0)
  all_experiments <- character(0)

  # Process each file
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)

    # Progress update
    cat(sprintf("[%d/%d] Processing file: %s\n", i, total_files, file_name))

    tryCatch({
      # Read current RDS file
      current_df <- readRDS(file_path)

      # Display column names to debug
      cat("  Columns in file:", paste(names(current_df), collapse=", "), "\n")

      # Check for model and experiment information
      has_model <- "Model" %in% names(current_df)
      has_experiment <- "Experiment" %in% names(current_df)

      if (!has_model || !has_experiment) {
        cat("  Warning: File missing Model or Experiment column\n")
        next
      }

      # Convert to data.table
      current_dt <- as.data.table(current_df)

      # Extract unique model and experiment combinations
      model_exp_combos <- unique(current_dt[, .(Model, Experiment)])

      # Track model and experiment combinations found in this file
      for (j in 1:nrow(model_exp_combos)) {
        model <- model_exp_combos[j, Model]
        experiment <- model_exp_combos[j, Experiment]

        # Count rows for this combination
        row_count <- nrow(current_dt[Model == model & Experiment == experiment])

        # Add to tracking data frame
        file_contents <- rbind(file_contents, data.frame(
          file_name = file_name,
          model = model,
          experiment = experiment,
          row_count = row_count,
          stringsAsFactors = FALSE
        ))

        # Update overall tracking
        all_models <- unique(c(all_models, model))
        all_experiments <- unique(c(all_experiments, experiment))
      }

      # Process and save by model and experiment
      for (model in unique(current_dt$Model)) {
        for (experiment in unique(current_dt[Model == model, Experiment])) {
          # Extract data for this model and experiment
          subset_data <- current_dt[Model == model & Experiment == experiment]

          # Create a filename for this combination
          output_file <- file.path("processed_data",
                                   sprintf("%s_%s.rds", model, experiment))

          # Check if file already exists
          if (file.exists(output_file)) {
            # Append to existing file
            existing_data <- readRDS(output_file)
            combined_data <- rbindlist(list(existing_data, subset_data))
            saveRDS(combined_data, output_file)
            cat(sprintf("  Appended %d rows to %s\n", nrow(subset_data), output_file))
          } else {
            # Create new file
            saveRDS(subset_data, output_file)
            cat(sprintf("  Saved %d rows to %s\n", nrow(subset_data), output_file))
          }

          total_rows_processed <- total_rows_processed + nrow(subset_data)
        }
      }

      # Clean up
      rm(current_df, current_dt)
      gc()

    }, error = function(e) {
      cat(sprintf("Error processing file %s: %s\n", file_name, e$message))
    })
  }

  # Save model and experiment tracking info
  write.csv(file_contents, "file_model_experiment_tracking.csv", row.names = FALSE)

  # Summary of what was found
  cat("\nSummary of processing:\n")
  cat("Total rows processed:", total_rows_processed, "\n")
  cat("Models found:", paste(all_models, collapse=", "), "\n")
  cat("Experiments found:", paste(all_experiments, collapse=", "), "\n")

  # Check for missing models/experiments
  missing_models <- setdiff(expected_models, all_models)
  missing_exps <- setdiff(expected_experiments, all_experiments)

  if (length(missing_models) > 0) {
    cat("WARNING: The following expected models were not found:",
        paste(missing_models, collapse=", "), "\n")
  }

  if (length(missing_exps) > 0) {
    cat("WARNING: The following expected experiments were not found:",
        paste(missing_exps, collapse=", "), "\n")
  }

  return(list(
    total_rows = total_rows_processed,
    models_found = all_models,
    experiments_found = all_experiments,
    file_contents = file_contents
  ))
}

# Function to combine data and create visualizations following Jason's approach
create_visualizations <- function() {
  # Get all processed files
  processed_files <- list.files("processed_data", pattern = "\\.rds$", full.names = TRUE)

  if (length(processed_files) == 0) {
    cat("No processed data files found.\n")
    return(NULL)
  }

  # Create directory for figures
  dir.create("Figures", showWarnings = FALSE)

  # Load and combine all data
  cat("Loading and combining data from", length(processed_files), "files...\n")

  # Initialize an empty data frame
  df <- NULL

  # Process each file and track which models/experiments we have
  model_exp_data <- data.frame(
    model = character(),
    experiment = character(),
    row_count = integer(),
    stringsAsFactors = FALSE
  )

  for (file_path in processed_files) {
    # Extract model and experiment from filename
    file_name <- basename(file_path)
    file_parts <- tools::file_path_sans_ext(file_name)
    file_parts <- strsplit(file_parts, "_")[[1]]

    # Assume format is model_experiment.rds
    if (length(file_parts) >= 2) {
      current_model <- file_parts[1]
      current_exp <- file_parts[2]

      cat(sprintf("Loading data for model=%s, experiment=%s\n",
                  current_model, current_exp))

      # Load the data
      current_data <- readRDS(file_path)

      # Track row count
      row_count <- nrow(current_data)
      model_exp_data <- rbind(model_exp_data, data.frame(
        model = current_model,
        experiment = current_exp,
        row_count = row_count,
        stringsAsFactors = FALSE
      ))

      # Combine with main data frame
      if (is.null(df)) {
        df <- current_data
      } else {
        df <- rbindlist(list(df, current_data), use.names = TRUE, fill = TRUE)
      }

      # Clean up to save memory
      rm(current_data)
      gc()
    }
  }

  # Display summary of what we have
  cat("\nSummary of combined data:\n")
  cat("Total rows:", nrow(df), "\n")
  print(model_exp_data)

  # Ensure we have all required columns
  required_cols <- c("SST", "Chl_log10", "Model", "Experiment")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    cat("ERROR: Missing required columns:", paste(missing_cols, collapse=", "), "\n")
    return(NULL)
  }

  # Standardize column names if needed
  if(!all(c("Lon", "Lat") %in% names(df)) && all(c("lon", "lat") %in% names(df))) {
    df <- df %>% rename(Lon = lon, Lat = lat)
  }

  # Create plots as in Jason's script
  cat("Creating plots...\n")

  # Plot 1: All data
  p1 <- ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex() +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    theme_bw() +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll Relationship (All Models and Experiments)"
    )

  ggsave("Figures/SSTChl_All_v2.pdf", p1, width = 6, height = 6)
  ggsave("Figures/SSTChl_All_v2.png", p1, width = 6, height = 6, dpi = 300)

  # Plot 2: Facet by Model
  p2 <- ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex() +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    theme_bw() +
    facet_wrap(~ Model, scales = "fixed") +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll Relationship by Model"
    )

  ggsave("Figures/SSTChl_ModelFacet_v2.pdf", p2, width = 10, height = 6)
  ggsave("Figures/SSTChl_ModelFacet_v2.png", p2, width = 10, height = 6, dpi = 300)

  # Plot 3: Facet by Model and Experiment
  p3 <- ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex() +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    theme_bw() +
    facet_grid(Experiment ~ Model) +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll by Model and Experiment"
    )

  ggsave("Figures/SSTChl_ModelExperimentFacet_v2.pdf", p3, width = 12, height = 8)
  ggsave("Figures/SSTChl_ModelExperimentFacet_v2.png", p3, width = 12, height = 8, dpi = 300)

  # Create distinct dataset
  cat("Creating distinct SST-Chl combinations dataset...\n")
  ds <- df %>%
    distinct(SST, Chl_log10, .keep_all = TRUE)

  cat(sprintf("Reduced from %d rows to %d unique SST-Chl combinations\n",
              nrow(df), nrow(ds)))

  # Plot 4: Distinct combinations
  p4 <- ggplot(data = ds, mapping = aes(x = SST, y = Chl_log10)) +
    geom_point(alpha = 0.3, size = 0.5) +
    theme_bw() +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "Unique SST-Chlorophyll Combinations"
    )

  ggsave("Figures/SSTChl_Distinct_v2.pdf", p4, width = 6, height = 6)
  ggsave("Figures/SSTChl_Distinct_v2.png", p4, width = 6, height = 6, dpi = 300)

  # Create and save enviro_data
  cat("Creating and saving environmental data space...\n")
  enviro_data <- ds %>%
    mutate(chlo = 10^Chl_log10) %>%
    select(SST, chlo) %>%
    arrange(desc(chlo), desc(SST)) %>%
    filter(!is.na(chlo), !is.na(SST)) %>%
    rename(sst = SST)

  saveRDS(enviro_data, "enviro_CMIP_Matrix.RDS")

  cat("Processing and visualization complete!\n")

  return(list(
    combined_data = df,
    distinct_data = ds,
    enviro_data = enviro_data
  ))
}

# Main function to run the workflow
run_workflow <- function(folder_path) {
  cat("Starting SST-Chl processing and visualization workflow\n")

  # Step 1: Process RDS files
  cat("\n===== STEP 1: PROCESSING RDS FILES =====\n")
  process_result <- process_rds_files(folder_path)

  # Step 2: Create visualizations
  cat("\n===== STEP 2: CREATING VISUALIZATIONS =====\n")
  viz_result <- create_visualizations()

  cat("\nWorkflow complete!\n")
}

# Execute the workflow with your folder path
folder_path <- "~/R Projects/ZooMSS_2300/Input/2300_processed/"
run_workflow(folder_path)
