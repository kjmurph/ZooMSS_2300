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
saveRDS(master_dt, "Output/enviro_matrix_2300_unique_sst_chl_log10_combinations.rds")

enviro_data <- master_dt %>%
  mutate(chlo = 10^Chl_log10) %>%
  dplyr::select(c(SST, chlo)) %>%
  arrange(desc(chlo), desc(SST)) %>%
  filter(is.na(chlo)==FALSE) %>%
  filter(is.na(SST)==FALSE) %>%
  rename(sst = SST)


process_rds_files <- function(folder_path, chunk_size = 1000000) {
  # Get list of all .rds files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Track progress
  total_files <- length(all_files)
  cat("Processing", total_files, ".rds files...\n")

  # Focus only on SST, Chl_log10, Model, and Experiment
  master_dt <- NULL
  processed_count <- 0

  # Process each file
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)

    # Progress update
    cat(sprintf("[%d/%d] Processing file: %s\n", i, total_files, file_name))

    tryCatch({
      # Read current RDS file
      current_df <- readRDS(file_path)

      # Convert to data.table if it's not already
      if (!is.data.table(current_df)) {
        current_dt <- as.data.table(current_df)
      } else {
        current_dt <- copy(current_df)
      }

      # Extract just the columns we need
      if (all(c("SST", "Chl_log10", "Model", "Experiment") %in% names(current_dt))) {
        current_subset <- current_dt[, .(SST, Chl_log10, Model, Experiment)]
      } else {
        cat("  Warning: Missing required columns in file", file_name, "\n")
        cat("  Available columns:", paste(names(current_dt), collapse=", "), "\n")
        next
      }

      # Get unique combinations
      current_unique <- unique(current_subset)
      cat(sprintf("  File contains %d unique SST, Chl_log10, Model, Experiment combinations\n",
                  nrow(current_unique)))

      # First file initialization
      if (is.null(master_dt)) {
        master_dt <- current_unique
        cat(sprintf("  Initialized master data.table with %d rows\n", nrow(master_dt)))
      } else {
        # Process in chunks to avoid memory issues
        total_rows <- nrow(current_unique)
        chunks <- ceiling(total_rows / chunk_size)
        added_count <- 0

        for (chunk in 1:chunks) {
          start_idx <- (chunk - 1) * chunk_size + 1
          end_idx <- min(chunk * chunk_size, total_rows)

          if (start_idx > total_rows) break

          chunk_data <- current_unique[start_idx:end_idx]

          # For efficiency, use a set operation to find new combinations
          old_count <- nrow(master_dt)
          # Combine and find unique rows
          master_dt <- unique(rbindlist(list(master_dt, chunk_data)))
          new_count <- nrow(master_dt)
          added_count <- added_count + (new_count - old_count)
        }

        cat(sprintf("  Added %d new unique combinations to master data.table\n", added_count))
      }

      # Report current size of master dataframe
      cat(sprintf("  Master data.table now contains %d unique combinations\n", nrow(master_dt)))

      processed_count <- processed_count + 1

      # Save intermediate results every few files
      if (i %% 3 == 0 && !is.null(master_dt)) {
        temp_filename <- paste0("master_unique_combinations_intermediate_", i, ".rds")
        saveRDS(master_dt, temp_filename)
        cat(sprintf("  Saved intermediate results to %s\n", temp_filename))
      }

    }, error = function(e) {
      cat(sprintf("Error processing file %s: %s\n", file_name, e$message))
    })
  }

  cat("Processing complete!\n")
  cat(sprintf("Successfully processed %d out of %d files\n", processed_count, total_files))
  if (!is.null(master_dt)) {
    cat("Final master data.table contains", nrow(master_dt), "unique combinations\n")
    cat("Columns in master data.table:", paste(names(master_dt), collapse=", "), "\n")

    # Set keys for faster access later
    setkeyv(master_dt, c("SST", "Chl_log10", "Model", "Experiment"))
  } else {
    cat("No data was successfully processed\n")
  }

  # Return the master data.table
  return(master_dt)
}

# Example usage
folder_path <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"
master_dt <- process_rds_files(folder_path)

# Save results
saveRDS(master_dt, "master_unique_sst_chl_model_combinations.rds")

# Display summary of models and experiments
if (!is.null(master_dt)) {
  cat("\nSummary of Models and Experiments in the dataset:\n")
  model_summary <- master_dt[, .N, by = .(Model, Experiment)]
  print(model_summary)

  # Optional: Also show the range of SST and Chl_log10 values
  sst_range <- master_dt[, .(min_sst = min(SST), max_sst = max(SST))]
  chl_range <- master_dt[, .(min_chl = min(Chl_log10), max_chl = max(Chl_log10))]

  cat("\nRange of SST values:", sst_range$min_sst, "to", sst_range$max_sst, "\n")
  cat("Range of Chl_log10 values:", chl_range$min_chl, "to", chl_range$max_chl, "\n")
}

enviro_data <- master_dt %>%
  mutate(chlo = 10^Chl_log10) %>%
  # dplyr::select(c(SST, chlo)) %>%
  arrange(desc(chlo), desc(SST)) %>%
  filter(is.na(chlo)==FALSE) %>%
  filter(is.na(SST)==FALSE) %>%
  rename(sst = SST)

```
```{r fig.width=12}
ggplot(data = enviro_data, mapping = aes(x = sst, y = log10(chlo))) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~Model) +
  theme_bw()
```

```{r}
# Adapted SST vs Chlorophyll visualization script
# Original by Jason Everett (UQ), modified to work with existing RDS data

library(tidyverse)
library(viridis)
library(hexbin)
library(data.table)

# Read in the processed data
cat("Loading master data file...\n")
df_raw <- readRDS("master_data_final_batch.rds")

# Convert to tibble for tidyverse operations
cat("Converting to tibble format...\n")
df <- as_tibble(df_raw)

# Free up memory
rm(df_raw)
gc()

# Verify columns and structure
cat("Data structure:\n")
str(df)

# Process the data similar to original script
cat("Processing data...\n")

# Standardize column names if needed
if("sst" %in% names(df) && !"SST" %in% names(df)) {
  df <- df %>% rename(SST = sst)
}

if("chl" %in% names(df) && !"Chl_log10" %in% names(df)) {
  df <- df %>%
    rename(Chl = chl) %>%
    mutate(Chl_log10 = log10(Chl))
}

# Add coordinates if they're not present and we were able to extract them
if(!all(c("lon", "lat") %in% names(df))) {
  cat("Note: Geographic coordinates not found in data\n")
  # We'll use the data without coordinates
} else if(!all(c("Lon", "Lat") %in% names(df))) {
  # Standardize coordinate column names
  df <- df %>% rename(Lon = lon, Lat = lat)
}

# Create directory for figures if it doesn't exist
dir.create("Figures", showWarnings = FALSE)

# Sample data for plotting if it's very large
plot_sample_size <- 2000000
if(nrow(df) > plot_sample_size) {
  cat(sprintf("Sampling %d rows for plotting from %d total rows\n", plot_sample_size, nrow(df)))
  set.seed(123)  # For reproducibility
  plot_df <- df %>% sample_n(plot_sample_size)
} else {
  plot_df <- df
}

cat("Creating plots...\n")

# Plot 1: All data
p1 <- ggplot(data = plot_df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_viridis_c(name = "count", trans = "log10") +
  theme_bw() +
  labs(
    x = "SST (°C)",
    y = "log10(Chlorophyll)",
    title = "SST vs Chlorophyll Relationship (All Models)"
  )

# Save the plot
output_file1 <- "Figures/SSTChl_All.pdf"
ggsave(output_file1, p1, width = 6, height = 6)
cat("Plot saved to", output_file1, "\n")

# Plot 2: Facet by Model
p2 <- ggplot(data = plot_df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_viridis_c(name = "count", trans = "log10") +
  theme_bw() +
  facet_wrap(facets = "Model", scales = "fixed") +
  labs(
    x = "SST (°C)",
    y = "log10(Chlorophyll)",
    title = "SST vs Chlorophyll Relationship by Model"
  )

# Save the plot
output_file2 <- "Figures/SSTChl_ModelFacet.pdf"
ggsave(output_file2, p2, width = 10, height = 6)
cat("Plot saved to", output_file2, "\n")

# Create a distinct dataset (like in the original script)
cat("Creating distinct dataset...\n")
ds <- df %>%
  distinct(SST, Chl_log10, .keep_all = TRUE)

cat(sprintf("Reduced from %d rows to %d unique SST-Chl combinations\n", nrow(df), nrow(ds)))

# Sample if the distinct dataset is still very large
distinct_sample_size <- 100000
if(nrow(ds) > distinct_sample_size) {
  cat(sprintf("Sampling %d rows for distinct plot from %d total distinct rows\n",
              distinct_sample_size, nrow(ds)))
  set.seed(456)  # Different seed for variety
  plot_ds <- ds %>% sample_n(distinct_sample_size)
} else {
  plot_ds <- ds
}

# Plot 3: Distinct combinations
p3 <- ggplot(data = plot_ds, mapping = aes(x = SST, y = Chl_log10)) +
  geom_point(alpha = 0.3, size = 0.5) +
  theme_bw() +
  labs(
    x = "SST (°C)",
    y = "log10(Chlorophyll)",
    title = "Unique SST-Chlorophyll Combinations"
  )

# Save the plot
output_file3 <- "Figures/SSTChl_Distinct.pdf"
ggsave(output_file3, p3, width = 6, height = 6)
cat("Plot saved to", output_file3, "\n")

# Create and save enviro_data (like in the original script)
cat("Creating and saving environmental data space...\n")
enviro_data <- ds %>%
  mutate(chlo = 10^Chl_log10) %>%
  select(SST, chlo) %>%
  arrange(desc(chlo), desc(SST)) %>%
  filter(!is.na(chlo), !is.na(SST)) %>%
  rename(sst = SST)

# Save the environmental data
output_file4 <- "enviro_CMIP_Matrix.RDS"
saveRDS(enviro_data, output_file4)
cat("Environmental data saved to", output_file4, "\n")

# Bonus: If the data has experiment information, create a model×experiment facet plot
if("Experiment" %in% names(df)) {
  cat("Creating Model×Experiment facet plot...\n")

  p4 <- ggplot(data = plot_df, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex() +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    theme_bw() +
    facet_grid(Experiment ~ Model) +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll by Model and Experiment"
    )

  # Save the plot
  output_file5 <- "Figures/SSTChl_ModelExperimentFacet.pdf"
  ggsave(output_file5, p4, width = 12, height = 8)
  cat("Model×Experiment plot saved to", output_file5, "\n")
}

cat("Processing complete!\n")
```

```{r}
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

  ggsave("Figures/SSTChl_All.pdf", p1, width = 6, height = 6)
  ggsave("Figures/SSTChl_All.png", p1, width = 6, height = 6, dpi = 300)

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

  ggsave("Figures/SSTChl_ModelFacet.pdf", p2, width = 10, height = 6)
  ggsave("Figures/SSTChl_ModelFacet.png", p2, width = 10, height = 6, dpi = 300)

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

  ggsave("Figures/SSTChl_ModelExperimentFacet.pdf", p3, width = 12, height = 8)
  ggsave("Figures/SSTChl_ModelExperimentFacet.png", p3, width = 12, height = 8, dpi = 300)

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

  ggsave("Figures/SSTChl_Distinct.pdf", p4, width = 6, height = 6)
  ggsave("Figures/SSTChl_Distinct.png", p4, width = 6, height = 6, dpi = 300)

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
folder_path <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"
run_workflow(folder_path)
```


```{r}
library(data.table)
library(ggplot2)
library(dplyr)
library(hexbin)
library(viridis)

process_rds_files_with_all_data <- function(folder_path, chunk_size = 500000) {
  # Get list of all .rds files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Track progress
  total_files <- length(all_files)
  cat("Processing", total_files, ".rds files...\n")

  # Master data table will be created when first file is processed
  master_dt <- NULL
  processed_count <- 0
  total_rows_processed <- 0

  # Process each file
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)

    # Progress update
    cat(sprintf("[%d/%d] Processing file: %s\n", i, total_files, file_name))

    tryCatch({
      # Read current RDS file
      current_df <- readRDS(file_path)

      # Convert to data.table if it's not already
      if (!is.data.table(current_df)) {
        current_dt <- as.data.table(current_df)
      } else {
        current_dt <- copy(current_df)
      }

      # Check for required columns
      req_cols <- c("SST", "Chl_log10", "Model", "Experiment")
      opt_cols <- c("lat", "lon") # Optional but preferred columns

      # Identify available columns
      avail_cols <- names(current_dt)
      missing_req <- setdiff(req_cols, avail_cols)

      if (length(missing_req) > 0) {
        cat("  Warning: Missing required columns in file", file_name, ":",
            paste(missing_req, collapse=", "), "\n")
        cat("  Available columns:", paste(avail_cols, collapse=", "), "\n")
        next
      }

      # Determine which optional columns exist
      existing_opt <- intersect(opt_cols, avail_cols)
      selected_cols <- c(req_cols, existing_opt)

      # Extract just the columns we need to reduce memory usage
      current_subset <- current_dt[, ..selected_cols]

      # Free memory
      rm(current_df, current_dt)
      gc()

      # Process in chunks to avoid memory issues
      total_rows <- nrow(current_subset)
      chunks <- ceiling(total_rows / chunk_size)

      cat(sprintf("  File contains %d rows with SST and Chl_log10 data\n", total_rows))

      # Process and save each chunk
      for (chunk in 1:chunks) {
        start_idx <- (chunk - 1) * chunk_size + 1
        end_idx <- min(chunk * chunk_size, total_rows)

        if (start_idx > total_rows) break

        # Get current chunk
        chunk_data <- current_subset[start_idx:end_idx]

        # Initialize master_dt with first chunk if null
        if (is.null(master_dt)) {
          master_dt <- chunk_data
          cat(sprintf("  Initialized master data.table with %d rows\n", nrow(master_dt)))
        } else {
          # Combine with master data table
          master_dt <- rbindlist(list(master_dt, chunk_data), use.names = TRUE)
          cat(sprintf("  Added chunk %d of %d with %d rows to master data.table\n",
                      chunk, chunks, nrow(chunk_data)))
        }

        # Check if master_dt is getting too large, save and reset if needed
        if (nrow(master_dt) >= 5 * chunk_size) {
          temp_filename <- paste0("master_data_batch_", processed_count, "_", i, "_", chunk, ".rds")
          saveRDS(master_dt, temp_filename)
          cat(sprintf("  Saved batch of %d rows to %s\n", nrow(master_dt), temp_filename))
          total_rows_processed <- total_rows_processed + nrow(master_dt)
          master_dt <- NULL
          gc()
        }
      }

      # Report progress
      if (!is.null(master_dt)) {
        cat(sprintf("  Master data.table currently contains %d rows\n", nrow(master_dt)))
      }

      processed_count <- processed_count + 1

    }, error = function(e) {
      cat(sprintf("Error processing file %s: %s\n", file_name, e$message))
    })
  }

  # Save any remaining data
  if (!is.null(master_dt) && nrow(master_dt) > 0) {
    final_filename <- "master_data_final_batch.rds"
    saveRDS(master_dt, final_filename)
    total_rows_processed <- total_rows_processed + nrow(master_dt)
    cat(sprintf("  Saved final batch of %d rows to %s\n", nrow(master_dt), final_filename))
  }

  cat("Processing complete!\n")
  cat(sprintf("Successfully processed %d out of %d files\n", processed_count, total_files))
  cat(sprintf("Total rows processed: %d\n", total_rows_processed))

  # Return info about saved files
  return(list(
    processed_files = processed_count,
    total_rows = total_rows_processed,
    saved_pattern = "master_data_batch_*.rds"
  ))
}

# Function to combine saved batches (run after processing all files)
combine_data_batches <- function(pattern = "master_data_batch_*.rds", output_file = "all_sst_chl_data.rds") {
  # Get all batch files
  batch_files <- list.files(pattern = pattern, full.names = TRUE)

  if (length(batch_files) == 0) {
    cat("No batch files found matching pattern:", pattern, "\n")
    return(NULL)
  }

  cat("Found", length(batch_files), "batch files to combine\n")

  # Combine in chunks to manage memory
  combined_dt <- NULL
  total_rows <- 0

  for (i in seq_along(batch_files)) {
    cat(sprintf("Processing batch file %d of %d: %s\n", i, length(batch_files), basename(batch_files[i])))

    # Load current batch
    current_batch <- readRDS(batch_files[i])
    current_rows <- nrow(current_batch)

    if (is.null(combined_dt)) {
      combined_dt <- current_batch
    } else {
      # Combine with existing data
      combined_dt <- rbindlist(list(combined_dt, current_batch), use.names = TRUE)
    }

    total_rows <- total_rows + current_rows
    cat(sprintf("  Added %d rows, combined data now has %d rows\n", current_rows, total_rows))

    # Clean up to save memory
    rm(current_batch)
    gc()
  }

  # Save combined data
  if (!is.null(combined_dt)) {
    saveRDS(combined_dt, output_file)
    cat(sprintf("Saved combined data with %d rows to %s\n", nrow(combined_dt), output_file))
  }

  return(combined_dt)
}

# Function to create hexbin plot with all data or sampling if needed
plot_sst_chl_by_model <- function(data_file, sample_size = NULL, bins = 70) {
  # Load data
  cat("Loading data from", data_file, "\n")
  full_data <- readRDS(data_file)

  # Sample if necessary
  if (!is.null(sample_size) && nrow(full_data) > sample_size) {
    cat(sprintf("Sampling %d rows from %d total for plotting\n", sample_size, nrow(full_data)))
    set.seed(123) # For reproducibility
    plot_data <- full_data[sample(.N, sample_size)]
  } else {
    plot_data <- full_data
  }

  # Create plot
  cat("Creating hexbin plot...\n")
  p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex(bins = bins) +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    facet_wrap(~Model) +
    theme_bw() +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll Relationship by Model"
    )

  # Save plot
  output_file <- "sst_chl_by_model_hexbin.png"
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Plot saved to", output_file, "\n")

  # Clean up to save memory
  rm(full_data)
  gc()

  return(p)
}

# Function to create a more detailed plot with Experiment faceting
plot_sst_chl_detailed <- function(data_file, sample_size = NULL, bins = 70) {
  # Load data
  cat("Loading data from", data_file, "\n")
  full_data <- readRDS(data_file)

  # Sample if necessary
  if (!is.null(sample_size) && nrow(full_data) > sample_size) {
    cat(sprintf("Sampling %d rows from %d total for detailed plotting\n", sample_size, nrow(full_data)))
    set.seed(123) # For reproducibility
    plot_data <- full_data[sample(.N, sample_size)]
  } else {
    plot_data <- full_data
  }

  # Check if lat/lon columns exist
  has_coords <- all(c("lat", "lon") %in% names(plot_data))

  # Create appropriate plot based on available data
  cat("Creating detailed plot...\n")

  if (has_coords) {
    # Plot with lat/lon information if available
    p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10, color = lat)) +
      geom_point(alpha = 0.3, size = 0.5) +
      scale_color_viridis_c(name = "Latitude") +
      facet_grid(Experiment ~ Model) +
      theme_bw() +
      labs(
        x = "SST (°C)",
        y = "log10(Chlorophyll)",
        title = "SST vs Chlorophyll by Model and Experiment",
        subtitle = "Color indicates latitude"
      )
    output_file <- "sst_chl_by_model_experiment_with_lat.png"
  } else {
    # Hexbin plot if no geographic coordinates
    p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10)) +
      geom_hex(bins = bins) +
      scale_fill_viridis_c(name = "count", trans = "log10") +
      facet_grid(Experiment ~ Model) +
      theme_bw() +
      labs(
        x = "SST (°C)",
        y = "log10(Chlorophyll)",
        title = "SST vs Chlorophyll by Model and Experiment"
      )
    output_file <- "sst_chl_by_model_experiment_hexbin.png"
  }

  # Save plot
  ggsave(output_file, p, width = 15, height = 12, dpi = 300)
  cat("Detailed plot saved to", output_file, "\n")

  # Clean up to save memory
  rm(full_data)
  gc()

  return(p)
}

# Example usage workflow
folder_path <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"

# Step 1: Process all files, saving data in batches to manage memory
process_results <- process_rds_files_with_all_data(folder_path, chunk_size = 400000)

# Step 2: Combine all batches into one dataset
all_data <- combine_data_batches(pattern = "master_data_batch_*.rds",
                                 output_file = "all_sst_chl_data_complete.rds")

# Step 3: Create plots from the combined data
# Basic plot by Model (using sampling if needed for very large datasets)
p1 <- plot_sst_chl_by_model("all_sst_chl_data_complete.rds", sample_size = 2000000, bins = 70)

# Detailed plot with Model and Experiment
p2 <- plot_sst_chl_detailed("all_sst_chl_data_complete.rds", sample_size = 1000000, bins = 70)

# Bonus: Calculate and display summary statistics by Model and Experiment
summarize_data <- function(data_file) {
  cat("Loading data for summary statistics...\n")
  dt <- readRDS(data_file)

  # Calculate summary by Model and Experiment
  summary_stats <- dt[, .(
    count = .N,
    min_sst = min(SST, na.rm = TRUE),
    max_sst = max(SST, na.rm = TRUE),
    mean_sst = mean(SST, na.rm = TRUE),
    min_chl = min(Chl_log10, na.rm = TRUE),
    max_chl = max(Chl_log10, na.rm = TRUE),
    mean_chl = mean(Chl_log10, na.rm = TRUE)
  ), by = .(Model, Experiment)]

  # Save summary stats
  out_file <- "sst_chl_summary_stats.csv"
  fwrite(summary_stats, out_file)
  cat("Summary statistics saved to", out_file, "\n")

  return(summary_stats)
}

# Get summary statistics
summary_stats <- summarize_data("all_sst_chl_data_complete.rds")
print(summary_stats)
```
```{r}
library(data.table)
library(ggplot2)
library(dplyr)
library(hexbin)
library(viridis)

# Since we already have master_data_final_batch.rds with all the data,
# let's adjust the process to work with this file directly

# Function to create hexbin plot with all data or sampling if needed
plot_sst_chl_by_model <- function(data_file, sample_size = NULL, bins = 70) {
  # Load data
  cat("Loading data from", data_file, "\n")
  full_data <- readRDS(data_file)

  # Sample if necessary
  if (!is.null(sample_size) && nrow(full_data) > sample_size) {
    cat(sprintf("Sampling %d rows from %d total for plotting\n", sample_size, nrow(full_data)))
    set.seed(123) # For reproducibility
    plot_data <- full_data[sample(.N, sample_size)]
  } else {
    plot_data <- full_data
  }

  # Create plot
  cat("Creating hexbin plot...\n")
  p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10)) +
    geom_hex(bins = bins) +
    scale_fill_viridis_c(name = "count", trans = "log10") +
    facet_wrap(~Model) +
    theme_bw() +
    labs(
      x = "SST (°C)",
      y = "log10(Chlorophyll)",
      title = "SST vs Chlorophyll Relationship by Model"
    )

  # Save plot
  output_file <- "sst_chl_by_model_hexbin.png"
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Plot saved to", output_file, "\n")

  return(p)
}

# Function to create a more detailed plot with Experiment faceting
plot_sst_chl_detailed <- function(data_file, sample_size = NULL, bins = 70) {
  # Load data
  cat("Loading data from", data_file, "\n")
  full_data <- readRDS(data_file)

  # Sample if necessary
  if (!is.null(sample_size) && nrow(full_data) > sample_size) {
    cat(sprintf("Sampling %d rows from %d total for detailed plotting\n", sample_size, nrow(full_data)))
    set.seed(123) # For reproducibility
    plot_data <- full_data[sample(.N, sample_size)]
  } else {
    plot_data <- full_data
  }

  # Check if lat/lon columns exist
  has_coords <- all(c("lat", "lon") %in% names(plot_data))

  # Create appropriate plot based on available data
  cat("Creating detailed plot...\n")

  if (has_coords) {
    # Plot with lat/lon information if available
    p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10, color = lat)) +
      geom_point(alpha = 0.3, size = 0.5) +
      scale_color_viridis_c(name = "Latitude") +
      facet_grid(Experiment ~ Model) +
      theme_bw() +
      labs(
        x = "SST (°C)",
        y = "log10(Chlorophyll)",
        title = "SST vs Chlorophyll by Model and Experiment",
        subtitle = "Color indicates latitude"
      )
    output_file <- "sst_chl_by_model_experiment_with_lat.png"
  } else {
    # Hexbin plot if no geographic coordinates
    p <- ggplot(data = plot_data, mapping = aes(x = SST, y = Chl_log10)) +
      geom_hex(bins = bins) +
      scale_fill_viridis_c(name = "count", trans = "log10") +
      facet_grid(Experiment ~ Model) +
      theme_bw() +
      labs(
        x = "SST (°C)",
        y = "log10(Chlorophyll)",
        title = "SST vs Chlorophyll by Model and Experiment"
      )
    output_file <- "sst_chl_by_model_experiment_hexbin.png"
  }

  # Save plot
  ggsave(output_file, p, width = 15, height = 12, dpi = 300)
  cat("Detailed plot saved to", output_file, "\n")

  return(p)
}

# Calculate and display summary statistics by Model and Experiment
summarize_data <- function(data_file) {
  cat("Loading data for summary statistics...\n")
  dt <- readRDS(data_file)

  # Calculate summary by Model and Experiment
  summary_stats <- dt[, .(
    count = .N,
    min_sst = min(SST, na.rm = TRUE),
    max_sst = max(SST, na.rm = TRUE),
    mean_sst = mean(SST, na.rm = TRUE),
    min_chl = min(Chl_log10, na.rm = TRUE),
    max_chl = max(Chl_log10, na.rm = TRUE),
    mean_chl = mean(Chl_log10, na.rm = TRUE)
  ), by = .(Model, Experiment)]

  # Save summary stats
  out_file <- "sst_chl_summary_stats.csv"
  fwrite(summary_stats, out_file)
  cat("Summary statistics saved to", out_file, "\n")

  return(summary_stats)
}

# Use the existing final batch file that was successfully created
final_batch_file <- "master_data_final_batch.rds"

# Create basic plot by Model
p1 <- plot_sst_chl_by_model(final_batch_file, sample_size = 2000000, bins = 70)

# Create detailed plot with Model and Experiment
p2 <- plot_sst_chl_detailed(final_batch_file, sample_size = 1000000, bins = 70)

# Get summary statistics
summary_stats <- summarize_data(final_batch_file)
print(summary_stats)
```


```{r}
# Create a dataframe of unique combinations from enviro_data
enviro_unique <- unique(enviro_data[, c("sst", "chlo")])

# Create a dataframe of unique combinations from ClimateChange_Compiled_Distinct
climate_unique <- unique(ClimateChange_Compiled_Distinct[, c("sst", "chlo")])

# Create a dataframe of unique combinations from ClimateChange_Compiled_Distinct
climate_CMIP_unique <- unique(enviro_CMIP_Matrix[, c("sst", "chlo")])

# Find rows in enviro_unique that are not in climate_unique
enviro_2300_distinct <- anti_join(enviro_unique, climate_unique, by = c("sst", "chlo"))

climate_CMIP_distinct <- anti_join(climate_CMIP_unique, climate_unique, by = c("sst", "chlo"))

enviro_2300_distinct_trim <- enviro_2300_distinct %>%
  filter(!chlo>10)
```


```{r}
# Alternative approach with explicit binning for larger datasets
heatmap_data_bins <- enviro_2300_distinct %>%
  # Create explicit bins
  mutate(sst_bin = cut(sst, breaks = seq(-5, 45, by = 2)),  # 2°C bins
         chlo_bin = cut(chlo, breaks = c(0, 0.1, 0.5, 1, 2, 5, 10, 20))) %>%  # Custom bins for chlorophyll
  # Count occurrences
  count(sst_bin, chlo_bin) %>%
  rename(frequency = n)

# Create the heatmap with explicit bins
ggplot(heatmap_data_bins, aes(x = sst_bin, y = chlo_bin, fill = frequency)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Heatmap of SST and CHLO Value Combinations (Binned)",
       x = "Sea Surface Temperature (°C)",
       y = "Chlorophyll Concentration (mg/m³)",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = enviro_2300_distinct, mapping = aes(x = sst, y = log10(chlo))) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data = enviro_unique, mapping = aes(x = sst, y = log10(chlo))) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
```



```{r}
library(data.table)

process_rds_files <- function(folder_path) {
  # Get list of all .rds files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

  # Create empty master data.table
  master_dt <- data.table(SST = numeric(), Chl = numeric())

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

    # Convert to data.table if it's not already and select only SST and Chl
    if (!is.data.table(current_df)) {
      current_dt <- as.data.table(current_df[, c("SST", "Chl")])
    } else {
      current_dt <- current_df[, .(SST, Chl)]
    }

    # Round Chl values to 3 decimal places
    current_dt[, Chl := round(Chl, 3)]

    # Get unique combinations from current file
    current_unique <- unique(current_dt)

    # If master is empty, initialize it with first file's unique values
    if (nrow(master_dt) == 0) {
      master_dt <- current_unique
      cat(sprintf("  Added %d unique combinations to master data.table\n", nrow(current_unique)))
    } else {
      # Find new combinations efficiently using data.table syntax
      setkey(master_dt, SST, Chl)
      setkey(current_unique, SST, Chl)

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
  cat("Final master data.table contains", nrow(master_dt), "unique SST and Chl combinations\n")

  # Return the master data.table
  return(master_dt)
}

# Example usage
folder_path <- "~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/output"
master_dt <- process_rds_files(folder_path)

# Save results
saveRDS(master_dt, "master_unique_sst_chl_combinations.rds")

# You might also want to save as CSV for easier sharing
write.csv(master_dt, "master_unique_sst_chl_combinations.csv", row.names = FALSE)
```



```{r}
fZooMSS_CalculatePhytoParam = function(df){ # chlo is chlorophyll concentration in mg m^-3

  ## Calculate pico, nano, micro phytoplankton proportions of total chlorophyll
  ## BREWIN ET AL., 2015
  pico <- (0.13*(1-exp(-0.8/0.13*df$chlo)))/df$chlo
  nano <- (0.77*(1-exp(-0.94/0.77*df$chlo)))/df$chlo - pico
  micro <- (df$chlo - 0.77*(1-exp(-0.94/0.77*df$chlo)))/df$chlo

  ## Convert total chlorophyll to g m^-3 total wet weight - biomass
  ## Allocate total chlorophyll to the three size classes
  c_chl <- ((df$chlo^0.89)*(10^1.79))/df$chlo # chlo:carbon ratio, from Mara??on et al. 2014
  tot_biom_c <- c_chl*df$chlo/1000 # (convert to grams carbon)
  tot_biom <- tot_biom_c*(1/0.1) # convert to grams wet weight, assuming 0.1 C:ww

  # Break up total biom into pico, nano and micro
  df$pico_biom <- pico*tot_biom
  df$nano_biom <- nano*tot_biom
  df$micro_biom <- micro*tot_biom

  ## Find abundances at boundaries of pico, nano size ranges, by analytically
  ## solving integral of N = aw^b

  w_0 <- -14.5 # log minimum size of picophytoplankton
  w_1 <- -11.5 # log minimum size of nanophytoplankton (max size of pico also)
  w_2 <- -8.5 # log minimum size of macrophytoplankton (max size of nano also)

  df$phyto_slope <- (log10(df$pico_biom) - log10(df$nano_biom) - w_1 + w_2)/(w_1 - w_2)  # Calculate slope
  df$phyto_int <- log10(df$pico_biom*(df$phyto_slope+1)/((10^(w_1))^(df$phyto_slope+1) - (10^(w_0))^(df$phyto_slope+1))) # Calculate intercept

  ## Calculate maximum size
  df$phyto_max <- 0.1*round((-8.4 + 2*micro)/0.1) # Maximum size depends on the proportion of micro
  max_phyto <- rep(-7, length(df$chlo)) # Set -7 to be the max possible size for phyto
  df$phyto_max <- pmin(max_phyto, df$phyto_max)

  return(df)
}

# Rename a variable
# New name = old name
master_dt <- master_dt %>%
  rename(chlo = Chl) %>%
  na.omit()

enviro_matrix_2300 <- fZooMSS_CalculatePhytoParam(master_dt)

```


```{r}
enviro_matrix_2300 <- enviro_matrix_2300 %>%
  rename(Chl = chlo)

library(data.table)

# Function to compare master dataframe with another dataframe
# Returns combinations in master that are not in the comparison dataframe
compare_with_other_dataset <- function(master_dt, comparison_file) {
  # Read the comparison dataset
  cat("Reading comparison dataset:", basename(comparison_file), "\n")

  # Check if it's an RDS file
  if (grepl("\\.rds$", comparison_file)) {
    comparison_df <- readRDS(comparison_file)
  } else if (grepl("\\.csv$", comparison_file)) {
    comparison_df <- fread(comparison_file)
  } else {
    stop("Comparison file must be .rds or .csv format")
  }

  # Convert to data.table if needed and select only the columns we need
  if (!is.data.table(comparison_df)) {
    comparison_dt <- as.data.table(comparison_df[, c("SST", "Chl")])
  } else {
    comparison_dt <- comparison_df[, .(SST, Chl)]
  }

  # Round Chl values to 3 decimal places (to match the master)
  comparison_dt[, Chl := round(Chl, 3)]

  # Get unique combinations from comparison dataset
  comparison_unique <- unique(comparison_dt)

  cat("Master dataset has", nrow(master_dt), "unique combinations\n")
  cat("Comparison dataset has", nrow(comparison_unique), "unique combinations\n")

  # Set keys for efficient comparison
  setkey(master_dt, SST, Chl)
  setkey(comparison_unique, SST, Chl)

  # Find combinations in master that are NOT in the comparison dataset
  unique_to_master <- master_dt[!comparison_unique]

  cat("Found", nrow(unique_to_master), "combinations in master that are not in comparison dataset\n")

  # Return the combinations unique to master
  return(unique_to_master)
}

# Example usage:
# First, load your master dataset (or create it if you haven't already)
# folder_path <- "path/to/your/data/files"
# master_dt <- process_rds_files(folder_path)

# Find combinations in master that are not in the comparison dataset
unique_combinations_2300 <- compare_with_other_dataset(enviro_matrix_2300, enviro_Matrix)

# Save the result
saveRDS(unique_combinations, "combinations_unique_to_master.rds")
write.csv(unique_combinations, "combinations_unique_to_master.csv", row.names = FALSE)

# If you want to see a sample of the combinations unique to master
if (nrow(unique_combinations) > 0) {
  cat("\nSample of combinations unique to master (first 10 rows):\n")
  print(head(unique_combinations, 10))
} else {
  cat("\nNo combinations unique to master found.\n")
}
```


