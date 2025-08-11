# Split large climate file by both model and experiment for manageable sizes
library(tidyverse)

base_dir <- "~/R Projects/ZooMSS_2300/"

cat("Splitting climate data by model AND experiment...\n")

# Load the original file
rds_file <- file.path(base_dir, "Output/ClimateChange_2300_Compiled.rds")

cat("Loading and analyzing the structure of the climate data...\n")

# First, let's check what models and experiments we have without loading everything
tryCatch({
  nc_2300 <- read_rds(rds_file)
  cat("Successfully loaded full file\n")

  # Get unique combinations of Model and Experiment
  model_exp_summary <- nc_2300 %>%
    group_by(Model, Experiment) %>%
    summarise(
      rows = n(),
      .groups = 'drop'
    ) %>%
    arrange(Model, Experiment)

  cat("\nModel-Experiment combinations found:\n")
  print(model_exp_summary)

  # Calculate expected file sizes
  total_rows <- nrow(nc_2300)
  original_size_gb <- file.size(rds_file) / 1024^3

  model_exp_summary <- model_exp_summary %>%
    mutate(
      proportion = rows / total_rows,
      estimated_size_gb = proportion * original_size_gb,
      estimated_size_mb = estimated_size_gb * 1024
    )

  cat("\nEstimated file sizes after splitting:\n")
  print(model_exp_summary %>%
          select(Model, Experiment, rows, estimated_size_mb) %>%
          mutate(estimated_size_mb = round(estimated_size_mb, 1)))

  # Split and save by model AND experiment
  cat("\nSplitting files...\n")

  for(i in 1:nrow(model_exp_summary)) {
    model <- model_exp_summary$Model[i]
    experiment <- model_exp_summary$Experiment[i]
    expected_rows <- model_exp_summary$rows[i]

    cat("Processing", model, "-", experiment, "(", expected_rows, "rows )\n")

    # Filter for this model-experiment combination
    nc_subset <- nc_2300 %>%
      filter(Model == model, Experiment == experiment)

    cat("- Actual rows extracted:", nrow(nc_subset), "\n")

    # Create filename
    model_clean <- str_replace_all(model, "[^a-zA-Z0-9]", "-")
    exp_clean <- str_replace_all(experiment, "[^a-zA-Z0-9]", "-")
    filename <- paste0("ClimateChange_2300_", model_clean, "_", exp_clean, ".rds")
    file_path <- file.path(base_dir, "Output", filename)

    # Save the subset
    write_rds(nc_subset, file_path)

    # Check actual file size
    file_size_mb <- file.size(file_path) / 1024^2
    cat("- Saved to:", filename, "(", round(file_size_mb, 1), "MB )\n")

    # Clean up
    rm(nc_subset)
    gc()
  }

  cat("\nSplitting complete!\n")

  # Show final summary
  cat("\nFinal file breakdown:\n")
  output_files <- list.files(file.path(base_dir, "Output"),
                             pattern = "ClimateChange_2300_.*\\.rds$",
                             full.names = TRUE)

  file_summary <- data.frame(
    filename = basename(output_files),
    size_mb = sapply(output_files, function(f) file.size(f) / 1024^2)
  ) %>%
    arrange(desc(size_mb))

  print(file_summary %>% mutate(size_mb = round(size_mb, 1)))

  total_split_size <- sum(file_summary$size_mb) / 1024
  cat("\nOriginal file:", round(original_size_gb, 2), "GB\n")
  cat("Total split files:", round(total_split_size, 2), "GB\n")
  cat("Number of files created:", nrow(file_summary), "\n")
  cat("Average file size:", round(mean(file_summary$size_mb), 1), "MB\n")
  cat("Largest file:", round(max(file_summary$size_mb), 1), "MB\n")

  # Save the model-experiment summary for reference
  write_csv(model_exp_summary, file.path(base_dir, "Output", "model_experiment_summary.csv"))
  cat("Model-experiment summary saved to: model_experiment_summary.csv\n")

}, error = function(e) {
  cat("Error loading full file:", e$message, "\n")
  cat("File is too large for available RAM.\n")
  cat("Consider using a machine with more RAM or processing on an HPC cluster.\n")
})

# Clean up
if(exists("nc_2300")) {
  rm(nc_2300)
  gc()
}

cat("\nTo use these files in your main script, you'll need to update the loading logic.\n")
cat("Each model-experiment combination is now in a separate, manageable file.\n")