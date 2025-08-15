# ESM Year Variable Checker
# Check Year variables across all ESM .rds files

library(dplyr)
library(purrr)

# Set the path to your data folder
data_path <- "~/R Projects/ZooMSS_2300/Output/processed_data"

# Function to extract year info from a single file
check_year_variable <- function(file_path) {
  file_name <- basename(file_path)

  tryCatch({
    # Read the .rds file
    data <- readRDS(file_path)

    # Check if Year variable exists
    if (!"Year" %in% names(data)) {
      return(data.frame(
        file = file_name,
        esm = gsub("_.*", "", file_name),
        scenario = gsub(".*_|\\.rds", "", file_name),
        year_exists = FALSE,
        year_class = NA,
        min_year = NA,
        max_year = NA,
        n_years = NA,
        year_range = NA,
        unique_years = NA,
        has_duplicates = NA,
        error = "Year variable not found"
      ))
    }

    year_data <- data$Year

    # Get year statistics
    year_summary <- data.frame(
      file = file_name,
      esm = gsub("_.*", "", file_name),
      scenario = gsub(".*_|\\.rds", "", file_name),
      year_exists = TRUE,
      year_class = class(year_data)[1],
      min_year = min(year_data, na.rm = TRUE),
      max_year = max(year_data, na.rm = TRUE),
      n_years = length(year_data),
      year_range = paste(min(year_data, na.rm = TRUE), "-", max(year_data, na.rm = TRUE)),
      unique_years = length(unique(year_data)),
      has_duplicates = length(year_data) != length(unique(year_data)),
      error = NA
    )

    return(year_summary)

  }, error = function(e) {
    return(data.frame(
      file = file_name,
      esm = gsub("_.*", "", file_name),
      scenario = gsub(".*_|\\.rds", "", file_name),
      year_exists = NA,
      year_class = NA,
      min_year = NA,
      max_year = NA,
      n_years = NA,
      year_range = NA,
      unique_years = NA,
      has_duplicates = NA,
      error = as.character(e)
    ))
  })
}

# Get all .rds files in the directory
rds_files <- list.files(data_path, pattern = "\\.rds$", full.names = TRUE)

print(paste("Found", length(rds_files), ".rds files"))
print("Files found:")
print(basename(rds_files))

# Check Year variable for all files
year_summary <- map_dfr(rds_files, check_year_variable)

# Display results
print("\n=== YEAR VARIABLE SUMMARY ===")
print(year_summary)

# Summary by scenario
print("\n=== SUMMARY BY SCENARIO ===")
scenario_summary <- year_summary %>%
  filter(year_exists == TRUE) %>%
  group_by(scenario, esm) %>%
  summarise(
    n_files = n(),
    min_year_across_files = min(min_year, na.rm = TRUE),
    max_year_across_files = max(max_year, na.rm = TRUE),
    year_classes = paste(unique(year_class), collapse = ", "),
    .groups = 'drop'
  )

print(scenario_summary)

# Check for potential issues
print("\n=== POTENTIAL ISSUES ===")

# Files with errors
error_files <- year_summary %>% filter(!is.na(error))
if (nrow(error_files) > 0) {
  print("Files with errors:")
  print(error_files[, c("file", "error")])
} else {
  print("No files with errors")
}

# Files with duplicated years
dup_files <- year_summary %>% filter(has_duplicates == TRUE)
if (nrow(dup_files) > 0) {
  print("\nFiles with duplicate years:")
  print(dup_files[, c("file", "scenario", "n_years", "unique_years")])
} else {
  print("No files with duplicate years")
}

# Check for unexpected year classes
year_summary %>%
  filter(year_exists == TRUE) %>%
  count(year_class) %>%
  print()

# Detailed year ranges by scenario
print("\n=== DETAILED YEAR RANGES BY SCENARIO ===")
year_summary %>%
  filter(year_exists == TRUE) %>%
  arrange(scenario, esm) %>%
  select(file, scenario, year_range, n_years) %>%
  print()

# Function to examine specific files in more detail if needed
examine_year_details <- function(file_name) {
  file_path <- file.path(data_path, file_name)
  data <- readRDS(file_path)

  cat("\n=== DETAILED EXAMINATION:", file_name, "===\n")
  cat("Year variable class:", class(data$Year), "\n")
  cat("First 10 years:", head(data$Year, 10), "\n")
  cat("Last 10 years:", tail(data$Year, 10), "\n")
  cat("Summary statistics:\n")
  print(summary(data$Year))

  # Check for any non-standard values
  if (any(is.na(data$Year))) {
    cat("WARNING: Contains NA values in Year\n")
  }

  # Check year sequence
  year_diff <- diff(data$Year)
  if (length(unique(year_diff)) == 1 && unique(year_diff) == 1) {
    cat("Year sequence: Consecutive annual data\n")
  } else {
    cat("Year sequence: Non-consecutive or irregular\n")
    cat("Year differences:", unique(year_diff), "\n")
  }
}

# Example usage (uncomment to examine specific files):
examine_year_details("ukesm1-0-ll_ssp585.rds")
examine_year_details("ipsl-cm6a-lr_ssp534-over.rds")
examine_year_details("cesm2-waccm_picontrol.rds")