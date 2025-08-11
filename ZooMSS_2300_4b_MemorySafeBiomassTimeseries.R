# ==============================================================================
# MEMORY-SAFE BIOMASS TIME SERIES PLOTTING
# ==============================================================================
# Purpose: Create time series plots from large biomass projection files
#          using memory-efficient strategies
# ==============================================================================

library(tidyverse)
library(lubridate)

# Set directories
base_dir <- "~/R Projects/ZooMSS_2300/"
output_dir <- paste0(base_dir, "Output/Biomass_projections/")
figure_dir <- paste0(base_dir, "Figures/Biomass_Timeseries/")

# Create figures directory if it doesn't exist
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== MEMORY-SAFE BIOMASS TIME SERIES ANALYSIS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: FILE SIZE ANALYSIS AND PRIORITIZATION
# ==============================================================================

cat("STEP 1: Analyzing file sizes...\n")

# Get file information
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)
file_info <- data.frame(
  filename = basename(biomass_files),
  filepath = biomass_files,
  size_mb = sapply(biomass_files, function(f) round(file.size(f) / 1024^2, 1))
) %>%
  arrange(size_mb)

cat("Files found:", nrow(file_info), "\n")
cat("Size range:", min(file_info$size_mb), "to", max(file_info$size_mb), "MB\n")

# Parse filename components
file_info <- file_info %>%
  extract(filename, into = c("model", "scenario"), 
          regex = "withZooMSS_([^_]+)_([^_]+)_Control", remove = FALSE) %>%
  mutate(
    size_category = case_when(
      size_mb < 2000 ~ "small",
      size_mb < 4000 ~ "medium", 
      size_mb >= 4000 ~ "large"
    )
  )

print(file_info %>% select(filename, model, scenario, size_mb, size_category))

# ==============================================================================
# STEP 2: MEMORY-EFFICIENT DATA STRUCTURE ANALYSIS
# ==============================================================================

cat("\nSTEP 2: Analyzing data structure (using smallest file)...\n")

# Start with the smallest file to understand structure
smallest_file <- file_info$filepath[1]
cat("Examining:", file_info$filename[1], "(", file_info$size_mb[1], "MB )\n")

# Function to safely peek at file structure
peek_file_structure <- function(filepath, max_rows = 1000) {
  
  cat("Loading sample data...\n")
  
  tryCatch({
    # Load data 
    data <- readRDS(filepath)
    
    # Basic info
    structure_info <- list(
      total_rows = nrow(data),
      total_cols = ncol(data),
      column_names = names(data),
      data_classes = sapply(data, class),
      memory_estimate_mb = round(object.size(data) / 1024^2, 1)
    )
    
    # Sample the data if it's large
    if(nrow(data) > max_rows) {
      cat("Sampling", max_rows, "rows for analysis...\n")
      data_sample <- data %>% 
        slice_sample(n = max_rows)
    } else {
      data_sample <- data
    }
    
    # Analyze key columns
    if("Year" %in% names(data_sample)) {
      year_range <- range(data_sample$Year, na.rm = TRUE)
      structure_info$year_range <- year_range
      structure_info$n_years <- diff(year_range) + 1
    }
    
    if("Date" %in% names(data_sample)) {
      date_range <- range(data_sample$Date, na.rm = TRUE)
      structure_info$date_range <- date_range
    }
    
    # Identify biomass columns (exclude coordinate and metadata columns)
    exclude_cols <- c("Lon", "Lat", "Date", "Year", "SST", "chlo", "Chl", "Chl_log10", 
                      "Model", "Experiment", "cellID", "sst", "phy", "pico_biom", 
                      "nano_biom", "micro_biom", "phyto_slope", "phyto_int", "phyto_max")
    
    biomass_cols <- setdiff(names(data_sample), exclude_cols)
    structure_info$biomass_columns <- biomass_cols
    structure_info$n_biomass_cols <- length(biomass_cols)
    
    # Quick summary statistics for biomass columns
    if(length(biomass_cols) > 0) {
      biomass_summary <- data_sample %>%
        select(all_of(biomass_cols)) %>%
        summarise_all(list(mean = ~mean(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE),
                          min = ~min(., na.rm = TRUE),
                          max = ~max(., na.rm = TRUE))) %>%
        pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
        separate(stat, into = c("species", "statistic"), sep = "_(?=mean|median|min|max)") %>%
        pivot_wider(names_from = statistic, values_from = value)
      
      structure_info$biomass_summary <- biomass_summary
    }
    
    # Clean up
    rm(data, data_sample)
    gc()
    
    return(structure_info)
    
  }, error = function(e) {
    cat("Error analyzing file:", e$message, "\n")
    return(NULL)
  })
}

# Analyze structure
structure_info <- peek_file_structure(smallest_file)

if(!is.null(structure_info)) {
  cat("Data structure analysis:\n")
  cat("- Total rows:", structure_info$total_rows, "\n")
  cat("- Total columns:", structure_info$total_cols, "\n")
  cat("- Memory estimate:", structure_info$memory_estimate_mb, "MB\n")
  cat("- Biomass columns:", structure_info$n_biomass_cols, "\n")
  
  if(!is.null(structure_info$year_range)) {
    cat("- Year range:", structure_info$year_range[1], "to", structure_info$year_range[2], "\n")
    cat("- Number of years:", structure_info$n_years, "\n")
  }
  
  cat("- Biomass species:", paste(head(structure_info$biomass_columns, 5), collapse = ", "), "...\n")
}

# ==============================================================================
# STEP 3: MEMORY-EFFICIENT TIME SERIES EXTRACTION FUNCTION
# ==============================================================================

cat("\nSTEP 3: Creating memory-efficient time series extraction function...\n")

# Function to extract global annual means without loading full dataset
extract_global_annual_means <- function(filepath, spatial_sample_fraction = 0.1) {
  
  filename <- basename(filepath)
  cat("Processing:", filename, "\n")
  
  tryCatch({
    # Load data
    data <- readRDS(filepath)
    
    cat("  Original data size:", nrow(data), "rows,", ncol(data), "cols\n")
    
    # Sample spatially to reduce memory usage
    if(spatial_sample_fraction < 1.0) {
      n_sample <- round(nrow(data) * spatial_sample_fraction)
      cat("  Sampling", n_sample, "spatial points\n")
      data <- data %>% slice_sample(n = n_sample)
    }
    
    # Identify columns
    exclude_cols <- c("Lon", "Lat", "Date", "SST", "chlo", "Chl", "Chl_log10", 
                      "Model", "Experiment", "cellID", "sst", "phy", "pico_biom", 
                      "nano_biom", "micro_biom", "phyto_slope", "phyto_int", "phyto_max")
    
    biomass_cols <- setdiff(names(data), exclude_cols)
    
    # Determine year column
    year_col <- if("Year" %in% names(data)) "Year" else if("Date" %in% names(data)) "Date" else NULL
    
    if(is.null(year_col)) {
      stop("No year/date column found")
    }
    
    # Calculate annual global means
    annual_means <- data %>%
      select(all_of(c(year_col, biomass_cols))) %>%
      rename(year = all_of(year_col)) %>%
      group_by(year) %>%
      summarise_all(~mean(., na.rm = TRUE), .groups = 'drop') %>%
      # Add metadata
      mutate(
        filename = filename,
        model = str_extract(filename, "(?<=withZooMSS_)[^_]+"),
        scenario = str_extract(filename, "(?<=_)[^_]+(?=_Control)")
      ) %>%
      # Convert to long format for plotting
      pivot_longer(cols = all_of(biomass_cols), 
                   names_to = "species", 
                   values_to = "biomass")
    
    cat("  Extracted", nrow(annual_means), "time series points\n")
    
    # Clean up
    rm(data)
    gc()
    
    return(annual_means)
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ==============================================================================
# STEP 4: PROCESS FILES BY SIZE CATEGORY  
# ==============================================================================

cat("\nSTEP 4: Processing files by size category...\n")

# Function to process files in a category
process_file_category <- function(file_subset, category_name, sample_fraction = 0.1) {
  
  cat("Processing", category_name, "files (", nrow(file_subset), "files )...\n")
  
  all_timeseries <- list()
  
  for(i in 1:nrow(file_subset)) {
    filepath <- file_subset$filepath[i]
    
    cat(sprintf("  [%d/%d] ", i, nrow(file_subset)))
    
    timeseries <- extract_global_annual_means(filepath, sample_fraction)
    
    if(!is.null(timeseries)) {
      all_timeseries[[i]] <- timeseries
    }
    
    # Force garbage collection between files
    gc()
  }
  
  # Combine all timeseries
  if(length(all_timeseries) > 0) {
    combined_timeseries <- bind_rows(all_timeseries)
    cat("  Combined", nrow(combined_timeseries), "total time series points\n")
    return(combined_timeseries)
  } else {
    cat("  No valid timeseries extracted\n")
    return(NULL)
  }
}

# Process files by category (start with small files)
small_files <- file_info %>% filter(size_category == "small")
medium_files <- file_info %>% filter(size_category == "medium") 
large_files <- file_info %>% filter(size_category == "large")

cat("File categories:\n")
cat("- Small files:", nrow(small_files), "\n")
cat("- Medium files:", nrow(medium_files), "\n")  
cat("- Large files:", nrow(large_files), "\n")

# Start with small files for testing
if(nrow(small_files) > 0) {
  small_timeseries <- process_file_category(small_files, "small", sample_fraction = 0.2)
  
  # Save intermediate results
  if(!is.null(small_timeseries)) {
    saveRDS(small_timeseries, paste0(base_dir, "Output/timeseries_small_files.rds"))
    cat("Saved small files timeseries\n")
  }
}

cat("\nSTEP 4 complete. Ready to proceed with medium and large files if needed.\n")
cat("Check the small files results before processing larger files.\n")

# ==============================================================================
# STEP 5: CREATE SAMPLE PLOTS
# ==============================================================================

if(exists("small_timeseries") && !is.null(small_timeseries)) {
  
  cat("\nSTEP 5: Creating sample plots from small files...\n")
  
  # Plot 1: Total biomass by scenario
  p1 <- small_timeseries %>%
    group_by(year, scenario, model) %>%
    summarise(total_biomass = sum(biomass, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = year, y = total_biomass, color = scenario)) +
    geom_line() +
    facet_wrap(~model, scales = "free") +
    labs(
      title = "Total Zooplankton Biomass Time Series (Small Files Sample)",
      x = "Year",
      y = "Total Biomass (g/m²)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(paste0(figure_dir, "total_biomass_timeseries_sample.png"), 
         p1, width = 12, height = 8, dpi = 300)
  
  # Plot 2: Individual species trends
  top_species <- small_timeseries %>%
    group_by(species) %>%
    summarise(mean_biomass = mean(biomass, na.rm = TRUE), .groups = 'drop') %>%
    top_n(6, mean_biomass) %>%
    pull(species)
  
  p2 <- small_timeseries %>%
    filter(species %in% top_species) %>%
    ggplot(aes(x = year, y = biomass, color = scenario)) +
    geom_line() +
    facet_grid(species ~ model, scales = "free_y") +
    labs(
      title = "Top Species Biomass Time Series (Small Files Sample)",
      x = "Year", 
      y = "Biomass (g/m²)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(paste0(figure_dir, "species_biomass_timeseries_sample.png"),
         p2, width = 14, height = 10, dpi = 300)
  
  cat("Sample plots saved to:", figure_dir, "\n")
  
  # Summary statistics
  cat("\nSample data summary:\n")
  summary_stats <- small_timeseries %>%
    group_by(model, scenario) %>%
    summarise(
      n_years = n_distinct(year),
      n_species = n_distinct(species),
      total_points = n(),
      .groups = 'drop'
    )
  
  print(summary_stats)
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Next steps:\n")
cat("1. Review sample plots in:", figure_dir, "\n")
cat("2. If satisfied, process medium files with: process_file_category(medium_files, 'medium', 0.05)\n")
cat("3. For large files, use even smaller sample: process_file_category(large_files, 'large', 0.01)\n")
cat("4. Combine all results for comprehensive analysis\n")
