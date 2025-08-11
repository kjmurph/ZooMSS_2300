# ==============================================================================
# CORRECTED BIOMASS TIME SERIES PLOTTING
# ==============================================================================
# Purpose: Create time series plots from biomass projection files
#          using the correct column structure
# ==============================================================================

library(tidyverse)
library(scales)

# Set directories
output_dir <- "Output/Biomass_projections/"
figure_dir <- "Figures/Biomass_Timeseries/"

# Create figures directory if it doesn't exist
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== BIOMASS TIME SERIES PLOTTING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Define biomass columns (species groups)
biomass_columns <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                     "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", 
                     "Jellyfish", "Fish_Small", "Fish_Med", "Fish_Large")

cat("Biomass species to analyze:", paste(biomass_columns, collapse = ", "), "\n")

# ==============================================================================
# MEMORY-EFFICIENT TIME SERIES EXTRACTION FUNCTION
# ==============================================================================

extract_annual_timeseries <- function(filepath, spatial_sample_fraction = 0.1) {
  
  filename <- basename(filepath)
  cat("Processing:", filename, "\n")
  
  tryCatch({
    # Load data
    data <- readRDS(filepath)
    
    cat("  Original data size:", nrow(data), "rows\n")
    
    # Sample spatially to reduce memory usage
    if(spatial_sample_fraction < 1.0) {
      n_sample <- round(nrow(data) * spatial_sample_fraction)
      cat("  Sampling", n_sample, "spatial points\n")
      data <- data %>% slice_sample(n = n_sample)
    }
    
    # Extract metadata from filename
    model <- str_extract(filename, "(?<=withZooMSS_)[^_]+")
    scenario <- str_extract(filename, "(?<=_)[^_]+(?=_Control)")
    
    # Calculate annual global means
    annual_means <- data %>%
      select(Year, all_of(biomass_columns)) %>%
      group_by(Year) %>%
      summarise(across(all_of(biomass_columns), ~mean(., na.rm = TRUE)), .groups = 'drop') %>%
      # Add metadata
      mutate(
        filename = filename,
        model = model,
        scenario = scenario
      ) %>%
      # Convert to long format for plotting
      pivot_longer(cols = all_of(biomass_columns), 
                   names_to = "species", 
                   values_to = "biomass")
    
    cat("  Extracted", nrow(annual_means), "time series points\n")
    cat("  Year range:", min(annual_means$Year), "to", max(annual_means$Year), "\n")
    
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
# PROCESS FILES BY SIZE
# ==============================================================================

# Get all biomass files
biomass_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)

# Get file sizes for prioritization
file_info <- data.frame(
  filepath = biomass_files,
  filename = basename(biomass_files),
  size_mb = sapply(biomass_files, function(f) round(file.size(f) / 1024^2, 1))
) %>%
  arrange(size_mb) %>%
  mutate(
    model = str_extract(filename, "(?<=withZooMSS_)[^_]+"),
    scenario = str_extract(filename, "(?<=_)[^_]+(?=_Control)"),
    size_category = case_when(
      size_mb < 2000 ~ "small",
      size_mb < 4000 ~ "medium", 
      size_mb >= 4000 ~ "large"
    )
  )

cat("Files to process:", nrow(file_info), "\n")
print(file_info %>% select(filename, model, scenario, size_mb, size_category))

# ==============================================================================
# PROCESS FILES PROGRESSIVELY
# ==============================================================================

cat("\nProcessing files progressively by size...\n")

all_timeseries <- list()

# Start with smaller files first
for(i in 1:nrow(file_info)) {
  
  filepath <- file_info$filepath[i]
  size_mb <- file_info$size_mb[i]
  category <- file_info$size_category[i]
  
  # Adjust sampling based on file size
  sample_fraction <- case_when(
    category == "small" ~ 0.2,
    category == "medium" ~ 0.05,
    category == "large" ~ 0.01
  )
  
  cat(sprintf("\n[%d/%d] Processing %s file (%.1f MB)\n", i, nrow(file_info), category, size_mb))
  
  timeseries <- extract_annual_timeseries(filepath, sample_fraction)
  
  if(!is.null(timeseries)) {
    all_timeseries[[i]] <- timeseries
  }
  
  # Force garbage collection and memory check
  gc()
  
  # Stop if memory becomes an issue (this is a safety check)
  if(i %% 3 == 0) {
    cat("  Memory check - processed", i, "files\n")
    
    # Save intermediate results
    if(length(all_timeseries) > 0) {
      intermediate_data <- bind_rows(all_timeseries)
      saveRDS(intermediate_data, paste0("Output/intermediate_timeseries_", i, "_files.rds"))
      cat("  Saved intermediate results\n")
    }
  }
}

# ==============================================================================
# COMBINE AND SAVE RESULTS
# ==============================================================================

if(length(all_timeseries) > 0) {
  
  cat("\nCombining all time series...\n")
  combined_timeseries <- bind_rows(all_timeseries)
  
  cat("Total time series points:", nrow(combined_timeseries), "\n")
  cat("Models:", paste(unique(combined_timeseries$model), collapse = ", "), "\n")
  cat("Scenarios:", paste(unique(combined_timeseries$scenario), collapse = ", "), "\n")
  cat("Year range:", min(combined_timeseries$Year), "to", max(combined_timeseries$Year), "\n")
  
  # Save combined results
  saveRDS(combined_timeseries, "Output/combined_biomass_timeseries.rds")
  cat("Saved combined time series data\n")
  
  # ==============================================================================
  # CREATE TIME SERIES PLOTS
  # ==============================================================================
  
  cat("\nCreating time series plots...\n")
  
  # Plot 1: Total biomass by scenario and model
  total_biomass <- combined_timeseries %>%
    group_by(Year, scenario, model) %>%
    summarise(total_biomass = sum(biomass, na.rm = TRUE), .groups = 'drop')
  
  p1 <- total_biomass %>%
    ggplot(aes(x = Year, y = total_biomass, color = scenario)) +
    geom_line(size = 0.8) +
    facet_wrap(~model, scales = "free_y") +
    labs(
      title = "Total Zooplankton Biomass Projections (2300)",
      subtitle = "Global annual means by ESM and scenario",
      x = "Year",
      y = "Total Biomass (g/m²)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    scale_y_continuous(labels = scientific_notation)
  
  ggsave(paste0(figure_dir, "total_biomass_by_model_scenario.png"), 
         p1, width = 14, height = 10, dpi = 300)
  
  # Plot 2: Species-specific trends for major groups
  major_species <- combined_timeseries %>%
    group_by(species) %>%
    summarise(mean_biomass = mean(biomass, na.rm = TRUE), .groups = 'drop') %>%
    top_n(8, mean_biomass) %>%
    pull(species)
  
  p2 <- combined_timeseries %>%
    filter(species %in% major_species) %>%
    ggplot(aes(x = Year, y = biomass, color = scenario)) +
    geom_line(size = 0.6) +
    facet_grid(species ~ model, scales = "free_y") +
    labs(
      title = "Major Species Biomass Projections (2300)",
      subtitle = "Top 8 species by mean biomass",
      x = "Year", 
      y = "Biomass (g/m²)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    scale_y_continuous(labels = scientific_notation)
  
  ggsave(paste0(figure_dir, "major_species_biomass_trends.png"),
         p2, width = 16, height = 12, dpi = 300)
  
  # Plot 3: Scenario comparison (relative to historical mean)
  # Calculate historical baseline (1850-2014 mean)
  historical_baseline <- combined_timeseries %>%
    filter(scenario == "historical", Year >= 1850, Year <= 2014) %>%
    group_by(model, species) %>%
    summarise(baseline_biomass = mean(biomass, na.rm = TRUE), .groups = 'drop')
  
  relative_change <- combined_timeseries %>%
    left_join(historical_baseline, by = c("model", "species")) %>%
    mutate(relative_biomass = biomass / baseline_biomass) %>%
    filter(!is.na(baseline_biomass), baseline_biomass > 0)
  
  p3 <- relative_change %>%
    group_by(Year, scenario, model) %>%
    summarise(total_relative = sum(relative_biomass, na.rm = TRUE), .groups = 'drop') %>%
    filter(scenario != "historical") %>%
    ggplot(aes(x = Year, y = total_relative, color = scenario)) +
    geom_line(size = 0.8) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    facet_wrap(~model) +
    labs(
      title = "Biomass Change Relative to Historical Baseline",
      subtitle = "Total biomass relative to 1850-2014 mean",
      x = "Year",
      y = "Relative Biomass (Historical = 1)",
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  
  ggsave(paste0(figure_dir, "biomass_relative_change.png"),
         p3, width = 14, height = 10, dpi = 300)
  
  # Plot 4: Species composition over time
  composition_data <- combined_timeseries %>%
    group_by(Year, scenario, model) %>%
    mutate(
      total_year_biomass = sum(biomass, na.rm = TRUE),
      proportion = biomass / total_year_biomass
    ) %>%
    ungroup()
  
  p4 <- composition_data %>%
    filter(Year %in% c(1900, 2000, 2100, 2200, 2300)) %>%
    ggplot(aes(x = factor(Year), y = proportion, fill = species)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(scenario ~ model) +
    labs(
      title = "Species Composition Changes Over Time",
      subtitle = "Proportion of total biomass by species",
      x = "Year",
      y = "Proportion of Total Biomass",
      fill = "Species"
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    scale_fill_viridis_d()
  
  ggsave(paste0(figure_dir, "species_composition_timeline.png"),
         p4, width = 16, height = 12, dpi = 300)
  
  cat("All plots saved to:", figure_dir, "\n")
  
  # ==============================================================================
  # SUMMARY STATISTICS
  # ==============================================================================
  
  cat("\nSummary statistics:\n")
  
  summary_stats <- combined_timeseries %>%
    group_by(model, scenario) %>%
    summarise(
      n_years = n_distinct(Year),
      n_species = n_distinct(species),
      total_points = n(),
      mean_total_biomass = mean(biomass, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print(summary_stats)
  
  # Save summary table
  write_csv(summary_stats, paste0(figure_dir, "biomass_summary_statistics.csv"))
  
} else {
  cat("No valid time series data extracted!\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check the plots in:", figure_dir, "\n")
