# Install required packages if not already installed
# install.packages(c("ncdf4", "raster"))

# Load libraries
library(ncdf4)
library(raster)
library(dplyr)

# Open the NetCDF file
nc_file <- nc_open("~/R Projects/ZooMSS_2300/Input/phyc/cesm2-waccm_r1i1p1f1_picontrol_phyc-top_60arcmin_global_annual_1601_2100.nc")

# Print information about the file
print(nc_file)

# Get specific information about the phyc variable
phyc_var <- nc_file$var$phyc  # Adjust variable name if different
print(phyc_var)

# Check the units attribute
phyc_units <- ncatt_get(nc_file, "phyc", "units")
print(paste("Phytoplankton biomass units:", phyc_units$value))

# You can also check other attributes
phyc_longname <- ncatt_get(nc_file, "phyc", "long_name")
print(paste("Long name:", phyc_longname$value))

# Close the NetCDF file
nc_close(nc_file)


# The following section will:
#
# Parse your filenames to extract the model and scenario
# Inspect each file to determine the units of the phyc variable
# Also extract a sample of the data values to help identify potential unit differences
# Summarize the findings by model and unit
# Check if units are consistent across all models
#
# This approach will give us a clear understanding of whether standardization is needed before conversion to chlorophyll. If we find inconsistencies, the value ranges can help us determine appropriate conversion factors.
# Run this code on your files, and let me know the results. Then I can help you develop a strategy for standardizing the units (if needed) and converting to chlorophyll.

# Define path to your folder
folder_path <- "~/R Projects/ZooMSS_2300/Input"

# Function to extract units and metadata for phyc variable
extract_phyc_info <- function(file_path) {
  # Open NetCDF file
  nc <- try(nc_open(file_path), silent = TRUE)

  if(inherits(nc, "try-error")) {
    return(data.frame(
      filename = basename(file_path),
      variable = "Error opening file",
      units = NA,
      long_name = NA,
      standard_name = NA,
      stringsAsFactors = FALSE
    ))
  }

  # Get filename for model and scenario identification
  filename <- basename(file_path)

  # Extract model name and scenario from filename
  # Based on your filenames: model_runid_scenario_variable_resolution_domain_temporal_startyear_endyear.nc
  parts <- strsplit(filename, "_")[[1]]
  model <- parts[1]
  scenario <- parts[3]

  # Check for the phyc variable
  if("phyc" %in% names(nc$var)) {
    var_name <- "phyc"
  } else {
    # If not found, try other possible names
    var_names <- names(nc$var)
    var_name <- var_names[1]  # Default to first variable if phyc not found
  }

  # Get units and other attributes
  units <- tryCatch(
    ncatt_get(nc, var_name, "units")$value,
    error = function(e) "No units attribute"
  )

  long_name <- tryCatch(
    ncatt_get(nc, var_name, "long_name")$value,
    error = function(e) NA
  )

  standard_name <- tryCatch(
    ncatt_get(nc, var_name, "standard_name")$value,
    error = function(e) NA
  )

  # Get a sample of the data to check for min/max values
  # This can help identify unit differences
  data_sample <- try({
    # Get a small sample of data (first time slice if 3D or 4D)
    dims <- nc$var[[var_name]]$dim
    if(length(dims) >= 3) {
      # For 3D+ arrays, get just the first time slice
      start <- rep(1, length(dims))
      count <- sapply(dims, function(d) d$len)
      count[3] <- 1  # Assuming 3rd dimension is time
      sample_data <- ncvar_get(nc, var_name, start=start, count=count)
      c(min=min(sample_data, na.rm=TRUE), max=max(sample_data, na.rm=TRUE))
    } else {
      # For 2D or smaller, get all data
      sample_data <- ncvar_get(nc, var_name)
      c(min=min(sample_data, na.rm=TRUE), max=max(sample_data, na.rm=TRUE))
    }
  }, silent=TRUE)

  if(inherits(data_sample, "try-error")) {
    min_val <- NA
    max_val <- NA
  } else {
    min_val <- data_sample["min"]
    max_val <- data_sample["max"]
  }

  # Close the file
  nc_close(nc)

  return(data.frame(
    filename = filename,
    model = model,
    scenario = scenario,
    variable = var_name,
    units = units,
    long_name = long_name,
    standard_name = standard_name,
    min_value = min_val,
    max_value = max_val,
    stringsAsFactors = FALSE
  ))
}

# Apply function to all files
all_phyc_info <- lapply(nc_files, extract_phyc_info)


all_phyc_info_df <- do.call(rbind, all_phyc_info)

# Print summary by model
model_summary <- all_phyc_info_df %>%
  group_by(model, units) %>%
  summarise(count = n(), .groups = 'drop')

print("Units by model:")
print(model_summary)

# Check if units are consistent
are_units_consistent <- length(unique(all_phyc_info_df$units[!is.na(all_phyc_info_df$units)])) == 1

cat("\nAre phytoplankton carbon units consistent across all models and scenarios? ",
    ifelse(are_units_consistent, "YES", "NO"), "\n")

if(!are_units_consistent) {
  cat("Found the following distinct units:\n")
  unique_units <- unique(all_phyc_info_df$units[!is.na(all_phyc_info_df$units)])
  print(unique_units)

  # Compare value ranges by unit to help understand potential conversion factors
  cat("\nValue ranges by unit:\n")
  all_phyc_info_df %>%
    group_by(model, units) %>%
    summarise(
      min_value = min(min_value, na.rm = TRUE),
      max_value = max(max_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    print()
}


# Path to your folder with phyc files
folder_path <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Input/phyc"

# Define output folder for converted files
output_folder <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Input/chla"

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Get list of all .nc files
nc_files <- list.files(folder_path, pattern = "*.nc$", full.names = TRUE)

# Constants for unit conversion
C_MOLAR_MASS <- 12.01  # g/mol
CONVERSION_FACTOR <- C_MOLAR_MASS * 1000  # Convert mol C to mg C

# Constants for carbon to chlorophyll conversion
LOG_SLOPE <- 0.89
LOG_INTERCEPT <- 1.79

# Function to convert phyc to chlorophyll
convert_phyc_to_chl <- function(file_path, output_folder) {
  # Get the filename for output file
  filename <- basename(file_path)
  output_filename <- gsub("phyc", "chla", filename)
  output_path <- file.path(output_folder, output_filename)

  # Open NetCDF file - don't check for misspellings in dim names
  nc <- nc_open(file_path, readunlim = FALSE, verbose = FALSE, suppress_dimvals = TRUE)

  # Print file info for debugging
  cat("Processing file:", filename, "\n")

  # Find the variable for phytoplankton carbon
  var_names <- names(nc$var)
  phyc_var <- grep("phyc", var_names, value = TRUE)

  if (length(phyc_var) == 0) {
    cat("  No phyc variable found! Available variables:", paste(var_names, collapse=", "), "\n")
    nc_close(nc)
    stop("No phyc variable found in this file")
  }

  var_name <- phyc_var[1]
  cat("  Using variable:", var_name, "\n")

  # Get the dimensions we need
  var_dims <- nc$var[[var_name]]$dim

  # Identify standard dimensions (lon, lat, time)
  dim_names <- sapply(var_dims, function(d) d$name)
  cat("  Dimensions:", paste(dim_names, collapse=", "), "\n")

  # Skip problematic 'bnds' dimensions
  valid_dims <- grep("bnds|bounds", dim_names, invert = TRUE, value = TRUE)

  # Get dimension values for valid dimensions only
  dims <- list()
  for (dim_name in valid_dims) {
    if (dim_name %in% names(nc$dim)) {
      dims[[dim_name]] <- ncvar_get(nc, dim_name)
      cat("  Dimension", dim_name, "size:", length(dims[[dim_name]]), "\n")
    }
  }

  # Get phyc data
  cat("  Reading phyc data...\n")
  phyc_data <- ncvar_get(nc, var_name)
  cat("  Data dimensions:", paste(dim(phyc_data), collapse=" x "), "\n")

  # Report original data stats
  valid_data <- phyc_data[!is.na(phyc_data)]
  if (length(valid_data) > 0) {
    cat("  Original phyc (mol/m³) min:", min(valid_data), "\n")
    cat("  Original phyc (mol/m³) max:", max(valid_data), "\n")

    # Step 1: Convert from mol C/m³ to mg C/m³
    carbon_mg_m3 <- phyc_data * CONVERSION_FACTOR

    cat("  Carbon (mg/m³) min:", min(carbon_mg_m3, na.rm = TRUE), "\n")
    cat("  Carbon (mg/m³) max:", max(carbon_mg_m3, na.rm = TRUE), "\n")

    # Step 2: Apply the inverse relationship from the paper
    # log10(Carbon) = 0.89 × log10(Chlorophyll) + 1.79
    # Therefore: log10(Chlorophyll) = (log10(Carbon) - 1.79) / 0.89

    # Create a copy to preserve dimensions
    chl_data <- carbon_mg_m3 * 0  # Initialize with zeros

    # Find indices of positive values (can take log)
    valid_indices <- which(carbon_mg_m3 > 0)

    if (length(valid_indices) > 0) {
      # Get values at valid indices
      valid_values <- carbon_mg_m3[valid_indices]

      # Convert these values
      log_carbon <- log10(valid_values)
      log_chlorophyll <- (log_carbon - LOG_INTERCEPT) / LOG_SLOPE
      chl_values <- 10^log_chlorophyll

      # Replace values at valid indices
      chl_data[valid_indices] <- chl_values

      # Set invalid/negative values to NA
      invalid_indices <- which(carbon_mg_m3 <= 0)
      if (length(invalid_indices) > 0) {
        chl_data[invalid_indices] <- NA
      }
    }

    # Report conversion stats
    cat("  Chlorophyll (mg/m³) min:", min(chl_data, na.rm = TRUE), "\n")
    cat("  Chlorophyll (mg/m³) max:", max(chl_data, na.rm = TRUE), "\n")

    # Report NA percentage
    na_percent <- sum(is.na(chl_data)) / length(chl_data) * 100
    cat("  Percentage of NA values:", sprintf("%.2f%%", na_percent), "\n")

    # Create dimensions for the new file - only use valid dimensions
    dim_defs <- list()
    for (dim_name in valid_dims) {
      if (dim_name %in% names(nc$dim)) {
        # Get dimension attributes
        dim_vals <- dims[[dim_name]]

        # Get units for this dimension
        units <- "unknown"
        if (dim_name %in% names(nc$var)) {
          units_att <- ncatt_get(nc, dim_name, "units")
          if (!is.null(units_att$hasatt) && units_att$hasatt) {
            units <- units_att$value
          } else {
            # Default units based on dimension name
            units <- switch(dim_name,
                            lon = "degrees_east",
                            lat = "degrees_north",
                            time = "days since 1850-01-01",
                            "unknown")
          }
        }

        dim_defs[[length(dim_defs) + 1]] <- ncdim_def(dim_name, units, dim_vals)
      }
    }

    # Define chlorophyll variable
    chl_var <- ncvar_def("chla", "mg m-3", dim_defs,
                         missval = NA,
                         longname = "Chlorophyll a concentration")

    # Create new NetCDF file
    cat("  Creating output file:", output_path, "\n")
    nc_out <- nc_create(output_path, list(chl_var))

    # Write chlorophyll data
    ncvar_put(nc_out, chl_var, chl_data)

    # Copy global attributes
    global_atts <- ncatt_get(nc, 0)
    for (att_name in names(global_atts)) {
      if (!is.null(global_atts[[att_name]])) {
        ncatt_put(nc_out, 0, att_name, global_atts[[att_name]])
      }
    }

    # Add provenance information
    ncatt_put(nc_out, 0, "source_file", filename)
    ncatt_put(nc_out, 0, "conversion", "Converted from phytoplankton carbon (mol m-3) to chlorophyll a (mg m-3)")
    ncatt_put(nc_out, 0, "conversion_equation", "Chl = 10^((log10(C) - 1.79) / 0.89)")
    ncatt_put(nc_out, 0, "reference", "PLOS ONE https://doi.org/10.1371/journal.pone.0099312")

    # Copy variable attributes from original to new file (for dimensions)
    for (dim_name in valid_dims) {
      if (dim_name %in% names(nc$var)) {
        var_atts <- ncatt_get(nc, dim_name)
        for (att_name in names(var_atts)) {
          if (!is.null(var_atts[[att_name]])) {
            try(ncatt_put(nc_out, dim_name, att_name, var_atts[[att_name]]), silent = TRUE)
          }
        }
      }
    }

    # Close output file
    nc_close(nc_out)

    cat("  Successfully created", output_filename, "\n")
    nc_close(nc)
    return(output_path)
  } else {
    cat("  No valid data found in this file\n")
    nc_close(nc)
    stop("No valid data found")
  }
}

# Process all files
results <- list()
for (i in seq_along(nc_files)) {
  file <- nc_files[i]
  cat("\nFile", i, "of", length(nc_files), "\n")

  result <- try({
    convert_phyc_to_chl(file, output_folder)
  }, silent = FALSE)

  if (inherits(result, "try-error")) {
    cat("Error processing", basename(file), "\n")
    results[[i]] <- NULL
  } else {
    results[[i]] <- result
  }
}

# Summary of conversions
successful <- sum(!sapply(results, is.null))
cat("\nConversion complete!\n")
cat("Successfully converted:", successful, "of", length(nc_files), "files\n")
if (length(nc_files) - successful > 0) {
  cat("Failed conversions:", length(nc_files) - successful, "files\n")
}


# Load necessary libraries
library(tidyr)
library(ggplot2)
library(stringr)

# Path to your chlorophyll files
chla_folder <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/R Projects/ZooMSS_2300/Input/chl"

# Get list of all .nc files
chla_files <- list.files(chla_folder, pattern = "*.nc$", full.names = TRUE)

# Function to extract mean chlorophyll for each time step
extract_mean_chla <- function(file_path) {
  # Extract model and scenario from filename
  filename <- basename(file_path)

  # More robust filename parsing
  parts <- str_split(filename, "_")[[1]]
  model <- parts[1]

  # Find the scenario part - it should be one of these patterns
  scenario_patterns <- c("historical", "picontrol", "ssp126", "ssp534-over", "ssp585")
  scenario_idx <- which(parts %in% scenario_patterns)
  if (length(scenario_idx) == 0) {
    # Default to third element if no match found
    scenario <- parts[3]
  } else {
    scenario <- parts[scenario_idx[1]]
  }

  # Extract year range from filename
  year_pattern <- "([0-9]{4})_([0-9]{4})"
  year_match <- str_match(filename, year_pattern)
  start_year <- as.numeric(year_match[2])
  end_year <- as.numeric(year_match[3])

  cat("Processing file:", filename, "\n")
  cat("Model:", model, "Scenario:", scenario, "\n")
  cat("Year range:", start_year, "-", end_year, "\n")

  # Open NetCDF file
  nc <- nc_open(file_path)

  # Print variables for debugging
  cat("Variables:", paste(names(nc$var), collapse=", "), "\n")

  # Find the chla variable
  var_names <- names(nc$var)
  chla_var <- var_names[grep("chla", var_names, ignore.case = TRUE)]
  if (length(chla_var) == 0) {
    chla_var <- var_names[1]  # Default to first variable if chla not found
  } else {
    chla_var <- chla_var[1]
  }

  cat("Using variable:", chla_var, "\n")

  # Get time dimension
  time_var <- "time"
  if (!(time_var %in% names(nc$dim))) {
    # Try to find a time-like dimension
    time_candidates <- c("t", "tim", "TIME")
    for (cand in time_candidates) {
      if (cand %in% names(nc$dim)) {
        time_var <- cand
        break
      }
    }
  }

  # Get time values
  time_vals <- ncvar_get(nc, time_var)
  time_length <- length(time_vals)

  cat("Time dimension size:", time_length, "\n")

  # Instead of relying on time units, generate years based on start and end years
  if (time_length > 1) {
    years <- seq(from = start_year, to = end_year, length.out = time_length)
  } else {
    # For single time point files
    years <- start_year
  }

  # Get chlorophyll data
  chla_data <- ncvar_get(nc, chla_var)
  cat("Data dimensions:", paste(dim(chla_data), collapse=" x "), "\n")

  # Calculate spatial mean for each time step
  # Determine which dimension is time based on the array shape
  dims <- dim(chla_data)

  if (length(dims) == 3) {
    # Assuming [lon, lat, time] format which is common
    mean_chla <- apply(chla_data, 3, function(x) mean(x, na.rm = TRUE))
  } else if (length(dims) == 4) {
    # For 4D data [lon, lat, depth, time] or similar
    mean_chla <- apply(chla_data, 4, function(x) mean(x, na.rm = TRUE))
  } else {
    # Fallback for other dimension structures
    last_dim <- length(dims)
    mean_chla <- apply(chla_data, last_dim, function(x) mean(x, na.rm = TRUE))
  }

  cat("Calculated", length(mean_chla), "time point means\n")

  # Make sure lengths match
  if (length(mean_chla) != length(years)) {
    warning("Mismatch between time points (", length(mean_chla),
            ") and years (", length(years), "). Adjusting years.")
    years <- seq(from = start_year, to = end_year, length.out = length(mean_chla))
  }

  # Create data frame
  result_df <- data.frame(
    model = model,
    scenario = scenario,
    year = years,
    mean_chla = mean_chla
  )

  cat("Created dataframe with", nrow(result_df), "rows\n")

  # Close the file
  nc_close(nc)

  return(result_df)
}

# Process all files
all_data <- list()

for (i in seq_along(chla_files)) {
  file <- chla_files[i]
  cat("\nProcessing", basename(file), "...\n")
  result <- try(extract_mean_chla(file))
  if (!inherits(result, "try-error")) {
    all_data[[i]] <- result
    cat("Successfully processed", basename(file), "\n")
  } else {
    cat("Error processing", basename(file), ":", result, "\n")
  }
}

# Remove NULL entries
all_data <- all_data[!sapply(all_data, is.null)]

# Check if we have data to plot
if (length(all_data) == 0) {
  stop("No data was successfully processed. Check the errors above.")
}

# Combine all data
all_data_df <- bind_rows(all_data)

# Print data summary
cat("\nData summary:\n")
summary(all_data_df)
cat("\nUnique models:", paste(unique(all_data_df$model), collapse=", "), "\n")
cat("Unique scenarios:", paste(unique(all_data_df$scenario), collapse=", "), "\n")
cat("Year range:", min(all_data_df$year), "-", max(all_data_df$year), "\n")

# Create a more readable scenario label
all_data_df$scenario_label <- factor(all_data_df$scenario,
                                     levels = c("historical", "picontrol", "ssp126", "ssp534-over", "ssp585"),
                                     labels = c("Historical", "PI Control", "SSP1-2.6", "SSP5-3.4-over", "SSP5-8.5"))

# Create nicer model labels
all_data_df$model_label <- factor(all_data_df$model,
                                  levels = c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll"),
                                  labels = c("CESM2-WACCM", "IPSL-CM6A-LR", "UKESM1-0-LL"))

# Create plot by scenario
p1 <- ggplot(all_data_df, aes(x = year, y = mean_chla, color = model_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ scenario_label, scales = "free") +
  theme_bw() +
  labs(
    title = "Mean Global Chlorophyll Concentration Over Time",
    subtitle = "By Earth System Model and Scenario",
    x = "Year",
    y = "Mean Chlorophyll (mg/m³)",
    color = "Earth System Model"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("Figures/global_mean_chlorophyll_by_scenario.png", plot = p1, width = 12, height = 8, dpi = 300)

# Alternative: Create plot by model
p2 <- ggplot(all_data_df, aes(x = year, y = mean_chla, color = scenario_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model_label, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Mean Global Chlorophyll Concentration Over Time",
    subtitle = "By Earth System Model and Scenario",
    x = "Year",
    y = "Mean Chlorophyll (mg/m³)",
    color = "Scenario"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save the alternative plot
ggsave("Figures/global_mean_chlorophyll_by_model.png", plot = p2, width = 12, height = 8, dpi = 300)

# You could also create a combined plot showing all models and scenarios on one plot
p3 <- ggplot(all_data_df, aes(x = year, y = mean_chla, color = interaction(model_label, scenario_label))) +
  geom_line(linewidth = 0.8) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    title = "Mean Global Chlorophyll Concentration Over Time",
    x = "Year",
    y = "Mean Chlorophyll (mg/m³)",
    color = "Model-Scenario"
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8)
  )

# Save the combined plot
ggsave("Figures/global_mean_chlorophyll_all.png", plot = p3, width = 14, height = 8, dpi = 300)

cat("\nPlotting complete! Saved three visualization files.\n")
