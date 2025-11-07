# ==============================================================================
# ZooMSS_2300: Convert Phytoplankton Carbon (PHYC) to Chlorophyll (CHL)
# ==============================================================================
# 
# Purpose: Convert phytoplankton carbon (mol m-3) to chlorophyll a (mg m-3)
#          using the inverse relationship from Sathyendranath et al. (2009)
#
# Input:  Input/raw/phyc/*.nc
# Output: Input/converted/chl/*.nc
#
# ==============================================================================

# Load libraries
library(ncdf4)
library(raster)
library(dplyr)

cat("==============================================================================\n")
cat("PHYC to Chlorophyll Conversion\n")
cat("==============================================================================\n\n")

# Path to your folder with phyc files
folder_path <- "Input/raw/phyc"

# Define output folder for converted files
output_folder <- "Input/converted/chl"

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("Created output directory:", output_folder, "\n")
}

# Get list of all .nc files
nc_files <- list.files(folder_path, pattern = "*.nc$", full.names = TRUE)

cat("Found", length(nc_files), "phyc files to convert\n\n")

# Constants for unit conversion
C_MOLAR_MASS <- 12.01  # g/mol
CONVERSION_FACTOR <- C_MOLAR_MASS * 1000  # Convert mol C to mg C

# Constants for carbon to chlorophyll conversion
# From Sathyendranath et al. (2009): log10(Carbon) = 0.89 × log10(Chlorophyll) + 1.79
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
    ncatt_put(nc_out, 0, "reference", "Sathyendranath et al. (2009) PLOS ONE https://doi.org/10.1371/journal.pone.0099312")
    ncatt_put(nc_out, 0, "processing_date", as.character(Sys.Date()))

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

    cat("  ✓ Successfully created", output_filename, "\n\n")
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
  cat("File", i, "of", length(nc_files), "\n")

  result <- try({
    convert_phyc_to_chl(file, output_folder)
  }, silent = FALSE)

  if (inherits(result, "try-error")) {
    cat("Error processing", basename(file), "\n\n")
    results[[i]] <- NULL
  } else {
    results[[i]] <- result
  }
}

# Summary of conversions
successful <- sum(!sapply(results, is.null))
cat("==============================================================================\n")
cat("Conversion Complete!\n")
cat("==============================================================================\n")
cat("Successfully converted:", successful, "of", length(nc_files), "files\n")
if (length(nc_files) - successful > 0) {
  cat("Failed conversions:", length(nc_files) - successful, "files\n")
}
cat("\nOutput files saved to:", output_folder, "\n")
cat("==============================================================================\n")
