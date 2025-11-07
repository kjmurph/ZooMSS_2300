# ==============================================================================
# ZooMSS_2300 Pipeline: Script 0 - Preprocess UKESM Monthly Data
# ==============================================================================
# 
# Purpose: Process UKESM SSP5-3.4-overshoot 2101-2300 monthly data:
#   1. Extract surface layer only from depth-structured phyc data
#   2. Calculate annual means for both phyc and tos
#   3. Validate structure matches other raw inputs
#   4. Save processed files to raw/phyc and raw/tos folders
#
# Input:  Input/raw/UKESM_monthly/phyc/*.nc (monthly, depth-structured)
#         Input/raw/UKESM_monthly/tos/*.nc (monthly)
# Output: Input/raw/phyc/*.nc (annual, surface only)
#         Input/raw/tos/*.nc (annual)
#
# Author: [Your name]
# Date: 2025-11-07
# ==============================================================================

# Load required libraries
library(ncdf4)
library(raster)
library(dplyr)

# Define paths
input_folder_phyc <- "Input/raw/UKESM_monthly/phyc"
input_folder_tos <- "Input/raw/UKESM_monthly/tos"
output_folder_phyc <- "Input/raw/phyc"
output_folder_tos <- "Input/raw/tos"

# Expected filename patterns
phyc_file <- "ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc_60arcmin_global_monthly_2101_2300.nc"
tos_file <- "ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_monthly_2101_2300.nc"

# Full paths
phyc_input_path <- file.path(input_folder_phyc, phyc_file)
tos_input_path <- file.path(input_folder_tos, tos_file)

cat("==============================================================================\n")
cat("UKESM SSP5-3.4-overshoot 2101-2300 Monthly Data Preprocessing\n")
cat("==============================================================================\n\n")

# ==============================================================================
# Part 1: Process PHYC - Extract Surface Layer and Calculate Annual Means
# ==============================================================================

cat("Part 1: Processing PHYC data\n")
cat("----------------------------------------------------------------------\n")

# Check if input file exists
if (!file.exists(phyc_input_path)) {
  stop("PHYC input file not found: ", phyc_input_path)
}

cat("Opening file:", phyc_file, "\n")

# Open NetCDF file
nc_phyc <- nc_open(phyc_input_path)

# Print file structure for inspection
cat("\n=== File Structure ===\n")
print(nc_phyc)

# Identify dimensions
cat("\n=== Dimensions ===\n")
dim_names <- names(nc_phyc$dim)
cat("Available dimensions:", paste(dim_names, collapse=", "), "\n")

# Get dimension information
lon_dim <- nc_phyc$dim$lon
lat_dim <- nc_phyc$dim$lat
time_dim <- nc_phyc$dim$time

cat("Longitude:", lon_dim$len, "points\n")
cat("Latitude:", lat_dim$len, "points\n")
cat("Time:", time_dim$len, "points\n")

# Check for depth dimension
depth_dim_names <- c("lev", "depth", "z", "olevel")
depth_dim_name <- NULL
for (dn in depth_dim_names) {
  if (dn %in% dim_names) {
    depth_dim_name <- dn
    break
  }
}

if (is.null(depth_dim_name)) {
  stop("Could not find depth dimension. Available dimensions: ", paste(dim_names, collapse=", "))
}

depth_dim <- nc_phyc$dim[[depth_dim_name]]
cat("Depth dimension name:", depth_dim_name, "\n")
cat("Depth levels:", depth_dim$len, "\n")

# Get depth values to confirm surface layer
depth_vals <- ncvar_get(nc_phyc, depth_dim_name)
cat("Depth values (first 5):", paste(head(depth_vals, 5), collapse=", "), "\n")
cat("Surface layer index: 1 (depth =", depth_vals[1], ")\n")

# Get variable name
var_names <- names(nc_phyc$var)
phyc_var_name <- var_names[grep("phyc", var_names, ignore.case = TRUE)]
if (length(phyc_var_name) == 0) {
  stop("Could not find phyc variable in file")
}
phyc_var_name <- phyc_var_name[1]
cat("PHYC variable name:", phyc_var_name, "\n")

# Get dimensions of phyc variable
phyc_var <- nc_phyc$var[[phyc_var_name]]
phyc_dims <- sapply(phyc_var$dim, function(d) d$name)
cat("PHYC dimensions:", paste(phyc_dims, collapse=" x "), "\n")

# Get lon, lat, time values
lon_vals <- ncvar_get(nc_phyc, "lon")
lat_vals <- ncvar_get(nc_phyc, "lat")
time_vals <- ncvar_get(nc_phyc, "time")

# Determine position of depth dimension in array
depth_pos <- which(phyc_dims == depth_dim_name)
cat("Depth dimension position:", depth_pos, "\n")

# Determine array structure
cat("\nData array structure:\n")
for (i in seq_along(phyc_dims)) {
  cat("  Dimension", i, ":", phyc_dims[i], "\n")
}

# Calculate number of years and months
n_months <- length(time_vals)
n_years <- n_months / 12
cat("\nTotal months:", n_months, "\n")
cat("Total years:", n_years, "\n")

if (n_years != round(n_years)) {
  warning("Number of months (", n_months, ") is not divisible by 12")
}

# Extract surface layer only (first depth level)
cat("\nExtracting surface layer...\n")

# Build the start and count vectors for ncvar_get
start_vec <- rep(1, length(phyc_dims))
count_vec <- sapply(phyc_var$dim, function(d) d$len)
count_vec[depth_pos] <- 1  # Extract only first depth level

cat("Reading data with start:", paste(start_vec, collapse=", "), "\n")
cat("             and count:", paste(count_vec, collapse=", "), "\n")

# Read surface layer data
phyc_surface <- ncvar_get(nc_phyc, phyc_var_name, start = start_vec, count = count_vec)

# Remove depth dimension (which now has length 1)
phyc_surface <- drop(phyc_surface)

cat("Surface data dimensions:", paste(dim(phyc_surface), collapse=" x "), "\n")
cat("Surface data range: [", min(phyc_surface, na.rm=TRUE), ",", 
    max(phyc_surface, na.rm=TRUE), "]\n")

# Calculate annual means
cat("\nCalculating annual means...\n")

# Reshape to [lon, lat, month, year] then average over months
# Assuming dimensions are [lon, lat, time]
dim_phyc <- dim(phyc_surface)
n_lon <- dim_phyc[1]
n_lat <- dim_phyc[2]
n_months_total <- dim_phyc[3]
n_years_calc <- floor(n_months_total / 12)

# Trim to complete years only
n_months_use <- n_years_calc * 12
if (n_months_total > n_months_use) {
  cat("Note: Trimming incomplete year (using", n_months_use, "of", n_months_total, "months)\n")
  phyc_surface <- phyc_surface[, , 1:n_months_use]
  time_vals <- time_vals[1:n_months_use]
}

# Reshape to [lon, lat, 12, year]
phyc_reshaped <- array(phyc_surface, dim = c(n_lon, n_lat, 12, n_years_calc))

# Calculate mean over months (dimension 3)
phyc_annual <- apply(phyc_reshaped, c(1, 2, 4), mean, na.rm = TRUE)

cat("Annual mean data dimensions:", paste(dim(phyc_annual), collapse=" x "), "\n")
cat("Annual mean data range: [", min(phyc_annual, na.rm=TRUE), ",", 
    max(phyc_annual, na.rm=TRUE), "]\n")

# Create time values for annual data (use middle of each year)
# Original time is in days since some reference
time_units <- ncatt_get(nc_phyc, "time", "units")$value
cat("Original time units:", time_units, "\n")

# Calculate annual time values (use January 1st of each year)
# Assuming time_vals are days since reference, calculate year centers
time_annual <- time_vals[seq(6, n_months_use, by=12)]  # Use June (month 6) as year center

cat("Annual time points:", n_years_calc, "\n")

# Get attributes from original file
phyc_atts <- list()
att_names <- names(ncatt_get(nc_phyc, phyc_var_name))
for (att_name in att_names) {
  phyc_atts[[att_name]] <- ncatt_get(nc_phyc, phyc_var_name, att_name)$value
}

# Close input file
nc_close(nc_phyc)

# Create output filename
output_phyc_file <- "ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc-top_60arcmin_global_annual_2101_2300.nc"
output_phyc_path <- file.path(output_folder_phyc, output_phyc_file)

cat("\nCreating output file:", output_phyc_file, "\n")

# Define dimensions for output file
dim_lon <- ncdim_def("lon", "degrees_east", lon_vals)
dim_lat <- ncdim_def("lat", "degrees_north", lat_vals)
dim_time <- ncdim_def("time", time_units, time_annual, unlim=FALSE)

# Define variable
var_phyc_out <- ncvar_def("phyc", "mol m-3", list(dim_lon, dim_lat, dim_time),
                          missval = NA,
                          longname = "Phytoplankton Carbon Concentration (surface, annual mean)")

# Create output NetCDF file
nc_out_phyc <- nc_create(output_phyc_path, list(var_phyc_out))

# Write data
ncvar_put(nc_out_phyc, var_phyc_out, phyc_annual)

# Copy/add attributes
for (att_name in names(phyc_atts)) {
  if (!att_name %in% c("_FillValue", "missing_value")) {
    ncatt_put(nc_out_phyc, "phyc", att_name, phyc_atts[[att_name]])
  }
}

# Add processing information
ncatt_put(nc_out_phyc, "phyc", "processing", "Extracted surface layer and calculated annual mean from monthly data")
ncatt_put(nc_out_phyc, "phyc", "source_file", phyc_file)
ncatt_put(nc_out_phyc, 0, "processing_date", as.character(Sys.Date()))
ncatt_put(nc_out_phyc, 0, "processing_script", "ZooMSS_2300_0_PreprocessUKESM_monthly.R")

# Close output file
nc_close(nc_out_phyc)

cat("✓ PHYC processing complete!\n\n")

# ==============================================================================
# Part 2: Process TOS - Calculate Annual Means
# ==============================================================================

cat("Part 2: Processing TOS data\n")
cat("----------------------------------------------------------------------\n")

# Check if input file exists
if (!file.exists(tos_input_path)) {
  stop("TOS input file not found: ", tos_input_path)
}

cat("Opening file:", tos_file, "\n")

# Open NetCDF file
nc_tos <- nc_open(tos_input_path)

# Get variable name
var_names_tos <- names(nc_tos$var)
tos_var_name <- var_names_tos[grep("tos", var_names_tos, ignore.case = TRUE)]
if (length(tos_var_name) == 0) {
  stop("Could not find tos variable in file")
}
tos_var_name <- tos_var_name[1]
cat("TOS variable name:", tos_var_name, "\n")

# Get dimensions
lon_vals_tos <- ncvar_get(nc_tos, "lon")
lat_vals_tos <- ncvar_get(nc_tos, "lat")
time_vals_tos <- ncvar_get(nc_tos, "time")

cat("Dimensions: lon=", length(lon_vals_tos), ", lat=", length(lat_vals_tos), 
    ", time=", length(time_vals_tos), "\n")

# Read TOS data
tos_data <- ncvar_get(nc_tos, tos_var_name)
cat("TOS data dimensions:", paste(dim(tos_data), collapse=" x "), "\n")
cat("TOS data range: [", min(tos_data, na.rm=TRUE), ",", 
    max(tos_data, na.rm=TRUE), "]\n")

# Check units - note that data is already in Celsius despite "K" label
tos_units <- ncatt_get(nc_tos, tos_var_name, "units")$value
cat("Original TOS units attribute:", tos_units, "\n")

# Data is already in Celsius (confirmed by value ranges), no conversion needed
# We'll correct the units attribute in the output file
if (!is.null(tos_units) && (tolower(tos_units) == "k" || tolower(tos_units) == "kelvin")) {
  cat("NOTE: Units attribute is '", tos_units, "' but data values are already in Celsius.\n", sep="")
  cat("      No numeric conversion applied. Will set output units to 'degC'.\n")
}

# Calculate number of years
n_months_tos <- length(time_vals_tos)
n_years_tos <- floor(n_months_tos / 12)

cat("\nTotal months:", n_months_tos, "\n")
cat("Total years:", n_years_tos, "\n")

# Trim to complete years
n_months_use_tos <- n_years_tos * 12
if (n_months_tos > n_months_use_tos) {
  cat("Note: Trimming incomplete year (using", n_months_use_tos, "of", n_months_tos, "months)\n")
  tos_data <- tos_data[, , 1:n_months_use_tos]
  time_vals_tos <- time_vals_tos[1:n_months_use_tos]
}

# Reshape to [lon, lat, 12, year]
dim_tos <- dim(tos_data)
n_lon_tos <- dim_tos[1]
n_lat_tos <- dim_tos[2]

tos_reshaped <- array(tos_data, dim = c(n_lon_tos, n_lat_tos, 12, n_years_tos))

# Calculate mean over months
cat("Calculating annual means...\n")
tos_annual <- apply(tos_reshaped, c(1, 2, 4), mean, na.rm = TRUE)

cat("Annual mean data dimensions:", paste(dim(tos_annual), collapse=" x "), "\n")
cat("Annual mean data range: [", min(tos_annual, na.rm=TRUE), ",", 
    max(tos_annual, na.rm=TRUE), "]\n")

# Create time values for annual data
time_units_tos <- ncatt_get(nc_tos, "time", "units")$value
cat("Original time units:", time_units_tos, "\n")
time_annual_tos <- time_vals_tos[seq(6, n_months_use_tos, by=12)]  # Use June as year center

# Get attributes
tos_atts <- list()
att_names_tos <- names(ncatt_get(nc_tos, tos_var_name))
for (att_name in att_names_tos) {
  tos_atts[[att_name]] <- ncatt_get(nc_tos, tos_var_name, att_name)$value
}

# Get original units to check if correction is needed
original_units <- ncatt_get(nc_tos, tos_var_name, "units")$value
cat("Original TOS units attribute:", original_units, "\n")

# Check if units are incorrectly labeled as Kelvin
if (!is.null(original_units) && original_units == "K") {
  cat("NOTE: Units attribute is 'K' but data range suggests degrees Celsius.\n")
  cat("      Will correct units to 'degC' in output file.\n")
}

# Close input file
nc_close(nc_tos)

# Create output filename
output_tos_file <- "ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_annual_2101_2300.nc"
output_tos_path <- file.path(output_folder_tos, output_tos_file)

cat("\nCreating output file:", output_tos_file, "\n")

# Define dimensions for output file
dim_lon_tos <- ncdim_def("lon", "degrees_east", lon_vals_tos)
dim_lat_tos <- ncdim_def("lat", "degrees_north", lat_vals_tos)
dim_time_tos <- ncdim_def("time", time_units_tos, time_annual_tos, unlim=FALSE)

# Define variable with corrected units (degC, not K)
var_tos_out <- ncvar_def("tos", "degC", list(dim_lon_tos, dim_lat_tos, dim_time_tos),
                         missval = NA,
                         longname = "Sea Surface Temperature (annual mean)")

# Create output NetCDF file
nc_out_tos <- nc_create(output_tos_path, list(var_tos_out))

# Write data
ncvar_put(nc_out_tos, var_tos_out, tos_annual)

# Copy/add attributes (skip units since we're correcting it)
for (att_name in names(tos_atts)) {
  if (!att_name %in% c("_FillValue", "missing_value", "units")) {
    ncatt_put(nc_out_tos, "tos", att_name, tos_atts[[att_name]])
  }
}

# Explicitly set corrected units
ncatt_put(nc_out_tos, "tos", "units", "degC")

# Add processing information
ncatt_put(nc_out_tos, "tos", "processing", "Calculated annual mean from monthly data; corrected units from K to degC (data already in Celsius)")
ncatt_put(nc_out_tos, "tos", "source_file", tos_file)
ncatt_put(nc_out_tos, "tos", "original_units", tos_units)
ncatt_put(nc_out_tos, 0, "processing_date", as.character(Sys.Date()))
ncatt_put(nc_out_tos, 0, "processing_script", "ZooMSS_2300_0_PreprocessUKESM_monthly.R")

# Close output file
nc_close(nc_out_tos)

cat("✓ TOS processing complete!\n\n")

# ==============================================================================
# Part 3: Validation - Compare Structure with Other Raw Inputs
# ==============================================================================

cat("Part 3: Validation\n")
cat("----------------------------------------------------------------------\n")

# Select a reference file from another model for comparison
ref_phyc_file <- file.path(output_folder_phyc, 
                           "cesm2-waccm_r1i1p1f1_ssp585_phyc-top_60arcmin_global_annual_2015_2299.nc")
ref_tos_file <- file.path(output_folder_tos,
                          "cesm2-waccm_r1i1p1f1_ssp585_tos_60arcmin_global_annual_2015_2299.nc")

cat("\nComparing PHYC structure...\n")
if (file.exists(ref_phyc_file)) {
  nc_ref <- nc_open(ref_phyc_file)
  nc_new <- nc_open(output_phyc_path)
  
  # Compare dimensions
  ref_dims <- sapply(nc_ref$dim, function(d) d$len)
  new_dims <- sapply(nc_new$dim, function(d) d$len)
  
  cat("Reference file dimensions:\n")
  print(ref_dims)
  cat("\nNew UKESM file dimensions:\n")
  print(new_dims)
  
  # Check lon/lat match
  if (ref_dims["lon"] == new_dims["lon"] && ref_dims["lat"] == new_dims["lat"]) {
    cat("✓ Spatial dimensions match!\n")
  } else {
    cat("✗ WARNING: Spatial dimensions differ!\n")
  }
  
  # Check variable units
  ref_units <- ncatt_get(nc_ref, "phyc", "units")$value
  new_units <- ncatt_get(nc_new, "phyc", "units")$value
  
  cat("\nReference units:", ref_units, "\n")
  cat("New file units:", new_units, "\n")
  
  if (ref_units == new_units) {
    cat("✓ Units match!\n")
  } else {
    cat("✗ WARNING: Units differ!\n")
  }
  
  nc_close(nc_ref)
  nc_close(nc_new)
} else {
  cat("Reference file not found. Skipping comparison.\n")
}

cat("\nComparing TOS structure...\n")
if (file.exists(ref_tos_file)) {
  nc_ref <- nc_open(ref_tos_file)
  nc_new <- nc_open(output_tos_path)
  
  # Compare dimensions
  ref_dims <- sapply(nc_ref$dim, function(d) d$len)
  new_dims <- sapply(nc_new$dim, function(d) d$len)
  
  cat("Reference file dimensions:\n")
  print(ref_dims)
  cat("\nNew UKESM file dimensions:\n")
  print(new_dims)
  
  # Check lon/lat match
  if (ref_dims["lon"] == new_dims["lon"] && ref_dims["lat"] == new_dims["lat"]) {
    cat("✓ Spatial dimensions match!\n")
  } else {
    cat("✗ WARNING: Spatial dimensions differ!\n")
  }
  
  # Check variable units
  ref_units <- ncatt_get(nc_ref, "tos", "units")$value
  new_units <- ncatt_get(nc_new, "tos", "units")$value
  
  cat("\nReference units:", ref_units, "\n")
  cat("New file units:", new_units, "\n")
  
  if (ref_units == new_units) {
    cat("✓ Units match!\n")
  } else {
    cat("✗ WARNING: Units differ!\n")
  }
  
  nc_close(nc_ref)
  nc_close(nc_new)
} else {
  cat("Reference file not found. Skipping comparison.\n")
}

# ==============================================================================
# Summary
# ==============================================================================

cat("\n==============================================================================\n")
cat("Processing Complete!\n")
cat("==============================================================================\n\n")

cat("Output files created:\n")
cat("  PHYC:", output_phyc_path, "\n")
cat("  TOS:", output_tos_path, "\n\n")

cat("Next steps:\n")
cat("  1. Run ZooMSS_2300_0a_ConvertPhycToChl.R to convert PHYC to chlorophyll\n")
cat("  2. Continue with standard pipeline for processing\n\n")

cat("==============================================================================\n")
