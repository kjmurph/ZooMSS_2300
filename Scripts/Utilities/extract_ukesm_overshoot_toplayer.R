# ==============================================================================
# UKESM OVERSHOOT 2101-2300: EXTRACT TOP LAYER & CONVERT TO ANNUAL
# ==============================================================================
# Purpose: Extract surface layer from 3D NetCDF and aggregate monthly → annual
# Author: ZooMSS_2300 Analysis Team
# Date: October 15, 2025
# ==============================================================================

library(ncdf4)
library(tidyverse)
library(raster)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

phyc_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/Input/phyc/"
output_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/Input/phyc/"

# Load inspection report if available
inspection_file <- file.path(output_dir, "ukesm_overshoot_2101-2300_inspection_report.rds")

if (file.exists(inspection_file)) {
  cat("Loading inspection report...\n")
  inspection <- readRDS(inspection_file)
  target_file <- inspection$file_path
  cat("✓ Using file from inspection:", basename(target_file), "\n\n")
} else {
  # Find the file manually
  phyc_files <- list.files(phyc_dir, 
                           pattern = "ukesm.*ssp534.*phyc.*2101.*\\.nc$",
                           full.names = TRUE,
                           ignore.case = TRUE)
  
  if (length(phyc_files) == 0) {
    phyc_files <- list.files(phyc_dir, 
                             pattern = "ukesm.*ssp534.*phyc.*\\.nc$",
                             full.names = TRUE,
                             ignore.case = TRUE)
  }
  
  if (length(phyc_files) == 0) {
    stop("ERROR: No UKESM overshoot phyc file found!")
  }
  
  target_file <- phyc_files[1]
  cat("Using file:", basename(target_file), "\n\n")
}

cat("=== UKESM OVERSHOOT TOP LAYER EXTRACTION ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Input file:", basename(target_file), "\n")
cat("File size:", round(file.size(target_file) / (1024^3), 2), "GB\n\n")

# ==============================================================================
# STEP 1: OPEN NETCDF AND IDENTIFY STRUCTURE
# ==============================================================================

cat("STEP 1: Opening NetCDF file...\n")

nc <- nc_open(target_file)

# Find phyc variable
phyc_var_names <- c("phyc", "phytoplankton", "phyto", "PHYC")
phyc_var <- NULL
for (var_name in phyc_var_names) {
  if (var_name %in% names(nc$var)) {
    phyc_var <- var_name
    break
  }
}

if (is.null(phyc_var)) {
  nc_close(nc)
  stop("ERROR: Cannot find phyc variable!")
}

cat("  Phyc variable:", phyc_var, "\n")

# Get dimensions
var_dims <- nc$var[[phyc_var]]$dim
dim_names <- sapply(var_dims, function(d) d$name)
dim_lengths <- sapply(var_dims, function(d) d$len)

cat("  Dimensions:", paste(paste0(dim_names, "=", dim_lengths), collapse = ", "), "\n")

# Identify key dimensions
lon_idx <- which(tolower(dim_names) %in% c("lon", "longitude", "x"))
lat_idx <- which(tolower(dim_names) %in% c("lat", "latitude", "y"))
time_idx <- which(tolower(dim_names) %in% c("time", "t"))
depth_idx <- which(tolower(dim_names) %in% c("lev", "depth", "z", "plev", "level"))

n_lon <- dim_lengths[lon_idx]
n_lat <- dim_lengths[lat_idx]
n_time <- dim_lengths[time_idx]

cat("  Lon:", n_lon, "| Lat:", n_lat, "| Time:", n_time, "\n")

# Get coordinate values
lon_vals <- var_dims[[lon_idx]]$vals
lat_vals <- var_dims[[lat_idx]]$vals
time_vals <- var_dims[[time_idx]]$vals

cat("  Lon range:", paste(range(lon_vals), collapse = " to "), "\n")
cat("  Lat range:", paste(range(lat_vals), collapse = " to "), "\n")

# Check for depth dimension
has_depth <- length(depth_idx) > 0

if (has_depth) {
  n_depth <- dim_lengths[depth_idx]
  depth_vals <- var_dims[[depth_idx]]$vals
  cat("  Depth levels:", n_depth, "\n")
  cat("  Depth values:", paste(head(depth_vals, 10), collapse = ", "), "\n")
  cat("  → Will extract top layer (index 1)\n")
} else {
  cat("  → No depth dimension (already surface data)\n")
}

# ==============================================================================
# STEP 2: EXTRACT TOP LAYER DATA
# ==============================================================================

cat("\nSTEP 2: Extracting top layer data...\n")

# Initialize storage for annual data
n_years <- floor(n_time / 12)
cat("  Processing", n_time, "monthly time steps →", n_years, "annual means\n")

# Create output arrays for annual means
annual_phyc <- array(NA, dim = c(n_lon, n_lat, n_years))

# Process in yearly chunks to manage memory
cat("  Processing by year:\n")

for (year_idx in 1:n_years) {
  if (year_idx %% 10 == 0) {
    cat("    Year", year_idx, "of", n_years, "\n")
  }
  
  # Time indices for this year (12 months)
  time_start <- (year_idx - 1) * 12 + 1
  time_end <- year_idx * 12
  
  # Build start and count vectors
  start_vec <- rep(1, length(dim_names))
  count_vec <- dim_lengths
  
  start_vec[time_idx] <- time_start
  count_vec[time_idx] <- 12  # 12 months
  
  if (has_depth) {
    start_vec[depth_idx] <- 1  # Top layer
    count_vec[depth_idx] <- 1  # Only top layer
  }
  
  # Extract 12 months of data
  tryCatch({
    monthly_data <- ncvar_get(nc, phyc_var, start = start_vec, count = count_vec)
    
    # Calculate annual mean
    # Data structure should be (lon, lat, time) after extraction
    if (length(dim(monthly_data)) == 3) {
      annual_phyc[,,year_idx] <- apply(monthly_data, c(1, 2), mean, na.rm = TRUE)
    } else if (length(dim(monthly_data)) == 4) {
      # If depth dimension still present, average over time first
      annual_phyc[,,year_idx] <- apply(monthly_data, c(1, 2), mean, na.rm = TRUE)
    } else {
      stop("Unexpected data dimensions: ", paste(dim(monthly_data), collapse = "x"))
    }
    
  }, error = function(e) {
    cat("    ERROR at year", year_idx, ":", e$message, "\n")
    annual_phyc[,,year_idx] <- NA
  })
  
  # Memory cleanup every 20 years
  if (year_idx %% 20 == 0) {
    gc()
  }
}

cat("  ✓ Extraction complete\n")

# ==============================================================================
# STEP 3: CREATE OUTPUT NETCDF FILE
# ==============================================================================

cat("\nSTEP 3: Creating output NetCDF file...\n")

# Create proper filename matching convention: *_phyc-top_60arcmin_global_annual_2101_2300.nc
# Extract key parts from original filename
filename_parts <- str_match(basename(target_file), 
                           "(ukesm[^_]+_[^_]+_[^_]+)_phyc.*_(\\d{4})_(\\d{4})")
model_info <- filename_parts[2]  # e.g., ukesm1-0-ll_r4i1p1f2_ssp534-over
start_year <- filename_parts[3]  # e.g., 2101
end_year <- filename_parts[4]    # e.g., 2300

output_filename <- paste0(model_info, "_phyc-top_60arcmin_global_annual_", 
                         start_year, "_", end_year, ".nc")
output_path <- file.path(output_dir, output_filename)

cat("  Output file:", output_filename, "\n")

# Define dimensions for output file
lon_dim <- ncdim_def("lon", "degrees_east", lon_vals)
lat_dim <- ncdim_def("lat", "degrees_north", lat_vals)

# Create time dimension (annual, starting from 2101)
year_vals <- 2101:(2101 + n_years - 1)
time_dim <- ncdim_def("time", 
                      units = "year", 
                      vals = year_vals,
                      longname = "Year")

# Define variable
phyc_def <- ncvar_def(
  name = "phyc",
  units = "mol m-3",
  dim = list(lon_dim, lat_dim, time_dim),
  missval = -999,
  longname = "Mole Concentration of Phytoplankton expressed as Carbon in sea water (surface layer, annual mean)",
  prec = "float"
)

# Create NetCDF file
nc_out <- nc_create(output_path, phyc_def)

# Write data
ncvar_put(nc_out, phyc_def, annual_phyc)

# Add global attributes
ncatt_put(nc_out, 0, "title", "UKESM1-0-LL SSP534-overshoot phytoplankton carbon (2101-2300)")
ncatt_put(nc_out, 0, "source", "Extracted from 3D monthly data, top layer only")
ncatt_put(nc_out, 0, "temporal_resolution", "annual_mean")
ncatt_put(nc_out, 0, "processing_date", as.character(Sys.Date()))
ncatt_put(nc_out, 0, "comment", "Extracted surface layer and aggregated monthly to annual means")
ncatt_put(nc_out, 0, "original_file", basename(target_file))

# Close output file
nc_close(nc_out)

cat("  ✓ NetCDF file created\n")
cat("  Size:", round(file.size(output_path) / (1024^2), 1), "MB\n")

# Close input file
nc_close(nc)

# ==============================================================================
# STEP 4: VERIFY OUTPUT
# ==============================================================================

cat("\nSTEP 4: Verifying output...\n")

nc_verify <- nc_open(output_path)
phyc_verify <- ncvar_get(nc_verify, "phyc")

cat("  Output dimensions:", paste(dim(phyc_verify), collapse = " x "), "\n")
cat("  Data range:", paste(range(phyc_verify, na.rm = TRUE), collapse = " to "), "mol/m³\n")
cat("  Non-NA values:", sum(!is.na(phyc_verify)), "/", length(phyc_verify), "\n")
cat("  Years:", min(year_vals), "to", max(year_vals), "(", length(year_vals), "years )\n")

nc_close(nc_verify)

# ==============================================================================
# STEP 5: CONVERT TO CHLOROPHYLL-A
# ==============================================================================

cat("\nSTEP 5: Converting phyc → chlorophyll-a...\n")

# Conversion factor from mol C/m³ to mg Chl/m³
# Typical C:Chl ratio is ~50-100, using 50 as conservative estimate
# 1 mol C = 12 g C
# C:Chl = 50 (g:g)
# So: phyc [mol/m³] * 12 [g C/mol] / 50 [g C/g Chl] * 1000 [mg/g] = Chl [mg/m³]

chl_output_path <- str_replace(output_path, "phyc.*\\.nc$", "chla-top_annual.nc")

cat("  Chlorophyll output:", basename(chl_output_path), "\n")

# Open phyc file again
nc_phyc <- nc_open(output_path)
phyc_data <- ncvar_get(nc_phyc, "phyc")

# Convert to chlorophyll
# Using C:Chl ratio of 50
chl_data <- phyc_data * 12 / 50 * 1000  # mg Chl/m³

cat("  Chlorophyll range:", paste(range(chl_data, na.rm = TRUE), collapse = " to "), "mg/m³\n")

# Create chlorophyll NetCDF
chl_def <- ncvar_def(
  name = "chla",
  units = "mg m-3",
  dim = list(lon_dim, lat_dim, time_dim),
  missval = -999,
  longname = "Mass Concentration of Chlorophyll in sea water (surface layer, annual mean)",
  prec = "float"
)

nc_chl <- nc_create(chl_output_path, chl_def)
ncvar_put(nc_chl, chl_def, chl_data)

# Add attributes
ncatt_put(nc_chl, 0, "title", "UKESM1-0-LL SSP534-overshoot chlorophyll-a (2101-2300)")
ncatt_put(nc_chl, 0, "source", "Converted from phytoplankton carbon")
ncatt_put(nc_chl, 0, "conversion", "C:Chl ratio = 50 (g:g)")
ncatt_put(nc_chl, 0, "temporal_resolution", "annual_mean")
ncatt_put(nc_chl, 0, "processing_date", as.character(Sys.Date()))

nc_close(nc_chl)
nc_close(nc_phyc)

cat("  ✓ Chlorophyll file created\n")
cat("  Size:", round(file.size(chl_output_path) / (1024^2), 1), "MB\n")

# ==============================================================================
# STEP 6: SUMMARY REPORT
# ==============================================================================

cat("\n=============================================================================\n")
cat("EXTRACTION COMPLETE\n")
cat("=============================================================================\n\n")

cat("INPUT:\n")
cat("  File:", basename(target_file), "\n")
cat("  Size:", round(file.size(target_file) / (1024^3), 2), "GB\n")
cat("  Structure: 3D monthly (lon × lat × depth × time)\n\n")

cat("OUTPUT:\n")
cat("  1. Phytoplankton carbon (surface, annual):\n")
cat("     ", basename(output_path), "\n")
cat("     Size:", round(file.size(output_path) / (1024^2), 1), "MB\n")
cat("     Years:", min(year_vals), "-", max(year_vals), "\n\n")

cat("  2. Chlorophyll-a (surface, annual):\n")
cat("     ", basename(chl_output_path), "\n")
cat("     Size:", round(file.size(chl_output_path) / (1024^2), 1), "MB\n")
cat("     Years:", min(year_vals), "-", max(year_vals), "\n\n")

cat("NEXT STEPS:\n")
cat("  1. Check if corresponding SST file (tos) exists for 2101-2300\n")
cat("  2. Process SST data (if needed, likely also monthly)\n")
cat("  3. Run validation script: validate_ukesm_overshoot_coverage.R\n")
cat("  4. Update environmental matrix if gaps are found\n")

cat("\n=============================================================================\n")

# Save processing summary
processing_summary <- list(
  processing_date = Sys.time(),
  input_file = target_file,
  input_size_gb = file.size(target_file) / (1024^3),
  output_phyc = output_path,
  output_chl = chl_output_path,
  n_years = n_years,
  year_range = c(min(year_vals), max(year_vals)),
  dimensions = list(
    lon = n_lon,
    lat = n_lat,
    time = n_years
  ),
  conversion_factor = "C:Chl = 50 (g:g)"
)

saveRDS(processing_summary, 
        file.path(output_dir, "ukesm_overshoot_2101-2300_processing_summary.rds"))

cat("Processing summary saved to: ukesm_overshoot_2101-2300_processing_summary.rds\n")
