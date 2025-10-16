# ==============================================================================
# UKESM OVERSHOOT 2101-2300: PROCESS SST DATA
# ==============================================================================
# Purpose: Extract and convert monthly SST to annual means
# Author: ZooMSS_2300 Analysis Team
# Date: October 15, 2025
# ==============================================================================

library(ncdf4)
library(tidyverse)
library(raster)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

tos_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/Input/tos/"
output_dir <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/Input/tos/"

cat("=== UKESM OVERSHOOT SST PROCESSING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOCATE SST FILE
# ==============================================================================

cat("STEP 1: Locating UKESM overshoot SST file (2101-2300)...\n")

tos_files <- list.files(tos_dir, 
                        pattern = "ukesm.*ssp534.*tos.*2101.*\\.nc$",
                        full.names = TRUE,
                        ignore.case = TRUE)

if (length(tos_files) == 0) {
  tos_files <- list.files(tos_dir, 
                          pattern = "ukesm.*ssp534.*tos.*monthly.*\\.nc$",
                          full.names = TRUE,
                          ignore.case = TRUE)
}

if (length(tos_files) == 0) {
  stop("ERROR: No UKESM overshoot SST file found!")
}

target_file <- tos_files[1]
cat("  Target file:", basename(target_file), "\n")
cat("  Size:", round(file.size(target_file) / (1024^2), 1), "MB\n\n")

# ==============================================================================
# STEP 2: INSPECT STRUCTURE
# ==============================================================================

cat("STEP 2: Inspecting NetCDF structure...\n")

nc <- nc_open(target_file)

cat("  Dimensions:\n")
for (dim_name in names(nc$dim)) {
  dim_obj <- nc$dim[[dim_name]]
  cat("    ", dim_name, ":", dim_obj$len, "\n")
}

cat("\n  Variables:\n")
for (var_name in names(nc$var)) {
  var_obj <- nc$var[[var_name]]
  cat("    ", var_name, "\n")
  cat("      Dims:", paste(sapply(var_obj$dim, function(d) paste0(d$name, "(", d$len, ")")), collapse = " × "), "\n")
  if (!is.null(var_obj$units)) {
    cat("      Units:", var_obj$units, "\n")
  }
}

# Find tos variable
tos_var_names <- c("tos", "sst", "temp", "TOS", "SST")
tos_var <- NULL
for (var_name in tos_var_names) {
  if (var_name %in% names(nc$var)) {
    tos_var <- var_name
    break
  }
}

if (is.null(tos_var)) {
  nc_close(nc)
  stop("ERROR: Cannot find SST variable!")
}

cat("\n✓ SST variable found:", tos_var, "\n")

# Get dimensions
var_dims <- nc$var[[tos_var]]$dim
dim_names <- sapply(var_dims, function(d) d$name)
dim_lengths <- sapply(var_dims, function(d) d$len)

cat("  Structure:", paste(paste0(dim_names, "=", dim_lengths), collapse = ", "), "\n")

# Check for depth dimension
depth_dim_names <- c("lev", "depth", "z", "plev", "level")
has_depth <- any(tolower(dim_names) %in% depth_dim_names)

if (has_depth) {
  cat("  ⚠️ Has depth dimension - will extract surface only\n")
} else {
  cat("  ✓ No depth dimension - already surface data\n")
}

# Identify indices
lon_idx <- which(tolower(dim_names) %in% c("lon", "longitude", "x"))
lat_idx <- which(tolower(dim_names) %in% c("lat", "latitude", "y"))
time_idx <- which(tolower(dim_names) %in% c("time", "t"))

n_lon <- dim_lengths[lon_idx]
n_lat <- dim_lengths[lat_idx]
n_time <- dim_lengths[time_idx]

lon_vals <- var_dims[[lon_idx]]$vals
lat_vals <- var_dims[[lat_idx]]$vals

cat("  Grid: ", n_lon, "× ", n_lat, "\n", sep = "")
cat("  Time steps:", n_time, "\n")

# ==============================================================================
# STEP 3: EXTRACT AND CONVERT TO ANNUAL MEANS
# ==============================================================================

cat("\nSTEP 3: Extracting and converting to annual means...\n")

n_years <- floor(n_time / 12)
cat("  Processing", n_time, "monthly steps →", n_years, "annual means\n")

# Initialize output array
annual_tos <- array(NA, dim = c(n_lon, n_lat, n_years))

cat("  Processing by year:\n")

for (year_idx in 1:n_years) {
  if (year_idx %% 10 == 0) {
    cat("    Year", year_idx, "of", n_years, "\n")
  }
  
  # Time indices for this year
  time_start <- (year_idx - 1) * 12 + 1
  time_end <- year_idx * 12
  
  # Build start and count vectors
  start_vec <- rep(1, length(dim_names))
  count_vec <- dim_lengths
  
  start_vec[time_idx] <- time_start
  count_vec[time_idx] <- 12  # 12 months
  
  # If depth exists, extract surface only
  if (has_depth) {
    depth_idx <- which(tolower(dim_names) %in% depth_dim_names)
    start_vec[depth_idx] <- 1
    count_vec[depth_idx] <- 1
  }
  
  # Extract monthly data
  tryCatch({
    monthly_data <- ncvar_get(nc, tos_var, start = start_vec, count = count_vec)
    
    # Calculate annual mean
    if (length(dim(monthly_data)) == 3) {
      annual_tos[,,year_idx] <- apply(monthly_data, c(1, 2), mean, na.rm = TRUE)
    } else if (length(dim(monthly_data)) == 2) {
      # If already 2D (single time step extracted)
      annual_tos[,,year_idx] <- monthly_data
    } else {
      stop("Unexpected data dimensions: ", paste(dim(monthly_data), collapse = "x"))
    }
    
  }, error = function(e) {
    cat("    ERROR at year", year_idx, ":", e$message, "\n")
    annual_tos[,,year_idx] <- NA
  })
  
  # Memory cleanup
  if (year_idx %% 20 == 0) {
    gc()
  }
}

cat("  ✓ Extraction complete\n")

# ==============================================================================
# STEP 4: CREATE OUTPUT NETCDF
# ==============================================================================

cat("\nSTEP 4: Creating output NetCDF file...\n")

# Create proper filename matching convention: *_tos_60arcmin_global_annual_2101_2300.nc
# Extract key parts from original filename
filename_parts <- str_match(basename(target_file), 
                           "(ukesm[^_]+_[^_]+_[^_]+)_tos.*_(\\d{4})_(\\d{4})")
model_info <- filename_parts[2]  # e.g., ukesm1-0-ll_r4i1p1f2_ssp534-over
start_year <- filename_parts[3]  # e.g., 2101
end_year <- filename_parts[4]    # e.g., 2300

output_filename <- paste0(model_info, "_tos_60arcmin_global_annual_", 
                         start_year, "_", end_year, ".nc")
output_path <- file.path(output_dir, output_filename)

cat("  Output file:", output_filename, "\n")

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon_vals)
lat_dim <- ncdim_def("lat", "degrees_north", lat_vals)

# Create time dimension (years)
year_vals <- 2101:(2101 + n_years - 1)
time_dim <- ncdim_def("time", 
                      units = "year", 
                      vals = year_vals,
                      longname = "Year")

# Define variable
tos_def <- ncvar_def(
  name = "tos",
  units = "degC",
  dim = list(lon_dim, lat_dim, time_dim),
  missval = -999,
  longname = "Sea Surface Temperature (annual mean)",
  prec = "float"
)

# Create NetCDF
nc_out <- nc_create(output_path, tos_def)
ncvar_put(nc_out, tos_def, annual_tos)

# Add attributes
ncatt_put(nc_out, 0, "title", "UKESM1-0-LL SSP534-overshoot SST (2101-2300)")
ncatt_put(nc_out, 0, "source", "Aggregated from monthly data to annual means")
ncatt_put(nc_out, 0, "temporal_resolution", "annual_mean")
ncatt_put(nc_out, 0, "processing_date", as.character(Sys.Date()))
ncatt_put(nc_out, 0, "original_file", basename(target_file))

nc_close(nc_out)
nc_close(nc)

cat("  ✓ NetCDF file created\n")
cat("  Size:", round(file.size(output_path) / (1024^2), 1), "MB\n")

# ==============================================================================
# STEP 5: VERIFY OUTPUT
# ==============================================================================

cat("\nSTEP 5: Verifying output...\n")

nc_verify <- nc_open(output_path)
tos_verify <- ncvar_get(nc_verify, "tos")

cat("  Output dimensions:", paste(dim(tos_verify), collapse = " × "), "\n")
cat("  SST range:", paste(range(tos_verify, na.rm = TRUE), collapse = " to "), "°C\n")
cat("  Non-NA values:", sum(!is.na(tos_verify)), "/", length(tos_verify), "\n")
cat("  Years:", min(year_vals), "to", max(year_vals), "\n")

nc_close(nc_verify)

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n=============================================================================\n")
cat("SST PROCESSING COMPLETE\n")
cat("=============================================================================\n\n")

cat("INPUT:\n")
cat("  File:", basename(target_file), "\n")
cat("  Size:", round(file.size(target_file) / (1024^2), 1), "MB\n")
cat("  Structure: Monthly SST data\n\n")

cat("OUTPUT:\n")
cat("  File:", basename(output_path), "\n")
cat("  Size:", round(file.size(output_path) / (1024^2), 1), "MB\n")
cat("  Years:", min(year_vals), "-", max(year_vals), "\n")
cat("  SST range:", paste(range(tos_verify, na.rm = TRUE), collapse = " to "), "°C\n\n")

cat("✓ SST data ready for processing!\n")
cat("✓ Chlorophyll data already processed (chla-top_annual.nc)\n\n")

cat("NEXT STEPS:\n")
cat("  1. Combine SST + Chlorophyll into processed RDS format\n")
cat("  2. Run validation: validate_ukesm_overshoot_coverage.R\n")
cat("  3. Update environmental matrix if needed\n")
cat("  4. Re-process UKESM overshoot scenario with complete data\n")

cat("\n=============================================================================\n")

# Save summary
processing_summary <- list(
  processing_date = Sys.time(),
  input_file = target_file,
  output_file = output_path,
  n_years = n_years,
  year_range = c(min(year_vals), max(year_vals)),
  sst_range = range(tos_verify, na.rm = TRUE),
  dimensions = list(lon = n_lon, lat = n_lat, time = n_years)
)

saveRDS(processing_summary, 
        file.path(output_dir, "ukesm_overshoot_2101-2300_tos_processing_summary.rds"))

cat("Processing summary saved!\n")
