# ==============================================================================
# UKESM OVERSHOOT 2101-2300 DATA INSPECTION AND EXTRACTION
# ==============================================================================
# Purpose: Inspect 3D monthly NetCDF file and extract top layer
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

cat("\n=== UKESM OVERSHOOT 2101-2300 DATA INSPECTION ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: LOCATE THE FILE
# ==============================================================================

cat("STEP 1: Locating UKESM overshoot NetCDF file...\n")

# Search for UKESM overshoot phyc files covering 2101-2300
phyc_files <- list.files(phyc_dir, 
                         pattern = "ukesm.*ssp534.*phyc.*\\.nc$",
                         full.names = TRUE,
                         ignore.case = TRUE)

cat("  Found", length(phyc_files), "potential file(s):\n")
for (f in phyc_files) {
  file_size <- file.size(f) / (1024^3)  # Convert to GB
  year_match <- str_extract(basename(f), "\\d{4}_\\d{4}")
  cat("    -", basename(f), "\n")
  cat("      Size:", round(file_size, 2), "GB\n")
  if (!is.na(year_match)) {
    cat("      Years:", year_match, "\n")
  }
}

# Find the 2101-2300 file specifically
target_file <- phyc_files[str_detect(phyc_files, "2101|21\\d{2}_23\\d{2}")]

if (length(target_file) == 0) {
  cat("\n⚠️  No file matching 2101-2300 pattern found.\n")
  cat("Available files:\n")
  print(basename(phyc_files))
  
  if (length(phyc_files) > 0) {
    cat("\nUsing first available file for inspection:\n")
    target_file <- phyc_files[1]
  } else {
    stop("ERROR: No UKESM overshoot phyc files found in directory!")
  }
}

if (length(target_file) > 1) {
  cat("\n⚠️  Multiple files found. Using first one:\n")
  target_file <- target_file[1]
}

cat("\n✓ Target file selected:\n")
cat("  ", basename(target_file), "\n")
cat("  Size:", round(file.size(target_file) / (1024^3), 2), "GB\n")

# ==============================================================================
# STEP 2: INSPECT NETCDF STRUCTURE (WITHOUT LOADING FULL DATA)
# ==============================================================================

cat("\nSTEP 2: Inspecting NetCDF structure...\n")

nc <- nc_open(target_file)

cat("\n--- FILE METADATA ---\n")
cat("Format:", nc$format, "\n")

cat("\n--- DIMENSIONS ---\n")
for (dim_name in names(nc$dim)) {
  dim_obj <- nc$dim[[dim_name]]
  cat("  ", dim_name, ":", dim_obj$len, dim_obj$units, "\n")
  
  # Show first few values for spatial dimensions
  if (dim_name %in% c("lon", "longitude", "lat", "latitude")) {
    vals <- dim_obj$vals
    if (length(vals) <= 10) {
      cat("      Values:", paste(head(vals, 10), collapse = ", "), "\n")
    } else {
      cat("      Range:", min(vals), "to", max(vals), "\n")
      cat("      First 5:", paste(head(vals, 5), collapse = ", "), "\n")
      cat("      Last 5:", paste(tail(vals, 5), collapse = ", "), "\n")
    }
  }
  
  # Show info for depth dimension
  if (dim_name %in% c("lev", "depth", "z", "plev")) {
    vals <- dim_obj$vals
    cat("      Depth levels:", length(vals), "\n")
    cat("      First 5 levels:", paste(head(vals, 5), collapse = ", "), "\n")
    if (length(vals) <= 10) {
      cat("      All levels:", paste(vals, collapse = ", "), "\n")
    }
  }
  
  # Show info for time dimension
  if (dim_name %in% c("time", "t")) {
    cat("      Time steps:", dim_obj$len, "\n")
    # Try to get time units
    if (!is.null(dim_obj$units)) {
      cat("      Units:", dim_obj$units, "\n")
    }
  }
}

cat("\n--- VARIABLES ---\n")
for (var_name in names(nc$var)) {
  var_obj <- nc$var[[var_name]]
  cat("\n  ", var_name, "\n")
  cat("    Dimensions:", paste(sapply(var_obj$dim, function(d) paste0(d$name, "(", d$len, ")")), collapse = " × "), "\n")
  
  # Get attributes
  if (!is.null(var_obj$units)) {
    cat("    Units:", var_obj$units, "\n")
  }
  if (!is.null(var_obj$longname)) {
    cat("    Long name:", var_obj$longname, "\n")
  }
  
  # Try to get standard_name attribute
  std_name <- ncatt_get(nc, var_name, "standard_name")
  if (std_name$hasatt) {
    cat("    Standard name:", std_name$value, "\n")
  }
  
  # Try to get missing/fill values
  fill_val <- ncatt_get(nc, var_name, "_FillValue")
  if (fill_val$hasatt) {
    cat("    Fill value:", fill_val$value, "\n")
  }
}

# ==============================================================================
# STEP 3: IDENTIFY KEY DIMENSIONS
# ==============================================================================

cat("\n\nSTEP 3: Identifying key dimensions for extraction...\n")

# Find the phyc variable
phyc_var_names <- c("phyc", "phytoplankton", "phyto", "PHYC")
phyc_var <- NULL
for (var_name in phyc_var_names) {
  if (var_name %in% names(nc$var)) {
    phyc_var <- var_name
    break
  }
}

if (is.null(phyc_var)) {
  cat("⚠️  'phyc' variable not found. Available variables:\n")
  print(names(nc$var))
  # Use the first variable that's not a coordinate
  non_coord_vars <- setdiff(names(nc$var), c("lat", "lon", "time", "lev", "depth", "latitude", "longitude"))
  if (length(non_coord_vars) > 0) {
    phyc_var <- non_coord_vars[1]
    cat("Using variable:", phyc_var, "\n")
  } else {
    nc_close(nc)
    stop("ERROR: Cannot identify phytoplankton variable!")
  }
}

cat("✓ Phytoplankton variable:", phyc_var, "\n")

# Get dimension information
var_dims <- nc$var[[phyc_var]]$dim
dim_names <- sapply(var_dims, function(d) d$name)
dim_lengths <- sapply(var_dims, function(d) d$len)

cat("  Dimensions:", paste(paste0(dim_names, "=", dim_lengths), collapse = ", "), "\n")

# Identify which dimension is depth
depth_dim_names <- c("lev", "depth", "z", "plev", "level")
depth_dim_idx <- which(dim_names %in% depth_dim_names)

if (length(depth_dim_idx) == 0) {
  cat("⚠️  No depth dimension found. This might already be surface data.\n")
  has_depth <- FALSE
} else {
  has_depth <- TRUE
  depth_dim_name <- dim_names[depth_dim_idx]
  n_depth_levels <- dim_lengths[depth_dim_idx]
  cat("  Depth dimension:", depth_dim_name, "with", n_depth_levels, "levels\n")
  
  # Get depth values
  depth_vals <- var_dims[[depth_dim_idx]]$vals
  cat("  Depth values:", paste(head(depth_vals, 10), collapse = ", "), "\n")
}

# Identify time dimension
time_dim_names <- c("time", "t", "Time")
time_dim_idx <- which(dim_names %in% time_dim_names)
if (length(time_dim_idx) > 0) {
  n_time_steps <- dim_lengths[time_dim_idx]
  cat("  Time dimension:", dim_names[time_dim_idx], "with", n_time_steps, "steps\n")
  
  # Calculate expected years
  if (n_time_steps == 12 * 200) {
    cat("    → Monthly data for 200 years (2101-2300)\n")
  } else if (n_time_steps == 12 * 199) {
    cat("    → Monthly data for 199 years\n")
  } else {
    cat("    → Unexpected number of time steps:", n_time_steps, "\n")
  }
}

# ==============================================================================
# STEP 4: EXTRACT SAMPLE DATA TO VERIFY STRUCTURE
# ==============================================================================

cat("\nSTEP 4: Extracting sample data to verify structure...\n")

if (has_depth) {
  cat("  Extracting first time step, top depth level, all lat/lon...\n")
  
  # Determine array structure (lon, lat, depth, time) or (lon, lat, time, depth) etc.
  # Need to construct start and count vectors
  
  # Build start vector (1-indexed)
  start_vec <- rep(1, length(dim_names))
  
  # Build count vector
  count_vec <- dim_lengths
  count_vec[depth_dim_idx] <- 1  # Only first depth level
  count_vec[time_dim_idx] <- 1   # Only first time step
  
  cat("  Start:", paste(start_vec, collapse = ", "), "\n")
  cat("  Count:", paste(count_vec, collapse = ", "), "\n")
  
  # Extract sample
  sample_data <- ncvar_get(nc, phyc_var, start = start_vec, count = count_vec)
  
  cat("  Sample data dimensions:", paste(dim(sample_data), collapse = " × "), "\n")
  cat("  Sample data range:", paste(range(sample_data, na.rm = TRUE), collapse = " to "), "\n")
  cat("  Non-NA values:", sum(!is.na(sample_data)), "/", length(sample_data), "\n")
  
} else {
  cat("  No depth dimension found - extracting first time step...\n")
  
  start_vec <- rep(1, length(dim_names))
  count_vec <- dim_lengths
  count_vec[time_dim_idx] <- 1   # Only first time step
  
  sample_data <- ncvar_get(nc, phyc_var, start = start_vec, count = count_vec)
  
  cat("  Sample data dimensions:", paste(dim(sample_data), collapse = " × "), "\n")
  cat("  Sample data range:", paste(range(sample_data, na.rm = TRUE), collapse = " to "), "\n")
}

# ==============================================================================
# STEP 5: STRATEGY RECOMMENDATION
# ==============================================================================

cat("\n\nSTEP 5: Extraction strategy recommendation...\n")

total_size_gb <- file.size(target_file) / (1024^3)
estimated_top_layer_size_mb <- (prod(dim_lengths) / dim_lengths[depth_dim_idx]) * 8 / (1024^2)  # 8 bytes per double

cat("\n--- EXTRACTION STRATEGY ---\n")
cat("Current file size:", round(total_size_gb, 2), "GB\n")

if (has_depth) {
  cat("Estimated top layer size:", round(estimated_top_layer_size_mb, 1), "MB\n")
  cat("Depth levels to extract: 1 (surface)\n")
  cat("Time steps:", n_time_steps, "(monthly)\n")
  cat("Will need to aggregate to annual means\n")
  
  cat("\nRecommended approach:\n")
  cat("  1. Extract top depth layer only (depth index = 1)\n")
  cat("  2. Process monthly data in chunks\n")
  cat("  3. Calculate annual means for each grid cell\n")
  cat("  4. Save as annual top-layer NetCDF (matching existing format)\n")
  cat("  5. Convert phyc → chlorophyll-a\n")
  
} else {
  cat("No depth extraction needed - data appears to be surface only\n")
  cat("Will need to aggregate monthly → annual\n")
}

# ==============================================================================
# STEP 6: USER PROMPT FOR EXTRACTION
# ==============================================================================

cat("\n\n=== EXTRACTION OPTIONS ===\n")
cat("Would you like to proceed with extraction? (requires manual confirmation)\n")
cat("\nOptions:\n")
cat("  A) Extract top layer and convert to annual means (recommended)\n")
cat("  B) Save detailed inspection report only\n")
cat("  C) Exit and review manually\n")

# Save inspection report
inspection_report <- list(
  file_path = target_file,
  file_size_gb = total_size_gb,
  phyc_variable = phyc_var,
  dimensions = data.frame(
    name = dim_names,
    length = dim_lengths
  ),
  has_depth = has_depth,
  depth_dimension = if(has_depth) depth_dim_name else NA,
  n_depth_levels = if(has_depth) n_depth_levels else NA,
  n_time_steps = n_time_steps,
  sample_data_range = range(sample_data, na.rm = TRUE),
  estimated_top_layer_size_mb = if(has_depth) estimated_top_layer_size_mb else NA
)

report_file <- file.path(output_dir, "ukesm_overshoot_2101-2300_inspection_report.rds")
saveRDS(inspection_report, report_file)
cat("\n✓ Inspection report saved:", report_file, "\n")

# Close NetCDF file
nc_close(nc)

cat("\n=== INSPECTION COMPLETE ===\n")
cat("Review the report above to determine next steps.\n")
cat("Inspection report saved to:", report_file, "\n\n")

cat("To proceed with extraction, run the companion script:\n")
cat("  Scripts/Utilities/extract_ukesm_overshoot_toplayer.R\n")
