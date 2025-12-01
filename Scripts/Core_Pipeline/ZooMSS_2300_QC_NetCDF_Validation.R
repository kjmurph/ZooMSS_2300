# ============================================================================
# ZooMSS 2300 - NetCDF QC Validation Script
# Validates NetCDF files against ISIMIP3b protocol requirements
# ============================================================================

library(ncdf4)
library(tidyverse)

cat("============================================================\n")
cat("ISIMIP QC Validation for ZooMSS NetCDF Files\n")
cat("============================================================\n\n")

base_dir <- "Output/FishMIP_NetCDF_v2_ISIMIP_compliant"
files <- list.files(base_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
cat("Total files found:", length(files), "\n\n")

# Initialize counters
total_errors <- 0
total_warnings <- 0
file_results <- list()

# ISIMIP3b Requirements
REQUIRED_FILL_VALUE <- 1e20
REQUIRED_LAT_COUNT <- 180
REQUIRED_LON_COUNT <- 360
REQUIRED_LAT_RANGE <- c(-89.5, 89.5)
REQUIRED_LON_RANGE <- c(-179.5, 179.5)

# Check all files
for (i in seq_along(files)) {
  f <- files[i]
  fname <- basename(f)
  errors <- c()
  warnings <- c()
  
  tryCatch({
    nc <- nc_open(f)
    
    # Extract variable name from filename
    # Pattern: zoomss_esm_nobasd_scenario_nat_default_VARNAME_global_annual_start_end.nc
    parts <- strsplit(fname, "_")[[1]]
    var_name <- parts[7]  # Variable is 7th element
    
    # === Check Dimensions ===
    dims <- names(nc$dim)
    
    # Check lat
    if ("lat" %in% dims) {
      lat <- ncvar_get(nc, "lat")
      if (length(lat) != REQUIRED_LAT_COUNT) {
        errors <- c(errors, paste0("Lat count: ", length(lat), " (expected ", REQUIRED_LAT_COUNT, ")"))
      }
      if (lat[1] < lat[length(lat)]) {
        errors <- c(errors, "Lat order: S to N (should be N to S)")
      }
      if (min(lat) != REQUIRED_LAT_RANGE[1] || max(lat) != REQUIRED_LAT_RANGE[2]) {
        warnings <- c(warnings, paste0("Lat range: ", min(lat), " to ", max(lat)))
      }
      
      # Check lat attributes
      lat_axis <- ncatt_get(nc, "lat", "axis")
      lat_std <- ncatt_get(nc, "lat", "standard_name")
      if (!lat_axis$hasatt) errors <- c(errors, "Missing lat axis attribute")
      if (!lat_std$hasatt) errors <- c(errors, "Missing lat standard_name")
    } else {
      errors <- c(errors, "Missing lat dimension")
    }
    
    # Check lon
    if ("lon" %in% dims) {
      lon <- ncvar_get(nc, "lon")
      if (length(lon) != REQUIRED_LON_COUNT) {
        errors <- c(errors, paste0("Lon count: ", length(lon), " (expected ", REQUIRED_LON_COUNT, ")"))
      }
      
      # Check lon attributes
      lon_axis <- ncatt_get(nc, "lon", "axis")
      lon_std <- ncatt_get(nc, "lon", "standard_name")
      if (!lon_axis$hasatt) errors <- c(errors, "Missing lon axis attribute")
      if (!lon_std$hasatt) errors <- c(errors, "Missing lon standard_name")
    } else {
      errors <- c(errors, "Missing lon dimension")
    }
    
    # Check time
    if ("time" %in% dims) {
      time <- ncvar_get(nc, "time")
      time_axis <- ncatt_get(nc, "time", "axis")
      time_units <- ncatt_get(nc, "time", "units")
      time_calendar <- ncatt_get(nc, "time", "calendar")
      
      if (!time_axis$hasatt) errors <- c(errors, "Missing time axis attribute")
      if (!time_units$hasatt) errors <- c(errors, "Missing time units")
      if (!time_calendar$hasatt) errors <- c(errors, "Missing time calendar")
      
      # Check time reference (should be 1601-01-01 for ISIMIP3b)
      if (time_units$hasatt && !grepl("1601-01-01", time_units$value)) {
        warnings <- c(warnings, paste0("Time units: ", time_units$value))
      }
    } else {
      errors <- c(errors, "Missing time dimension")
    }
    
    # Check bins dimension for tcblog10
    if (var_name == "tcblog10") {
      if (!"bins" %in% dims) {
        errors <- c(errors, "Missing bins dimension for tcblog10")
      } else {
        bins_axis <- ncatt_get(nc, "bins", "axis")
        if (!bins_axis$hasatt) errors <- c(errors, "Missing bins axis attribute")
      }
    }
    
    # === Check Variable Attributes ===
    if (var_name %in% names(nc$var)) {
      var <- nc$var[[var_name]]
      
      # Fill value
      fv <- ncatt_get(nc, var_name, "_FillValue")
      mv <- ncatt_get(nc, var_name, "missing_value")
      
      if (!fv$hasatt) {
        errors <- c(errors, "Missing _FillValue")
      } else if (fv$value != REQUIRED_FILL_VALUE) {
        errors <- c(errors, paste0("Wrong _FillValue: ", fv$value, " (expected ", REQUIRED_FILL_VALUE, ")"))
      }
      
      if (!mv$hasatt) {
        errors <- c(errors, "Missing missing_value attribute")
      }
      
      # Long name
      ln <- ncatt_get(nc, var_name, "long_name")
      if (!ln$hasatt) errors <- c(errors, "Missing long_name")
      
      # Units
      units <- ncatt_get(nc, var_name, "units")
      if (!units$hasatt) errors <- c(errors, "Missing units")
    } else {
      errors <- c(errors, paste0("Variable '", var_name, "' not found in file"))
    }
    
    # === Check Global Attributes ===
    title <- ncatt_get(nc, 0, "title")
    source <- ncatt_get(nc, 0, "source")
    contact <- ncatt_get(nc, 0, "contact")
    institution <- ncatt_get(nc, 0, "institution")
    
    if (!title$hasatt) errors <- c(errors, "Missing global title")
    if (!source$hasatt) errors <- c(errors, "Missing global source")
    if (!contact$hasatt) warnings <- c(warnings, "Missing global contact")
    if (!institution$hasatt) warnings <- c(warnings, "Missing global institution")
    
    nc_close(nc)
    
  }, error = function(e) {
    errors <- c(errors, paste0("File read error: ", e$message))
  })
  
  # Store results
  file_results[[fname]] <- list(
    errors = errors,
    warnings = warnings
  )
  
  total_errors <- total_errors + length(errors)
  total_warnings <- total_warnings + length(warnings)
  
  # Print progress for files with issues
  if (length(errors) > 0 || length(warnings) > 0) {
    cat("\n", fname, "\n")
    if (length(errors) > 0) {
      for (e in errors) cat("  ERROR:", e, "\n")
    }
    if (length(warnings) > 0) {
      for (w in warnings) cat("  WARNING:", w, "\n")
    }
  }
  
  # Progress indicator
  if (i %% 30 == 0) {
    cat("Checked", i, "of", length(files), "files...\n")
  }
}

# === Summary ===
cat("\n============================================================\n")
cat("QC VALIDATION SUMMARY\n")
cat("============================================================\n")
cat("Total files checked:", length(files), "\n")
cat("Total errors:", total_errors, "\n")
cat("Total warnings:", total_warnings, "\n")
cat("Files with errors:", sum(sapply(file_results, function(x) length(x$errors) > 0)), "\n")
cat("Files with warnings:", sum(sapply(file_results, function(x) length(x$warnings) > 0)), "\n")

if (total_errors == 0) {
  cat("\n*** ALL FILES PASSED QC VALIDATION! ***\n")
} else {
  cat("\n*** Some files have errors - review above output ***\n")
}

cat("============================================================\n")

# === Detailed check of one file per variable type ===
cat("\n\n=== DETAILED SAMPLE CHECK ===\n")

sample_files <- c(
  tcb = files[grep("_tcb_", files)[1]],
  bp30cm = files[grep("_bp30cm_", files)[1]],
  bp30to90cm = files[grep("_bp30to90cm_", files)[1]],
  bp90cm = files[grep("_bp90cm_", files)[1]],
  tcblog10 = files[grep("_tcblog10_", files)[1]]
)

for (var_name in names(sample_files)) {
  f <- sample_files[var_name]
  cat("\n---", var_name, ":", basename(f), "---\n")
  
  nc <- nc_open(f)
  
  # Dimensions
  cat("Dimensions:", paste(names(nc$dim), collapse = ", "), "\n")
  
  # Lat/Lon
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  time <- ncvar_get(nc, "time")
  cat("Lat:", length(lat), "values from", lat[1], "to", lat[length(lat)], "\n")
  cat("Lon:", length(lon), "values from", lon[1], "to", lon[length(lon)], "\n")
  cat("Time:", length(time), "values from", min(time), "to", max(time), "\n")
  
  # Time units
  time_units <- ncatt_get(nc, "time", "units")
  cat("Time units:", time_units$value, "\n")
  
  # Variable info
  if (var_name %in% names(nc$var)) {
    var <- nc$var[[var_name]]
    cat("Variable dims:", paste(sapply(var$dim, function(d) d$name), collapse = " x "), "\n")
    
    fv <- ncatt_get(nc, var_name, "_FillValue")
    ln <- ncatt_get(nc, var_name, "long_name")
    units <- ncatt_get(nc, var_name, "units")
    
    cat("_FillValue:", fv$value, "\n")
    cat("long_name:", ln$value, "\n")
    cat("units:", units$value, "\n")
    
    # Get data sample
    data <- ncvar_get(nc, var_name)
    valid_data <- data[!is.na(data) & data != fv$value]
    if (length(valid_data) > 0) {
      cat("Data range:", round(min(valid_data), 4), "to", round(max(valid_data), 4), "\n")
      cat("Data mean:", round(mean(valid_data), 4), "\n")
    }
  }
  
  # Global attrs
  title <- ncatt_get(nc, 0, "title")
  cat("Title:", substr(title$value, 1, 60), "...\n")
  
  nc_close(nc)
}

cat("\n============================================================\n")
cat("QC Validation Complete\n")
cat("============================================================\n")
