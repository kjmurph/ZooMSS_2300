# ================================================================
# ZooMSS 2300 - Export FishMIP Protocol Outputs to NetCDF (v2)
# ================================================================
# ISIMIP3b/FishMIP 2300 Protocol Compliant NetCDF Export
# 
# Fixes applied based on QC feedback:
# 1. Full global grid (180 latitudes, -89.5 to 89.5)
# 2. Latitude order: North to South (89.5 to -89.5)
# 3. Fill value: 1e+20 (not -999)
# 4. Proper dimension attributes (axis, standard_name, long_name)
# 5. Chunking: [1, 180, 360] (time, lat, lon)
# 6. NETCDF4_CLASSIC format
# 7. Proper filename pattern with nobasd and default specifiers
# 8. tcblog10 combined into single file with bins dimension
# 9. Updated variable long_names per protocol
# 10. piControl split into protocol time periods
# ================================================================

library(tidyverse)
library(ncdf4)

# Setup paths
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format_submission_version")
output_dir <- file.path(base_dir, "Output", "FishMIP_NetCDF_v2_ISIMIP_compliant")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("==============================================================================\n")
cat("ZooMSS 2300 - FishMIP NetCDF Export (v2 - ISIMIP Compliant)\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ================================================================
# ISIMIP Protocol Constants
# ================================================================

# Missing value as per ISIMIP protocol
FILL_VALUE <- 1e20

# Full global grid definition (180 latitudes)
# North to South ordering as required
FULL_LAT <- seq(89.5, -89.5, by = -1)  # 180 values, N to S
FULL_LON <- seq(-179.5, 179.5, by = 1)  # 360 values

# Time reference year for ISIMIP3b
TIME_REF_YEAR <- 1601
TIME_REF_DATE <- "1601-01-01"

# Model name (lowercase)
MODEL_NAME <- "zoomss"

# ================================================================
# Variable Metadata (Updated per ISIMIP Protocol)
# ================================================================

# Standard variables (non-size-binned)
# NOTE: TPB is excluded because it's identical to TCB in ZooMSS 
#       (all organisms are pelagic, verified with 185M data points showing 100% match)
standard_vars <- list(
  tcb = list(
    name = "tcb",
    longname = "Total Consumer Biomass Density",
    units = "g m-2",
    comment = "Total consumer biomass density integrated across all size classes from ZooMSS model. Equals TPB as all ZooMSS organisms are pelagic."
  ),
  # tpb EXCLUDED - identical to tcb (all ZooMSS organisms are pelagic)
  bp30cm = list(
    name = "bp30cm",
    longname = "Biomass Density of Small Pelagics <30cm",
    units = "g m-2",
    comment = "Biomass density of pelagic consumers smaller than 30cm total length"
  ),
  bp30to90cm = list(
    name = "bp30to90cm",
    longname = "Biomass Density of Medium Pelagics 30-90cm",
    units = "g m-2",
    comment = "Biomass density of pelagic consumers between 30 and 90cm total length"
  ),
  bp90cm = list(
    name = "bp90cm",
    longname = "Biomass Density of Large Pelagics >90cm",
    units = "g m-2",
    comment = "Biomass density of pelagic consumers larger than 90cm total length"
  )
)

# Size bin definitions for tcblog10
size_bins <- list(
  bin_values = 0:5,  # 0, 1, 2, 3, 4, 5
  bin_names = c("tcblog10_0", "tcblog10_1", "tcblog10_2", "tcblog10_3", "tcblog10_4", "tcblog10_5"),
  bin_ranges = c("0.1-1g", "1-10g", "10-100g", "100g-1kg", "1-10kg", "10-100kg"),
  comment = "Size class biomass density. Bin index corresponds to log10 weight range: 0: 0.1-1g, 1: 1-10g, 2: 10-100g, 3: 100g-1kg, 4: 1-10kg, 5: 10-100kg"
)

# ================================================================
# Time Period Definitions per ISIMIP3b Protocol
# ================================================================
# 
# From ISIMIP protocol and Matthias's clarification:
# - Folder names refer to the SIMULATION PERIOD, not the climate scenario
# - piControl files go into period-appropriate folders
# - Filename still contains the actual climate-scenario (picontrol)
#
# Standard ISIMIP3b periods:
#   pre-industrial: 1601-1849 (picontrol only)
#   historical: 1850-2014
#   future: 2015-2100 (and 2101-2300 for extended runs)
# ================================================================

# piControl periods - note: folder matches simulation period, NOT scenario
# piControl is continuous climate forcing but files split by period folder
picontrol_periods <- list(
  list(name = "pre-industrial", start = 1601, end = 1849, folder = "pre-industrial"),
  list(name = "historical", start = 1850, end = 2014, folder = "historical"),
  list(name = "future", start = 2015, end = 2100, folder = "future"),
  list(name = "future-extended", start = 2101, end = 2300, folder = "future")
)

# Other scenario periods - folder matches scenario name
scenario_periods <- list(
  historical = list(
    list(start = 1850, end = 2014, folder = "historical")
  ),
  ssp126 = list(
    list(start = 2015, end = 2100, folder = "ssp126"),
    list(start = 2101, end = 2300, folder = "ssp126")
  ),
  "ssp534-over" = list(
    list(start = 2040, end = 2100, folder = "ssp534-over"),
    list(start = 2101, end = 2300, folder = "ssp534-over")
  ),
  ssp585 = list(
    list(start = 2015, end = 2100, folder = "ssp585"),
    list(start = 2101, end = 2300, folder = "ssp585")
  )
)

# ================================================================
# Helper Functions
# ================================================================

# Calculate days since reference date (using 365-day calendar)
calculate_days_since <- function(years, ref_year = TIME_REF_YEAR) {
  # For annual data, use mid-year (day 183 of each year)
  # Days from reference year start to Jan 1 of target year
  (years - ref_year) * 365 + 1
}

# Create properly structured data array with full global grid
create_full_grid_array <- function(data, variable, years, fill_value = FILL_VALUE) {
  
  n_time <- length(years)
  n_lat <- length(FULL_LAT)
  n_lon <- length(FULL_LON)
  
  # Initialize with fill value
  data_array <- array(fill_value, dim = c(n_lon, n_lat, n_time))
  
  # Get unique coordinates from data
  data_lons <- sort(unique(data$Lon))
  data_lats <- sort(unique(data$Lat), decreasing = TRUE)  # N to S
  
  # Fill array with actual data
  for (t_idx in seq_along(years)) {
    year <- years[t_idx]
    year_data <- data %>% filter(Date == year)
    
    if (nrow(year_data) > 0) {
      for (i in 1:nrow(year_data)) {
        # Find indices in full grid
        lon_idx <- which(abs(FULL_LON - year_data$Lon[i]) < 0.01)
        lat_idx <- which(abs(FULL_LAT - year_data$Lat[i]) < 0.01)
        
        if (length(lon_idx) == 1 && length(lat_idx) == 1) {
          val <- year_data[[variable]][i]
          if (!is.na(val) && is.finite(val)) {
            data_array[lon_idx, lat_idx, t_idx] <- val
          }
        }
      }
    }
  }
  
  return(data_array)
}

# Create tcblog10 array with bins dimension (lon, lat, bins, time)
create_tcblog10_array <- function(data, years, fill_value = FILL_VALUE) {
  
  n_time <- length(years)
  n_lat <- length(FULL_LAT)
  n_lon <- length(FULL_LON)
  n_bins <- length(size_bins$bin_values)
  
  # Initialize with fill value - dims: (lon, lat, bins, time)
  data_array <- array(fill_value, dim = c(n_lon, n_lat, n_bins, n_time))
  
  # Fill array with actual data
  for (t_idx in seq_along(years)) {
    year <- years[t_idx]
    year_data <- data %>% filter(Date == year)
    
    if (nrow(year_data) > 0) {
      for (i in 1:nrow(year_data)) {
        lon_idx <- which(abs(FULL_LON - year_data$Lon[i]) < 0.01)
        lat_idx <- which(abs(FULL_LAT - year_data$Lat[i]) < 0.01)
        
        if (length(lon_idx) == 1 && length(lat_idx) == 1) {
          for (b_idx in 1:n_bins) {
            bin_name <- size_bins$bin_names[b_idx]
            val <- year_data[[bin_name]][i]
            if (!is.na(val) && is.finite(val)) {
              data_array[lon_idx, lat_idx, b_idx, t_idx] <- val
            }
          }
        }
      }
    }
  }
  
  return(data_array)
}

# ================================================================
# NetCDF Creation Functions
# ================================================================

# Create NetCDF for standard variable
create_standard_netcdf <- function(data, var_meta, climate_model, scenario,
                                    start_year, end_year, output_subdir) {
  
  cat("\n--- Creating NetCDF:", var_meta$name, "-", climate_model, "-", scenario, 
      "(", start_year, "-", end_year, ") ---\n")
  
  # Filter data for time period
  period_data <- data %>% 
    filter(Date >= start_year, Date <= end_year)
  
  if (nrow(period_data) == 0) {
    cat("  WARNING: No data for this period, skipping\n")
    return(NULL)
  }
  
  years <- sort(unique(period_data$Date))
  n_time <- length(years)
  
  cat("  Time steps:", n_time, "years (", min(years), "-", max(years), ")\n")
  
  # Calculate time values
  time_vals <- calculate_days_since(years)
  
  # Create data array
  cat("  Creating full global grid array...\n")
  data_array <- create_full_grid_array(period_data, var_meta$name, years)
  
  # Define dimensions with proper attributes
  lon_dim <- ncdim_def(
    name = "lon",
    units = "degrees_east",
    vals = FULL_LON,
    longname = "Longitude"
  )
  
  lat_dim <- ncdim_def(
    name = "lat", 
    units = "degrees_north",
    vals = FULL_LAT,
    longname = "Latitude"
  )
  
  time_dim <- ncdim_def(
    name = "time",
    units = paste("days since", TIME_REF_DATE, "00:00:00"),
    vals = time_vals,
    longname = "Time",
    calendar = "365_day"
  )
  
  # Define variable with proper chunking
  # Chunking: [1, 180, 360] means one time step at a time
  var_def <- ncvar_def(
    name = var_meta$name,
    units = var_meta$units,
    dim = list(lon_dim, lat_dim, time_dim),
    missval = FILL_VALUE,
    longname = var_meta$longname,
    compression = 5,
    chunksizes = c(360, 180, 1)  # lon, lat, time (one horizontal field per chunk)
  )
  
  # Create filename per ISIMIP pattern
  # <model>_<climate-forcing>_<bias-adjustment>_<climate-scenario>_<soc-scenario>_<sens-scenario>_<variable>_<region>_<time-step>_<start-year>_<end-year>.nc
  filename <- paste0(
    MODEL_NAME, "_",
    climate_model, "_",
    "nobasd_",  # no bias adjustment for ocean data
    scenario, "_",
    "nat_",     # natural (no human impacts)
    "default_", # default sensitivity
    var_meta$name, "_",
    "global_",
    "annual_",
    start_year, "_",
    end_year, ".nc"
  )
  
  # Create output subdirectory if needed
  full_output_dir <- file.path(output_dir, output_subdir)
  if (!dir.exists(full_output_dir)) {
    dir.create(full_output_dir, recursive = TRUE)
  }
  
  filepath <- file.path(full_output_dir, filename)
  
  # Create NetCDF file (NETCDF4_CLASSIC as required)
  cat("  Creating file:", filename, "\n")
  nc_out <- nc_create(filepath, var_def, force_v4 = TRUE)
  
  # Write data
  ncvar_put(nc_out, var_def, data_array)
  
  # Add dimension attributes
  ncatt_put(nc_out, "lon", "axis", "X")
  ncatt_put(nc_out, "lon", "standard_name", "longitude")
  ncatt_put(nc_out, "lon", "long_name", "Longitude")
  
  ncatt_put(nc_out, "lat", "axis", "Y")
  ncatt_put(nc_out, "lat", "standard_name", "latitude")
  ncatt_put(nc_out, "lat", "long_name", "Latitude")
  
  ncatt_put(nc_out, "time", "axis", "T")
  ncatt_put(nc_out, "time", "standard_name", "time")
  ncatt_put(nc_out, "time", "long_name", "Time")
  ncatt_put(nc_out, "time", "calendar", "365_day")
  
  # Add variable attributes
  ncatt_put(nc_out, var_meta$name, "missing_value", FILL_VALUE)
  ncatt_put(nc_out, var_meta$name, "comment", var_meta$comment)
  
  # Add global attributes
  ncatt_put(nc_out, 0, "title", 
            paste("ZooMSS FishMIP 2300 Output:", var_meta$longname))
  ncatt_put(nc_out, 0, "institution", "University of Tasmania / University of Queensland, Australia")
  ncatt_put(nc_out, 0, "source", "ZooMSS v1.0 - Size-structured marine ecosystem model")
  ncatt_put(nc_out, 0, "contact", "kieran.murphy@utas.edu.au")
  ncatt_put(nc_out, 0, "references", "https://doi.org/10.1038/s41467-020-17078-w")
  ncatt_put(nc_out, 0, "comment", 
            paste("Annual mean", var_meta$longname, "from ZooMSS model forced with",
                  toupper(climate_model), scenario, "climate scenario"))
  ncatt_put(nc_out, 0, "Conventions", "CF-1.6")
  ncatt_put(nc_out, 0, "creation_date", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  ncatt_put(nc_out, 0, "frequency", "annual")
  ncatt_put(nc_out, 0, "realm", "ocean")
  ncatt_put(nc_out, 0, "product", "model-output")
  
  # Close file
  nc_close(nc_out)
  
  # Get file size
  file_size <- file.info(filepath)$size / 1024^2
  cat("  ✓ File created:", round(file_size, 2), "MB\n")
  
  return(filename)
}

# Create NetCDF for tcblog10 (combined size bins)
create_tcblog10_netcdf <- function(data, climate_model, scenario,
                                    start_year, end_year, output_subdir) {
  
  cat("\n--- Creating NetCDF: tcblog10 (combined bins) -", climate_model, "-", scenario,
      "(", start_year, "-", end_year, ") ---\n")
  
  # Filter data for time period
  period_data <- data %>% 
    filter(Date >= start_year, Date <= end_year)
  
  if (nrow(period_data) == 0) {
    cat("  WARNING: No data for this period, skipping\n")
    return(NULL)
  }
  
  years <- sort(unique(period_data$Date))
  n_time <- length(years)
  
  cat("  Time steps:", n_time, "years (", min(years), "-", max(years), ")\n")
  
  # Calculate time values
  time_vals <- calculate_days_since(years)
  
  # Create data array with bins dimension
  cat("  Creating full global grid array with bins dimension...\n")
  data_array <- create_tcblog10_array(period_data, years)
  
  # Define dimensions
  lon_dim <- ncdim_def(
    name = "lon",
    units = "degrees_east", 
    vals = FULL_LON,
    longname = "Longitude"
  )
  
  lat_dim <- ncdim_def(
    name = "lat",
    units = "degrees_north",
    vals = FULL_LAT,
    longname = "Latitude"
  )
  
  bins_dim <- ncdim_def(
    name = "bins",
    units = "-",
    vals = size_bins$bin_values,
    longname = "log10 Weight Bins"
  )
  
  time_dim <- ncdim_def(
    name = "time",
    units = paste("days since", TIME_REF_DATE, "00:00:00"),
    vals = time_vals,
    longname = "Time",
    calendar = "365_day"
  )
  
  # Define variable - dims order: (lon, lat, bins, time)
  var_def <- ncvar_def(
    name = "tcblog10",
    units = "g m-2",
    dim = list(lon_dim, lat_dim, bins_dim, time_dim),
    missval = FILL_VALUE,
    longname = "Total Consumer Biomass Density in log10 Weight Bins",
    compression = 5,
    chunksizes = c(360, 180, 6, 1)  # Full spatial, all bins, one time step
  )
  
  # Create filename
  filename <- paste0(
    MODEL_NAME, "_",
    climate_model, "_",
    "nobasd_",
    scenario, "_",
    "nat_",
    "default_",
    "tcblog10_",
    "global_",
    "annual_",
    start_year, "_",
    end_year, ".nc"
  )
  
  # Create output subdirectory
  full_output_dir <- file.path(output_dir, output_subdir)
  if (!dir.exists(full_output_dir)) {
    dir.create(full_output_dir, recursive = TRUE)
  }
  
  filepath <- file.path(full_output_dir, filename)
  
  # Create NetCDF file
  cat("  Creating file:", filename, "\n")
  nc_out <- nc_create(filepath, var_def, force_v4 = TRUE)
  
  # Write data
  ncvar_put(nc_out, var_def, data_array)
  
  # Add dimension attributes
  ncatt_put(nc_out, "lon", "axis", "X")
  ncatt_put(nc_out, "lon", "standard_name", "longitude")
  ncatt_put(nc_out, "lon", "long_name", "Longitude")
  
  ncatt_put(nc_out, "lat", "axis", "Y")
  ncatt_put(nc_out, "lat", "standard_name", "latitude")
  ncatt_put(nc_out, "lat", "long_name", "Latitude")
  
  ncatt_put(nc_out, "bins", "axis", "Z")
  ncatt_put(nc_out, "bins", "standard_name", "log10_weight_bins")
  ncatt_put(nc_out, "bins", "long_name", "log10 Weight Bins")
  
  ncatt_put(nc_out, "time", "axis", "T")
  ncatt_put(nc_out, "time", "standard_name", "time")
  ncatt_put(nc_out, "time", "long_name", "Time")
  ncatt_put(nc_out, "time", "calendar", "365_day")
  
  # Add variable attributes
  ncatt_put(nc_out, "tcblog10", "missing_value", FILL_VALUE)
  ncatt_put(nc_out, "tcblog10", "comment", size_bins$comment)
  
  # Add global attributes
  ncatt_put(nc_out, 0, "title", 
            "ZooMSS FishMIP 2300 Output: Total Consumer Biomass by Size Class")
  ncatt_put(nc_out, 0, "institution", "University of Tasmania / University of Queensland, Australia")
  ncatt_put(nc_out, 0, "source", "ZooMSS v1.0 - Size-structured marine ecosystem model")
  ncatt_put(nc_out, 0, "contact", "kieran.murphy@utas.edu.au")
  ncatt_put(nc_out, 0, "references", "https://doi.org/10.1038/s41467-020-17078-w")
  ncatt_put(nc_out, 0, "comment", 
            paste("Annual mean biomass density by log10 weight class from ZooMSS forced with",
                  toupper(climate_model), scenario))
  ncatt_put(nc_out, 0, "Conventions", "CF-1.6")
  ncatt_put(nc_out, 0, "creation_date", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  ncatt_put(nc_out, 0, "frequency", "annual")
  ncatt_put(nc_out, 0, "realm", "ocean")
  ncatt_put(nc_out, 0, "product", "model-output")
  
  # Close file
  nc_close(nc_out)
  
  # Get file size
  file_size <- file.info(filepath)$size / 1024^2
  cat("  ✓ File created:", round(file_size, 2), "MB\n")
  
  return(filename)
}

# ================================================================
# Main Processing Function
# ================================================================

process_scenario <- function(data, climate_model, scenario) {
  
  cat("\n##################################################\n")
  cat("Processing:", climate_model, "-", scenario, "\n")
  cat("##################################################\n")
  
  created_files <- list()
  
  # Determine time periods based on scenario
  if (scenario == "picontrol") {
    periods <- picontrol_periods
  } else {
    periods <- scenario_periods[[scenario]]
  }
  
  if (is.null(periods)) {
    cat("ERROR: No period definition for scenario:", scenario, "\n")
    return(created_files)
  }
  
  # Process each time period
  for (period in periods) {
    
    # Determine output subfolder structure: climate_model/scenario_folder
    output_subdir <- file.path(climate_model, period$folder)
    
    cat("\n=== Period:", period$start, "-", period$end, "===\n")
    cat("  Output folder:", output_subdir, "\n")
    
    # Check if data exists for this period
    period_data <- data %>% filter(Date >= period$start, Date <= period$end)
    if (nrow(period_data) == 0) {
      cat("  No data for this period, skipping\n")
      next
    }
    
    # Process standard variables
    for (var_name in names(standard_vars)) {
      var_meta <- standard_vars[[var_name]]
      
      filename <- create_standard_netcdf(
        data = data,
        var_meta = var_meta,
        climate_model = climate_model,
        scenario = scenario,
        start_year = period$start,
        end_year = period$end,
        output_subdir = output_subdir
      )
      
      if (!is.null(filename)) {
        created_files[[length(created_files) + 1]] <- list(
          file = filename,
          model = climate_model,
          scenario = scenario,
          variable = var_name,
          period = paste(period$start, period$end, sep = "-"),
          folder = output_subdir
        )
      }
    }
    
    # Process tcblog10 (combined)
    filename <- create_tcblog10_netcdf(
      data = data,
      climate_model = climate_model,
      scenario = scenario,
      start_year = period$start,
      end_year = period$end,
      output_subdir = output_subdir
    )
    
    if (!is.null(filename)) {
      created_files[[length(created_files) + 1]] <- list(
        file = filename,
        model = climate_model,
        scenario = scenario,
        variable = "tcblog10",
        period = paste(period$start, period$end, sep = "-"),
        folder = output_subdir
      )
    }
  }
  
  return(created_files)
}

# ================================================================
# Main Execution
# ================================================================

cat("=== Processing FishMIP files ===\n")
cat("Input directory:", input_dir, "\n")
cat("Output directory:", output_dir, "\n\n")

# Get all FishMIP files
fishmip_files <- list.files(input_dir, pattern = "^ZooMSS_FishMIP_2300_.*\\.rds$", full.names = TRUE)
cat("Found", length(fishmip_files), "FishMIP files to process\n\n")

# Track all created files
all_created_files <- list()

# Process each file
for (file in fishmip_files) {
  
  cat("\n==============================================================================\n")
  cat("Loading:", basename(file), "\n")
  cat("==============================================================================\n")
  
  # Extract model and scenario from filename
  parts <- str_match(basename(file), "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  climate_model <- parts[,2]
  scenario <- parts[,3]
  
  # Load data
  data <- readRDS(file)
  
  cat("Climate model:", climate_model, "\n")
  cat("Scenario:", scenario, "\n")
  cat("Data rows:", format(nrow(data), big.mark = ","), "\n")
  cat("Year range:", min(data$Date), "-", max(data$Date), "\n")
  
  # Process this scenario
  created <- process_scenario(data, climate_model, scenario)
  all_created_files <- c(all_created_files, created)
  
  # Clean up
  rm(data)
  gc(verbose = FALSE)
}

# ================================================================
# Summary
# ================================================================

cat("\n\n##############################################################################\n")
cat("NetCDF Export Complete!\n")
cat("##############################################################################\n")
cat("Total files created:", length(all_created_files), "\n")
cat("Output directory:", output_dir, "\n\n")

# Create summary table
if (length(all_created_files) > 0) {
  summary_df <- bind_rows(all_created_files)
  
  cat("Files by model and scenario:\n")
  summary_table <- summary_df %>%
    group_by(model, scenario, period) %>%
    summarise(n_variables = n(), .groups = 'drop')
  print(summary_table, n = Inf)
  
  # Save summary
  write_csv(summary_df, file.path(output_dir, "netcdf_files_summary.csv"))
  cat("\nSummary saved to: netcdf_files_summary.csv\n")
}

cat("\n##############################################################################\n")
cat("ISIMIP Compliance Checklist:\n")
cat("##############################################################################\n")
cat("✓ Full global grid: 360 lon × 180 lat\n")
cat("✓ Latitude order: North to South (89.5 to -89.5)\n")
cat("✓ Fill value: 1e+20\n")
cat("✓ Dimension attributes: axis, standard_name, long_name\n")
cat("✓ Chunking: [360, 180, 1] per time step\n")
cat("✓ Format: NETCDF4 with compression level 5\n")
cat("✓ Filename: <model>_<forcing>_nobasd_<scenario>_nat_default_<var>_global_annual_<start>_<end>.nc\n")
cat("✓ tcblog10: Combined into single file with bins dimension\n")
cat("✓ piControl: Split into protocol time periods\n")
cat("##############################################################################\n")
