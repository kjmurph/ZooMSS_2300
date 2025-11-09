# ================================================================
# ZooMSS 2300 - Export FishMIP Protocol Outputs to NetCDF
# ================================================================
# Converts FishMIP RDS outputs to NetCDF format per ISIMIP protocol
# Each variable saved separately with proper metadata

library(tidyverse)
library(ncdf4)

# Setup paths
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format_submission_version")
output_dir <- file.path(base_dir, "Output", "FishMIP_NetCDF_submission_version")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("==============================================================================\n")
cat("ZooMSS 2300 - FishMIP NetCDF Export\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# FishMIP variables to export
fishmip_vars <- c("tcb", "tcblog10_0", "tcblog10_1", "tcblog10_2", 
                  "tcblog10_3", "tcblog10_4", "tcblog10_5",
                  "tpb", "bp30cm", "bp30to90cm", "bp90cm")

# Variable metadata (units and long names)
var_metadata <- list(
  tcb = list(
    name = "tcb",
    longname = "Total Consumer Biomass",
    units = "g m-2"
  ),
  tcblog10_0 = list(
    name = "tcblog10_0",
    longname = "Total Consumer Biomass in log10 size bin 0 (0.1-1 g)",
    units = "g m-2"
  ),
  tcblog10_1 = list(
    name = "tcblog10_1",
    longname = "Total Consumer Biomass in log10 size bin 1 (1-10 g)",
    units = "g m-2"
  ),
  tcblog10_2 = list(
    name = "tcblog10_2",
    longname = "Total Consumer Biomass in log10 size bin 2 (10-100 g)",
    units = "g m-2"
  ),
  tcblog10_3 = list(
    name = "tcblog10_3",
    longname = "Total Consumer Biomass in log10 size bin 3 (100g-1kg)",
    units = "g m-2"
  ),
  tcblog10_4 = list(
    name = "tcblog10_4",
    longname = "Total Consumer Biomass in log10 size bin 4 (1-10 kg)",
    units = "g m-2"
  ),
  tcblog10_5 = list(
    name = "tcblog10_5",
    longname = "Total Consumer Biomass in log10 size bin 5 (10-100 kg)",
    units = "g m-2"
  ),
  tpb = list(
    name = "tpb",
    longname = "Total Pelagic Biomass",
    units = "g m-2"
  ),
  bp30cm = list(
    name = "bp30cm",
    longname = "Biomass of pelagic consumers <30cm",
    units = "g m-2"
  ),
  bp30to90cm = list(
    name = "bp30to90cm",
    longname = "Biomass of pelagic consumers 30-90cm",
    units = "g m-2"
  ),
  bp90cm = list(
    name = "bp90cm",
    longname = "Biomass of pelagic consumers >90cm",
    units = "g m-2"
  )
)

# Model name for FishMIP
model_name <- "zoomss"  # lowercase as per ISIMIP requirements

# Climate forcing identifiers
climate_forcing <- c(
  "cesm2-waccm" = "cesm2-waccm",
  "ipsl-cm6a-lr" = "ipsl-cm6a-lr",
  "ukesm1-0-ll" = "ukesm1-0-ll"
)

# Scenario identifiers
scenario_names <- c(
  "historical" = "historical",
  "picontrol" = "picontrol",
  "ssp126" = "ssp126",
  "ssp534-over" = "ssp534-over",
  "ssp585" = "ssp585"
)

# Time reference dates for different scenarios
time_references <- list(
  historical = list(ref_date = "1850-01-01", ref_year = 1850),
  picontrol = list(ref_date = "1601-01-01", ref_year = 1601),
  ssp126 = list(ref_date = "2015-01-01", ref_year = 2015),
  "ssp534-over" = list(ref_date = "2040-01-01", ref_year = 2040),  # Overshoot starts 2040
  ssp585 = list(ref_date = "2015-01-01", ref_year = 2015)
)

# Function to calculate days since reference for annual data
# Using 365-day calendar (noleap) as standard for climate models
calculate_days_since <- function(years, ref_year) {
  # For annual data on Jan 1st of each year
  # Day 1 = Jan 1 of reference year
  # Each year adds 365 days
  days <- (years - ref_year) * 365 + 1
  return(days)
}

# Function to create NetCDF file for a single variable
create_netcdf_file <- function(data, variable, model, climate_model, scenario, 
                               start_year, end_year, output_dir) {
  
  cat("\n--- Creating NetCDF for:", variable, "-", climate_model, "-", scenario, "---\n")
  
  # Get unique spatial coordinates
  unique_coords <- data %>%
    select(Lon, Lat) %>%
    distinct() %>%
    arrange(Lat, Lon)
  
  n_lon <- length(unique(unique_coords$Lon))
  n_lat <- length(unique(unique_coords$Lat))
  
  lon_vals <- sort(unique(unique_coords$Lon))
  lat_vals <- sort(unique(unique_coords$Lat))
  
  # Get time dimension
  years <- sort(unique(data$Date))
  n_time <- length(years)
  
  # Calculate time values (days since reference)
  time_ref <- time_references[[scenario]]
  time_vals <- calculate_days_since(years, time_ref$ref_year)
  
  cat("  Dimensions: Lon =", n_lon, ", Lat =", n_lat, ", Time =", n_time, "\n")
  cat("  Time range:", min(years), "to", max(years), "\n")
  cat("  Time reference:", time_ref$ref_date, "\n")
  
  # Define dimensions
  lon_dim <- ncdim_def("lon", "degrees_east", lon_vals)
  lat_dim <- ncdim_def("lat", "degrees_north", lat_vals)
  time_dim <- ncdim_def("time", 
                        paste("days since", time_ref$ref_date, "00:00:00"),
                        time_vals,
                        calendar = "365_day")
  
  # Get variable metadata
  var_meta <- var_metadata[[variable]]
  
  # Define variable
  var_def <- ncvar_def(
    name = var_meta$name,
    units = var_meta$units,
    dim = list(lon_dim, lat_dim, time_dim),
    missval = -999,
    longname = var_meta$longname,
    compression = 5  # ISIMIP requires minimum compression level 5
  )
  
  # Create array to hold data (lon x lat x time)
  data_array <- array(-999, dim = c(n_lon, n_lat, n_time))
  
  # Fill array with data
  cat("  Filling data array...\n")
  for (t in 1:n_time) {
    year_data <- data %>% filter(Date == years[t])
    
    for (i in 1:nrow(year_data)) {
      lon_idx <- which(lon_vals == year_data$Lon[i])
      lat_idx <- which(lat_vals == year_data$Lat[i])
      data_array[lon_idx, lat_idx, t] <- year_data[[variable]][i]
    }
  }
  
  # Create filename following ISIMIP pattern
  # <model>_<climate-forcing>_<climate-scenario>_<soc-scenario>_<variable>_<global>_<time-step>_<start-year>_<end-year>.nc
  filename <- paste0(
    model_name, "_",
    climate_model, "_",
    scenario, "_",
    "nat_",  # soc-scenario: natural (no human impacts in ZooMSS)
    variable, "_",
    "global_",
    "annual_",
    start_year, "_",
    end_year, ".nc"
  )
  
  filepath <- file.path(output_dir, filename)
  
  # Create NetCDF file
  cat("  Creating file:", filename, "\n")
  nc_out <- nc_create(filepath, var_def, force_v4 = TRUE)
  
  # Write data
  ncvar_put(nc_out, var_def, data_array)
  
  # Add global attributes
  ncatt_put(nc_out, 0, "title", 
            paste("ZooMSS FishMIP 2300 Protocol Output:", var_meta$longname))
  ncatt_put(nc_out, 0, "institution", "University of Queensland, Australia")
  ncatt_put(nc_out, 0, "source", "ZooMSS v1.0 - Size-structured marine ecosystem model")
  ncatt_put(nc_out, 0, "contact", "Kieran.Murphy@uq.edu.au")
  ncatt_put(nc_out, 0, "references", "FishMIP 2300 Protocol")
  ncatt_put(nc_out, 0, "comment", 
            paste("Annual mean", var_meta$longname, "from ZooMSS model forced with",
                  climate_model, scenario, "climate"))
  ncatt_put(nc_out, 0, "Conventions", "CF-1.6")
  ncatt_put(nc_out, 0, "creation_date", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  # Close file
  nc_close(nc_out)
  
  # Get file size
  file_size <- file.info(filepath)$size / 1024^2  # MB
  cat("  File created successfully! Size:", round(file_size, 2), "MB\n")
  
  return(filename)
}

# Main processing loop
cat("=== Processing FishMIP files ===\n")

# Get all FishMIP files
fishmip_files <- list.files(input_dir, pattern = "^ZooMSS_FishMIP_2300_.*\\.rds$", full.names = TRUE)
cat("Found", length(fishmip_files), "FishMIP files to process\n\n")

# Track created files
created_files <- list()

# Process each file
for (file in fishmip_files) {
  cat("\n==================================================\n")
  cat("Processing:", basename(file), "\n")
  cat("==================================================\n")
  
  # Extract model and scenario from filename
  parts <- str_match(basename(file), "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  climate_model <- parts[,2]
  scenario <- parts[,3]
  
  # Load data
  cat("Loading data...\n")
  data <- readRDS(file)
  
  # Get year range
  start_year <- min(data$Date, na.rm = TRUE)
  end_year <- max(data$Date, na.rm = TRUE)
  
  cat("Climate model:", climate_model, "\n")
  cat("Scenario:", scenario, "\n")
  cat("Year range:", start_year, "to", end_year, "\n")
  cat("Total rows:", nrow(data), "\n")
  
  # Process each variable
  for (var in fishmip_vars) {
    tryCatch({
      filename <- create_netcdf_file(
        data = data,
        variable = var,
        model = model_name,
        climate_model = climate_model,
        scenario = scenario,
        start_year = start_year,
        end_year = end_year,
        output_dir = output_dir
      )
      
      created_files[[length(created_files) + 1]] <- list(
        file = filename,
        model = climate_model,
        scenario = scenario,
        variable = var
      )
      
    }, error = function(e) {
      cat("  ERROR creating NetCDF for", var, ":", e$message, "\n")
    })
  }
  
  # Clean up
  rm(data)
  gc(verbose = FALSE)
}

# Summary
cat("\n==============================================================================\n")
cat("NetCDF Export Complete!\n")
cat("==============================================================================\n")
cat("Total files created:", length(created_files), "\n")
cat("Output directory:", output_dir, "\n\n")

# Create summary table
summary_df <- bind_rows(created_files)
summary_table <- summary_df %>%
  group_by(model, scenario) %>%
  summarise(
    n_variables = n(),
    variables = paste(variable, collapse = ", "),
    .groups = 'drop'
  )

print(summary_table)

# Save summary
write_csv(summary_df, file.path(output_dir, "netcdf_files_summary.csv"))
cat("\nSummary saved to: netcdf_files_summary.csv\n")

cat("\n==============================================================================\n")
cat("Files ready for upload to ISIMIP server:\n")
cat("/work/bb0820/scratch/FishMIP_2300_outputs\n")
cat("==============================================================================\n")
