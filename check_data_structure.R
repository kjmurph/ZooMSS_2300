# Check data structure
library(tidyverse)

# Load a sample file to see the structure
sample_data <- readRDS("Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds")

cat("Data structure:\n")
cat("Columns:", paste(names(sample_data), collapse = ", "), "\n")
cat("Dimensions:", nrow(sample_data), "rows x", ncol(sample_data), "columns\n")
cat("Sample rows:\n")
print(head(sample_data, 3))

# Check if zooplankton + fish columns exist for TCB calculation
zoop_cols <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
fish_cols <- c("Fish_Small", "Fish_Med", "Fish_Large")

missing_zoop <- setdiff(zoop_cols, names(sample_data))
missing_fish <- setdiff(fish_cols, names(sample_data))

if (length(missing_zoop) > 0) {
  cat("Missing zooplankton columns:", paste(missing_zoop, collapse = ", "), "\n")
}
if (length(missing_fish) > 0) {
  cat("Missing fish columns:", paste(missing_fish, collapse = ", "), "\n")
}

# Check coordinate columns
if ("Lon" %in% names(sample_data) & "Lat" %in% names(sample_data)) {
  # Calculate spatial resolution
  sample_subset <- sample_data %>%
    filter(Year >= 1990 & Year <= 1999) %>%
    group_by(Lon, Lat) %>%
    slice(1) %>%
    ungroup()
  
  cat("\nSpatial coverage:\n")
  cat("Grid cells:", nrow(sample_subset), "\n")
  cat("Longitude range:", min(sample_subset$Lon), "to", max(sample_subset$Lon), "\n")
  cat("Latitude range:", min(sample_subset$Lat), "to", max(sample_subset$Lat), "\n")
  
  # Check grid resolution
  lon_res <- sample_subset %>% 
    arrange(Lon) %>%
    mutate(lon_diff = Lon - lag(Lon)) %>%
    filter(!is.na(lon_diff) & lon_diff > 0) %>%
    pull(lon_diff) %>%
    min(na.rm = TRUE)
  
  lat_res <- sample_subset %>%
    arrange(Lat) %>%
    mutate(lat_diff = Lat - lag(Lat)) %>%
    filter(!is.na(lat_diff) & lat_diff > 0) %>%
    pull(lat_diff) %>%
    min(na.rm = TRUE)
  
  cat("Grid resolution:\n")
  cat("  Longitude:", lon_res, "degrees\n") 
  cat("  Latitude:", lat_res, "degrees\n")
  
  if (abs(lon_res - 1.0) < 0.1 && abs(lat_res - 1.0) < 0.1) {
    cat("\n✅ CONFIRMED: 1-degree resolution global ocean grid\n")
  } else {
    cat("\n⚠️  Grid resolution appears different than expected\n")
  }
}
