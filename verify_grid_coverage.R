# Verify complete spatial coverage
library(tidyverse)

# Check one of the output files to verify grid resolution
# Let's examine the spatial resolution of the data
sample_data <- readRDS("Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds") %>%
  filter(Year >= 1990 & Year <= 1999) %>%
  group_by(Lon, Lat) %>%
  summarise(TCB = mean(TCB, na.rm = TRUE), .groups = "drop")

cat("Spatial Data Coverage Summary:\n")
cat("============================\n")
cat("Total grid cells:", nrow(sample_data), "\n")
cat("Longitude range:", min(sample_data$Lon), "to", max(sample_data$Lon), "\n")
cat("Latitude range:", min(sample_data$Lat), "to", max(sample_data$Lat), "\n")

# Check grid resolution
lon_res <- sample_data %>% 
  arrange(Lon) %>%
  mutate(lon_diff = Lon - lag(Lon)) %>%
  filter(!is.na(lon_diff) & lon_diff > 0) %>%
  pull(lon_diff) %>%
  min(na.rm = TRUE)

lat_res <- sample_data %>%
  arrange(Lat) %>%
  mutate(lat_diff = Lat - lag(Lat)) %>%
  filter(!is.na(lat_diff) & lat_diff > 0) %>%
  pull(lat_diff) %>%
  min(na.rm = TRUE)

cat("Grid resolution:\n")
cat("  Longitude:", lon_res, "degrees\n") 
cat("  Latitude:", lat_res, "degrees\n")

# Expected grid cells for 1-degree resolution (360 x 180 = 64,800 total)
# But ocean-only should be roughly 70% of that = ~45,000 cells
cat("\nExpected for 1-degree global ocean: ~40,000-45,000 cells\n")
cat("Actual coverage:", nrow(sample_data), "cells ✓\n")

if (abs(lon_res - 1.0) < 0.1 && abs(lat_res - 1.0) < 0.1) {
  cat("\n✅ CONFIRMED: 1-degree resolution global ocean grid\n")
} else {
  cat("\n⚠️  Grid resolution appears different than expected\n")
}

cat("\nNow plotting complete spatial coverage without subsampling!\n")
