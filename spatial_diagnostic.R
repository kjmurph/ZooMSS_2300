# Quick diagnostic for spatial plotting issues
library(tidyverse)

cat("=== SPATIAL DATA DIAGNOSTIC ===\n")

# Check one SSP585 file
file_path <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_ssp585_Control.rds"
cat("Loading:", basename(file_path), "\n")

data <- readRDS(file_path)
cat("Total rows:", nrow(data), "\n")
cat("Year range:", min(data$Year), "to", max(data$Year), "\n")

# Check specific time periods
cat("\nTime period availability:\n")
cat("1990-1999:", sum(data$Year >= 1990 & data$Year <= 1999), "rows\n")
cat("2090-2099:", sum(data$Year >= 2090 & data$Year <= 2099), "rows\n") 
cat("2290-2299:", sum(data$Year >= 2290 & data$Year <= 2299), "rows\n")

# Check a few sample biomass values
cat("\nSample biomass values:\n")
sample_data <- data %>% slice_head(n = 5)
print(sample_data %>% select(Year, Lon, Lat, Fish_Small, Fish_Med, Fish_Large))

# Test biomass calculation
test_calc <- data %>%
  slice_head(n = 1000) %>%
  mutate(
    Fish_Total = Fish_Small + Fish_Med + Fish_Large,
    TCB_test = Fish_Small + Fish_Med + Fish_Large + Flagellates + Ciliates
  )

cat("\nBiomass calculation test:\n")
cat("Fish_Total range:", min(test_calc$Fish_Total, na.rm=TRUE), "to", max(test_calc$Fish_Total, na.rm=TRUE), "\n")
cat("TCB_test range:", min(test_calc$TCB_test, na.rm=TRUE), "to", max(test_calc$TCB_test, na.rm=TRUE), "\n")

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
