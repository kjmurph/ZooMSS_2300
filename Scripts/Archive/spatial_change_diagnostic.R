# Focused diagnostic for spatial change calculations
library(tidyverse)

cat("=== SPATIAL CHANGE DATA DIAGNOSTIC ===\n")

# Load historical data (1990-1999)
hist_file <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds"
cat("Loading historical data...\n")
hist_data <- readRDS(hist_file)

# Filter to 1990-1999
hist_1990s <- hist_data %>% filter(Year >= 1990 & Year <= 1999)
cat("Historical 1990-1999 rows:", nrow(hist_1990s), "\n")

# Calculate historical spatial means
hist_spatial <- hist_1990s %>%
  mutate(
    Fish_Total = Fish_Small + Fish_Med + Fish_Large,
    TCB = Fish_Small + Fish_Med + Fish_Large + Flagellates + Ciliates + Larvaceans + 
          OmniCopepods + CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish
  ) %>%
  group_by(Lon, Lat) %>%
  summarise(
    TCB_hist = mean(TCB, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Historical spatial summary - unique locations:", nrow(hist_spatial), "\n")
cat("Historical TCB range:", min(hist_spatial$TCB_hist, na.rm=TRUE), "to", max(hist_spatial$TCB_hist, na.rm=TRUE), "\n")

# Load future data (2290-2299) 
fut_file <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_ssp585_Control.rds"
cat("\nLoading future SSP585 data...\n")
fut_data <- readRDS(fut_file)

# Filter to 2290-2299
fut_2290s <- fut_data %>% filter(Year >= 2290 & Year <= 2299)
cat("Future 2290-2299 rows:", nrow(fut_2290s), "\n")

# Calculate future spatial means
fut_spatial <- fut_2290s %>%
  mutate(
    Fish_Total = Fish_Small + Fish_Med + Fish_Large,
    TCB = Fish_Small + Fish_Med + Fish_Large + Flagellates + Ciliates + Larvaceans + 
          OmniCopepods + CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish
  ) %>%
  group_by(Lon, Lat) %>%
  summarise(
    TCB_fut = mean(TCB, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Future spatial summary - unique locations:", nrow(fut_spatial), "\n")
cat("Future TCB range:", min(fut_spatial$TCB_fut, na.rm=TRUE), "to", max(fut_spatial$TCB_fut, na.rm=TRUE), "\n")

# Test the join
cat("\nTesting spatial join...\n")
joined_data <- fut_spatial %>%
  left_join(hist_spatial, by = c("Lon", "Lat")) %>%
  filter(!is.na(TCB_hist) & !is.na(TCB_fut)) %>%
  mutate(
    TCB_Change = (TCB_fut - TCB_hist) / TCB_hist * 100
  )

cat("Successfully joined locations:", nrow(joined_data), "\n")
cat("TCB change range:", min(joined_data$TCB_Change, na.rm=TRUE), "to", max(joined_data$TCB_Change, na.rm=TRUE), "\n")
cat("Sample TCB changes:", paste(head(joined_data$TCB_Change, 10), collapse=", "), "\n")

# Check a few specific locations
cat("\nSample joined data:\n")
print(head(joined_data %>% select(Lon, Lat, TCB_hist, TCB_fut, TCB_Change), 5))

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
