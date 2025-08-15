# Diagnostic script to identify the source of extreme TCB percentage changes
library(tidyverse)

# Load one of the recent change files to examine the data
historical_data <- readRDS("Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds") %>%
  filter(Year >= 1990 & Year <= 1999) %>%
  group_by(Lon, Lat) %>%
  summarise(
    TCB_historical = mean(Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                         CarnCopepods + Euphausiids + Chaetognaths + Salps + 
                         Jellyfish + Fish_Small + Fish_Med + Fish_Large, na.rm = TRUE),
    .groups = "drop"
  )

future_data <- readRDS("Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_ssp585_Control.rds") %>%
  filter(Year >= 2290 & Year <= 2299) %>%
  group_by(Lon, Lat) %>%
  summarise(
    TCB_future = mean(Flagellates + Ciliates + Larvaceans + OmniCopepods + 
                     CarnCopepods + Euphausiids + Chaetognaths + Salps + 
                     Jellyfish + Fish_Small + Fish_Med + Fish_Large, na.rm = TRUE),
    .groups = "drop"
  )

# Join and calculate percentage change
combined_data <- historical_data %>%
  inner_join(future_data, by = c("Lon", "Lat")) %>%
  mutate(
    TCB_Change = (TCB_future - TCB_historical) / TCB_historical * 100
  )

cat("DIAGNOSTIC: TCB Change Analysis\n")
cat("===============================\n")

# Basic statistics
cat("Historical TCB statistics:\n")
cat("  Min:", min(combined_data$TCB_historical, na.rm = TRUE), "\n")
cat("  Max:", max(combined_data$TCB_historical, na.rm = TRUE), "\n")
cat("  Mean:", mean(combined_data$TCB_historical, na.rm = TRUE), "\n")
cat("  Median:", median(combined_data$TCB_historical, na.rm = TRUE), "\n")

# Check for very small historical values (near zero)
very_small_historical <- combined_data %>% filter(TCB_historical < 0.001)
cat("\nGrid cells with very small historical TCB (< 0.001):", nrow(very_small_historical), "\n")

if (nrow(very_small_historical) > 0) {
  cat("Sample very small historical values:\n")
  print(head(very_small_historical %>% arrange(TCB_historical), 5))
}

# Check for extremely large percentage changes
extreme_changes <- combined_data %>% filter(abs(TCB_Change) > 1000)
cat("\nGrid cells with extreme changes (> ±1000%):", nrow(extreme_changes), "\n")

if (nrow(extreme_changes) > 0) {
  cat("Sample extreme changes:\n")
  print(head(extreme_changes %>% arrange(desc(abs(TCB_Change))), 5))
}

# Distribution of percentage changes
cat("\nTCB Change statistics:\n")
cat("  Min:", min(combined_data$TCB_Change, na.rm = TRUE), "\n")
cat("  Max:", max(combined_data$TCB_Change, na.rm = TRUE), "\n")
cat("  Mean:", mean(combined_data$TCB_Change, na.rm = TRUE), "\n")
cat("  Median:", median(combined_data$TCB_Change, na.rm = TRUE), "\n")

# Percentiles
percentiles <- quantile(combined_data$TCB_Change, c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99), na.rm = TRUE)
cat("\nPercentiles:\n")
for (i in 1:length(percentiles)) {
  cat("  ", names(percentiles)[i], ":", percentiles[i], "\n")
}

# Recommended approach: Use a minimum baseline threshold
cat("\n=== RECOMMENDED SOLUTION ===\n")
cat("Problem: Division by very small numbers creates extreme percentages\n")
cat("Solution: Apply minimum baseline threshold (e.g., 0.01) to avoid division by near-zero values\n")

# Test with minimum threshold
min_threshold <- 0.01
corrected_data <- combined_data %>%
  mutate(
    TCB_historical_corrected = pmax(TCB_historical, min_threshold),
    TCB_Change_corrected = (TCB_future - TCB_historical_corrected) / TCB_historical_corrected * 100
  )

cat("\nWith minimum threshold of", min_threshold, ":\n")
cat("  Corrected Min:", min(corrected_data$TCB_Change_corrected, na.rm = TRUE), "\n")
cat("  Corrected Max:", max(corrected_data$TCB_Change_corrected, na.rm = TRUE), "\n")
cat("  Corrected Mean:", mean(corrected_data$TCB_Change_corrected, na.rm = TRUE), "\n")

# Check how many values were affected
affected_count <- sum(combined_data$TCB_historical < min_threshold, na.rm = TRUE)
cat("  Grid cells affected by threshold:", affected_count, "out of", nrow(combined_data), "\n")
cat("  Percentage affected:", round(affected_count / nrow(combined_data) * 100, 2), "%\n")
