# ================================================================
# Summary of Separate Biomass Plots Created
# ================================================================
# This script provides information about the three separate biomass plots
# that were created for Fish, TCB, and Zooplankton

library(tidyverse)

# Load the data that was used
cat("=== DATA SUMMARY FOR SEPARATE BIOMASS PLOTS ===\n")
cat("Date:", Sys.time(), "\n\n")

# Load the combined weighted biomass timeseries
data <- readRDS("Output/combined_weighted_biomass_timeseries.rds")

# Get the processed data (same as in the plotting script)
spatial_means <- data %>%
  select(Year, model, scenario, Zooplankton_Total, Fish_Total, TCB) %>%
  distinct() %>%
  filter(!is.na(Zooplankton_Total), !is.na(Fish_Total), !is.na(TCB))

cat("Data Summary:\n")
cat("- Total unique Year/Model/Scenario combinations:", nrow(spatial_means), "\n")
cat("- Models:", paste(unique(spatial_means$model), collapse = ", "), "\n")
cat("- Scenarios:", paste(unique(spatial_means$scenario), collapse = ", "), "\n")
cat("- Year range:", min(spatial_means$Year), "to", max(spatial_means$Year), "\n\n")

# Calculate some basic statistics for each biomass group
cat("Biomass Group Ranges (across all models/scenarios/years):\n")
cat("- Zooplankton Total: ", round(min(spatial_means$Zooplankton_Total, na.rm=TRUE), 3), 
    " to ", round(max(spatial_means$Zooplankton_Total, na.rm=TRUE), 3), " g C/m²\n")
cat("- Fish Total: ", round(min(spatial_means$Fish_Total, na.rm=TRUE), 3), 
    " to ", round(max(spatial_means$Fish_Total, na.rm=TRUE), 3), " g C/m²\n")
cat("- Total Consumer Biomass: ", round(min(spatial_means$TCB, na.rm=TRUE), 3), 
    " to ", round(max(spatial_means$TCB, na.rm=TRUE), 3), " g C/m²\n\n")

# Show the baseline period data
historical_baseline <- spatial_means %>%
  filter(scenario == "historical", Year >= 1990, Year <= 1999) %>%
  group_by(model) %>%
  summarise(
    Zoop_hist_baseline = round(mean(Zooplankton_Total, na.rm = TRUE), 3),
    Fish_hist_baseline = round(mean(Fish_Total, na.rm = TRUE), 3),
    TCB_hist_baseline = round(mean(TCB, na.rm = TRUE), 3),
    .groups = 'drop'
  )

cat("Historical Baseline Values (1990-1999 average, g C/m²):\n")
print(historical_baseline)

cat("\n=== PLOTS CREATED ===\n")
cat("Three separate plots were created, each showing percentage change relative to\n")
cat("the 1990-1999 historical baseline for that model:\n\n")

cat("1. zooplankton_percentage_change_by_model.png\n")
cat("   - Shows Zooplankton Total biomass changes\n")
cat("   - Three panels (one per Earth System Model)\n")
cat("   - Historical, SSP1-2.6, SSP5-8.5, and SSP5-3.4-overshoot scenarios\n\n")

cat("2. fish_percentage_change_by_model.png\n")
cat("   - Shows Fish Total biomass changes (Small + Medium + Large fish)\n")
cat("   - Three panels (one per Earth System Model)\n")
cat("   - Historical, SSP1-2.6, SSP5-8.5, and SSP5-3.4-overshoot scenarios\n\n")

cat("3. tcb_percentage_change_by_model.png\n")
cat("   - Shows Total Consumer Biomass changes (Zooplankton + Fish)\n")
cat("   - Three panels (one per Earth System Model)\n")
cat("   - Historical, SSP1-2.6, SSP5-8.5, and SSP5-3.4-overshoot scenarios\n\n")

cat("Plot Features:\n")
cat("- Blue shaded area: 1990-1999 baseline period\n")
cat("- Horizontal dashed line: 0% change (baseline reference)\n")
cat("- Vertical dotted lines: Baseline period boundaries\n")
cat("- Time series from 1970 to 2300\n")
cat("- Separate panel for each Earth System Model\n\n")

cat("Analysis complete!\n")
