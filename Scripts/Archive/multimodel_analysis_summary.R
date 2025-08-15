# ================================================================
# Multi-Model Mean Biomass Analysis Summary
# ================================================================
# Analyzes the ensemble statistics and provides key insights

library(tidyverse)

cat("=== MULTI-MODEL MEAN BIOMASS ANALYSIS SUMMARY ===\n")
cat("Date:", Sys.time(), "\n\n")

# Load the ensemble statistics
ensemble_stats <- read.csv("Figures/Biomass_Enhanced/ensemble_biomass_statistics.csv")

cat("Ensemble data loaded:", nrow(ensemble_stats), "Year/Scenario combinations\n")
cat("Scenarios:", paste(unique(ensemble_stats$scenario), collapse = ", "), "\n")
cat("Year range:", min(ensemble_stats$Year), "to", max(ensemble_stats$Year), "\n\n")

# Analysis of key time periods
cat("=== KEY FINDINGS BY SCENARIO ===\n\n")

# Function to analyze a specific scenario
analyze_scenario <- function(data, scenario_name) {
  scenario_data <- data %>% filter(scenario == scenario_name)
  
  if(nrow(scenario_data) == 0) {
    cat("No data for scenario:", scenario_name, "\n")
    return()
  }
  
  cat("**", toupper(scenario_name), "**\n")
  cat("Time period:", min(scenario_data$Year), "to", max(scenario_data$Year), "\n")
  
  # Get end-of-century and end-of-period values
  end_century <- scenario_data %>% filter(Year == 2100) %>% slice(1)
  end_period <- scenario_data %>% filter(Year == max(Year)) %>% slice(1)
  
  if(nrow(end_century) > 0) {
    cat("By 2100:\n")
    cat("  Zooplankton change: ", round(end_century$Zoop_Mean, 1), "% (range: ", 
        round(end_century$Zoop_Min, 1), " to ", round(end_century$Zoop_Max, 1), "%)\n")
    cat("  Fish change: ", round(end_century$Fish_Mean, 1), "% (range: ", 
        round(end_century$Fish_Min, 1), " to ", round(end_century$Fish_Max, 1), "%)\n")
    cat("  TCB change: ", round(end_century$TCB_Mean, 1), "% (range: ", 
        round(end_century$TCB_Min, 1), " to ", round(end_century$TCB_Max, 1), "%)\n")
  }
  
  if(nrow(end_period) > 0 && end_period$Year > 2100) {
    cat("By", end_period$Year, ":\n")
    cat("  Zooplankton change: ", round(end_period$Zoop_Mean, 1), "% (range: ", 
        round(end_period$Zoop_Min, 1), " to ", round(end_period$Zoop_Max, 1), "%)\n")
    cat("  Fish change: ", round(end_period$Fish_Mean, 1), "% (range: ", 
        round(end_period$Fish_Min, 1), " to ", round(end_period$Fish_Max, 1), "%)\n")
    cat("  TCB change: ", round(end_period$TCB_Mean, 1), "% (range: ", 
        round(end_period$TCB_Min, 1), " to ", round(end_period$TCB_Max, 1), "%)\n")
  }
  
  # Find maximum and minimum changes over the entire period
  max_zoop <- scenario_data[which.max(scenario_data$Zoop_Mean), ]
  min_zoop <- scenario_data[which.min(scenario_data$Zoop_Mean), ]
  max_fish <- scenario_data[which.max(scenario_data$Fish_Mean), ]
  min_fish <- scenario_data[which.min(scenario_data$Fish_Mean), ]
  
  cat("Extreme changes:\n")
  cat("  Zooplankton peak: ", round(max_zoop$Zoop_Mean, 1), "% in ", max_zoop$Year, "\n")
  cat("  Zooplankton minimum: ", round(min_zoop$Zoop_Mean, 1), "% in ", min_zoop$Year, "\n")
  cat("  Fish peak: ", round(max_fish$Fish_Mean, 1), "% in ", max_fish$Year, "\n")
  cat("  Fish minimum: ", round(min_fish$Fish_Mean, 1), "% in ", min_fish$Year, "\n")
  
  cat("\n")
}

# Analyze each scenario
analyze_scenario(ensemble_stats, "historical")
analyze_scenario(ensemble_stats, "ssp126")
analyze_scenario(ensemble_stats, "ssp585")
analyze_scenario(ensemble_stats, "ssp534-over")

# Compare scenarios at key time points
cat("=== SCENARIO COMPARISON AT KEY TIME POINTS ===\n\n")

time_points <- c(2050, 2100, 2200, 2300)

for(year in time_points) {
  year_data <- ensemble_stats %>% filter(Year == year, scenario != "historical")
  
  if(nrow(year_data) > 0) {
    cat("**YEAR", year, "**\n")
    year_summary <- year_data %>%
      select(scenario, Zoop_Mean, Fish_Mean, TCB_Mean) %>%
      arrange(desc(TCB_Mean))
    
    print(year_summary %>% 
          mutate(across(where(is.numeric), ~round(.x, 1))))
    cat("\n")
  }
}

# Model agreement analysis
cat("=== MODEL AGREEMENT ANALYSIS ===\n\n")

# Calculate model agreement (percentage of models agreeing on direction of change)
model_agreement <- ensemble_stats %>%
  filter(scenario %in% c("ssp126", "ssp585", "ssp534-over"), Year >= 2020) %>%
  mutate(
    # Models agree if the interquartile range doesn't span zero
    Zoop_Agreement = ifelse(sign(Zoop_Q25) == sign(Zoop_Q75), "High", "Low"),
    Fish_Agreement = ifelse(sign(Fish_Q25) == sign(Fish_Q75), "High", "Low"),
    TCB_Agreement = ifelse(sign(TCB_Q25) == sign(TCB_Q75), "High", "Low")
  ) %>%
  group_by(scenario) %>%
  summarise(
    Years_analyzed = n(),
    Zoop_High_Agreement = sum(Zoop_Agreement == "High") / n() * 100,
    Fish_High_Agreement = sum(Fish_Agreement == "High") / n() * 100,
    TCB_High_Agreement = sum(TCB_Agreement == "High") / n() * 100,
    .groups = 'drop'
  )

cat("Model agreement (% of years where IQR doesn't span zero):\n")
print(model_agreement %>% 
      mutate(across(where(is.numeric) & !Years_analyzed, ~round(.x, 1))))

cat("\n=== FILES CREATED ===\n")
cat("Multi-model mean plots:\n")
cat("- zooplankton_multimodel_mean.png\n")
cat("- fish_multimodel_mean.png\n")
cat("- tcb_multimodel_mean.png\n")
cat("- all_biomass_multimodel_mean_combined.png\n\n")

cat("Data files:\n")
cat("- ensemble_biomass_statistics.csv\n\n")

cat("Plot interpretation:\n")
cat("- Thick colored lines: Multi-model ensemble mean\n")
cat("- Dark shaded bands: Inter-quartile range (middle 50% of model predictions)\n")
cat("- Light shaded bands: Full model range (min to max across all models)\n")
cat("- Blue shaded area: 1990-1999 baseline reference period\n\n")

cat("Analysis complete!\n")
