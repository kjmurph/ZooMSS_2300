# ==============================================================================
# ENHANCED BIOMASS PLOTTING (Starting from saved data)
# ==============================================================================
# Purpose: Create enhanced plots from the processed weighted time series data
# ==============================================================================

library(tidyverse)
library(patchwork)
library(scales)
library(viridis)
library(RColorBrewer)

# Set directories
figure_dir <- "Figures/Biomass_Enhanced/"

# Create figures directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("=== ENHANCED BIOMASS PLOTTING ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Load the corrected processed data
cat("Loading corrected combined weighted time series data...\n")
combined_weighted_timeseries <- readRDS("Output/combined_corrected_biomass_timeseries.rds")

cat("Data loaded. Total time series points:", nrow(combined_weighted_timeseries), "\n")
cat("Models:", paste(unique(combined_weighted_timeseries$model), collapse = ", "), "\n")
cat("Scenarios:", paste(unique(combined_weighted_timeseries$scenario), collapse = ", "), "\n")

# Define species groups
zooplankton_species <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
                        "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish")
fish_species <- c("Fish_Small", "Fish_Med", "Fish_Large")

# ==============================================================================
# CREATE AGGREGATE TIME SERIES FOR PLOTTING
# ==============================================================================

cat("\nCreating aggregate time series...\n")

# Create summary data for aggregate groups
aggregate_timeseries <- combined_weighted_timeseries %>%
  group_by(Year, scenario, model) %>%
  summarise(
    Zooplankton_Total = first(Zooplankton_Total),
    Fish_Total = first(Fish_Total),
    TCB = first(TCB),
    total_ocean_area_km2 = first(total_ocean_area_km2),
    .groups = 'drop'
  ) %>%
  # Calculate percentage changes relative to baseline
  group_by(model, scenario) %>%
  arrange(Year) %>%
  mutate(
    # Use first 20 years as baseline for each scenario
    year_min = min(Year),
    year_max = max(Year),
    Zoop_baseline = mean(Zooplankton_Total[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    Fish_baseline = mean(Fish_Total[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    TCB_baseline = mean(TCB[Year >= year_min & Year <= (year_min + 19)], na.rm = TRUE),
    
    Zoop_Change = (Zooplankton_Total - Zoop_baseline) / Zoop_baseline * 100,
    Fish_Change = (Fish_Total - Fish_baseline) / Fish_baseline * 100,
    TCB_Change = (TCB - TCB_baseline) / TCB_baseline * 100
  ) %>%
  ungroup()

cat("Aggregate time series created with", nrow(aggregate_timeseries), "data points\n")

# ==============================================================================
# ENHANCED PLOTTING
# ==============================================================================

cat("\nCreating enhanced plots...\n")

# Define color schemes
scenario_colors <- c(
  "historical" = "#2E2E2E",
  "picontrol" = "#808080", 
  "ssp126" = "#1f77b4",
  "ssp534-over" = "#ff7f0e",
  "ssp585" = "#d62728"
)

model_shapes <- c("cesm2-waccm" = 16, "ipsl-cm6a-lr" = 17, "ukesm1-0-ll" = 18)

# Plot 1: Total Biomass Time Series by Group
p1 <- aggregate_timeseries %>%
  filter(scenario != "picontrol") %>%
  dplyr::select(Year, scenario, model, Zooplankton_Total, Fish_Total) %>%
  pivot_longer(cols = c(Zooplankton_Total, Fish_Total), 
               names_to = "Group", values_to = "Biomass") %>%
  mutate(Group = str_replace(Group, "_Total", "")) %>%
  ggplot(aes(x = Year, y = Biomass, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.8) +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Total Biomass Projections: Zooplankton vs Fish",
    subtitle = "Area-weighted global means (all 3 Earth System Models)",
    x = "Year",
    y = "Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "total_biomass_zooplankton_vs_fish.png"), 
       p1, width = 14, height = 8, dpi = 300)

# Plot 2a: Percentage Changes - Updated baseline (1990-1999 from historical)
cat("Creating 1990-1999 baseline percentage changes...\n")

# Calculate changes relative to 1990-1999 baseline from HISTORICAL simulation
aggregate_1990s_baseline <- aggregate_timeseries %>%
  group_by(model) %>%
  # Get 1990-1999 baseline from historical scenario for each model
  mutate(
    # Calculate 1990-1999 baselines from historical scenario only
    Zoop_1990s_baseline = first(Zoop_baseline[scenario == "historical" & 
                                               any(Year[scenario == "historical"] >= 1990 & 
                                                   Year[scenario == "historical"] <= 1999)]),
    Fish_1990s_baseline = first(Fish_baseline[scenario == "historical" & 
                                               any(Year[scenario == "historical"] >= 1990 & 
                                                   Year[scenario == "historical"] <= 1999)]),
    TCB_1990s_baseline = first(TCB_baseline[scenario == "historical" & 
                                             any(Year[scenario == "historical"] >= 1990 & 
                                                 Year[scenario == "historical"] <= 1999)])
  ) %>%
  # For scenarios, use the historical 1990s baseline, but calculate from actual historical data
  group_by(model) %>%
  mutate(
    # Get actual 1990-1999 values from historical scenario
    hist_data = list(filter(cur_data(), scenario == "historical" & Year >= 1990 & Year <= 1999)),
    Zoop_hist_baseline = ifelse(length(hist_data[[1]]$Zooplankton_Total) > 0,
                               mean(hist_data[[1]]$Zooplankton_Total, na.rm = TRUE),
                               NA),
    Fish_hist_baseline = ifelse(length(hist_data[[1]]$Fish_Total) > 0,
                               mean(hist_data[[1]]$Fish_Total, na.rm = TRUE),
                               NA),
    TCB_hist_baseline = ifelse(length(hist_data[[1]]$TCB) > 0,
                              mean(hist_data[[1]]$TCB, na.rm = TRUE),
                              NA),
    
    # Calculate percentage changes relative to historical 1990-1999 baseline
    Zoop_Change_1990s = (Zooplankton_Total - Zoop_hist_baseline) / Zoop_hist_baseline * 100,
    Fish_Change_1990s = (Fish_Total - Fish_hist_baseline) / Fish_hist_baseline * 100,
    TCB_Change_1990s = (TCB - TCB_hist_baseline) / TCB_hist_baseline * 100
  ) %>%
  ungroup() %>%
  select(-hist_data)  # Remove list column

# Plot 2: Updated Percentage Changes (using 1990-1999 baseline from historical)
p2 <- aggregate_1990s_baseline %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
         Year >= 1970) %>%
  dplyr::select(Year, scenario, model, Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s) %>%
  pivot_longer(cols = c(Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = case_when(
    Group == "Zoop_Change_1990s" ~ "Zooplankton",
    Group == "Fish_Change_1990s" ~ "Fish", 
    Group == "TCB_Change_1990s" ~ "Total Consumer Biomass"
  )) %>%
  ggplot(aes(x = Year, y = Change, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
  annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "blue") +
  annotate("text", x = 1994.5, y = Inf, label = "Baseline\n1990-1999", 
           vjust = 1.1, hjust = 0.5, size = 3, color = "darkblue") +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Biomass Changes Relative to Historical 1990-1999 Baseline",
    subtitle = "Percentage change from historical simulation 1990-1999 reference period",
    x = "Year",
    y = "Change (%)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "biomass_percentage_change.png"), 
       p2, width = 14, height = 8, dpi = 300)

# Plot 2b: NEW - Biomass Changes Through 2100 Only (1990-1999 baseline from historical)
cat("Creating 2100-focused percentage change plot...\n")

p2b <- aggregate_1990s_baseline %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
         Year <= 2100, Year >= 1970) %>%
  dplyr::select(Year, scenario, model, Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s) %>%
  pivot_longer(cols = c(Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = case_when(
    Group == "Zoop_Change_1990s" ~ "Zooplankton",
    Group == "Fish_Change_1990s" ~ "Fish", 
    Group == "TCB_Change_1990s" ~ "Total Consumer Biomass"
  )) %>%
  ggplot(aes(x = Year, y = Change, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
  annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "blue") +
  annotate("text", x = 1994.5, y = Inf, label = "Baseline\n1990-1999", 
           vjust = 1.1, hjust = 0.5, size = 3, color = "darkblue") +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Marine Biomass Changes Through 2100 (Validation Period)",
    subtitle = "Percentage change from historical 1990-1999 baseline - For comparison with previous projections",
    x = "Year",
    y = "Change (%)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "biomass_percentage_change_through_2100.png"), 
       p2b, width = 14, height = 8, dpi = 300)

# Plot 2c: NEW - IPSL Only Biomass Changes Through 2100
cat("Creating IPSL-only 2100-focused percentage change plot...\n")

p2c <- aggregate_1990s_baseline %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
         Year <= 2100, Year >= 1970,
         model == "ipsl-cm6a-lr") %>%
  dplyr::select(Year, scenario, model, Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s) %>%
  pivot_longer(cols = c(Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s), 
               names_to = "Group", values_to = "Change") %>%
  mutate(Group = case_when(
    Group == "Zoop_Change_1990s" ~ "Zooplankton",
    Group == "Fish_Change_1990s" ~ "Fish", 
    Group == "TCB_Change_1990s" ~ "Total Consumer Biomass"
  )) %>%
  ggplot(aes(x = Year, y = Change, color = scenario)) +
  geom_line(linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.5, color = "darkblue") +
  annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "blue") +
  annotate("text", x = 1994.5, y = Inf, label = "Baseline\n1990-1999", 
           vjust = 1.1, hjust = 0.5, size = 3, color = "darkblue") +
  facet_wrap(~Group, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  labs(
    title = "Marine Biomass Changes Through 2100: IPSL-CM6A-LR Model",
    subtitle = "Percentage change from historical 1990-1999 baseline - IPSL model validation",
    x = "Year",
    y = "Change (%)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "biomass_percentage_change_through_2100_IPSL_only.png"), 
       p2c, width = 14, height = 8, dpi = 300)

# Plot 3: Individual Species Time Series (Top 6 species)
top_species <- combined_weighted_timeseries %>%
  group_by(species) %>%
  summarise(mean_biomass = mean(biomass_weighted, na.rm = TRUE), .groups = 'drop') %>%
  top_n(6, mean_biomass) %>%
  pull(species)

p3 <- combined_weighted_timeseries %>%
  filter(species %in% top_species,
         scenario %in% c("historical", "ssp126", "ssp585")) %>%
  ggplot(aes(x = Year, y = biomass_weighted, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 0.6) +
  facet_wrap(~species, scales = "free_y", ncol = 3) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Top 6 Species Biomass Projections",
    subtitle = "Area-weighted global means for most abundant species",
    x = "Year",
    y = "Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "top_species_biomass_projections.png"), 
       p3, width = 16, height = 10, dpi = 300)

# Plot 4: Long-term Trajectory Comparison (focus on 2100-2300)
p4 <- aggregate_timeseries %>%
  filter(Year >= 2100,
         scenario %in% c("ssp126", "ssp585", "ssp534-over")) %>%
  ggplot(aes(x = Year, y = TCB, color = scenario)) +
  geom_line(aes(linetype = model), linewidth = 1.0) +
  facet_wrap(~model) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Long-term Marine Biomass Trajectories (2100-2300)",
    subtitle = "Total Consumer Biomass by scenario and model", 
    x = "Year",
    y = "Total Consumer Biomass (g/m²)",
    color = "Scenario",
    linetype = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(paste0(figure_dir, "longterm_biomass_trajectories_2100_2300.png"), 
       p4, width = 14, height = 8, dpi = 300)

# Save summary statistics
summary_stats <- aggregate_timeseries %>%
  group_by(model, scenario) %>%
  summarise(
    n_years = n_distinct(Year),
    year_range = paste(min(Year), max(Year), sep = "-"),
    mean_zoop_biomass = round(mean(Zooplankton_Total, na.rm = TRUE), 4),
    mean_fish_biomass = round(mean(Fish_Total, na.rm = TRUE), 4),
    mean_total_biomass = round(mean(TCB, na.rm = TRUE), 4),
    final_zoop_change = round(last(Zoop_Change[!is.na(Zoop_Change)]), 1),
    final_fish_change = round(last(Fish_Change[!is.na(Fish_Change)]), 1),
    final_tcb_change = round(last(TCB_Change[!is.na(TCB_Change)]), 1),
    .groups = 'drop'
  )

write_csv(summary_stats, paste0(figure_dir, "enhanced_biomass_summary_statistics.csv"))

cat("Enhanced plotting complete!\n")
cat("Plots saved to:", figure_dir, "\n")
cat("Summary stats:", nrow(summary_stats), "scenario-model combinations\n")

print(summary_stats)

# Additional analysis: Model comparison
cat("\nModel comparison summary:\n")
model_comparison <- aggregate_timeseries %>%
  filter(scenario == "ssp585", Year == 2300) %>%
  dplyr::select(model, Fish_Total, Zooplankton_Total, TCB, Fish_Change, Zoop_Change, TCB_Change) %>%
  arrange(model)

print(model_comparison)

cat("\n=== ENHANCED PLOTTING COMPLETE ===\n")
