# Quick fix and plot creation from the processed data
library(tidyverse)
library(scales)

# Load the processed time series data
combined_timeseries <- readRDS("Output/combined_biomass_timeseries.rds")

# Create figures directory
figure_dir <- "Figures/Biomass_Timeseries/"
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("Creating biomass time series plots...\n")
cat("Data loaded:", nrow(combined_timeseries), "time series points\n")
cat("Models:", paste(unique(combined_timeseries$model), collapse = ", "), "\n")
cat("Scenarios:", paste(unique(combined_timeseries$scenario), collapse = ", "), "\n")

# Plot 1: Total biomass by scenario and model
total_biomass <- combined_timeseries %>%
  group_by(Year, scenario, model) %>%
  summarise(total_biomass = sum(biomass, na.rm = TRUE), .groups = 'drop')

p1 <- total_biomass %>%
  ggplot(aes(x = Year, y = total_biomass, color = scenario)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~model, scales = "free_y") +
  labs(
    title = "Total Zooplankton Biomass Projections (2300)",
    subtitle = "Global annual means by ESM and scenario",
    x = "Year",
    y = "Total Biomass (g/m²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scientific_format())

ggsave(paste0(figure_dir, "total_biomass_by_model_scenario.png"), 
       p1, width = 14, height = 10, dpi = 300)

cat("Plot 1 saved: total_biomass_by_model_scenario.png\n")

# Plot 2: Species-specific trends for major groups
major_species <- combined_timeseries %>%
  group_by(species) %>%
  summarise(mean_biomass = mean(biomass, na.rm = TRUE), .groups = 'drop') %>%
  top_n(8, mean_biomass) %>%
  pull(species)

p2 <- combined_timeseries %>%
  filter(species %in% major_species) %>%
  ggplot(aes(x = Year, y = biomass, color = scenario)) +
  geom_line(linewidth = 0.6) +
  facet_grid(species ~ model, scales = "free_y") +
  labs(
    title = "Major Species Biomass Projections (2300)",
    subtitle = "Top 8 species by mean biomass",
    x = "Year", 
    y = "Biomass (g/m²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scientific_format())

ggsave(paste0(figure_dir, "major_species_biomass_trends.png"),
       p2, width = 16, height = 12, dpi = 300)

cat("Plot 2 saved: major_species_biomass_trends.png\n")

# Plot 3: Scenario comparison (focusing on 21st century onwards)
future_data <- combined_timeseries %>%
  filter(Year >= 2000) %>%
  group_by(Year, scenario, model) %>%
  summarise(total_biomass = sum(biomass, na.rm = TRUE), .groups = 'drop')

p3 <- future_data %>%
  filter(scenario != "picontrol") %>%
  ggplot(aes(x = Year, y = total_biomass, color = scenario)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~model, scales = "free_y") +
  labs(
    title = "Future Biomass Projections (2000-2300)",
    subtitle = "Total zooplankton biomass by scenario",
    x = "Year",
    y = "Total Biomass (g/m²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scientific_format())

ggsave(paste0(figure_dir, "future_biomass_projections.png"),
       p3, width = 14, height = 10, dpi = 300)

cat("Plot 3 saved: future_biomass_projections.png\n")

# Plot 4: Species composition at key time points
composition_data <- combined_timeseries %>%
  filter(Year %in% c(1900, 2000, 2100, 2200, 2300)) %>%
  group_by(Year, scenario, model) %>%
  mutate(
    total_year_biomass = sum(biomass, na.rm = TRUE),
    proportion = biomass / total_year_biomass
  ) %>%
  ungroup()

p4 <- composition_data %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585")) %>%
  ggplot(aes(x = factor(Year), y = proportion, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(scenario ~ model) +
  labs(
    title = "Species Composition Changes Over Time",
    subtitle = "Proportion of total biomass by species",
    x = "Year",
    y = "Proportion of Total Biomass",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  scale_fill_viridis_d()

ggsave(paste0(figure_dir, "species_composition_timeline.png"),
       p4, width = 16, height = 12, dpi = 300)

cat("Plot 4 saved: species_composition_timeline.png\n")

# Summary statistics
summary_stats <- combined_timeseries %>%
  group_by(model, scenario) %>%
  summarise(
    n_years = n_distinct(Year),
    n_species = n_distinct(species),
    total_points = n(),
    mean_total_biomass = mean(biomass, na.rm = TRUE),
    .groups = 'drop'
  )

write_csv(summary_stats, paste0(figure_dir, "biomass_summary_statistics.csv"))
cat("Summary statistics saved: biomass_summary_statistics.csv\n")

print(summary_stats)

cat("\nAll plots successfully created in:", figure_dir, "\n")
