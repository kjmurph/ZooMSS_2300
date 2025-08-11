library(tidyverse)
library(purrr)

# Read in projections
df <- read_rds("~/R Projects/ZooMSS_2300/Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_ssp585_Control.rds")

# Inspect data

glimpse(df)

# Function to summarize a single file
summarize_biomass_file <- function(file_path) {
  df <- read_rds(file_path)

  # Create different aggregation levels
  df %>%
    group_by(Model, Experiment, Year) %>%
    summarise(
      # Global means
      across(c(Flagellates:Fish_Large), ~ mean(.x, na.rm = TRUE), .names = "global_{.col}"),
      # Latitudinal bands
      across(c(Flagellates:Fish_Large), ~ mean(.x[Lat > 60], na.rm = TRUE), .names = "arctic_{.col}"),
      across(c(Flagellates:Fish_Large), ~ mean(.x[Lat > -60], na.rm = TRUE), .names = "antarctic_{.col}"),
      across(c(Flagellates:Fish_Large), ~ mean(.x[Lat >= -30 & Lat <= 30], na.rm = TRUE), .names = "tropical_{.col}"),
      .groups = "drop"
    )
}

# Process all files
file_list <- list.files(path = "~/R Projects/ZooMSS_2300/Output/Biomass_projections", pattern = "*.rds", full.names = TRUE)
combined_summary <- map_dfr(file_list, summarize_biomass_file)


# Reshape for plotting
plot_data <- combined_summary %>%
  pivot_longer(
    cols = starts_with(c("global_", "arctic_", "tropical_")),
    names_to = c("region", "functional_group"),
    names_sep = "_",
    values_to = "biomass"
  ) %>%
  mutate(
    group_type = case_when(
      functional_group %in% c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods",
                              "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", "Jellyfish") ~ "Zooplankton",
      functional_group %in% c("Fish_Small", "Fish_Med", "Fish_Large") ~ "Fish",
      TRUE ~ "Other"
    )
  )

# Total biomass by group type
p1 <- plot_data %>%
  filter(region == "global") %>%
  group_by(Model, Experiment, Year, group_type) %>%
  summarise(total_biomass = sum(biomass, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = total_biomass, color = group_type)) +
  geom_line(linewidth = 1) +
  facet_grid(Experiment ~ Model) +
  labs(title = "Biomass Projections by Model and SSP Scenario",
       y = "Total Biomass", color = "Group Type") +
  theme_minimal()

# Zooplankton groups
p2 <- plot_data %>%
  filter(region == "global", group_type == "Zooplankton") %>%
  ggplot(aes(x = Year, y = biomass, color = Experiment)) +
  geom_line() +
  facet_grid(functional_group ~ Model, scales = "free_y") +
  labs(title = "Zooplankton Biomass Projections") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fish size classes
p3 <- plot_data %>%
  filter(region == "global", group_type == "Fish") %>%
  ggplot(aes(x = Year, y = biomass, color = Experiment)) +
  geom_line() +
  facet_grid(~Model) +
  labs(title = "Fish Biomass Projections by Size Class") +
  theme_minimal()

# Create spatial summaries
create_spatial_summary <- function(file_path, time_periods = c(2020, 2050, 2080)) {
  df <- read_rds(file_path)

  df %>%
    filter(Year %in% time_periods) %>%
    group_by(Lon, Lat, Year, Model, Experiment) %>%
    summarise(
      total_zooplankton = sum(c_across(Flagellates:Jellyfish), na.rm = TRUE),
      total_fish = sum(c_across(Fish_Small:Fish_Large), na.rm = TRUE),
      .groups = "drop"
    )
}
