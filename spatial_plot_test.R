# Simple test to generate one spatial change plot with debugging
library(tidyverse)
library(maps)
library(viridis)

cat("=== SPATIAL PLOT TEST ===\n")

# Recreate the change data from diagnostic
hist_file <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds"
fut_file <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_ssp585_Control.rds"

# Load and process data
hist_data <- readRDS(hist_file)
fut_data <- readRDS(fut_file)

# Calculate spatial changes
hist_spatial <- hist_data %>% 
  filter(Year >= 1990 & Year <= 1999) %>%
  mutate(TCB = Fish_Small + Fish_Med + Fish_Large + Flagellates + Ciliates + Larvaceans + 
               OmniCopepods + CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish) %>%
  group_by(Lon, Lat) %>%
  summarise(TCB_hist = mean(TCB, na.rm = TRUE), .groups = 'drop')

fut_spatial <- fut_data %>% 
  filter(Year >= 2290 & Year <= 2299) %>%
  mutate(TCB = Fish_Small + Fish_Med + Fish_Large + Flagellates + Ciliates + Larvaceans + 
               OmniCopepods + CarnCopepods + Euphausiids + Chaetognaths + Salps + Jellyfish) %>%
  group_by(Lon, Lat) %>%
  summarise(TCB_fut = mean(TCB, na.rm = TRUE), .groups = 'drop')

change_data <- fut_spatial %>%
  left_join(hist_spatial, by = c("Lon", "Lat")) %>%
  filter(!is.na(TCB_hist) & !is.na(TCB_fut)) %>%
  mutate(TCB_Change = (TCB_fut - TCB_hist) / TCB_hist * 100)

cat("Change data ready - rows:", nrow(change_data), "\n")
cat("TCB_Change range:", min(change_data$TCB_Change, na.rm=TRUE), "to", max(change_data$TCB_Change, na.rm=TRUE), "\n")

# Create simple spatial plot
world_map <- map_data("world")

# Use symmetric color scale
max_abs <- max(abs(change_data$TCB_Change), na.rm = TRUE)
limits <- c(-max_abs, max_abs)

# Subsample for better visualization
plot_data <- change_data %>% 
  slice_sample(n = min(5000, nrow(change_data))) %>%
  # Remove extreme outliers for better visualization
  filter(abs(TCB_Change) <= quantile(abs(change_data$TCB_Change), 0.95, na.rm = TRUE))

cat("Plot data - rows:", nrow(plot_data), "\n")
cat("Plot TCB_Change range:", min(plot_data$TCB_Change, na.rm=TRUE), "to", max(plot_data$TCB_Change, na.rm=TRUE), "\n")

p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white", linewidth = 0.1) +
  geom_point(data = plot_data, aes(x = Lon, y = Lat, color = TCB_Change), 
             size = 0.5, alpha = 0.7) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "TCB Change (%)") +
  coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(title = "TEST: Total Consumer Biomass Change by 2290s (SSP5-8.5)",
       subtitle = paste("n =", nrow(plot_data), "points")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave("Figures/Spatial_Biomass/TEST_future_biomass_change.png", 
       p, width = 12, height = 8, dpi = 300)

cat("Test plot saved!\n")
cat("=== TEST COMPLETE ===\n")
