# Test the archived project's approach with extreme values
library(tidyverse)
library(ggplot2)
library(scales)
library(maps)

# Load world map
world_map <- map_data("world")

# Create sample data with extreme percentage changes (like our diagnostic found)
set.seed(123)
test_data <- expand.grid(
  Lon = seq(-90, -80, by = 2),
  Lat = seq(-75, -70, by = 2)
) %>%
  mutate(
    # Create some extreme changes similar to what we found
    TCB_Change = c(-95, -80, 50, 150, 500, 2000, 8814, -90, 100, 200, 300, 400, 
                   600, 800, 1000, 1200, 1500, 2000, 3000, 4000, 5000, 6000, 
                   7000, 8000, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300)[1:n()]
  )

cat("Test data TCB_Change range:", min(test_data$TCB_Change), "to", max(test_data$TCB_Change), "\n")

# Approach 1: No limits (original problematic approach)
p1 <- ggplot() +
  geom_tile(data = test_data, aes(x = Lon, y = Lat, fill = TCB_Change)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  coord_fixed(ratio = 1, xlim = c(-95, -75), ylim = c(-80, -65)) +
  labs(title = "BEFORE: No Limits", subtitle = "Extreme values dominate color scale") +
  theme_minimal()

# Approach 2: Archived project approach (±100% limits + oob squish)
p2 <- ggplot() +
  geom_tile(data = test_data, aes(x = Lon, y = Lat, fill = TCB_Change)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-100, 100), oob = scales::squish) +
  coord_fixed(ratio = 1, xlim = c(-95, -75), ylim = c(-80, -65)) +
  labs(title = "AFTER: ±100% Limits + scales::squish", 
       subtitle = "Extreme values capped, moderate changes visible") +
  theme_minimal()

# Save comparison
ggsave("Figures/Spatial_Biomass/extreme_values_comparison_before.png", p1, 
       width = 10, height = 6, dpi = 300)
ggsave("Figures/Spatial_Biomass/extreme_values_comparison_after.png", p2, 
       width = 10, height = 6, dpi = 300)

cat("Comparison plots saved:\n")
cat("- Before: Figures/Spatial_Biomass/extreme_values_comparison_before.png\n")
cat("- After: Figures/Spatial_Biomass/extreme_values_comparison_after.png\n")
cat("\nUsing archived project approach: ±100% limits with oob = scales::squish\n")
