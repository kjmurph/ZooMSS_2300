# Quick verification that spatial plots now use tiles instead of points
library(tidyverse)
library(ggplot2)
library(maps)

# Load world map
world_map <- map_data("world")

# Create a simple test with sample data
set.seed(123)
test_data <- expand.grid(
  Lon = seq(-180, 180, by = 10),
  Lat = seq(-90, 90, by = 10)
) %>%
  mutate(
    TCB_Change = rnorm(n(), mean = 0, sd = 50),
    TCB_Change = pmax(pmin(TCB_Change, 100), -100) # Cap at ±100%
  )

# Create comparison plots
p_points <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white", linewidth = 0.1) +
  geom_point(data = test_data, aes(x = Lon, y = Lat, color = TCB_Change), 
             size = 1, alpha = 0.7) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-100, 100)) +
  coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(title = "OLD FORMAT: Points", subtitle = "geom_point() visualization") +
  theme_minimal()

p_tiles <- ggplot() +
  geom_tile(data = test_data, aes(x = Lon, y = Lat, fill = TCB_Change)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                      midpoint = 0, limits = c(-100, 100)) +
  coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(title = "NEW FORMAT: Tiles", subtitle = "geom_tile() visualization") +
  theme_minimal()

# Save comparison
ggsave("Figures/Spatial_Biomass/format_comparison_points.png", p_points, 
       width = 12, height = 6, dpi = 300)
ggsave("Figures/Spatial_Biomass/format_comparison_tiles.png", p_tiles, 
       width = 12, height = 6, dpi = 300)

cat("Format comparison plots saved!\n")
cat("- Points format: Figures/Spatial_Biomass/format_comparison_points.png\n")
cat("- Tiles format: Figures/Spatial_Biomass/format_comparison_tiles.png\n")
