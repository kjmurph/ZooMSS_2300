# Test enhanced plotting themes
library(tidyverse)
library(ggplot2)

# Test the IPSL theme components
test_data <- data.frame(
  Year = 1970:2100,
  Change = sin((1970:2100 - 1970) * 0.1) * 20 + rnorm(131, 0, 5),
  scenario = rep(c("historical", "ssp126", "ssp585"), length.out = 131),
  Group = rep(c("Zooplankton", "Fish", "Total Consumer Biomass"), each = 44)[1:131]
)

# Test enhanced theme
p_test <- test_data %>%
  ggplot(aes(x = Year, y = Change, color = scenario)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.8, color = "gray30", linewidth = 0.5) +
  facet_wrap(~Group, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c(
      "historical" = "#2166ac",
      "ssp126" = "#5aae61", 
      "ssp585" = "#d73027"
    )
  ) +
  labs(
    title = "Test: Enhanced Theme for Marine Biomass Projections",
    subtitle = "Following Tittensor et al. styling conventions",
    x = "Year",
    y = "Biomass change (%)",
    color = "Climate scenario"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom"
  )

ggsave("Figures/test_enhanced_theme.png", p_test, width = 12, height = 6, dpi = 300)

cat("Enhanced theme test completed successfully!\n")
cat("Test plot saved to: Figures/test_enhanced_theme.png\n")
