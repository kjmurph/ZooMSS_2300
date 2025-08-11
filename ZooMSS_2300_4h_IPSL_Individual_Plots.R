# Individual IPSL Time Series Plots with Enhanced Styling
# Separate figures for Fish, Zooplankton, and Total Consumer Biomass

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)

# Load the data
cat("=== INDIVIDUAL IPSL TIME SERIES PLOTS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create output directory
figure_dir <- "Figures/IPSL_Individual/"
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

# Load corrected combined weighted time series data
data_file <- "Output/combined_corrected_biomass_timeseries.rds"
cat("Loading corrected combined weighted time series data...\n")
aggregate_data <- readRDS(data_file)

# Create aggregate time series with corrected 1990-1999 baseline
aggregate_1990s_baseline <- aggregate_data %>%
  # First create historical data lookup for each model
  nest_by(model) %>%
  mutate(
    hist_data = list({
      current_data <- data
      hist_subset <- current_data %>% 
        filter(scenario == "historical", Year >= 1990, Year <= 1999)
      
      list(
        Zoop_hist_baseline = mean(hist_subset$Zooplankton_Total, na.rm = TRUE),
        Fish_hist_baseline = mean(hist_subset$Fish_Total, na.rm = TRUE), 
        TCB_hist_baseline = mean(hist_subset$TCB, na.rm = TRUE)
      )
    })
  ) %>%
  unnest(cols = c(data)) %>%
  unnest_wider(hist_data) %>%
  mutate(
    # Calculate percentage changes relative to historical 1990-1999 baseline
    Zoop_Change_1990s = (Zooplankton_Total - Zoop_hist_baseline) / Zoop_hist_baseline * 100,
    Fish_Change_1990s = (Fish_Total - Fish_hist_baseline) / Fish_hist_baseline * 100,
    TCB_Change_1990s = (TCB - TCB_hist_baseline) / TCB_hist_baseline * 100
  ) %>%
  ungroup()

# Filter for IPSL model only and create individual plots
ipsl_data <- aggregate_1990s_baseline %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over"),
         Year <= 2100, Year >= 1970,
         model == "ipsl-cm6a-lr")

# Enhanced color palette for better distinction
enhanced_colors <- c(
  "historical" = "#1f4e79",      # Deep blue
  "ssp126" = "#2e8b57",          # Sea green
  "ssp585" = "#cd5c5c",          # Indian red
  "ssp534-over" = "#ff8c00"      # Dark orange
)

# Enhanced scenario labels
scenario_labels <- c(
  "historical" = "Historical",
  "ssp126" = "SSP1-2.6", 
  "ssp585" = "SSP5-8.5",
  "ssp534-over" = "SSP5-3.4-OS"
)

# Common theme function
create_enhanced_theme <- function() {
  theme_classic() +
  theme(
    # Panel and plot styling
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Text styling
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold", 
                              margin = margin(b = 8)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30",
                                 margin = margin(b = 20)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    
    # Enhanced legend styling with larger text
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    legend.key.width = unit(2.0, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.margin = margin(t = 15),
    legend.box.margin = margin(t = 10),
    
    # Plot margins
    plot.margin = margin(20, 25, 20, 20)
  )
}

# Common elements function
add_common_elements <- function(p) {
  p +
    geom_hline(yintercept = 0, linetype = "solid", alpha = 0.8, color = "gray30", linewidth = 0.6) +
    geom_vline(xintercept = c(1990, 1999), linetype = "dotted", alpha = 0.6, color = "steelblue", linewidth = 0.5) +
    annotate("rect", xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf, 
             alpha = 0.15, fill = "steelblue") +
    annotate("text", x = 1994.5, y = Inf, label = "Baseline\n(1990-1999)", 
             vjust = 1.3, hjust = 0.5, size = 3.5, color = "steelblue", fontface = "italic") +
    scale_color_manual(
      values = enhanced_colors,
      labels = scenario_labels,
      name = "Climate scenario"
    ) +
    scale_x_continuous(breaks = seq(1970, 2100, 20), expand = c(0.02, 0))
}

# 1. ZOOPLANKTON PLOT
cat("Creating individual Zooplankton plot...\n")
p_zoop <- ipsl_data %>%
  ggplot(aes(x = Year, y = Zoop_Change_1990s, color = scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9)

p_zoop <- add_common_elements(p_zoop) +
  labs(
    title = "Zooplankton Biomass Projections Through 2100",
    subtitle = "IPSL-CM6A-LR Model | Percentage change from 1990-1999 baseline",
    x = "Year",
    y = "Zooplankton biomass change (%)"
  ) +
  create_enhanced_theme()

ggsave(paste0(figure_dir, "IPSL_Zooplankton_timeseries_2100.png"), 
       p_zoop, width = 12, height = 8, dpi = 300)

# 2. FISH PLOT
cat("Creating individual Fish plot...\n")
p_fish <- ipsl_data %>%
  ggplot(aes(x = Year, y = Fish_Change_1990s, color = scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9)

p_fish <- add_common_elements(p_fish) +
  labs(
    title = "Fish Biomass Projections Through 2100",
    subtitle = "IPSL-CM6A-LR Model | Percentage change from 1990-1999 baseline",
    x = "Year",
    y = "Fish biomass change (%)"
  ) +
  create_enhanced_theme()

ggsave(paste0(figure_dir, "IPSL_Fish_timeseries_2100.png"), 
       p_fish, width = 12, height = 8, dpi = 300)

# 3. TOTAL CONSUMER BIOMASS PLOT
cat("Creating individual Total Consumer Biomass plot...\n")
p_tcb <- ipsl_data %>%
  ggplot(aes(x = Year, y = TCB_Change_1990s, color = scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9)

p_tcb <- add_common_elements(p_tcb) +
  labs(
    title = "Total Consumer Biomass Projections Through 2100",
    subtitle = "IPSL-CM6A-LR Model | Percentage change from 1990-1999 baseline",
    x = "Year",
    y = "Total consumer biomass change (%)"
  ) +
  create_enhanced_theme()

ggsave(paste0(figure_dir, "IPSL_TCB_timeseries_2100.png"), 
       p_tcb, width = 12, height = 8, dpi = 300)

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
summary_stats <- ipsl_data %>%
  filter(Year == 2100) %>%
  select(scenario, Zoop_Change_1990s, Fish_Change_1990s, TCB_Change_1990s) %>%
  arrange(scenario)

print(summary_stats)

cat("\nIndividual IPSL plotting complete!\n")
cat("Plots saved to:", figure_dir, "\n")
cat("Files created:\n")
cat("- IPSL_Zooplankton_timeseries_2100.png\n")
cat("- IPSL_Fish_timeseries_2100.png\n") 
cat("- IPSL_TCB_timeseries_2100.png\n")

cat("\n=== INDIVIDUAL IPSL PLOTTING COMPLETE ===\n")
