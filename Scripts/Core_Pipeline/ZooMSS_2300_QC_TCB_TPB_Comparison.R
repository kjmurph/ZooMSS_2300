# ================================================================
# ZooMSS 2300 - QC Check: TCB vs TPB Comparison
# ================================================================
# Verifies that Total Consumer Biomass (TCB) equals Total Pelagic 
# Biomass (TPB) since all ZooMSS organisms are pelagic

library(tidyverse)
library(patchwork)

# Setup paths
base_dir <- getwd()
input_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format_submission_version")
output_dir <- file.path(base_dir, "Figures", "FishMIP_Outputs", "QC_Checks")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("==============================================================================\n")
cat("ZooMSS 2300 - TCB vs TPB Comparison QC Check\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Find all FishMIP files
fishmip_files <- list.files(input_dir, pattern = "^ZooMSS_FishMIP_2300_.*\\.rds$", full.names = TRUE)

cat("Found", length(fishmip_files), "FishMIP files\n\n")

# Load and combine all data
all_data <- map_dfr(fishmip_files, function(file) {
  cat("Loading:", basename(file), "\n")
  data <- readRDS(file)
  
  # Extract metadata from filename
  filename <- basename(file)
  parts <- str_split(filename, "_")[[1]]
  model <- parts[4]
  scenario <- str_remove(parts[5], "\\.rds$")
  
  # Select only TCB and TPB columns plus identifiers
  data %>%
    select(Date, Lat, Lon, tcb, tpb) %>%
    mutate(
      model = model,
      scenario = scenario
    )
})

cat("\nTotal rows loaded:", nrow(all_data), "\n")
cat("Date range:", min(all_data$Date), "to", max(all_data$Date), "\n\n")

# ================================================================
# 1. Calculate differences
# ================================================================

cat("=== Calculating TCB vs TPB Differences ===\n")

all_data <- all_data %>%
  mutate(
    difference = tcb - tpb,
    pct_difference = if_else(tcb > 0, (difference / tcb) * 100, 0),
    abs_pct_diff = abs(pct_difference)
  )

# Summary statistics
summary_stats <- all_data %>%
  group_by(model, scenario) %>%
  summarise(
    n_cells = n(),
    mean_tcb = mean(tcb, na.rm = TRUE),
    mean_tpb = mean(tpb, na.rm = TRUE),
    mean_diff = mean(difference, na.rm = TRUE),
    max_abs_diff = max(abs(difference), na.rm = TRUE),
    mean_pct_diff = mean(abs_pct_diff, na.rm = TRUE),
    max_pct_diff = max(abs_pct_diff, na.rm = TRUE),
    n_exact_match = sum(difference == 0, na.rm = TRUE),
    pct_exact_match = (n_exact_match / n_cells) * 100,
    .groups = "drop"
  )

cat("\n=== Summary Statistics by Model and Scenario ===\n")
print(summary_stats, n = Inf)

# Overall summary
overall <- all_data %>%
  summarise(
    total_cells = n(),
    mean_tcb = mean(tcb, na.rm = TRUE),
    mean_tpb = mean(tpb, na.rm = TRUE),
    mean_diff = mean(difference, na.rm = TRUE),
    max_abs_diff = max(abs(difference), na.rm = TRUE),
    mean_pct_diff = mean(abs_pct_diff, na.rm = TRUE),
    max_pct_diff = max(abs_pct_diff, na.rm = TRUE),
    n_exact_match = sum(difference == 0, na.rm = TRUE),
    pct_exact_match = (n_exact_match / total_cells) * 100
  )

cat("\n=== Overall Summary ===\n")
cat("Total data points:", overall$total_cells, "\n")
cat("Mean TCB:", round(overall$mean_tcb, 4), "g/m²\n")
cat("Mean TPB:", round(overall$mean_tpb, 4), "g/m²\n")
cat("Mean difference:", round(overall$mean_diff, 6), "g/m²\n")
cat("Max absolute difference:", round(overall$max_abs_diff, 6), "g/m²\n")
cat("Mean % difference:", round(overall$mean_pct_diff, 6), "%\n")
cat("Max % difference:", round(overall$max_pct_diff, 6), "%\n")
cat("Exact matches:", overall$n_exact_match, "(", round(overall$pct_exact_match, 2), "%)\n")

# Save summary statistics
write_csv(summary_stats, file.path(output_dir, "TCB_TPB_comparison_summary.csv"))
cat("\nSaved summary to:", file.path(output_dir, "TCB_TPB_comparison_summary.csv"), "\n")

# ================================================================
# 2. Create timeseries comparison plots
# ================================================================

cat("\n=== Creating Timeseries Comparison Plots ===\n")

# Calculate annual means for plotting
annual_data <- all_data %>%
  group_by(Date, model, scenario) %>%
  summarise(
    tcb = mean(tcb, na.rm = TRUE),
    tpb = mean(tpb, na.rm = TRUE),
    difference = mean(difference, na.rm = TRUE),
    pct_difference = mean(pct_difference, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    model_scenario = paste(model, scenario, sep = "_")
  )

# Plot 1: TCB vs TPB overlaid timeseries
p1 <- annual_data %>%
  pivot_longer(cols = c(tcb, tpb), names_to = "variable", values_to = "biomass") %>%
  ggplot(aes(x = Date, y = biomass, color = variable, linetype = model)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c(tcb = "#D55E00", tpb = "#0072B2"),
    labels = c(tcb = "TCB (Total Consumer Biomass)", tpb = "TPB (Total Pelagic Biomass)")
  ) +
  labs(
    title = "TCB vs TPB Comparison Across All ESMs and Scenarios",
    subtitle = "Lines should overlap perfectly if TCB = TPB (all organisms are pelagic)",
    x = "Year",
    y = "Biomass (g/m²)",
    color = "Variable",
    linetype = "ESM"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(output_dir, "TCB_TPB_timeseries_overlay.png"),
  plot = p1,
  width = 12,
  height = 14,
  dpi = 300,
  bg = "white"
)
cat("✓ Saved: TCB_TPB_timeseries_overlay.png\n")

# Plot 2: Difference timeseries
p2 <- annual_data %>%
  ggplot(aes(x = Date, y = difference, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c(
      "cesm2-waccm" = "#E69F00",
      "ipsl-cm6a-lr" = "#56B4E9",
      "ukesm1-0-ll" = "#009E73"
    )
  ) +
  labs(
    title = "Difference Between TCB and TPB (TCB - TPB)",
    subtitle = "Should be zero or near-zero if all organisms are classified as pelagic",
    x = "Year",
    y = "Difference (g/m²)",
    color = "ESM"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(output_dir, "TCB_TPB_difference_timeseries.png"),
  plot = p2,
  width = 12,
  height = 14,
  dpi = 300,
  bg = "white"
)
cat("✓ Saved: TCB_TPB_difference_timeseries.png\n")

# Plot 3: Percentage difference timeseries
p3 <- annual_data %>%
  ggplot(aes(x = Date, y = pct_difference, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c(
      "cesm2-waccm" = "#E69F00",
      "ipsl-cm6a-lr" = "#56B4E9",
      "ukesm1-0-ll" = "#009E73"
    )
  ) +
  labs(
    title = "Percentage Difference Between TCB and TPB",
    subtitle = "((TCB - TPB) / TCB) × 100",
    x = "Year",
    y = "% Difference",
    color = "ESM"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(output_dir, "TCB_TPB_pct_difference_timeseries.png"),
  plot = p3,
  width = 12,
  height = 14,
  dpi = 300,
  bg = "white"
)
cat("✓ Saved: TCB_TPB_pct_difference_timeseries.png\n")

# ================================================================
# 3. Create scatter plot
# ================================================================

cat("\n=== Creating Scatter Plot ===\n")

# Sample data for scatter (too many points otherwise)
set.seed(123)
sample_data <- all_data %>%
  filter(tcb > 0, tpb > 0) %>%  # Only non-zero values
  sample_n(min(50000, n()))  # Max 50k points

p4 <- ggplot(sample_data, aes(x = tcb, y = tpb, color = model)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(
    values = c(
      "cesm2-waccm" = "#E69F00",
      "ipsl-cm6a-lr" = "#56B4E9",
      "ukesm1-0-ll" = "#009E73"
    )
  ) +
  labs(
    title = "TCB vs TPB Scatter Plot (Log Scale)",
    subtitle = paste0("Sample of ", nrow(sample_data), " points. Red line = perfect 1:1 match"),
    x = "Total Consumer Biomass (g/m²)",
    y = "Total Pelagic Biomass (g/m²)",
    color = "ESM"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(output_dir, "TCB_TPB_scatter.png"),
  plot = p4,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
cat("✓ Saved: TCB_TPB_scatter.png\n")

cat("\n==============================================================================\n")
cat("QC Check Complete!\n")
cat("==============================================================================\n")
cat("\nOutput files saved to:", output_dir, "\n")
cat("  - TCB_TPB_comparison_summary.csv\n")
cat("  - TCB_TPB_timeseries_overlay.png\n")
cat("  - TCB_TPB_difference_timeseries.png\n")
cat("  - TCB_TPB_pct_difference_timeseries.png\n")
cat("  - TCB_TPB_scatter.png\n")
cat("\n")
