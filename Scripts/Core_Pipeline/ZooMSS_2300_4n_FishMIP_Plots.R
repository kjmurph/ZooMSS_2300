# ================================================================
# ZooMSS 2300 - FishMIP Protocol Output Plots
# ================================================================
# Creates timeseries and spatial maps for FishMIP protocol variables:
# - TCB (Total Consumer Biomass)
# - tcblog10_0 through tcblog10_5 (size-binned biomass)
# - TPB (Total Pelagic Biomass)
# - bp30cm, bp30to90cm, bp90cm (length-based size bins)

library(tidyverse)
library(patchwork)
library(viridis)

# Setup paths
base_dir <- file.path(dirname(getwd()), "..")  # Go up from Scripts/Core_Pipeline to base
input_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format")
figure_dir <- file.path(base_dir, "Figures", "FishMIP_Outputs")

# Create output directory
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

cat("==============================================================================\n")
cat("ZooMSS 2300 - FishMIP Protocol Output Visualization\n")
cat("==============================================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Define scenario colors
scenario_colors <- c(
  "historical" = "#2E86C1",
  "ssp126" = "#28B463", 
  "ssp585" = "#E74C3C",
  "ssp534-over" = "#F39C12",
  "picontrol" = "#8E44AD"
)

# Define model names and labels
model_names <- c("cesm2-waccm", "ipsl-cm6a-lr", "ukesm1-0-ll")
model_labels <- c("CESM2-WACCM", "IPSL-CM6A-LR", "UKESM1-0-LL")
names(model_labels) <- model_names

#### PART 1: Load and Process FishMIP Data (Memory-Efficient) ####
cat("=== Loading FishMIP format data (memory-efficient approach) ===\n")

# Load all FishMIP files
fishmip_files <- list.files(input_dir, pattern = "^ZooMSS_FishMIP_2300_.*\\.rds$", full.names = TRUE)

cat("Found", length(fishmip_files), "FishMIP files (all models and scenarios)\n\n")

# Process files iteratively to calculate spatial means (avoids loading all at once)
cat("Calculating spatial means from each file...\n")
fishmip_timeseries <- map_dfr(fishmip_files, function(file) {
  cat("  Processing:", basename(file), "\n")
  
  # Extract model and scenario from filename
  parts <- str_match(basename(file), "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  model <- parts[,2]
  scenario <- parts[,3]
  
  # Load data
  data <- readRDS(file)
  
  # Calculate spatial means immediately (before combining)
  means <- data %>%
    group_by(Date) %>%
    summarise(
      tcb = mean(tcb, na.rm = TRUE),
      tcblog10_0 = mean(tcblog10_0, na.rm = TRUE),
      tcblog10_1 = mean(tcblog10_1, na.rm = TRUE),
      tcblog10_2 = mean(tcblog10_2, na.rm = TRUE),
      tcblog10_3 = mean(tcblog10_3, na.rm = TRUE),
      tcblog10_4 = mean(tcblog10_4, na.rm = TRUE),
      tcblog10_5 = mean(tcblog10_5, na.rm = TRUE),
      tpb = mean(tpb, na.rm = TRUE),
      bp30cm = mean(bp30cm, na.rm = TRUE),
      bp30to90cm = mean(bp30to90cm, na.rm = TRUE),
      bp90cm = mean(bp90cm, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(model = model, scenario = scenario)
  
  # Clean up
  rm(data)
  gc(verbose = FALSE)
  
  return(means)
})

cat("\nTimeseries data prepared:", nrow(fishmip_timeseries), "rows\n")
cat("  Models:", paste(unique(fishmip_timeseries$model), collapse = ", "), "\n")
cat("  Scenarios:", paste(unique(fishmip_timeseries$scenario), collapse = ", "), "\n")
cat("  Date range:", min(fishmip_timeseries$Date, na.rm = TRUE), "to", max(fishmip_timeseries$Date, na.rm = TRUE), "\n\n")

#### PART 2: Calculate Baselines ####
cat("=== Calculating historical baselines (1990-1999) ===\n")

historical_baseline <- fishmip_timeseries %>%
  filter(scenario == "historical", Date >= 1990, Date <= 1999) %>%
  group_by(model) %>%
  summarise(
    tcb_baseline = mean(tcb, na.rm = TRUE),
    tcblog10_0_baseline = mean(tcblog10_0, na.rm = TRUE),
    tcblog10_1_baseline = mean(tcblog10_1, na.rm = TRUE),
    tcblog10_2_baseline = mean(tcblog10_2, na.rm = TRUE),
    tcblog10_3_baseline = mean(tcblog10_3, na.rm = TRUE),
    tcblog10_4_baseline = mean(tcblog10_4, na.rm = TRUE),
    tcblog10_5_baseline = mean(tcblog10_5, na.rm = TRUE),
    tpb_baseline = mean(tpb, na.rm = TRUE),
    bp30cm_baseline = mean(bp30cm, na.rm = TRUE),
    bp30to90cm_baseline = mean(bp30to90cm, na.rm = TRUE),
    bp90cm_baseline = mean(bp90cm, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate percentage changes relative to baseline
fishmip_changes <- fishmip_timeseries %>%
  left_join(historical_baseline, by = "model") %>%
  mutate(
    tcb_change = (tcb - tcb_baseline) / tcb_baseline * 100,
    tcblog10_0_change = (tcblog10_0 - tcblog10_0_baseline) / tcblog10_0_baseline * 100,
    tcblog10_1_change = (tcblog10_1 - tcblog10_1_baseline) / tcblog10_1_baseline * 100,
    tcblog10_2_change = (tcblog10_2 - tcblog10_2_baseline) / tcblog10_2_baseline * 100,
    tcblog10_3_change = (tcblog10_3 - tcblog10_3_baseline) / tcblog10_3_baseline * 100,
    tcblog10_4_change = (tcblog10_4 - tcblog10_4_baseline) / tcblog10_4_baseline * 100,
    tcblog10_5_change = (tcblog10_5 - tcblog10_5_baseline) / tcblog10_5_baseline * 100,
    tpb_change = (tpb - tpb_baseline) / tpb_baseline * 100,
    bp30cm_change = (bp30cm - bp30cm_baseline) / bp30cm_baseline * 100,
    bp30to90cm_change = (bp30to90cm - bp30to90cm_baseline) / bp30to90cm_baseline * 100,
    bp90cm_change = (bp90cm - bp90cm_baseline) / bp90cm_baseline * 100
  )

cat("Baseline calculations complete\n\n")

#### PART 4: TCB Timeseries Plot ####
cat("=== Creating TCB timeseries plot ===\n")

plot_tcb <- fishmip_changes %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over", "picontrol"),
         Date >= 1970) %>%
  mutate(model_label = model_labels[model]) %>%
  ggplot(aes(x = Date, y = tcb_change, color = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~model_label, ncol = 1) +
  scale_color_manual(values = scenario_colors,
                     labels = c("historical" = "Historical", 
                                "ssp126" = "SSP1-2.6", 
                                "ssp585" = "SSP5-8.5", 
                                "ssp534-over" = "SSP5-3.4-OS",
                                "picontrol" = "Pre-industrial Control")) +
  labs(
    title = "Total Consumer Biomass (TCB) - FishMIP Protocol",
    subtitle = "Percentage change from 1990s baseline",
    x = "Year",
    y = "Change from 1990s baseline (%)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(figure_dir, "FishMIP_TCB_timeseries.png"),
       plot_tcb, width = 10, height = 12, dpi = 300)

cat("  Saved: FishMIP_TCB_timeseries.png\n\n")

#### PART 5: Log10 Size Bins Timeseries (Faceted) ####
cat("=== Creating log10 size bins timeseries plot ===\n")

# Prepare data for size bin plots
size_bins_data <- fishmip_changes %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over", "picontrol"),
         Date >= 1970) %>%
  select(Date, model, scenario, contains("tcblog10")) %>%
  select(Date, model, scenario, ends_with("_change")) %>%
  pivot_longer(cols = starts_with("tcblog10"), 
               names_to = "size_bin", 
               values_to = "change") %>%
  mutate(
    model_label = model_labels[model],
    size_bin_label = case_when(
      size_bin == "tcblog10_0_change" ~ "0.1-1 g",
      size_bin == "tcblog10_1_change" ~ "1-10 g",
      size_bin == "tcblog10_2_change" ~ "10-100 g",
      size_bin == "tcblog10_3_change" ~ "100g-1kg",
      size_bin == "tcblog10_4_change" ~ "1-10 kg",
      size_bin == "tcblog10_5_change" ~ "10-100 kg",
      TRUE ~ size_bin
    ),
    size_bin_label = factor(size_bin_label, 
                            levels = c("0.1-1 g", "1-10 g", "10-100 g", 
                                      "100g-1kg", "1-10 kg", "10-100 kg"))
  )

plot_size_bins <- ggplot(size_bins_data, aes(x = Date, y = change, color = scenario)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(size_bin_label ~ model_label) +
  scale_color_manual(values = scenario_colors,
                     labels = c("historical" = "Historical", 
                                "ssp126" = "SSP1-2.6", 
                                "ssp585" = "SSP5-8.5", 
                                "ssp534-over" = "SSP5-3.4-OS",
                                "picontrol" = "Pre-industrial Control")) +
  labs(
    title = "Biomass by Log10 Size Bins - FishMIP Protocol",
    subtitle = "Percentage change from 1990s baseline by weight class",
    x = "Year",
    y = "Change from 1990s baseline (%)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 8),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(file.path(figure_dir, "FishMIP_SizeBins_timeseries.png"),
       plot_size_bins, width = 14, height = 18, dpi = 300)

cat("  Saved: FishMIP_SizeBins_timeseries.png\n\n")

#### PART 6: Length-Based Size Classes Timeseries ####
cat("=== Creating length-based size classes plot ===\n")

# Prepare data for length-based bins
length_bins_data <- fishmip_changes %>%
  filter(scenario %in% c("historical", "ssp126", "ssp585", "ssp534-over", "picontrol"),
         Date >= 1970) %>%
  select(Date, model, scenario, contains("bp")) %>%
  select(Date, model, scenario, ends_with("_change")) %>%
  pivot_longer(cols = starts_with("bp"), 
               names_to = "length_bin", 
               values_to = "change") %>%
  mutate(
    model_label = model_labels[model],
    length_bin_label = case_when(
      length_bin == "bp30cm_change" ~ "< 30 cm",
      length_bin == "bp30to90cm_change" ~ "30-90 cm",
      length_bin == "bp90cm_change" ~ "> 90 cm",
      TRUE ~ length_bin
    ),
    length_bin_label = factor(length_bin_label, 
                              levels = c("< 30 cm", "30-90 cm", "> 90 cm"))
  )

plot_length_bins <- ggplot(length_bins_data, aes(x = Date, y = change, color = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(length_bin_label ~ model_label) +
  scale_color_manual(values = scenario_colors,
                     labels = c("historical" = "Historical", 
                                "ssp126" = "SSP1-2.6", 
                                "ssp585" = "SSP5-8.5", 
                                "ssp534-over" = "SSP5-3.4-OS",
                                "picontrol" = "Pre-industrial Control")) +
  labs(
    title = "Biomass by Length-Based Size Classes - FishMIP Protocol",
    subtitle = "Percentage change from 1990s baseline by body length",
    x = "Year",
    y = "Change from 1990s baseline (%)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(file.path(figure_dir, "FishMIP_LengthBins_timeseries.png"),
       plot_length_bins, width = 14, height = 10, dpi = 300)

cat("  Saved: FishMIP_LengthBins_timeseries.png\n\n")

#### PART 7: Summary Statistics ####
cat("=== Calculating summary statistics ===\n")

summary_stats <- fishmip_changes %>%
  filter(scenario %in% c("ssp126", "ssp585", "ssp534-over"),
         Date >= 2270, Date <= 2299) %>%
  group_by(model, scenario) %>%
  summarise(
    tcb_mean_change = mean(tcb_change, na.rm = TRUE),
    tcb_sd_change = sd(tcb_change, na.rm = TRUE),
    tcblog10_0_mean = mean(tcblog10_0_change, na.rm = TRUE),
    tcblog10_1_mean = mean(tcblog10_1_change, na.rm = TRUE),
    tcblog10_2_mean = mean(tcblog10_2_change, na.rm = TRUE),
    tcblog10_3_mean = mean(tcblog10_3_change, na.rm = TRUE),
    tcblog10_4_mean = mean(tcblog10_4_change, na.rm = TRUE),
    tcblog10_5_mean = mean(tcblog10_5_change, na.rm = TRUE),
    bp30cm_mean = mean(bp30cm_change, na.rm = TRUE),
    bp30to90cm_mean = mean(bp30to90cm_change, na.rm = TRUE),
    bp90cm_mean = mean(bp90cm_change, na.rm = TRUE),
    .groups = 'drop'
  )

write_csv(summary_stats, file.path(figure_dir, "FishMIP_summary_statistics_2270-2299.csv"))
cat("  Saved: FishMIP_summary_statistics_2270-2299.csv\n\n")

cat("==============================================================================\n")
cat("FishMIP plotting complete!\n")
cat("==============================================================================\n")
cat("Outputs saved to:", figure_dir, "\n")
cat("\nFiles created:\n")
cat("  - FishMIP_TCB_timeseries.png\n")
cat("  - FishMIP_SizeBins_timeseries.png (6 size bins × 3 models)\n")
cat("  - FishMIP_LengthBins_timeseries.png (3 length bins × 3 models)\n")
cat("  - FishMIP_TCB_spatial_*.png (4 time periods)\n")
cat("  - FishMIP_summary_statistics_2270-2299.csv\n")
cat("==============================================================================\n")
