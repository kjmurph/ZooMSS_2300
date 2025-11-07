# ================================================================
# Prepare FishMIP-style outputs from ZooMSS compiled projections (v2)
# ================================================================
# - Reads compiled per-grid annual ZooMSS outputs
# - Builds FishMIP biomass variables (tcb, tpb, bp30cm, bp30to90cm, bp90cm)
# - Writes RDS/CSV outputs and creates quick diagnostics (timeseries + maps)
# - Focused on 2015–2300 scenarios (e.g., UKESM ssp534-over combined)
#
# Usage (from repo root):
#   Rscript Scripts/Core_Pipeline/FishMIP_2300_prepare_v2.R <model> <scenario>
# Example:
#   Rscript Scripts/Core_Pipeline/FishMIP_2300_prepare_v2.R ukesm1-0-ll ssp534-over

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript Scripts/Core_Pipeline/FishMIP_2300_prepare_v2.R <model> <scenario>")
}

model <- args[[1]]
scenario <- args[[2]]

repo_root <- getwd()
bioproj_dir <- file.path(repo_root, "Output", "Biomass_projections")
out_dir <- file.path(repo_root, "Output", "FishMIP_2300_Priority1_CORRECTED")
fig_dir <- file.path(repo_root, "Figures", "FishMIP_2300_v2")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

cat("=== FishMIP 2300 v2 export ===\n")
cat("Model:", model, " Scenario:", scenario, "\n")

# Choose input file; prefer combined 2015–2300 if present for overshoot
preferred_file <- file.path(bioproj_dir, sprintf(
  "Biomass_ClimateChange_Compiled_withZooMSS_%s_%s_2015-2300_Control.rds", model, scenario))
default_file <- file.path(bioproj_dir, sprintf(
  "Biomass_ClimateChange_Compiled_withZooMSS_%s_%s_Control.rds", model, scenario))

in_file <- if (file.exists(preferred_file)) preferred_file else default_file
if (!file.exists(in_file)) {
  stop("Input file not found: ", in_file)
}

cat("Reading:\n - ", in_file, "\n", sep = "")
d <- readRDS(in_file)

# Confirm required columns exist
species_cols <- c(
  "Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", "CarnCopepods",
  "Euphausiids", "Chaetognaths", "Salps", "Jellyfish",
  "Fish_Small", "Fish_Med", "Fish_Large"
)
req_cols <- c("Lon","Lat","Year", species_cols)
missing <- setdiff(req_cols, names(d))
if (length(missing) > 0) stop("Missing columns in input: ", paste(missing, collapse=","))

# Build FishMIP variables
# Assumptions:
# - Units are g m-2 wet weight (consistent with project summaries)
# - Model contains only pelagic consumers (so tpb == tcb)
# - Fish size classes map to length bins: Small=<30cm, Med=30-90cm, Large>=90cm

dm <- d %>%
  mutate(
    tcb = rowSums(across(all_of(species_cols)), na.rm = TRUE),
    tpb = tcb,
    bp30cm = Fish_Small,
    bp30to90cm = Fish_Med,
    bp90cm = Fish_Large
  ) %>%
  select(Lon, Lat, Year, tcb, tpb, bp30cm, bp30to90cm, bp90cm)

# Save as RDS and CSV
stem <- sprintf("zoomss_%s_%s_fishmip2300_v2", model, scenario)
out_rds <- file.path(out_dir, paste0(stem, ".rds"))
out_csv <- file.path(out_dir, paste0(stem, ".csv"))

saveRDS(dm, out_rds)
readr::write_csv(dm, out_csv)
cat("Wrote:\n - ", out_rds, "\n - ", out_csv, "\n", sep = "")

# Diagnostics: area-weighted global time series and quick maps
calculate_grid_area <- function(lat, resolution = 1) {
  R <- 6371
  res_rad <- resolution * pi / 180
  lat_rad <- lat * pi / 180
  lat_min <- lat_rad - res_rad/2
  lat_max <- lat_rad + res_rad/2
  area_km2 <- R^2 * res_rad * (sin(lat_max) - sin(lat_min))
  return(area_km2)
}

dm_aw <- dm %>%
  mutate(area_km2 = calculate_grid_area(Lat)) %>%
  group_by(Year) %>%
  summarise(
    tcb = sum(tcb * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(tcb)], na.rm = TRUE),
    tpb = sum(tpb * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(tpb)], na.rm = TRUE),
    bp30cm = sum(bp30cm * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(bp30cm)], na.rm = TRUE),
    bp30to90cm = sum(bp30to90cm * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(bp30to90cm)], na.rm = TRUE),
    bp90cm = sum(bp90cm * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(bp90cm)], na.rm = TRUE),
    .groups = 'drop'
  )

# Time series plot
p_ts <- dm_aw %>%
  pivot_longer(cols = tcb:bp90cm, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = sprintf("FishMIP variables (v2) — %s %s", model, scenario),
       y = "g m-2 (area-weighted)", x = "Year", color = "Variable") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, sprintf("%s_timeseries_v2.png", stem)), p_ts,
       width = 12, height = 6, dpi = 300)

# Quick maps for endpoint years if available
plot_map <- function(df, var, year, title_suffix) {
  df %>% filter(Year == year) %>%
    ggplot(aes(x = Lon, y = Lat, fill = .data[[var]])) +
    geom_raster(interpolate = FALSE) +
    coord_fixed(1.0) +
    scale_fill_viridis_c(option = "C", na.value = "grey80") +
    labs(title = sprintf("%s (v2) — %s %s — %s", var, model, scenario, title_suffix),
         fill = "g m-2", x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

yr_min <- min(dm$Year, na.rm = TRUE)
yr_max <- max(dm$Year, na.rm = TRUE)

for (yr in unique(c(yr_min, 2100, 2200, yr_max))) {
  if (!is.finite(yr) || !(yr %in% dm$Year)) next
  pm <- plot_map(dm, "tcb", yr, paste0("Year ", yr))
  ggsave(file.path(fig_dir, sprintf("%s_tcb_map_%d_v2.png", stem, yr)), pm,
         width = 12, height = 5.5, dpi = 300)
}

cat("Diagnostics written to:", fig_dir, "\n")
cat("=== Done ===\n")
