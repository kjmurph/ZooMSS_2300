# ==================================================================
# Diagnostics: Compare sum(tcblog10 bins) versus tcb (FishMIP v2)
# Usage:
#   Rscript Scripts/FishMIP_Export/diagnose_tcblog10_vs_tcb_v2.R <model> <scenario>
# Example:
#   Rscript Scripts/FishMIP_Export/diagnose_tcblog10_vs_tcb_v2.R ipsl-cm6a-lr ssp126
# ==================================================================

suppressPackageStartupMessages({
  library(ncdf4)
  library(tidyverse)
  library(viridis)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript ... <model> <scenario>")
model <- args[[1]]
scenario <- args[[2]]

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
out_nc_dir <- file.path(project_root, "Output", "FishMIP_NetCDF_v2")
fig_dir <- file.path(project_root, "Figures", "FishMIP_2300_v2", "Diagnostics")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

file_tcblog10 <- file.path(out_nc_dir, sprintf("zoomss_%s_%s_nat_default_tcblog10_global_annual_%%s_%%s.nc", model, scenario))
file_tcb      <- file.path(out_nc_dir, sprintf("zoomss_%s_%s_nat_default_tcb_global_annual_%%s_%%s.nc", model, scenario))

# Discover the time range from tcb file by matching wildcard years
nc_candidates <- list.files(out_nc_dir, pattern = sprintf("zoomss_%s_%s_nat_default_tcb_global_annual_.*.nc", model, scenario), full.names = TRUE)
if (length(nc_candidates) == 0) stop("tcb NetCDF not found for ", model, " ", scenario)
# Pick the first (there should be only one)
file_tcb_resolved <- nc_candidates[[1]]
# Derive years
yr_part <- sub(".*tcb_global_annual_(.*)\\.nc$", "\\1", file_tcb_resolved)
yrs <- strsplit(yr_part, "_")[[1]]
y1 <- as.integer(yrs[1]); y2 <- as.integer(yrs[2])

file_tcblog10_resolved <- sprintf(file_tcblog10, y1, y2)
stopifnot(file.exists(file_tcblog10_resolved))

# Open files
nct <- nc_open(file_tcb_resolved)
ncb <- nc_open(file_tcblog10_resolved)

on.exit({
  try(nc_close(nct), silent = TRUE)
  try(nc_close(ncb), silent = TRUE)
})

# Dimensions
lon <- ncvar_get(nct, "lon")
lat <- ncvar_get(nct, "lat")
time <- ncvar_get(nct, "time")
years <- 1901 + time/365

# Helper: grid cell area (km^2)
calculate_grid_area <- function(lat, resolution = 1) {
  R <- 6371
  res_rad <- resolution * pi / 180
  lat_rad <- lat * pi / 180
  lat_min <- lat_rad - res_rad/2
  lat_max <- lat_rad + res_rad/2
  R^2 * res_rad * (sin(lat_max) - sin(lat_min))
}
area_lat <- calculate_grid_area(lat) # km^2 per 1x1 cell at each latitude
area_mat <- matrix(rep(area_lat, each = length(lon)), nrow = length(lat), ncol = length(lon), byrow = FALSE)
area_vec <- as.numeric(area_mat)
A_tot <- sum(area_vec, na.rm = TRUE)

# Iterate time to compute global area-weighted means
nT <- length(time)
mu_tcb <- numeric(nT)
mu_bins <- numeric(nT)

for (ti in seq_len(nT)) {
  tcb_slice <- ncvar_get(nct, "tcb", start = c(ti, 1, 1), count = c(1, -1, -1))
  bins_slice <- ncvar_get(ncb, "tcblog10", start = c(ti, 1, 1, 1), count = c(1, -1, -1, -1))
  sum_bins <- apply(bins_slice[1,,,drop=FALSE], c(2,3), sum, na.rm = TRUE) # sum over bins -> [lat,lon]
  # align to vectors
  x_tcb <- as.numeric(tcb_slice)
  x_bins <- as.numeric(sum_bins)
  w <- area_vec
  # mask NA consistently
  m <- is.finite(x_tcb) & is.finite(x_bins)
  if (!any(m)) { mu_tcb[ti] <- NA; mu_bins[ti] <- NA; next }
  mu_tcb[ti] <- sum(x_tcb[m] * w[m], na.rm = TRUE) / sum(w[m])
  mu_bins[ti] <- sum(x_bins[m] * w[m], na.rm = TRUE) / sum(w[m])
}

# Combine
p <- tibble(Year = as.integer(round(years)),
            tcb = mu_tcb,
            tcblog10_sum = mu_bins) %>%
  mutate(pct_diff = 100 * (tcblog10_sum - tcb) / pmax(tcb, .Machine$double.eps))

# Plot time series
p_ts <- ggplot(p, aes(x = Year)) +
  geom_line(aes(y = tcb, color = "tcb"), linewidth = 0.8) +
  geom_line(aes(y = tcblog10_sum, color = "sum(tcblog10)"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("tcb" = "#1b9e77", "sum(tcblog10)" = "#d95f02"), name = "Series") +
  labs(title = sprintf("Global mean biomass: %s %s", model, scenario), y = "g m-2", x = "Year") +
  theme_minimal(base_size = 12)

ggsave(filename = file.path(fig_dir, sprintf("%s_%s_global_mean_tcb_vs_tcblog10_v2.png", model, scenario)), p_ts, width = 9, height = 4.5, dpi = 150)

# Plot percent difference
p_pd <- ggplot(p, aes(x = Year, y = pct_diff)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line(color = "#7570b3", linewidth = 0.8) +
  labs(title = sprintf("Percent difference: sum(tcblog10) - tcb (%%): %s %s", model, scenario), y = "%", x = "Year") +
  theme_minimal(base_size = 12)

ggsave(filename = file.path(fig_dir, sprintf("%s_%s_percent_diff_tcblog10_vs_tcb_v2.png", model, scenario)), p_pd, width = 9, height = 4.5, dpi = 150)

# Map of difference for first and last years
make_diff_map <- function(ti, label) {
  tcb_slice <- ncvar_get(nct, "tcb", start = c(ti, 1, 1), count = c(1, -1, -1))
  bins_slice <- ncvar_get(ncb, "tcblog10", start = c(ti, 1, 1, 1), count = c(1, -1, -1, -1))
  sum_bins <- apply(bins_slice[1,,,drop=FALSE], c(2,3), sum, na.rm = TRUE)
  diff <- sum_bins - tcb_slice
  df <- expand.grid(Lon = lon, Lat = lat)
  df$diff <- as.numeric(diff)
  ggplot(df, aes(x = Lon, y = Lat, fill = diff)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_viridis(option = "C", na.value = "transparent") +
    coord_equal(expand = FALSE) +
    labs(title = sprintf("Difference (sum(tcblog10) - tcb): %s %s %s", model, scenario, label), fill = "g m-2") +
    theme_minimal(base_size = 12)
}

p_map_start <- make_diff_map(1, sprintf("%d", y1))
GG_out1 <- file.path(fig_dir, sprintf("%s_%s_diffmap_%d_v2.png", model, scenario, y1))
ggsave(GG_out1, p_map_start, width = 9, height = 4.5, dpi = 150)

p_map_end <- make_diff_map(length(time), sprintf("%d", y2))
GG_out2 <- file.path(fig_dir, sprintf("%s_%s_diffmap_%d_v2.png", model, scenario, y2))
ggsave(GG_out2, p_map_end, width = 9, height = 4.5, dpi = 150)

cat("Wrote diagnostics to:", fig_dir, "\n")
