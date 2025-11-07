# ==================================================================
# Diagnostics from mapping: Compare sum(tcblog10 bins) versus tcb
# without writing NetCDFs (memory-safe, per-year kNN mapping)
# Usage:
#   Rscript Scripts/FishMIP_Export/diagnose_tcblog10_vs_tcb_from_mapping_v2.R <model> <scenario>
# Example:
#   Rscript Scripts/FishMIP_Export/diagnose_tcblog10_vs_tcb_from_mapping_v2.R ipsl-cm6a-lr ssp126
# ==================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaImpute)
  library(viridis)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript ... <model> <scenario>")
model_name <- args[[1]]
scenario <- args[[2]]

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "Scripts/Utilities/fZooMSS_Xtras.R"))

env_dir <- file.path(project_root, "Enviro_Matrix")
proc_env_dir <- file.path(project_root, "Input", "2300_processed")
fig_dir <- file.path(project_root, "Figures", "FishMIP_2300_v2", "Diagnostics")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Load ZooMSS enviro reference (original + novel)
enviro_data_original <- readr::read_rds(file.path(env_dir, "ClimateChange_Compiled_Distinct.rds"))
enviro_data_novel    <- readr::read_rds(file.path(env_dir, "novel_sst_chl_combinations_2300_wPhyto.rds"))
common_cols <- intersect(names(enviro_data_original), names(enviro_data_novel))
enviro_data <- bind_rows(
  enviro_data_original %>% select(all_of(common_cols)),
  enviro_data_novel %>% select(all_of(common_cols))
) %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

# Load ZooMSS size results
zoo_original <- readr::read_rds(file.path(project_root, "Input", "res_Control.RDS"))
zoo_novel    <- readr::read_rds(file.path(project_root, "Input", "res_ZooMSS_2300.RDS"))
mdl          <- readr::read_rds(file.path(project_root, "Input", "model_Control.RDS"))

minb <- 1; maxb <- 158
mdl2 <- mdl; mdl2$param$w <- mdl$param$w[minb:maxb]
size_biom_list <- fZooMSS_SizeBiomass(fZooMSS_ExtractSizeRange(c(zoo_original, zoo_novel), minb, maxb), mdl2)
size_mat <- do.call(rbind, size_biom_list)

# FishMIP bins
w <- mdl2$param$w
bin_edges <- list(c(1,10), c(10,100), c(100,1000), c(1000,10000), c(10000,100000), c(100000, Inf))
bin_indices <- lapply(bin_edges, function(e) which(w >= e[1] & w < e[2]))

# Load processed env for target model/scenario
env_file_main <- file.path(proc_env_dir, sprintf("2300_%s_%s.rds", model_name, scenario))
env_file_extra <- file.path(proc_env_dir, sprintf("2300_%s_%s_2101-2300.rds", model_name, scenario))
read_env <- function(f) if (file.exists(f)) readr::read_rds(f) else NULL
nc_main <- read_env(env_file_main)
nc_extra <- read_env(env_file_extra)
if (is.null(nc_main) && is.null(nc_extra)) stop("No processed enviro files found: ", model_name, " ", scenario)

harmonize_env <- function(df) {
  if (is.null(df)) return(NULL)
  if ("Chl" %in% names(df)) chl <- df$Chl
  else if ("chlo" %in% names(df)) chl <- df$chlo
  else if ("chl" %in% names(df)) chl <- df$chl
  else stop("No Chl column found")
  df$Chl <- chl
  if (!"Chl_log10" %in% names(df)) df$Chl_log10 <- log10(chl)
  df
}

nc_all <- bind_rows(harmonize_env(nc_main), harmonize_env(nc_extra))

# Grid
lon_vals <- sort(unique(nc_all$Lon))
lat_vals <- sort(unique(nc_all$Lat))
year_vals <- sort(unique(nc_all$Year))
lon_to_idx <- setNames(seq_along(lon_vals), lon_vals)
lat_to_idx <- setNames(seq_along(lat_vals), lat_vals)

# Area weights
calculate_grid_area <- function(lat, resolution = 1) {
  R <- 6371
  res_rad <- resolution * pi / 180
  lat_rad <- lat * pi / 180
  lat_min <- lat_rad - res_rad/2
  lat_max <- lat_rad + res_rad/2
  R^2 * res_rad * (sin(lat_max) - sin(lat_min))
}
area_lat <- calculate_grid_area(lat_vals)
area_mat <- matrix(rep(area_lat, each = length(lon_vals)), nrow = length(lat_vals), ncol = length(lon_vals), byrow = FALSE)
area_vec <- as.numeric(area_mat)

# Precompute ref env matrix
ref_mat <- as.matrix(enviro_data[, c("SST", "Chl_log10")])
fillv <- NA_real_

mu_tcb <- numeric(length(year_vals))
mu_bins <- numeric(length(year_vals))
first_diff <- NULL
last_diff <- NULL

for (i in seq_along(year_vals)) {
  yr <- year_vals[i]
  sel <- nc_all$Year == yr
  tgt_mat <- as.matrix(nc_all[sel, c("SST", "Chl_log10")])
  # chunked kNN
  n_pts <- nrow(tgt_mat)
  this_cid <- integer(n_pts)
  chunk_size <- 20000L
  s <- 1L
  while (s <= n_pts) {
    e <- min(s + chunk_size - 1L, n_pts)
    knn_out <- yaImpute::ann(ref_mat, tgt_mat[s:e, , drop = FALSE], k = 1, verbose = FALSE)
    this_cid[s:e] <- knn_out$knnIndexDist[,1]
    rm(knn_out); gc()
    s <- e + 1L
  }
  lon_i <- lon_to_idx[as.character(nc_all$Lon[sel])]
  lat_i <- lat_to_idx[as.character(nc_all$Lat[sel])]
  # Extract size rows and compute bin sums per point
  sm <- size_mat[this_cid, , drop = FALSE]
  bin_sums <- lapply(bin_indices, function(cols) if (length(cols)>0) rowSums(sm[, cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(sm)))
  tcb_vals <- Reduce(`+`, bin_sums)
  sum_bins_vals <- tcb_vals # identical by definition
  # Grid slices
  nlon <- length(lon_vals); nlat <- length(lat_vals)
  tcb_slice <- matrix(NA_real_, nrow = nlat, ncol = nlon)
  sum_slice <- matrix(NA_real_, nrow = nlat, ncol = nlon)
  lin <- (lat_i - 1) * nlon + lon_i
  tcb_slice[lin] <- tcb_vals
  sum_slice[lin] <- sum_bins_vals
  # Area-weighted means
  x_tcb <- as.numeric(tcb_slice)
  x_bins <- as.numeric(sum_slice)
  m <- is.finite(x_tcb) & is.finite(x_bins)
  mu_tcb[i] <- sum(x_tcb[m] * area_vec[m], na.rm = TRUE) / sum(area_vec[m])
  mu_bins[i] <- sum(x_bins[m] * area_vec[m], na.rm = TRUE) / sum(area_vec[m])
  if (i == 1) first_diff <- sum_slice - tcb_slice
  if (i == length(year_vals)) last_diff <- sum_slice - tcb_slice
  cat("Done year:", yr, "\n")
}

p <- tibble(Year = year_vals, tcb = mu_tcb, tcblog10_sum = mu_bins) %>%
  mutate(pct_diff = 100 * (tcblog10_sum - tcb) / pmax(tcb, .Machine$double.eps))

# Plots
p_ts <- ggplot(p, aes(x = Year)) +
  geom_line(aes(y = tcb, color = "tcb"), linewidth = 0.8) +
  geom_line(aes(y = tcblog10_sum, color = "sum(tcblog10)"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("tcb" = "#1b9e77", "sum(tcblog10)" = "#d95f02"), name = "Series") +
  labs(title = sprintf("Global mean biomass: %s %s", model_name, scenario), y = "g m-2", x = "Year") +
  theme_minimal(base_size = 12)

ggsave(filename = file.path(fig_dir, sprintf("%s_%s_global_mean_tcb_vs_tcblog10_FROMMAP_v2.png", model_name, scenario)), p_ts, width = 9, height = 4.5, dpi = 150)

p_pd <- ggplot(p, aes(x = Year, y = pct_diff)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line(color = "#7570b3", linewidth = 0.8) +
  labs(title = sprintf("Percent difference: sum(tcblog10) - tcb (%%): %s %s", model_name, scenario), y = "%", x = "Year") +
  theme_minimal(base_size = 12)

ggsave(filename = file.path(fig_dir, sprintf("%s_%s_percent_diff_tcblog10_vs_tcb_FROMMAP_v2.png", model_name, scenario)), p_pd, width = 9, height = 4.5, dpi = 150)

make_map_df <- function(diff_mat) {
  expand.grid(Lon = lon_vals, Lat = lat_vals) %>% mutate(diff = as.numeric(diff_mat))
}

p_map_start <- ggplot(make_map_df(first_diff), aes(x = Lon, y = Lat, fill = diff)) +
  geom_raster() + scale_fill_viridis(option = "C", na.value = "transparent") + coord_equal(expand = FALSE) +
  labs(title = sprintf("Difference (sum(tcblog10) - tcb): %s %s %d", model_name, scenario, min(year_vals)), fill = "g m-2") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, sprintf("%s_%s_diffmap_%d_FROMMAP_v2.png", model_name, scenario, min(year_vals))), p_map_start, width = 9, height = 4.5, dpi = 150)

p_map_end <- ggplot(make_map_df(last_diff), aes(x = Lon, y = Lat, fill = diff)) +
  geom_raster() + scale_fill_viridis(option = "C", na.value = "transparent") + coord_equal(expand = FALSE) +
  labs(title = sprintf("Difference (sum(tcblog10) - tcb): %s %s %d", model_name, scenario, max(year_vals)), fill = "g m-2") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, sprintf("%s_%s_diffmap_%d_FROMMAP_v2.png", model_name, scenario, max(year_vals))), p_map_end, width = 9, height = 4.5, dpi = 150)

cat("Diagnostics written to:", fig_dir, "\n")
