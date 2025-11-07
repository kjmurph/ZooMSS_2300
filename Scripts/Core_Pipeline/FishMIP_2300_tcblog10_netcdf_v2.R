# ================================================================
# FishMIP 2300 NetCDF exporter with tcblog10 reconstruction (v2)
# ================================================================
# - Reconstructs per-log10 weight-bin biomass (tcblog10) from ZooMSS size spectrum
# - Maps to ESM enviro via kNN in (SST, log10(Chl)) as in core pipeline
# - Writes NetCDF files per FishMIP naming/format (annual for now)
# - Also writes tcb, tpb, bp30cm, bp30to90cm, bp90cm as separate NetCDFs
#
# Usage (from repo root):
#   Rscript Scripts/Core_Pipeline/FishMIP_2300_tcblog10_netcdf_v2.R <model> <scenario>
# Example:
#   Rscript Scripts/Core_Pipeline/FishMIP_2300_tcblog10_netcdf_v2.R ukesm1-0-ll ssp534-over

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaImpute)
  library(ncdf4)
})

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "Scripts/Utilities/fZooMSS_Xtras.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript ... <model> <scenario>")
model_name <- args[[1]]
scenario <- args[[2]]

base_dir <- project_root
env_dir <- file.path(base_dir, "Enviro_Matrix")
proc_env_dir <- file.path(base_dir, "Input", "2300_processed")
out_nc_dir <- file.path(base_dir, "Output", "FishMIP_NetCDF_v2")
diag_dir <- file.path(base_dir, "Figures", "FishMIP_2300_v2")
dir.create(out_nc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== FishMIP NetCDF v2 export (tcblog10) ===\n")
cat("Model:", model_name, " Scenario:", scenario, "\n")

# ------------------------------------------------
# Load ZooMSS enviro reference (original + novel)
# ------------------------------------------------
enviro_data_original <- readr::read_rds(file.path(env_dir, "ClimateChange_Compiled_Distinct.rds"))
enviro_data_novel    <- readr::read_rds(file.path(env_dir, "novel_sst_chl_combinations_2300_wPhyto.rds"))

common_cols <- intersect(names(enviro_data_original), names(enviro_data_novel))
enviro_data <- bind_rows(
  enviro_data_original %>% select(all_of(common_cols)) %>% mutate(source = "original"),
  enviro_data_novel %>% select(all_of(common_cols)) %>% mutate(source = "novel")
) %>%
  mutate(cellID = 1:n()) %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl))

# ------------------------------------------------
# Load ZooMSS size-structured outputs and model
# ------------------------------------------------
zoo_original <- readr::read_rds(file.path(base_dir, "Input", "res_Control.RDS"))
zoo_novel    <- readr::read_rds(file.path(base_dir, "Input", "res_ZooMSS_2300.RDS"))
mdl          <- readr::read_rds(file.path(base_dir, "Input", "model_Control.RDS"))

zoo_combined <- c(zoo_original, zoo_novel)
minb <- 1
maxb <- 158  # up to ~100 kg as in core pipeline
mdl2 <- mdl
mdl2$param$w <- mdl$param$w[minb:maxb]

cat("Size bins considered:", length(mdl2$param$w), "\n")

# Compute size-biomass per enviro cell: vector length = nweights
size_biom_list <- fZooMSS_SizeBiomass(fZooMSS_ExtractSizeRange(zoo_combined, minb, maxb), mdl2)

# Convert to matrix (rows: cellID, cols: size index)
size_mat <- do.call(rbind, size_biom_list)
stopifnot(nrow(size_mat) == nrow(enviro_data))

# ------------------------------------------------
# Define FishMIP log10 weight bins (g):
# 1–10, 10–100, 100–1000, 1–10kg, 10–100kg, >100kg
# ------------------------------------------------
w <- mdl2$param$w  # grams
bin_edges <- list(
  c(1, 10),
  c(10, 100),
  c(100, 1000),
  c(1000, 10000),
  c(10000, 100000),
  c(100000, Inf)
)
bin_indices <- lapply(bin_edges, function(edges) which(w >= edges[1] & w < edges[2]))
bin_names <- paste0("bin", 0:5)

# ------------------------------------------------
# Load ESM enviro for mapping (2015–2300 for target model/scenario)
# ------------------------------------------------
env_file_main <- file.path(proc_env_dir, sprintf("2300_%s_%s.rds", model_name, scenario))
env_file_extra <- file.path(proc_env_dir, sprintf("2300_%s_%s_2101-2300.rds", model_name, scenario))

nc_main <- if (file.exists(env_file_main)) readr::read_rds(env_file_main) else NULL
nc_extra <- if (file.exists(env_file_extra)) readr::read_rds(env_file_extra) else NULL
if (is.null(nc_main) && is.null(nc_extra)) stop("No processed enviro files found for: ", model_name, " ", scenario)

harmonize_env <- function(df) {
  if (is.null(df)) return(NULL)
  # Standardize chlorophyll column name and log10
  if ("Chl" %in% names(df)) chl <- df$Chl
  else if ("chlo" %in% names(df)) chl <- df$chlo
  else if ("chl" %in% names(df)) chl <- df$chl
  else stop("Chlorophyll column not found in env dataset (expected 'Chl' or 'chlo' or 'chl')")
  df$Chl <- chl
  if (!"Chl_log10" %in% names(df)) df$Chl_log10 <- log10(chl)
  df
}

nc_all <- bind_rows(harmonize_env(nc_main), harmonize_env(nc_extra))

cat("Enviro rows:", nrow(nc_all), " Years:", min(nc_all$Year), "-", max(nc_all$Year), "\n")

# Determine grid and time axes
lon_vals <- sort(unique(nc_all$Lon))
lat_vals <- sort(unique(nc_all$Lat))
year_vals <- sort(unique(nc_all$Year))
nlon <- length(lon_vals); nlat <- length(lat_vals); ntime <- length(year_vals)

lon_to_idx <- setNames(seq_along(lon_vals), lon_vals)
lat_to_idx <- setNames(seq_along(lat_vals), lat_vals)
year_to_idx <- setNames(seq_along(year_vals), year_vals)

# kNN mapping (one go for all rows)
# Note: For memory efficiency (e.g., long picontrol), compute kNN per year below

# ------------------------------------------------
# NetCDF helpers
# ------------------------------------------------
fillv <- 1e20
make_time_days <- function(years) {
  # 365-day calendar, days since 1901-01-01
  (years - 1901) * 365
}

create_nc <- function(varname, longname, units, extra_dim = NULL, extra_dim_name = NULL, extra_dim_vals = NULL) {
  lon_dim <- ncdim_def("lon", "degrees_east", vals = lon_vals)
  lat_dim <- ncdim_def("lat", "degrees_north", vals = lat_vals)
  time_dim <- ncdim_def("time", sprintf("days since %s", "1901-01-01 00:00:00"), vals = make_time_days(year_vals), unlim = TRUE)
  dims <- list(time_dim, lat_dim, lon_dim)
  chunks <- c(1L, max(1L, min(90L, length(lat_vals))), max(1L, min(180L, length(lon_vals))))
  if (!is.null(extra_dim)) {
    bins_dim <- ncdim_def(extra_dim_name, "", vals = seq_len(length(extra_dim_vals)))
    dims <- c(list(time_dim, bins_dim), list(lat_dim, lon_dim))
    chunks <- c(1L, length(extra_dim_vals), chunks[2], chunks[3])
  }
  var_def <- ncvar_def(varname, units, dims, missval = fillv, longname = longname,
                       prec = "double", compression = 4, shuffle = TRUE, chunksizes = chunks)
  ncfile <- file.path(out_nc_dir, sprintf("zoomss_%s_%s_nat_default_%s_global_annual_%d_%d.nc",
                                          model_name, scenario, varname, min(year_vals), max(year_vals)))
  ncout <- nc_create(ncfile, vars = list(var_def), force_v4 = TRUE)
  # write lon/lat/time
  ncvar_put(ncout, "lon", lon_vals)
  ncvar_put(ncout, "lat", lat_vals)
  ncvar_put(ncout, "time", make_time_days(year_vals))
  if (!is.null(extra_dim)) {
    # add a variable with bin bounds as attributes
    ncatt_put(ncout, var_def, "bins", paste0(extra_dim_vals, collapse = ","))
  }
  # Global CF-style metadata
  now_str <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  ncatt_put(ncout, 0, "Conventions", "CF-1.7")
  ncatt_put(ncout, 0, "title", sprintf("ZooMSS-derived FishMIP variables: %s %s %s", model_name, scenario, varname))
  ncatt_put(ncout, 0, "summary", "Size-structured zooplankton/fish biomass mapped via kNN in (SST, log10(Chl)) to ESM grid; annual means.")
  ncatt_put(ncout, 0, "institution", "University of Tasmania")
  ncatt_put(ncout, 0, "source", "ZooMSS model outputs mapped to CMIP6 ESM environmental drivers")
  ncatt_put(ncout, 0, "history", sprintf("%s: created by Scripts/Core_Pipeline/FishMIP_2300_tcblog10_netcdf_v2.R", now_str))
  ncatt_put(ncout, 0, "references", "FishMIP protocol; ZooMSS documentation; see repository README")
  ncatt_put(ncout, 0, "contact", "Primary contact: kjmurphy@utas.edu.au (update as needed)")
  ncatt_put(ncout, 0, "model_id", model_name)
  ncatt_put(ncout, 0, "scenario", scenario)
  ncatt_put(ncout, 0, "grid_resolution", "1x1 degree")
  ncatt_put(ncout, 0, "spatial_resolution", "1 degree")
  ncatt_put(ncout, 0, "geospatial_lon_min", min(lon_vals))
  ncatt_put(ncout, 0, "geospatial_lon_max", max(lon_vals))
  ncatt_put(ncout, 0, "geospatial_lat_min", min(lat_vals))
  ncatt_put(ncout, 0, "geospatial_lat_max", max(lat_vals))
  ncatt_put(ncout, 0, "time_coverage_start", sprintf("%d-01-01", min(year_vals)))
  ncatt_put(ncout, 0, "time_coverage_end", sprintf("%d-12-31", max(year_vals)))
  ncatt_put(ncout, 0, "frequency", "annual")
  ncatt_put(ncout, 0, "realm", "marine ecosystem model")
  ncatt_put(ncout, 0, "license", "See LICENSE in repository (update if needed)")
  # Variable-level attributes
  ncatt_put(ncout, var_def, "coordinates", "time lat lon")
  ncatt_put(ncout, var_def, "cell_methods", "time: mean (interval: 1 year)")
  return(ncout)
}

write_time_slice <- function(ncout, varname, year, arr) {
  # arr dims must match [lat, lon] or [bins, lat, lon] when writing a single time
  t_idx <- year_to_idx[[as.character(year)]]
  if (length(dim(arr)) == 2) {
    start <- c(t_idx, 1, 1)
    count <- c(1, nlat, nlon)
  } else if (length(dim(arr)) == 3) {
    nbins <- dim(arr)[1]
    start <- c(t_idx, 1, 1, 1)
    count <- c(1, nbins, nlat, nlon)
  } else stop("Unexpected array dimensions")
  ncvar_put(ncout, varname, arr, start = start, count = count)
}

# Create nc files
nc_tcb <- create_nc("tcb", "Total Consumer Biomass Density", "g m-2")
nc_tpb <- create_nc("tpb", "Total Pelagic Biomass Density", "g m-2")
# bp variables are optional (require compiled species totals); enable lazily below
nc_bp30 <- NULL; nc_bp3090 <- NULL; nc_bp90 <- NULL
bin_bounds_str <- c("1-10g","10-100g","100g-1kg","1-10kg","10-100kg",">100kg")
nc_tcblog10 <- create_nc("tcblog10", "Total Consumer Biomass Density in log10 Weight Bins", "g m-2",
                         extra_dim = TRUE, extra_dim_name = "bins", extra_dim_vals = bin_bounds_str)

# ------------------------------------------------
# Mapping and writing per year (chunked)
# ------------------------------------------------
ref_mat <- as.matrix(enviro_data[, c("SST","Chl_log10")])

species_cols <- c("Flagellates","Ciliates","Larvaceans","OmniCopepods","CarnCopepods",
                  "Euphausiids","Chaetognaths","Salps","Jellyfish","Fish_Small","Fish_Med","Fish_Large")

# Preload compiled species totals to derive bp30cm etc., or compute from size bins? Use compiled file for consistency.
compiled_file <- file.path(base_dir, "Output", "Biomass_projections",
                           sprintf("Biomass_ClimateChange_Compiled_withZooMSS_%s_%s_2015-2300_Control.rds", model_name, scenario))
if (!file.exists(compiled_file)) {
  compiled_file <- file.path(base_dir, "Output", "Biomass_projections",
                             sprintf("Biomass_ClimateChange_Compiled_withZooMSS_%s_%s_Control.rds", model_name, scenario))
}
has_bp <- FALSE
compiled <- NULL
bp_mat <- NULL
compiled_try <- try({ readr::read_rds(compiled_file) }, silent = TRUE)
if (!inherits(compiled_try, "try-error")) {
  compiled <- compiled_try
  nc_bp30 <- create_nc("bp30cm", "Biomass density of small pelagics <30cm", "g m-2")
  nc_bp3090 <- create_nc("bp30to90cm", "Biomass density of medium pelagics >=30cm and <90cm", "g m-2")
  nc_bp90 <- create_nc("bp90cm", "Biomass density of large pelagics >=90cm", "g m-2")
  # Build a fast lookup for bp variables
  bp_mat <- compiled %>%
    select(Year, Lon, Lat, Fish_Small, Fish_Med, Fish_Large) %>%
    mutate(lon_i = lon_to_idx[as.character(Lon)],
           lat_i = lat_to_idx[as.character(Lat)])
  has_bp <- TRUE
} else {
  warning("BP variables skipped: could not read compiled species totals (", conditionMessage(attr(compiled_try, "condition")), ")")
}

for (yr in year_vals) {
  sel <- nc_all$Year == yr
  if (!any(sel)) next
  # Compute kNN mapping for this year's target points only, in chunks to limit memory
  tgt_mat <- as.matrix(nc_all[sel, c("SST","Chl_log10")])
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
  this_lon_i <- lon_to_idx[as.character(nc_all$Lon[sel])]
  this_lat_i <- lat_to_idx[as.character(nc_all$Lat[sel])]

  # tcblog10 arrays per bin
  nb <- length(bin_indices)
  tcblog10_arr <- array(fillv, dim = c(nb, nlat, nlon))

  # Extract size rows for matched cellIDs
  sm <- size_mat[this_cid, , drop = FALSE]  # rows: points, cols: weights

  # Compute sums per bin for each point
  bin_sums <- lapply(bin_indices, function(cols) if (length(cols)>0) rowSums(sm[, cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(sm)))

  # Place into grid
  for (b in seq_len(nb)) {
    vals <- bin_sums[[b]]
    # assign each point into grid cell
    # create an index to flatten [lat, lon]
    lin <- (this_lat_i - 1) * nlon + this_lon_i
    # Build a temporary slice to avoid repeated writes
    slice <- matrix(fillv, nrow = nlat, ncol = nlon)
    slice[lin] <- vals
    tcblog10_arr[b,,] <- slice
  }

  # tcb/tpb from sum over bins
  tcb_slice <- matrix(fillv, nrow = nlat, ncol = nlon)
  tcb_vals <- Reduce(`+`, bin_sums)
  tcb_slice[(this_lat_i - 1) * nlon + this_lon_i] <- tcb_vals
  tpb_slice <- tcb_slice  # pelagic-only model

  # bp variables from compiled (optional)
  if (has_bp) {
    bp_sel <- bp_mat %>% filter(Year == yr)
    bp_slice_small <- matrix(fillv, nrow = nlat, ncol = nlon)
    bp_slice_med   <- matrix(fillv, nrow = nlat, ncol = nlon)
    bp_slice_large <- matrix(fillv, nrow = nlat, ncol = nlon)
    # align by indices
    lin_bp <- (bp_sel$lat_i - 1) * nlon + bp_sel$lon_i
    bp_slice_small[lin_bp] <- bp_sel$Fish_Small
    bp_slice_med[lin_bp]   <- bp_sel$Fish_Med
    bp_slice_large[lin_bp] <- bp_sel$Fish_Large
    write_time_slice(nc_bp30, "bp30cm", yr, bp_slice_small)
    write_time_slice(nc_bp3090, "bp30to90cm", yr, bp_slice_med)
    write_time_slice(nc_bp90, "bp90cm", yr, bp_slice_large)
  }

  # Write slices
  write_time_slice(nc_tcblog10, "tcblog10", yr, tcblog10_arr)
  write_time_slice(nc_tcb, "tcb", yr, tcb_slice)
  write_time_slice(nc_tpb, "tpb", yr, tpb_slice)
  cat("Wrote year:", yr, "\n")
  gc()
}

nc_close(nc_tcblog10); nc_close(nc_tcb); nc_close(nc_tpb);
if (!is.null(nc_bp30)) nc_close(nc_bp30)
if (!is.null(nc_bp3090)) nc_close(nc_bp3090)
if (!is.null(nc_bp90)) nc_close(nc_bp90)

cat("NetCDF written to:", out_nc_dir, "\n")

cat("=== Done ===\n")
