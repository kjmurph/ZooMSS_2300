# Convert UKESM ssp534-over 2101-2300 phyc-top annual to chla-top annual (mg m-3)
# Uses the relationship: log10(C) = 0.89*log10(Chl) + 1.79 (Brewin/Marañón)
# Therefore: Chl = 10^((log10(C) - 1.79) / 0.89)

suppressPackageStartupMessages({
  library(ncdf4)
})

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
input_nc  <- file.path(project_root, "Input/phyc",  "ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc-top_60arcmin_global_annual_2101_2300.nc")
output_nc <- file.path(project_root, "Input/chl",  "ukesm1-0-ll_r4i1p1f2_ssp534-over_chla-top_60arcmin_global_annual_2101_2300.nc")

if (!file.exists(input_nc)) {
  stop("Missing input file: ", input_nc)
}

# Constants
C_MOLAR_MASS <- 12.01  # g/mol
CONVERSION_FACTOR <- C_MOLAR_MASS * 1000  # mol C -> mg C per m3
LOG_SLOPE <- 0.89
LOG_INTERCEPT <- 1.79

nc <- nc_open(input_nc)
# Find phyc variable name (phyc/phyc_top)
var_names <- names(nc$var)
phyc_var <- grep("phyc", var_names, value = TRUE)[1]
if (is.na(phyc_var)) stop("No phyc-like variable found in ", input_nc)

# Read full array (expected dims: lon x lat x time)
phyc <- ncvar_get(nc, phyc_var)
# Convert to mg C m-3
carbon_mg <- phyc * CONVERSION_FACTOR

# Convert to chlorophyll mg m-3 using inverse relation
chl <- array(NA_real_, dim = dim(carbon_mg))
positive_idx <- which(carbon_mg > 0)
if (length(positive_idx)) {
  logC <- log10(carbon_mg[positive_idx])
  logChl <- (logC - LOG_INTERCEPT) / LOG_SLOPE
  chl[positive_idx] <- 10^logChl
}

# Prepare dimensions (copy lon/lat/time if available)
dims <- nc$var[[phyc_var]]$dim
ncdims <- vector("list", length(dims))
for (i in seq_along(dims)) {
  d <- dims[[i]]
  d_vals <- tryCatch(ncvar_get(nc, d$name), error = function(e) NULL)
  if (is.null(d_vals)) {
    # fallback to integer sequence
    d_vals <- seq_len(d$len)
  }
  # Try to fetch units
  u <- tryCatch(ncatt_get(nc, d$name, "units")$value, error = function(e) NA)
  if (is.na(u)) {
    u <- switch(d$name,
                lon = "degrees_east",
                lat = "degrees_north",
                time = "days since 1850-01-01",
                "unknown")
  }
  ncdims[[i]] <- ncdim_def(name = d$name, units = u, vals = d_vals)
}

chl_var <- ncvar_def(name = "chla", units = "mg m-3", dim = ncdims, missval = NA_real_,
                     longname = "Chlorophyll a concentration (top layer)")

# Create output file
nc_out <- nc_create(output_nc, vars = list(chl_var))
ncvar_put(nc_out, "chla", chl)

# Copy global attributes and provenance
gatts <- ncatt_get(nc, 0)
for (nm in names(gatts)) {
  val <- gatts[[nm]]
  if (!is.null(val)) try(ncatt_put(nc_out, 0, nm, val), silent = TRUE)
}
ncatt_put(nc_out, 0, "source_file", basename(input_nc))
ncatt_put(nc_out, 0, "conversion", "phyc-top (mol C m-3) -> chla-top (mg m-3)")
ncatt_put(nc_out, 0, "equation", "Chl = 10^((log10(C) - 1.79)/0.89)")

nc_close(nc_out)
nc_close(nc)

cat("Wrote:", output_nc, "\n")
