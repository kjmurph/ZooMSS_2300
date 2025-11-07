# ================================================================
# Combine UKESM ssp534-over biomass (2015–2300)
# ================================================================
# Reads the pre-2101 and 2101–2300 UKESM ssp534-over compiled biomass files
# and outputs a single combined file covering 2015–2300.

suppressPackageStartupMessages({
  library(tidyverse)
})

bioproj_dir <- file.path("Output", "Biomass_projections")
pre_file <- file.path(bioproj_dir, "Biomass_ClimateChange_Compiled_withZooMSS_ukesm1-0-ll_ssp534-over_Control.rds")
post_file <- file.path(bioproj_dir, "Biomass_ClimateChange_Compiled_withZooMSS_ukesm1-0-ll_ssp534-over_2101-2300_Control.rds")
out_file <- file.path(bioproj_dir, "Biomass_ClimateChange_Compiled_withZooMSS_ukesm1-0-ll_ssp534-over_2015-2300_Control.rds")

cat("=== Combining UKESM ssp534-over biomass (2015–2300) ===\n")
cat("Reading:\n - ", pre_file, "\n - ", post_file, "\n", sep = "")

stopifnot(file.exists(pre_file))
stopifnot(file.exists(post_file))

pre <- readRDS(pre_file)
post <- readRDS(post_file)

# Basic sanity checks
req_cols <- c("Lon","Lat","Year")
missing_pre <- setdiff(req_cols, names(pre))
missing_post <- setdiff(req_cols, names(post))
if (length(missing_pre) > 0) stop("Missing columns in pre file: ", paste(missing_pre, collapse=","))
if (length(missing_post) > 0) stop("Missing columns in post file: ", paste(missing_post, collapse=","))

cat("Pre years:", min(pre$Year, na.rm = TRUE), "to", max(pre$Year, na.rm = TRUE), "\n")
cat("Post years:", min(post$Year, na.rm = TRUE), "to", max(post$Year, na.rm = TRUE), "\n")

# Harmonize columns (union) to be safe
all_cols <- union(names(pre), names(post))
pre  <- pre  %>% select(all_of(intersect(all_cols, names(pre))))
post <- post %>% select(all_of(intersect(all_cols, names(post))))

# Row-bind and deduplicate any overlapping (Lon,Lat,Year)
combined <- bind_rows(pre, post) %>%
  arrange(Year, Lat, Lon)

if (any(duplicated(combined[c("Lon","Lat","Year")]))) {
  cat("Found overlapping Lon-Lat-Year entries; deduplicating by keeping last...\n")
  combined <- combined %>%
    group_by(Lon, Lat, Year) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    arrange(Year, Lat, Lon)
}

# Final checks
yr_min <- min(combined$Year, na.rm = TRUE)
yr_max <- max(combined$Year, na.rm = TRUE)
cat("Combined years:", yr_min, "to", yr_max, "\n")

# Write output
saveRDS(combined, out_file)
cat("Saved combined file to:\n - ", out_file, "\n", sep = "")

# Optional: brief summary to logs
try({
  logs_dir <- file.path("logs")
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  msg <- paste0(
    "Combined UKESM ssp534-over 2015–2300 created ", ts, "\n",
    "Pre:  ", basename(pre_file), " (", nrow(pre), " rows)\n",
    "Post: ", basename(post_file), " (", nrow(post), " rows)\n",
    "Out:  ", basename(out_file), " (", nrow(combined), " rows)\n",
    "Year range: ", yr_min, "-", yr_max, "\n"
  )
  writeLines(msg, file.path(logs_dir, paste0("processing_summary_", ts, ".txt")))
}, silent = TRUE)

cat("=== Done ===\n")
