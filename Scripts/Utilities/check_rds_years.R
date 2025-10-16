# Quick check of RDS file year range
library(tidyverse)

rds_file <- "C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300/Input/2300_processed/2300_ukesm1-0-ll_ssp534-over_2101-2300.rds"

cat("Loading RDS file...\n")
data <- readRDS(rds_file)

cat("\nData Summary:\n")
cat("  Rows:", nrow(data), "\n")
cat("  Columns:", ncol(data), "\n")
cat("  Column names:", paste(names(data), collapse = ", "), "\n")
cat("\nYear Summary:\n")
cat("  Min year:", min(data$Year, na.rm = TRUE), "\n")
cat("  Max year:", max(data$Year, na.rm = TRUE), "\n")
cat("  Unique years:", length(unique(data$Year)), "\n")
cat("  Year range:", paste(unique(sort(data$Year)), collapse = ", "), "\n")

# Count rows per year
year_counts <- data %>% count(Year)
cat("\nFirst 10 years:\n")
print(head(year_counts, 10))
cat("\nLast 10 years:\n")
print(tail(year_counts, 10))
