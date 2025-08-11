# Quick fix to analyze the actual structure and create plots
library(tidyverse)

# Load one file to understand the exact structure
test_file <- "Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds"

cat("Loading test file to understand structure...\n")
sample_data <- readRDS(test_file)

cat("Columns in the data:\n")
print(names(sample_data))

cat("\nFirst few rows:\n")
print(head(sample_data, 3))

cat("\nData types:\n")
print(sapply(sample_data, class))

# Save structure info for later use
saveRDS(list(
  column_names = names(sample_data),
  sample_data = head(sample_data, 100),
  total_rows = nrow(sample_data)
), "Output/data_structure_analysis.rds")

cat("\nStructure analysis saved. Now creating a working time series function...\n")
