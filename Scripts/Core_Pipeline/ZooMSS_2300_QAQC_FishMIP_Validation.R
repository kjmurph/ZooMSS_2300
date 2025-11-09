#!/usr/bin/env Rscript
# ==============================================================================
# ZooMSS 2300 - FishMIP Output Quality Control
# ==============================================================================
# Purpose: Validate FishMIP protocol outputs against standard ZooMSS outputs
# Checks:
#   1. TCB values are realistic (0.01 - 1000 g/m²)
#   2. TCB matches between FishMIP and standard outputs
#   3. Size bin sums equal total TCB
#   4. No NA or negative values
#   5. Temporal consistency (no sudden jumps)
# ==============================================================================

library(tidyverse)

cat("==============================================================================\n")
cat("FishMIP Output Quality Control\n")
cat("==============================================================================\n\n")

# Setup paths
base_dir <- getwd()
fishmip_dir <- file.path(base_dir, "Output", "Step3d_FishMIP_Format")
standard_dir <- file.path(base_dir, "Output", "Step3d_ZooMSS_Biomass_Projections_2300")

# Get file lists
fishmip_files <- list.files(fishmip_dir, pattern = "^ZooMSS_FishMIP_2300_.*\\.rds$", full.names = TRUE)
standard_files <- list.files(standard_dir, pattern = "^ZooMSS_Biomass_2300_.*\\.rds$", full.names = TRUE)

cat("Found", length(fishmip_files), "FishMIP files\n")
cat("Found", length(standard_files), "standard files\n\n")

# Initialize results list
qc_results <- list()

#### CHECK 1: Value Ranges ####
cat("=== CHECK 1: Biomass Value Ranges ===\n")

for (file in fishmip_files) {
  filename <- basename(file)
  parts <- str_match(filename, "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  model <- parts[,2]
  scenario <- parts[,3]
  
  cat(sprintf("  %s - %s: ", model, scenario))
  
  data <- readRDS(file)
  
  # Check TCB range
  tcb_range <- range(data$tcb, na.rm = TRUE)
  tcb_mean <- mean(data$tcb, na.rm = TRUE)
  
  # Check for NAs
  na_count <- sum(is.na(data$tcb))
  
  # Check for negatives
  neg_count <- sum(data$tcb < 0, na.rm = TRUE)
  
  # Flag issues
  issues <- c()
  if (tcb_range[1] < 0.001 || tcb_range[2] > 10000) {
    issues <- c(issues, sprintf("Range [%.2e, %.2e] g/m²", tcb_range[1], tcb_range[2]))
  }
  if (na_count > 0) {
    issues <- c(issues, sprintf("%d NAs", na_count))
  }
  if (neg_count > 0) {
    issues <- c(issues, sprintf("%d negatives", neg_count))
  }
  
  if (length(issues) > 0) {
    cat("⚠ ", paste(issues, collapse = ", "), "\n")
  } else {
    cat(sprintf("✓ Range [%.2f, %.2f], Mean %.2f g/m²\n", 
                tcb_range[1], tcb_range[2], tcb_mean))
  }
  
  qc_results[[paste0(model, "_", scenario, "_range")]] <- list(
    model = model,
    scenario = scenario,
    tcb_min = tcb_range[1],
    tcb_max = tcb_range[2],
    tcb_mean = tcb_mean,
    na_count = na_count,
    neg_count = neg_count
  )
}

cat("\n")

#### CHECK 2: FishMIP vs Standard TCB Comparison ####
cat("=== CHECK 2: FishMIP vs Standard TCB Comparison ===\n")

for (i in seq_along(fishmip_files)) {
  fishmip_file <- fishmip_files[i]
  
  # Match to standard file
  filename <- basename(fishmip_file)
  parts <- str_match(filename, "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  model <- parts[,2]
  scenario <- parts[,3]
  
  standard_file <- file.path(standard_dir, paste0("ZooMSS_Biomass_2300_", model, "_", scenario, ".rds"))
  
  if (!file.exists(standard_file)) {
    cat(sprintf("  %s - %s: ⚠ Standard file not found\n", model, scenario))
    next
  }
  
  cat(sprintf("  %s - %s: ", model, scenario))
  
  # Load both files
  fishmip <- readRDS(fishmip_file)
  standard <- readRDS(standard_file)
  
  # Calculate TCB from standard (sum across functional groups)
  fg_cols <- c("Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", 
               "CarnCopepods", "Euphausiids", "Chaetognaths", "Salps", 
               "Jellyfish", "Fish_Small", "Fish_Med", "Fish_Large")
  
  # Sample comparison (first 100 cells from year 2050 if available)
  test_year <- if (scenario == "historical") 2000 else if (scenario == "picontrol") 1700 else 2050
  
  fishmip_sample <- fishmip %>% 
    filter(Date == test_year) %>% 
    slice(1:min(100, n()))
  
  standard_sample <- standard %>% 
    filter(Date == test_year) %>% 
    slice(1:min(100, n())) %>%
    mutate(tcb_standard = rowSums(select(., all_of(fg_cols)), na.rm = TRUE) * 60)  # × 60m MLD
  
  if (nrow(fishmip_sample) > 0 && nrow(standard_sample) > 0) {
    # Compare TCB values
    correlation <- cor(fishmip_sample$tcb, standard_sample$tcb_standard, use = "complete.obs")
    mean_diff <- mean(abs(fishmip_sample$tcb - standard_sample$tcb_standard), na.rm = TRUE)
    max_diff <- max(abs(fishmip_sample$tcb - standard_sample$tcb_standard), na.rm = TRUE)
    
    if (correlation > 0.99 && mean_diff < 1) {
      cat(sprintf("✓ Correlation %.4f, Mean diff %.4f g/m²\n", correlation, mean_diff))
    } else {
      cat(sprintf("⚠ Correlation %.4f, Mean diff %.4f, Max diff %.4f g/m²\n", 
                  correlation, mean_diff, max_diff))
    }
    
    qc_results[[paste0(model, "_", scenario, "_comparison")]] <- list(
      model = model,
      scenario = scenario,
      correlation = correlation,
      mean_diff = mean_diff,
      max_diff = max_diff
    )
  } else {
    cat("⚠ No data for year", test_year, "\n")
  }
}

cat("\n")

#### CHECK 3: Size Bin Sum Validation ####
cat("=== CHECK 3: Size Bin Summation Check ===\n")

for (file in sample(fishmip_files, min(5, length(fishmip_files)))) {
  filename <- basename(file)
  parts <- str_match(filename, "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  model <- parts[,2]
  scenario <- parts[,3]
  
  cat(sprintf("  %s - %s: ", model, scenario))
  
  data <- readRDS(file)
  
  # Sample 1000 random cells
  sample_data <- data %>% 
    slice_sample(n = min(1000, nrow(data)))
  
  # Check if sum of log10 bins equals TCB
  sample_data <- sample_data %>%
    mutate(
      bin_sum = tcblog10_0 + tcblog10_1 + tcblog10_2 + tcblog10_3 + tcblog10_4 + tcblog10_5,
      diff = abs(tcb - bin_sum),
      length_sum = bp30cm + bp30to90cm + bp90cm,
      length_diff = abs(tcb - length_sum)
    )
  
  mean_diff <- mean(sample_data$diff, na.rm = TRUE)
  max_diff <- max(sample_data$diff, na.rm = TRUE)
  mean_length_diff <- mean(sample_data$length_diff, na.rm = TRUE)
  
  if (mean_diff < 0.01 && mean_length_diff < 0.01) {
    cat(sprintf("✓ Size bins sum correctly (diff < 0.01 g/m²)\n"))
  } else {
    cat(sprintf("⚠ Mean diff %.4f, Max diff %.4f g/m²\n", mean_diff, max_diff))
  }
  
  qc_results[[paste0(model, "_", scenario, "_bins")]] <- list(
    model = model,
    scenario = scenario,
    mean_bin_diff = mean_diff,
    max_bin_diff = max_diff,
    mean_length_diff = mean_length_diff
  )
}

cat("\n")

#### CHECK 4: Temporal Consistency ####
cat("=== CHECK 4: Temporal Consistency (No Sudden Jumps) ===\n")

for (file in sample(fishmip_files, min(3, length(fishmip_files)))) {
  filename <- basename(file)
  parts <- str_match(filename, "ZooMSS_FishMIP_2300_(.+)_(.+)\\.rds")
  model <- parts[,2]
  scenario <- parts[,3]
  
  cat(sprintf("  %s - %s: ", model, scenario))
  
  data <- readRDS(file)
  
  # Calculate global mean TCB by year
  temporal <- data %>%
    group_by(Date) %>%
    summarise(mean_tcb = mean(tcb, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Date) %>%
    mutate(
      tcb_change = mean_tcb - lag(mean_tcb),
      pct_change = abs(tcb_change / lag(mean_tcb) * 100)
    )
  
  # Check for sudden jumps (>50% change year-to-year)
  big_jumps <- sum(temporal$pct_change > 50, na.rm = TRUE)
  max_jump <- max(temporal$pct_change, na.rm = TRUE)
  
  if (big_jumps == 0) {
    cat(sprintf("✓ No sudden jumps (max annual change %.1f%%)\n", max_jump))
  } else {
    cat(sprintf("⚠ %d sudden jumps detected (max %.1f%%)\n", big_jumps, max_jump))
  }
  
  qc_results[[paste0(model, "_", scenario, "_temporal")]] <- list(
    model = model,
    scenario = scenario,
    sudden_jumps = big_jumps,
    max_annual_change_pct = max_jump
  )
}

cat("\n")

#### SUMMARY ####
cat("==============================================================================\n")
cat("QUALITY CONTROL SUMMARY\n")
cat("==============================================================================\n\n")

# Convert results to data frame
qc_df <- bind_rows(qc_results, .id = "test")

# Count issues
range_issues <- qc_df %>% 
  filter(!is.na(tcb_min)) %>%
  filter(tcb_min < 0.001 | tcb_max > 10000 | na_count > 0 | neg_count > 0) %>%
  nrow()

comparison_issues <- qc_df %>%
  filter(!is.na(correlation)) %>%
  filter(correlation < 0.99 | mean_diff > 1) %>%
  nrow()

bin_issues <- qc_df %>%
  filter(!is.na(mean_bin_diff)) %>%
  filter(mean_bin_diff > 0.01) %>%
  nrow()

temporal_issues <- qc_df %>%
  filter(!is.na(sudden_jumps)) %>%
  filter(sudden_jumps > 0) %>%
  nrow()

total_tests <- nrow(qc_df)
total_issues <- range_issues + comparison_issues + bin_issues + temporal_issues

cat(sprintf("Total tests performed: %d\n", total_tests))
cat(sprintf("Tests with issues: %d\n\n", total_issues))

cat("By category:\n")
cat(sprintf("  Range checks: %d issues\n", range_issues))
cat(sprintf("  FishMIP vs Standard: %d issues\n", comparison_issues))
cat(sprintf("  Size bin summation: %d issues\n", bin_issues))
cat(sprintf("  Temporal consistency: %d issues\n\n", temporal_issues))

if (total_issues == 0) {
  cat("✅ ALL CHECKS PASSED - FishMIP outputs are valid!\n\n")
} else {
  cat("⚠️  SOME ISSUES DETECTED - Review above for details\n\n")
}

# Save detailed results
output_file <- file.path(base_dir, "Output", "FishMIP_QC_Results.csv")
write_csv(qc_df, output_file)
cat("Detailed QC results saved to:", output_file, "\n")

cat("==============================================================================\n")
