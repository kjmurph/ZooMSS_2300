# ==============================================================================
# REDUNDANCY ANALYSIS: Current ZooMSS 2300 Environmental Matrix Files
# ==============================================================================
# Purpose: Analyze the current environmental matrix files to identify
#          redundancy and recommend cleanup
# ==============================================================================

library(tidyverse)

# Set directories
enviro_dir <- "Enviro_Matrix/"

cat("=== REDUNDANCY ANALYSIS ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# STEP 1: INVENTORY EXISTING FILES
# ==============================================================================

cat("STEP 1: Inventory of existing environmental matrix files\n")

# List all .rds files in Enviro_Matrix directory
enviro_files <- list.files(enviro_dir, pattern = "\\.rds$|\\.RDS$", full.names = FALSE)

# Get file sizes and creation dates
file_info <- data.frame(
  filename = enviro_files,
  size_mb = sapply(paste0(enviro_dir, enviro_files), function(f) {
    if(file.exists(f)) round(file.size(f) / 1024^2, 2) else NA
  }),
  created = sapply(paste0(enviro_dir, enviro_files), function(f) {
    if(file.exists(f)) format(file.info(f)$mtime, "%Y-%m-%d %H:%M") else NA
  })
) %>%
  arrange(desc(size_mb))

cat("Files found:", nrow(file_info), "\n")
print(file_info)

# ==============================================================================
# STEP 2: ANALYZE FILE CONTENTS
# ==============================================================================

cat("\nSTEP 2: Analyzing file contents and overlap\n")

# Function to safely load and summarize each file
analyze_file <- function(filename) {
  filepath <- paste0(enviro_dir, filename)
  
  result <- tryCatch({
    data <- readRDS(filepath)
    
    # Get basic info
    if(is.data.frame(data)) {
      summary_info <- list(
        filename = filename,
        rows = nrow(data),
        cols = ncol(data),
        columns = paste(names(data), collapse = ", "),
        has_sst = "sst" %in% names(data) || "SST" %in% names(data),
        has_chlo = "chlo" %in% names(data) || "Chl" %in% names(data) || "Chl_log10" %in% names(data),
        unique_sst_chlo = NA,
        data_type = "data.frame"
      )
      
      # Count unique SST-Chl combinations if both present
      if(summary_info$has_sst && summary_info$has_chlo) {
        # Standardize column names for counting
        data_std <- data
        if("SST" %in% names(data)) data_std$sst <- data$SST
        if("Chl" %in% names(data)) data_std$chlo <- data$Chl
        if("Chl_log10" %in% names(data)) data_std$chlo <- 10^data$Chl_log10
        
        if("sst" %in% names(data_std) && "chlo" %in% names(data_std)) {
          unique_combos <- data_std %>%
            select(sst, chlo) %>%
            filter(!is.na(sst), !is.na(chlo)) %>%
            distinct() %>%
            nrow()
          summary_info$unique_sst_chlo <- unique_combos
        }
      }
    } else {
      summary_info <- list(
        filename = filename,
        rows = length(data),
        cols = NA,
        columns = "Non-dataframe object",
        has_sst = FALSE,
        has_chlo = FALSE,
        unique_sst_chlo = NA,
        data_type = class(data)[1]
      )
    }
    
    return(summary_info)
    
  }, error = function(e) {
    return(list(
      filename = filename,
      rows = NA,
      cols = NA,
      columns = paste("ERROR:", e$message),
      has_sst = FALSE,
      has_chlo = FALSE,
      unique_sst_chlo = NA,
      data_type = "ERROR"
    ))
  })
  
  return(result)
}

# Analyze all files
file_analysis <- map_dfr(enviro_files, analyze_file)

# Combine with file info
complete_analysis <- file_info %>%
  left_join(file_analysis, by = "filename") %>%
  arrange(desc(unique_sst_chlo))

cat("\nFile analysis summary:\n")
print(complete_analysis %>% 
      select(filename, size_mb, rows, unique_sst_chlo, has_sst, has_chlo))

# ==============================================================================
# STEP 3: IDENTIFY REDUNDANCY PATTERNS
# ==============================================================================

cat("\nSTEP 3: Identifying redundancy patterns\n")

# Group files by purpose/content
file_categories <- complete_analysis %>%
  mutate(
    category = case_when(
      str_detect(filename, "ClimateChange_Compiled_Distinct") ~ "Original_Reference",
      str_detect(filename, "enviro_CMIP_Matrix") ~ "Original_Reference", 
      str_detect(filename, "all_2300.*complete") ~ "All_2300_Raw",
      str_detect(filename, "all.*2300.*wPhyto") ~ "All_2300_Processed",
      str_detect(filename, "novel.*vs_climate") ~ "Novel_Raw",
      str_detect(filename, "novel.*filtered") ~ "Novel_Filtered",
      str_detect(filename, "novel.*wPhyto") ~ "Novel_Processed",
      TRUE ~ "Other"
    ),
    purpose = case_when(
      str_detect(filename, "ClimateChange_Compiled_Distinct|enviro_CMIP") ~ "Reference data from original project",
      str_detect(filename, "all_2300") ~ "All combinations from 2300 scenarios",
      str_detect(filename, "novel") ~ "Novel combinations not in original project",
      TRUE ~ "Other/Unknown"
    )
  )

cat("\nFile categorization:\n")
print(file_categories %>% 
      select(filename, category, purpose, unique_sst_chlo) %>%
      arrange(category, filename))

# ==============================================================================
# STEP 4: REDUNDANCY RECOMMENDATIONS
# ==============================================================================

cat("\nSTEP 4: Redundancy recommendations\n")

# Count files by category
category_summary <- file_categories %>%
  count(category, name = "file_count") %>%
  arrange(desc(file_count))

cat("\nFiles by category:\n")
print(category_summary)

# Identify potential redundant files
redundant_files <- file_categories %>%
  filter(
    category %in% c("All_2300_Raw", "Novel_Raw", "Novel_Filtered") |
    (category == "All_2300_Processed" & str_detect(filename, "complete"))
  ) %>%
  arrange(category, filename)

cat("\nPotentially redundant files:\n")
print(redundant_files %>% 
      select(filename, category, size_mb, unique_sst_chlo))

# ==============================================================================
# STEP 5: RECOMMENDED CLEANUP PLAN
# ==============================================================================

cat("\n=== RECOMMENDED CLEANUP PLAN ===\n")

cat("\n1. KEEP (Essential files):\n")
essential_files <- file_categories %>%
  filter(
    category %in% c("Original_Reference", "Novel_Processed") |
    (category == "All_2300_Processed" & !str_detect(filename, "complete"))
  ) %>%
  pull(filename)

for(f in essential_files) {
  cat("   -", f, "\n")
}

cat("\n2. BACKUP AND REMOVE (Redundant/intermediate files):\n")
redundant_file_list <- file_categories %>%
  filter(
    category %in% c("All_2300_Raw", "Novel_Raw", "Novel_Filtered") |
    (category == "All_2300_Processed" & str_detect(filename, "complete"))
  ) %>%
  pull(filename)

for(f in redundant_file_list) {
  cat("   -", f, "\n")
}

cat("\n3. RECOMMENDED FINAL FILE STRUCTURE:\n")
cat("   - ClimateChange_Compiled_Distinct.rds (original reference)\n")
cat("   - enviro_CMIP_Matrix_wPhyto.RDS (original reference)\n")
cat("   - master_enviro_matrix_2300.rds (NEW - comprehensive mapping)\n")
cat("   - novel_sst_chl_combinations_2300_wPhyto.rds (novel combinations only)\n")
cat("   - all_sst_chl_combinations_2300_wPhyto.rds (all 2300 combinations)\n")

# ==============================================================================
# STEP 6: GENERATE CLEANUP SCRIPT
# ==============================================================================

cat("\nSTEP 6: Generating cleanup script\n")

cleanup_script <- paste0(
  "# AUTOMATED CLEANUP SCRIPT\n",
  "# Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
  "# Create backup directory\n",
  "backup_dir <- 'Enviro_Matrix/backup_redundant_", format(Sys.Date(), "%Y%m%d"), "/'\n",
  "dir.create(backup_dir, recursive = TRUE)\n\n",
  "# Files to backup and remove\n",
  "redundant_files <- c(\n"
)

for(i in seq_along(redundant_file_list)) {
  cleanup_script <- paste0(cleanup_script, "  '", redundant_file_list[i], "'")
  if(i < length(redundant_file_list)) cleanup_script <- paste0(cleanup_script, ",")
  cleanup_script <- paste0(cleanup_script, "\n")
}

cleanup_script <- paste0(cleanup_script, 
  ")\n\n",
  "# Backup and remove redundant files\n",
  "for(file in redundant_files) {\n",
  "  old_path <- paste0('Enviro_Matrix/', file)\n",
  "  new_path <- paste0(backup_dir, file)\n",
  "  \n",
  "  if(file.exists(old_path)) {\n",
  "    file.copy(old_path, new_path)\n",
  "    file.remove(old_path)\n",
  "    cat('Moved:', file, '\\n')\n",
  "  }\n",
  "}\n\n",
  "cat('Cleanup complete!\\n')\n"
)

# Save cleanup script
writeLines(cleanup_script, "cleanup_redundant_enviro_files.R")

cat("Cleanup script saved as: cleanup_redundant_enviro_files.R\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Total files analyzed:", nrow(complete_analysis), "\n")
cat("Total size of all files:", round(sum(complete_analysis$size_mb, na.rm = TRUE), 1), "MB\n")
cat("Essential files to keep:", length(essential_files), "\n")
cat("Redundant files identified:", length(redundant_file_list), "\n")

redundant_size <- complete_analysis %>%
  filter(filename %in% redundant_file_list) %>%
  summarise(total_mb = sum(size_mb, na.rm = TRUE)) %>%
  pull(total_mb)

cat("Space to be freed:", round(redundant_size, 1), "MB\n")
cat("\nNext steps:\n")
cat("1. Review the recommendations above\n")
cat("2. Run the streamlined environmental matrix script\n")
cat("3. Execute the cleanup script if satisfied with the analysis\n")
