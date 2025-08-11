# AUTOMATED CLEANUP SCRIPT
# Generated on: 2025-08-11 15:12:43

# Create backup directory
backup_dir <- 'Enviro_Matrix/backup_redundant_20250811/'
dir.create(backup_dir, recursive = TRUE)

# Files to backup and remove
redundant_files <- c(
  'all_2300_sst_chl_combinations_complete.rds',
  'novel_sst_chl_combinations_2300_vs_climate_change.rds',
  'novel_sst_chl_combinations_2300_filtered.rds'
)

# Backup and remove redundant files
for(file in redundant_files) {
  old_path <- paste0('Enviro_Matrix/', file)
  new_path <- paste0(backup_dir, file)
  
  if(file.exists(old_path)) {
    file.copy(old_path, new_path)
    file.remove(old_path)
    cat('Moved:', file, '\n')
  }
}

cat('Cleanup complete!\n')

