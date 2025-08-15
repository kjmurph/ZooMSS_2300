# ZooMSS 2300 Repository Cleanup Script
# =====================================
# Purpose: Complete the repository reorganization by removing redundancy
# Date: August 15, 2025

cat("=== ZooMSS 2300 REPOSITORY CLEANUP ===\n")
cat("Starting comprehensive cleanup...\n\n")

# Set working directory
setwd("c:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300")

# Create backup log
cleanup_log <- c()
add_to_log <- function(action) {
  cleanup_log <<- c(cleanup_log, paste(Sys.time(), "-", action))
  cat(action, "\n")
}

# ============================================================================
# PHASE 1: REMOVE DUPLICATE SCRIPTS FROM ROOT
# ============================================================================

add_to_log("PHASE 1: Removing duplicate scripts from root directory")

# Scripts that exist in both root and Scripts/Core_Pipeline (keep Core_Pipeline versions)
duplicates_to_remove <- c(
  "ZooMSS_2300_4h_CorrectedAreaWeighting.R",
  "ZooMSS_2300_4i_SpatialPlotting.R", 
  "ZooMSS_2300_4k_MultiModelEnsemble.R",
  "ZooMSS_2300_4l_SeparateBiomassPlots.R",
  "ZooMSS_2300_4m_MultiModelMeanBiomass.R",
  "ZooMSS_2300_MasterPipeline.R"
)

for (script in duplicates_to_remove) {
  if (file.exists(script)) {
    file.remove(script)
    add_to_log(paste("  ✓ Removed duplicate:", script))
  }
}

# ============================================================================
# PHASE 2: MOVE DEVELOPMENT/ARCHIVED SCRIPTS TO ARCHIVE
# ============================================================================

add_to_log("\nPHASE 2: Moving development scripts to Archive")

# Scripts to move to Archive (development versions)
archive_scripts <- c(
  "ZooMSS_2300_2c_STREAMLINED_EnviroMatrix.R",
  "ZooMSS_2300_4b_MemorySafeBiomassTimeseries.R",
  "ZooMSS_2300_4c_BiomassTimeseriesPlots.R", 
  "ZooMSS_2300_4d_EnhancedBiomassAnalysis.R",
  "ZooMSS_2300_4e_SpatialChangeAnalysis.R",
  "ZooMSS_2300_4f_SimplifiedSpatialAnalysis.R",
  "ZooMSS_2300_4g_EnhancedPlotting.R",
  "ZooMSS_2300_4h_IPSL_Individual_Plots.R", 
  "ZooMSS_2300_4j_SeparateBiomassPlots.R"
)

for (script in archive_scripts) {
  if (file.exists(script)) {
    destination <- file.path("Scripts/Archive", script)
    if (!file.exists(destination)) {  # Only move if not already in archive
      file.rename(script, destination)
      add_to_log(paste("  ✓ Moved to Archive:", script))
    } else {
      file.remove(script)  # Remove duplicate if already in archive
      add_to_log(paste("  ✓ Removed duplicate (already in Archive):", script))
    }
  }
}

# ============================================================================
# PHASE 3: MOVE TEMPORARY/DIAGNOSTIC SCRIPTS TO ARCHIVE  
# ============================================================================

add_to_log("\nPHASE 3: Moving temporary/diagnostic scripts to Archive")

# Temporary and diagnostic scripts to archive
temp_scripts <- c(
  "analyze_redundancy.R",
  "check_biomass_data.R",
  "check_data_structure.R", 
  "create_biomass_plots_final.R",
  "diagnose_extreme_changes.R",
  "extreme_values_solution_summary.R",
  "implement_reorganization.R",
  "multimodel_analysis_summary.R",
  "quick_structure_check.R",
  "repository_reorganization_plan.R",
  "separate_plots_summary.R",
  "spatial_change_diagnostic.R",
  "spatial_diagnostic.R",
  "spatial_plot_test.R", 
  "test_enhanced_theme.R",
  "test_extreme_values_approach.R",
  "update_cv_legends.R",
  "validate_area_weighting.R",
  "validate_spatial_coverage.R",
  "validate_spatial_coverage_simple.R",
  "verify_grid_coverage.R",
  "verify_reorganization.R", 
  "verify_reorganized_outputs.R",
  "verify_tile_format.R"
)

for (script in temp_scripts) {
  if (file.exists(script)) {
    destination <- file.path("Scripts/Archive", script)
    if (!file.exists(destination)) {
      file.rename(script, destination)
      add_to_log(paste("  ✓ Moved to Archive:", script))
    } else {
      file.remove(script)
      add_to_log(paste("  ✓ Removed duplicate (already in Archive):", script))
    }
  }
}

# ============================================================================
# PHASE 4: MOVE UTILITY SCRIPTS TO UTILITIES
# ============================================================================

add_to_log("\nPHASE 4: Moving utility scripts to Utilities")

if (file.exists("setup_packages.R")) {
  destination <- "Scripts/Utilities/setup_packages.R"
  if (!file.exists(destination)) {
    file.rename("setup_packages.R", destination)
    add_to_log("  ✓ Moved to Utilities: setup_packages.R")
  } else {
    file.remove("setup_packages.R")
    add_to_log("  ✓ Removed duplicate (already in Utilities): setup_packages.R")
  }
}

# ============================================================================
# PHASE 5: VERIFICATION AND SUMMARY
# ============================================================================

add_to_log("\nPHASE 5: Verification and summary")

# Check final root directory contents
root_files <- list.files(pattern = "\\.(R|r)$")
add_to_log(paste("Remaining R scripts in root:", length(root_files)))
if (length(root_files) > 0) {
  add_to_log("WARNING: Unexpected R scripts still in root:")
  for (file in root_files) {
    add_to_log(paste("  -", file))
  }
}

# Count scripts in each directory
core_count <- length(list.files("Scripts/Core_Pipeline", pattern = "\\.(R|r)$"))
archive_count <- length(list.files("Scripts/Archive", pattern = "\\.(R|r)$"))  
utilities_count <- length(list.files("Scripts/Utilities", pattern = "\\.(R|r)$"))

add_to_log(paste("Scripts/Core_Pipeline:", core_count, "scripts"))
add_to_log(paste("Scripts/Archive:", archive_count, "scripts"))
add_to_log(paste("Scripts/Utilities:", utilities_count, "scripts"))

# ============================================================================
# SAVE CLEANUP LOG
# ============================================================================

writeLines(cleanup_log, "Scripts/Archive/cleanup_log.txt")
add_to_log("\n✅ CLEANUP COMPLETE!")
add_to_log("Log saved to: Scripts/Archive/cleanup_log.txt")

# Show final directory structure
cat("\n=== FINAL REPOSITORY STRUCTURE ===\n")
cat("Root directory (essential files only):\n")
essential_files <- list.files(pattern = "\\.(md|yml|Rproj|gitignore|LICENSE)$|^\\.")
for (file in sort(essential_files)) {
  if (!grepl("Rhistory|git", file)) {
    cat(paste("  ✓", file, "\n"))
  }
}

cat("\nScripts organization:\n")
cat(paste("  📁 Scripts/Core_Pipeline/  (", core_count, "scripts)\n"))
cat(paste("  📁 Scripts/Utilities/      (", utilities_count, "scripts)\n")) 
cat(paste("  📁 Scripts/Archive/        (", archive_count, "scripts)\n"))

cat("\n🎯 Repository is now clean and well-organized!\n")
