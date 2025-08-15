# Repository Cleanup Analysis
# =========================

# CURRENT PROBLEM: Partial reorganization with significant redundancy
# - Many scripts exist in both root directory AND Scripts/Core_Pipeline/ 
# - Temporary/diagnostic scripts still in root directory
# - Multiple versions of similar scripts

# ANALYSIS OF ROOT DIRECTORY REDUNDANCY:

# 1. SCRIPTS THAT EXIST IN BOTH ROOT AND Scripts/Core_Pipeline/:
duplicates <- c(
  "ZooMSS_2300_4h_CorrectedAreaWeighting.R",  # In both root and Core_Pipeline
  "ZooMSS_2300_4i_SpatialPlotting.R",         # In both root and Core_Pipeline  
  "ZooMSS_2300_4k_MultiModelEnsemble.R",      # In both root and Core_Pipeline
  "ZooMSS_2300_4l_SeparateBiomassPlots.R",    # In both root and Core_Pipeline
  "ZooMSS_2300_4m_MultiModelMeanBiomass.R",   # In both root and Core_Pipeline
  "ZooMSS_2300_MasterPipeline.R"              # In both root and Core_Pipeline
)

# 2. SCRIPTS THAT SHOULD BE MOVED TO Scripts/Archive/:
archive_candidates <- c(
  "ZooMSS_2300_2c_STREAMLINED_EnviroMatrix.R",  # Already in Archive
  "ZooMSS_2300_4b_MemorySafeBiomassTimeseries.R", # Already in Archive
  "ZooMSS_2300_4c_BiomassTimeseriesPlots.R",      # Already in Archive
  "ZooMSS_2300_4d_EnhancedBiomassAnalysis.R",     # Already in Archive
  "ZooMSS_2300_4e_SpatialChangeAnalysis.R",       # Already in Archive
  "ZooMSS_2300_4f_SimplifiedSpatialAnalysis.R",   # Already in Archive
  "ZooMSS_2300_4g_EnhancedPlotting.R",            # Already in Archive
  "ZooMSS_2300_4h_IPSL_Individual_Plots.R",       # Already in Archive
  "ZooMSS_2300_4j_SeparateBiomassPlots.R"         # Already in Archive (different from 4l)
)

# 3. TEMPORARY/DIAGNOSTIC SCRIPTS TO ARCHIVE:
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

# 4. UTILITY SCRIPTS TO MOVE TO Scripts/Utilities/:
utility_scripts <- c(
  "setup_packages.R"  # Should be in Utilities
)

# RECOMMENDED ACTIONS:
cat("=== REPOSITORY CLEANUP PLAN ===\n")
cat("1. Remove duplicates from root (keep Scripts/Core_Pipeline versions)\n")
cat("2. Move development scripts to Scripts/Archive/\n") 
cat("3. Move utility scripts to Scripts/Utilities/\n")
cat("4. Clean root directory to essential files only\n")

# FINAL ROOT DIRECTORY SHOULD CONTAIN ONLY:
essential_files <- c(
  ".gitignore",
  "LICENSE", 
  "README.md",
  "ZooMSS_2300.Rproj",
  "config.yml",
  "VERIFICATION_RESULTS.md",
  "README_REORGANIZED.md",
  "REORGANIZATION_COMPLETE.md"
)

cat("\nEstimated cleanup:\n")
cat("- Remove", length(duplicates), "duplicate scripts from root\n")
cat("- Move", length(archive_candidates), "development scripts to Archive\n") 
cat("- Move", length(temp_scripts), "temporary scripts to Archive\n")
cat("- Move", length(utility_scripts), "utility scripts to Utilities\n")
cat("- Final root directory:", length(essential_files), "essential files only\n")

total_cleanup <- length(duplicates) + length(archive_candidates) + length(temp_scripts) + length(utility_scripts)
cat("\nTotal scripts to organize:", total_cleanup, "\n")
