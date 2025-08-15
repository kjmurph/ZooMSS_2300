# ================================================================
# ZooMSS 2300 File Reorganization Implementation
# ================================================================
# This script implements the reorganization plan by moving files
# to their appropriate new locations

library(fs)

cat("=== ZooMSS 2300 FILE REORGANIZATION ===\n")
cat("Date:", Sys.time(), "\n\n")

base_dir <- "c:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300"
setwd(base_dir)

# ================================================================
# PHASE 1: ORGANIZE FIGURES
# ================================================================

cat("=== PHASE 1: ORGANIZING FIGURES ===\n\n")

# Environmental timeseries figures (move from root Figures/)
environmental_figures <- c(
  "chla_all_scenarios_combined.png",
  "chla_all_scenarios_facet.png", 
  "chla_difference_ssp126_ssp_585.png",
  "temp_all_scenarios_combined.png",
  "temp_all_scenarios_facet.png",
  "temp_difference_ssp126_ssp585_scenarios.png",
  "SSTChl_All.png",
  "SSTChl_All_v2.png",
  "SSTChl_All_v2.pdf",
  "SSTChl_Distinct.png",
  "SSTChl_Distinct_v2.png", 
  "SSTChl_Distinct_v2.pdf",
  "SSTChl_ModelExperimentFacet_v2.png",
  "SSTChl_ModelExperimentFacet_v2.pdf",
  "SSTChl_ModelFacet_v2.png",
  "SSTChl_ModelFacet_v2.pdf",
  "Novel_SSTChl_2300.png"
)

cat("Moving environmental timeseries figures...\n")
for(fig in environmental_figures) {
  src <- file.path("Figures", fig)
  dst <- file.path("Figures", "Environmental_Timeseries", fig)
  if(file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    file.remove(src)
    cat("  ✓ Moved:", fig, "\n")
  } else {
    cat("  ⚠ Not found:", fig, "\n")
  }
}

# Move biomass timeseries figures from Biomass_Enhanced/ to new structure
cat("\nMoving biomass timeseries figures...\n")

# Individual model plots
individual_model_files <- c(
  "zooplankton_percentage_change_by_model.png",
  "fish_percentage_change_by_model.png", 
  "tcb_percentage_change_by_model.png"
)

for(fig in individual_model_files) {
  src <- file.path("Figures", "Biomass_Enhanced", fig)
  dst <- file.path("Figures", "Biomass_Timeseries", "Individual_Models", fig)
  if(file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    file.remove(src)
    cat("  ✓ Moved to Individual_Models:", fig, "\n")
  }
}

# Multi-model ensemble plots
ensemble_files <- c(
  "zooplankton_multimodel_mean.png",
  "fish_multimodel_mean.png",
  "tcb_multimodel_mean.png", 
  "all_biomass_multimodel_mean_combined.png",
  "ensemble_biomass_statistics.csv"
)

for(fig in ensemble_files) {
  src <- file.path("Figures", "Biomass_Enhanced", fig)
  dst <- file.path("Figures", "Biomass_Timeseries", "Multi_Model_Ensemble", fig)
  if(file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    file.remove(src)
    cat("  ✓ Moved to Multi_Model_Ensemble:", fig, "\n")
  }
}

# Quality check figures to Quality_Checks/
quality_files <- list.files("Figures/Spatial_Coverage_Validation", full.names = TRUE, recursive = TRUE)
if(length(quality_files) > 0) {
  cat("\nMoving quality check figures...\n")
  for(qfile in quality_files) {
    if(file.exists(qfile)) {
      filename <- basename(qfile)
      dst <- file.path("Figures", "Quality_Checks", filename)
      file.copy(qfile, dst, overwrite = TRUE)
      cat("  ✓ Moved to Quality_Checks:", filename, "\n")
    }
  }
}

# ================================================================
# PHASE 2: ORGANIZE SCRIPTS
# ================================================================

cat("\n=== PHASE 2: ORGANIZING SCRIPTS ===\n\n")

# Core pipeline scripts (move to Scripts/Core_Pipeline/)
core_scripts <- c(
  "ZooMSS_2300_0a_ConvertPhycToChl.R",
  "ZooMSS_2300_1a_PlotGlobalTimeseriesTemp.R",
  "ZooMSS_2300_1b_PlotGlobalTimeseriesChl.R", 
  "ZooMSS_2300_1c_PlotSSTChlDistribution.R",
  "ZooMSS_2300_2a_CreateEnviroMatrix.R",
  "ZooMSS_2300_2b_SetupInputs.R",
  "ZooMSS_2300_2d_CompileInputs.R",
  "ZooMSS_2300_2e_SplitModels.R",
  "ZooMSS_2300_3d_Experiments.R",
  "ZooMSS_2300_4i_SpatialPlotting.R",
  "ZooMSS_2300_4h_CorrectedAreaWeighting.R",
  "ZooMSS_2300_4k_MultiModelEnsemble.R",
  "ZooMSS_2300_4l_SeparateBiomassPlots.R",
  "ZooMSS_2300_4m_MultiModelMeanBiomass.R",
  "ZooMSS_2300_MasterPipeline.R"
)

cat("Moving core pipeline scripts...\n")
for(script in core_scripts) {
  if(file.exists(script)) {
    dst <- file.path("Scripts", "Core_Pipeline", script)
    file.copy(script, dst, overwrite = TRUE)
    file.remove(script)
    cat("  ✓ Moved:", script, "\n")
  } else {
    cat("  ⚠ Not found:", script, "\n")
  }
}

# Utility scripts (move to Scripts/Utilities/)
utility_scripts <- c(
  "fDownloadFiles.R",
  "fZooMSS_CheckFileIDs.R", 
  "fZooMSS_Xtras.R",
  "setup_packages.R"
)

cat("\nMoving utility scripts...\n")
for(script in utility_scripts) {
  if(file.exists(script)) {
    dst <- file.path("Scripts", "Utilities", script)
    file.copy(script, dst, overwrite = TRUE)
    file.remove(script)
    cat("  ✓ Moved:", script, "\n")
  }
}

# Archive redundant/diagnostic scripts
archive_scripts <- c(
  "ZooMSS_2300_2c_CheckDateStructure.R",
  "ZooMSS_2300_2c_CreateMappingMatrix.R",
  "ZooMSS_2300_2c_Unique2300EnviroMatrix.R",
  "ZooMSS_2300_2c_STREAMLINED_EnviroMatrix.R",
  "ZooMSS_2300_2d_OrderEnviroMatrix.R",
  "ZooMSS_2300_4a_Plotting.R",
  "ZooMSS_2300_4b_MemorySafeBiomassTimeseries.R",
  "ZooMSS_2300_4c_BiomassTimeseriesPlots.R",
  "ZooMSS_2300_4d_EnhancedBiomassAnalysis.R",
  "ZooMSS_2300_4e_SpatialChangeAnalysis.R",
  "ZooMSS_2300_4f_SimplifiedSpatialAnalysis.R",
  "ZooMSS_2300_4g_EnhancedPlotting.R",
  "ZooMSS_2300_4h_IPSL_Individual_Plots.R",
  "ZooMSS_2300_4j_SeparateBiomassPlots.R",
  "analyze_redundancy.R",
  "check_biomass_data.R",
  "check_data_structure.R",
  "cleanup_redundant_enviro_files.R",
  "diagnose_extreme_changes.R",
  "extreme_values_solution_summary.R",
  "multimodel_analysis_summary.R",
  "quick_structure_check.R",
  "separate_plots_summary.R",
  "spatial_change_diagnostic.R",
  "spatial_diagnostic.R",
  "spatial_plot_test.R",
  "test_enhanced_theme.R",
  "test_extreme_values_approach.R",
  "validate_area_weighting.R",
  "validate_spatial_coverage.R",
  "validate_spatial_coverage_simple.R",
  "verify_grid_coverage.R",
  "verify_tile_format.R",
  "create_biomass_plots_final.R",
  "repository_reorganization_plan.R"
)

cat("\nArchiving redundant/diagnostic scripts...\n")
archived_count <- 0
for(script in archive_scripts) {
  if(file.exists(script)) {
    dst <- file.path("Scripts", "Archive", script)
    file.copy(script, dst, overwrite = TRUE)
    file.remove(script)
    archived_count <- archived_count + 1
    cat("  📦 Archived:", script, "\n")
  }
}

cat("\n=== REORGANIZATION SUMMARY ===\n")
cat("Environmental figures organized: ✓\n")
cat("Biomass figures reorganized: ✓\n") 
cat("Core scripts moved to Scripts/Core_Pipeline/: ✓\n")
cat("Utility scripts moved to Scripts/Utilities/: ✓\n")
cat("Scripts archived:", archived_count, "\n")

cat("\n=== REMAINING FILES IN ROOT ===\n")
remaining_r_files <- list.files(".", pattern = "\\.R$")
if(length(remaining_r_files) > 0) {
  cat("R scripts still in root directory:\n")
  for(f in remaining_r_files) {
    cat("  -", f, "\n")
  }
} else {
  cat("No R scripts remaining in root directory ✓\n")
}

cat("\n=== NEW DIRECTORY STRUCTURE ===\n")
cat("Scripts/\n")
cat("├── Core_Pipeline/ (", length(list.files("Scripts/Core_Pipeline")), " files)\n")
cat("├── Utilities/ (", length(list.files("Scripts/Utilities")), " files)\n") 
cat("└── Archive/ (", length(list.files("Scripts/Archive")), " files)\n\n")

cat("Figures/\n")
cat("├── Environmental_Timeseries/ (", length(list.files("Figures/Environmental_Timeseries")), " files)\n")
cat("├── Biomass_Timeseries/\n")
cat("│   ├── Individual_Models/ (", length(list.files("Figures/Biomass_Timeseries/Individual_Models")), " files)\n")
cat("│   └── Multi_Model_Ensemble/ (", length(list.files("Figures/Biomass_Timeseries/Multi_Model_Ensemble")), " files)\n")
cat("├── Quality_Checks/ (", length(list.files("Figures/Quality_Checks")), " files)\n")
cat("└── [Existing subdirectories preserved]\n")

cat("\nReorganization complete! ✅\n")
