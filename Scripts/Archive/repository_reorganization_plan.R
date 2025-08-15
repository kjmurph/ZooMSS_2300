# ================================================================
# ZooMSS 2300 Repository Reorganization Plan
# ================================================================
# This script analyzes the current repository structure and creates
# a reorganization plan to streamline scripts and organize outputs

# Current Analysis Results:
# - 134 R scripts total (many redundant)
# - Scattered figure outputs
# - Multiple intermediate data files
# - Unclear workflow progression

cat("=== ZooMSS 2300 REPOSITORY REORGANIZATION ANALYSIS ===\n")
cat("Date:", Sys.time(), "\n\n")

# ================================================================
# PROPOSED NEW DIRECTORY STRUCTURE
# ================================================================

cat("=== PROPOSED NEW DIRECTORY STRUCTURE ===\n\n")

proposed_structure <- "
ZooMSS_2300/
├── Scripts/
│   ├── Core_Pipeline/
│   │   ├── 01_Data_Preprocessing/
│   │   ├── 02_Environmental_Matrix/
│   │   ├── 03_Model_Execution/
│   │   └── 04_Analysis_Visualization/
│   ├── Utilities/
│   └── Archive/
├── Data/
│   ├── Input/
│   ├── Environmental_Matrix/
│   ├── Model_Outputs/
│   └── Processed/
├── Figures/
│   ├── Environmental_Timeseries/
│   ├── Spatial_Plots/
│   ├── Biomass_Timeseries/
│   └── Multi_Model_Ensemble/
├── Output/
│   ├── Final_Results/
│   ├── Quality_Checks/
│   └── Statistics/
├── Documentation/
└── Config/
"

cat(proposed_structure)

# ================================================================
# SCRIPT CLASSIFICATION AND CONSOLIDATION PLAN
# ================================================================

cat("\n=== SCRIPT CONSOLIDATION PLAN ===\n\n")

# Core Pipeline Scripts (KEEP - these are essential)
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
  "ZooMSS_2300_4k_MultiModelEnsemble.R",
  "ZooMSS_2300_4l_SeparateBiomassPlots.R",
  "ZooMSS_2300_4m_MultiModelMeanBiomass.R"
)

# Enhanced Analysis Scripts (CONSOLIDATE into fewer scripts)
analysis_scripts <- c(
  "ZooMSS_2300_4i_SpatialPlotting.R",
  "ZooMSS_2300_4h_CorrectedAreaWeighting.R"
)

# Redundant/Development Scripts (ARCHIVE or DELETE)
redundant_scripts <- c(
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
  "ZooMSS_2300_4j_SeparateBiomassPlots.R"
)

# Utility Scripts (KEEP but organize)
utility_scripts <- c(
  "fDownloadFiles.R",
  "fZooMSS_CheckFileIDs.R",
  "fZooMSS_Xtras.R",
  "setup_packages.R"
)

# Analysis/Diagnostic Scripts (ARCHIVE)
diagnostic_scripts <- c(
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
  "create_biomass_plots_final.R"
)

cat("CORE PIPELINE SCRIPTS (", length(core_scripts), " scripts - KEEP):\n")
for(script in core_scripts) {
  cat("  ✓", script, "\n")
}

cat("\nANALYSIS SCRIPTS (", length(analysis_scripts), " scripts - CONSOLIDATE):\n")
for(script in analysis_scripts) {
  cat("  →", script, "\n")
}

cat("\nUTILITY SCRIPTS (", length(utility_scripts), " scripts - ORGANIZE):\n")
for(script in utility_scripts) {
  cat("  📁", script, "\n")
}

cat("\nREDUNDANT SCRIPTS (", length(redundant_scripts), " scripts - ARCHIVE):\n")
for(script in redundant_scripts[1:10]) { # Show first 10
  cat("  ❌", script, "\n")
}
cat("  ... and", length(redundant_scripts) - 10, "more\n")

cat("\nDIAGNOSTIC SCRIPTS (", length(diagnostic_scripts), " scripts - ARCHIVE):\n")
for(script in diagnostic_scripts[1:10]) { # Show first 10
  cat("  📊", script, "\n")
}
cat("  ... and", length(diagnostic_scripts) - 10, "more\n")

# ================================================================
# CONSOLIDATED WORKFLOW SCRIPTS
# ================================================================

cat("\n=== PROPOSED CONSOLIDATED WORKFLOW ===\n\n")

consolidated_workflow <- "
STREAMLINED WORKFLOW (4 main stages):

📊 Stage 1: Data Preprocessing
   - ZooMSS_01_DataPreprocessing.R
     (consolidates: 0a_ConvertPhycToChl + data validation)

🌍 Stage 2: Environmental Analysis  
   - ZooMSS_02_EnvironmentalTimeseries.R
     (consolidates: 1a_PlotTemp + 1b_PlotChl + 1c_PlotDistribution)
   - ZooMSS_03_EnvironmentalMatrix.R
     (consolidates: 2a_CreateEnviro + 2b_SetupInputs + 2d_CompileInputs)

🔬 Stage 3: Model Execution
   - ZooMSS_04_ModelExecution.R
     (consolidates: 2e_SplitModels + 3d_Experiments)

📈 Stage 4: Analysis & Visualization
   - ZooMSS_05_SpatialAnalysis.R
     (consolidates: 4i_SpatialPlotting + 4h_AreaWeighting)
   - ZooMSS_06_BiomassAnalysis.R
     (consolidates: 4k_MultiModel + 4l_SeparatePlots + 4m_MultiModelMean)

🎯 Master Pipeline
   - ZooMSS_MasterPipeline.R (updated to use new structure)
"

cat(consolidated_workflow)

cat("\n=== FIGURE ORGANIZATION PLAN ===\n\n")

figure_organization <- "
CURRENT ISSUES:
- Figures scattered across multiple directories
- Inconsistent naming conventions
- Some figures in root Figures/ directory

PROPOSED ORGANIZATION:
Figures/
├── Environmental_Timeseries/
│   ├── temperature_scenarios.png
│   ├── chlorophyll_scenarios.png
│   └── sst_chl_distributions.png
├── Spatial_Plots/
│   ├── Biomass_Maps/
│   ├── Change_Maps/
│   └── Coverage_Validation/
├── Biomass_Timeseries/
│   ├── Individual_Models/
│   │   ├── zooplankton_by_model.png
│   │   ├── fish_by_model.png
│   │   └── tcb_by_model.png
│   └── Multi_Model_Ensemble/
│       ├── ensemble_mean_plots.png
│       ├── uncertainty_analysis.png
│       └── model_agreement.png
└── Quality_Checks/
    ├── area_weighting_validation.png
    └── spatial_coverage_plots.png
"

cat(figure_organization)

cat("\n=== DATA ORGANIZATION PLAN ===\n\n")

data_organization <- "
CURRENT ISSUES:
- Multiple intermediate files with unclear purpose
- Redundant data processing outputs
- Inconsistent file naming

PROPOSED ORGANIZATION:
Data/
├── Raw_Input/              (original downloaded data)
├── Environmental_Matrix/   (processed environmental data)
├── Model_Outputs/         (ZooMSS results)
├── Processed_Timeseries/  (final biomass timeseries)
└── Quality_Checks/        (validation data)

OUTPUT CONSOLIDATION:
- Keep: combined_weighted_biomass_timeseries.rds
- Keep: ClimateChange_2300_Compiled.rds
- Archive: intermediate_*_timeseries_*.rds (development files)
- Organize: model-specific outputs by ESM
"

cat(data_organization)

cat("\n=== IMPLEMENTATION PRIORITY ===\n\n")

implementation_plan <- "
PHASE 1 (Immediate - File Organization):
1. Create new directory structure
2. Move figures to organized subdirectories
3. Archive redundant scripts to Archive/ folder

PHASE 2 (Script Consolidation):
1. Create consolidated workflow scripts
2. Update master pipeline
3. Test new workflow

PHASE 3 (Data Cleanup):
1. Organize data files by purpose
2. Remove intermediate development files
3. Update file paths in scripts

PHASE 4 (Documentation):
1. Update README with new structure
2. Create workflow documentation
3. Add script descriptions
"

cat(implementation_plan)

cat("\n=== ESTIMATED BENEFITS ===\n\n")

benefits <- "
CURRENT STATE:
- 134 R scripts (many redundant)
- Figures in 5+ different locations
- Unclear workflow progression
- Difficult to maintain

IMPROVED STATE:
- ~15 core scripts (organized workflow)
- All figures in organized subdirectories
- Clear 6-stage pipeline
- Easy to understand and maintain

SPACE SAVINGS:
- ~119 scripts moved to archive
- Intermediate data files organized
- Cleaner repository structure
"

cat(benefits)

cat("\n=== NEXT STEPS ===\n")
cat("1. Review this reorganization plan\n")
cat("2. Approve directory structure changes\n") 
cat("3. Begin Phase 1 implementation\n")
cat("4. Test consolidated workflow\n\n")

cat("Analysis complete!\n")
