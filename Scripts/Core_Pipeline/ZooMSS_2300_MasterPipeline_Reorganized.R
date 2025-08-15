# ================================================================
# ZooMSS 2300 Master Pipeline - REORGANIZED
# ================================================================
# Updated master pipeline for the reorganized repository structure
# This script runs the complete ZooMSS 2300 analysis workflow

# ================================================================
# SETUP AND CONFIGURATION
# ================================================================

cat("=== ZooMSS 2300 MASTER PIPELINE - REORGANIZED ===\n")
cat("Pipeline start time:", Sys.time(), "\n\n")

# Set working directory and paths
base_dir <- "c:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300"
setwd(base_dir)

# Define paths to organized script directories
scripts_dir <- file.path(base_dir, "Scripts")
core_pipeline_dir <- file.path(scripts_dir, "Core_Pipeline")
utilities_dir <- file.path(scripts_dir, "Utilities")

# Load utility functions
cat("Loading utility functions...\n")
source(file.path(utilities_dir, "setup_packages.R"))
source(file.path(utilities_dir, "fZooMSS_Xtras.R"))

# ================================================================
# PIPELINE CONFIGURATION
# ================================================================

# Define which stages to run (set to FALSE to skip stages)
run_stages <- list(
  data_preprocessing = TRUE,
  environmental_analysis = TRUE,
  environmental_matrix = TRUE, 
  model_execution = TRUE,
  spatial_analysis = TRUE,
  biomass_analysis = TRUE
)

# ================================================================
# STAGE 1: DATA PREPROCESSING
# ================================================================

if(run_stages$data_preprocessing) {
  cat("\n=== STAGE 1: DATA PREPROCESSING ===\n")
  cat("Converting phytoplankton carbon to chlorophyll...\n")
  
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_0a_ConvertPhycToChl.R"))
    cat("✓ Data preprocessing completed successfully\n")
  }, error = function(e) {
    cat("❌ Error in data preprocessing:", e$message, "\n")
  })
}

# ================================================================
# STAGE 2: ENVIRONMENTAL TIMESERIES ANALYSIS
# ================================================================

if(run_stages$environmental_analysis) {
  cat("\n=== STAGE 2: ENVIRONMENTAL TIMESERIES ANALYSIS ===\n")
  
  # Temperature timeseries
  cat("Generating global temperature timeseries...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_1a_PlotGlobalTimeseriesTemp.R"))
    cat("✓ Temperature timeseries completed\n")
  }, error = function(e) {
    cat("❌ Error in temperature analysis:", e$message, "\n")
  })
  
  # Chlorophyll timeseries
  cat("Generating global chlorophyll timeseries...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_1b_PlotGlobalTimeseriesChl.R"))
    cat("✓ Chlorophyll timeseries completed\n")
  }, error = function(e) {
    cat("❌ Error in chlorophyll analysis:", e$message, "\n")
  })
  
  # SST-Chlorophyll distribution analysis
  cat("Analyzing SST-Chlorophyll distributions...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_1c_PlotSSTChlDistribution.R"))
    cat("✓ SST-Chlorophyll distribution analysis completed\n")
  }, error = function(e) {
    cat("❌ Error in distribution analysis:", e$message, "\n")
  })
}

# ================================================================
# STAGE 3: ENVIRONMENTAL MATRIX CREATION
# ================================================================

if(run_stages$environmental_matrix) {
  cat("\n=== STAGE 3: ENVIRONMENTAL MATRIX CREATION ===\n")
  
  # Create environmental matrix
  cat("Creating environmental matrix...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_2a_CreateEnviroMatrix.R"))
    cat("✓ Environmental matrix creation completed\n")
  }, error = function(e) {
    cat("❌ Error in environmental matrix creation:", e$message, "\n")
  })
  
  # Setup inputs
  cat("Setting up model inputs...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_2b_SetupInputs.R"))
    cat("✓ Input setup completed\n")
  }, error = function(e) {
    cat("❌ Error in input setup:", e$message, "\n")
  })
  
  # Compile inputs
  cat("Compiling model inputs...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_2d_CompileInputs.R"))
    cat("✓ Input compilation completed\n")
  }, error = function(e) {
    cat("❌ Error in input compilation:", e$message, "\n")
  })
}

# ================================================================
# STAGE 4: MODEL EXECUTION
# ================================================================

if(run_stages$model_execution) {
  cat("\n=== STAGE 4: MODEL EXECUTION ===\n")
  
  # Split models for parallel processing
  cat("Splitting models for processing...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_2e_SplitModels.R"))
    cat("✓ Model splitting completed\n")
  }, error = function(e) {
    cat("❌ Error in model splitting:", e$message, "\n")
  })
  
  # Run ZooMSS experiments
  cat("Running ZooMSS experiments...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_3d_Experiments.R"))
    cat("✓ ZooMSS experiments completed\n")
  }, error = function(e) {
    cat("❌ Error in ZooMSS experiments:", e$message, "\n")
  })
}

# ================================================================
# STAGE 5: SPATIAL ANALYSIS
# ================================================================

if(run_stages$spatial_analysis) {
  cat("\n=== STAGE 5: SPATIAL ANALYSIS ===\n")
  
  # Area weighting corrections
  cat("Applying area weighting corrections...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_4h_CorrectedAreaWeighting.R"))
    cat("✓ Area weighting corrections completed\n")
  }, error = function(e) {
    cat("❌ Error in area weighting:", e$message, "\n")
  })
  
  # Spatial plotting
  cat("Generating spatial plots...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_4i_SpatialPlotting.R"))
    cat("✓ Spatial plotting completed\n")
  }, error = function(e) {
    cat("❌ Error in spatial plotting:", e$message, "\n")
  })
}

# ================================================================
# STAGE 6: BIOMASS TIMESERIES ANALYSIS
# ================================================================

if(run_stages$biomass_analysis) {
  cat("\n=== STAGE 6: BIOMASS TIMESERIES ANALYSIS ===\n")
  
  # Multi-model ensemble analysis
  cat("Running multi-model ensemble analysis...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_4k_MultiModelEnsemble.R"))
    cat("✓ Multi-model ensemble analysis completed\n")
  }, error = function(e) {
    cat("❌ Error in ensemble analysis:", e$message, "\n")
  })
  
  # Separate biomass plots by model
  cat("Creating separate biomass plots by model...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_4l_SeparateBiomassPlots.R"))
    cat("✓ Separate biomass plots completed\n")
  }, error = function(e) {
    cat("❌ Error in separate biomass plots:", e$message, "\n")
  })
  
  # Multi-model mean biomass analysis
  cat("Creating multi-model mean biomass plots...\n")
  tryCatch({
    source(file.path(core_pipeline_dir, "ZooMSS_2300_4m_MultiModelMeanBiomass.R"))
    cat("✓ Multi-model mean analysis completed\n")
  }, error = function(e) {
    cat("❌ Error in multi-model mean analysis:", e$message, "\n")
  })
}

# ================================================================
# PIPELINE COMPLETION SUMMARY
# ================================================================

cat("\n=== PIPELINE COMPLETION SUMMARY ===\n")
cat("Pipeline end time:", Sys.time(), "\n")

# Check output directories
cat("\nOutput Summary:\n")

# Environmental timeseries figures
env_fig_count <- length(list.files("Figures/Environmental_Timeseries", pattern = "\\.(png|pdf)$"))
cat("Environmental timeseries figures:", env_fig_count, "\n")

# Biomass analysis figures
individual_plots <- length(list.files("Figures/Biomass_Timeseries/Individual_Models", pattern = "\\.png$"))
ensemble_plots <- length(list.files("Figures/Biomass_Timeseries/Multi_Model_Ensemble", pattern = "\\.png$"))
cat("Individual model biomass plots:", individual_plots, "\n")
cat("Multi-model ensemble plots:", ensemble_plots, "\n")

# Spatial plots
if(dir.exists("Figures/Spatial_Biomass")) {
  spatial_plots <- length(list.files("Figures/Spatial_Biomass", pattern = "\\.png$", recursive = TRUE))
  cat("Spatial plots:", spatial_plots, "\n")
}

# Quality check figures
quality_plots <- length(list.files("Figures/Quality_Checks", pattern = "\\.(png|csv)$"))
cat("Quality check files:", quality_plots, "\n")

# Data outputs
if(dir.exists("Output")) {
  output_files <- length(list.files("Output", pattern = "\\.rds$"))
  cat("Output data files:", output_files, "\n")
}

cat("\n=== REORGANIZED DIRECTORY STRUCTURE ===\n")
cat("✅ Scripts organized in Scripts/Core_Pipeline/, Scripts/Utilities/, Scripts/Archive/\n")
cat("✅ Figures organized by analysis type in Figures/ subdirectories\n")
cat("✅ Redundant scripts archived (", length(list.files("Scripts/Archive")), " files)\n")
cat("✅ Clean repository structure with clear workflow\n")

cat("\n=== RECOMMENDED NEXT STEPS ===\n")
cat("1. Review generated figures in organized subdirectories\n")
cat("2. Check quality validation plots in Figures/Quality_Checks/\n")
cat("3. Examine multi-model ensemble results\n")
cat("4. Update documentation to reflect new structure\n")

cat("\nZooMSS 2300 Master Pipeline completed! 🎉\n")
