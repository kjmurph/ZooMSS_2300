# ==============================================================================
# ZooMSS_2300 MASTER PIPELINE
# ==============================================================================
# Purpose: Single entry point for complete marine ecosystem analysis
# Author: Marine Ecosystem Analysis Team
# Date: August 11, 2025
# Version: 1.0.0
# ==============================================================================

# Clear environment and setup
rm(list = ls())
gc()

# Load required libraries
required_packages <- c("tidyverse", "raster", "ncdf4", "viridis", "scales", 
                      "maps", "config", "logger")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Source utility functions
source("R/logging_utils.R")

# ==============================================================================
# PIPELINE CONFIGURATION
# ==============================================================================

# Load configuration
cat("Loading configuration...\n")
config <- config::get(file = "config_simple.yml")

# Setup logging
log_file <- setup_logging(
  log_dir = config$paths$logs_dir,
  log_level = config$logging$level,
  script_name = "ZooMSS_MasterPipeline"
)

# Set random seed for reproducibility
set.seed(config$analysis$random_seed)
log_info("Random seed set to: {config$analysis$random_seed}")

# ==============================================================================
# PIPELINE PARAMETERS
# ==============================================================================

# Parse command line arguments for pipeline options
args <- commandArgs(trailingOnly = TRUE)

# Pipeline execution options
run_area_weighting <- TRUE
run_spatial_validation <- TRUE  
run_enhanced_plotting <- TRUE
run_full_validation <- TRUE

# Override with command line arguments if provided
if(length(args) > 0) {
  if("--skip-area-weighting" %in% args) run_area_weighting <- FALSE
  if("--skip-spatial-validation" %in% args) run_spatial_validation <- FALSE
  if("--skip-plotting" %in% args) run_enhanced_plotting <- FALSE
  if("--skip-validation" %in% args) run_full_validation <- FALSE
  if("--area-weighting-only" %in% args) {
    run_spatial_validation <- FALSE
    run_enhanced_plotting <- FALSE
    run_full_validation <- FALSE
  }
}

log_info("Pipeline execution plan:")
log_info("  - Area Weighting: {run_area_weighting}")
log_info("  - Spatial Validation: {run_spatial_validation}")
log_info("  - Enhanced Plotting: {run_enhanced_plotting}")
log_info("  - Full Validation: {run_full_validation}")

# ==============================================================================
# PRE-EXECUTION VALIDATION
# ==============================================================================

log_checkpoint("Pre-execution validation")

# Check system requirements (basic validation)
current_memory_mb <- sum(gc()[,2])
log_info("Current memory usage: {round(current_memory_mb, 1)} MB")

# Basic check - if we can't allocate reasonable memory, warn but continue
tryCatch({
  test_allocation <- numeric(1e6)  # Try to allocate ~8MB
  rm(test_allocation)
  log_info("Memory allocation test passed")
}, error = function(e) {
  log_warn("Memory allocation test failed: {e$message}")
})

# Validate directory structure
required_dirs <- c(
  config$paths$input_dir,
  config$paths$biomass_projections_dir,
  config$paths$output_dir
)

for(dir in required_dirs) {
  if(!dir.exists(dir)) {
    log_error_context(
      "Required directory not found: {dir}",
      "Please ensure all input directories exist",
      stop_execution = TRUE
    )
  }
}

# Check for required input files
biomass_files <- list.files(config$paths$biomass_projections_dir, 
                           pattern = "*.rds", full.names = TRUE)

if(length(biomass_files) == 0) {
  log_error_context(
    "No biomass projection files found",
    "Directory: {config$paths$biomass_projections_dir}",
    stop_execution = TRUE
  )
}

log_info("Found {length(biomass_files)} biomass projection files")

# Create output directories if needed
for(dir in c(config$paths$figures_dir, config$paths$enhanced_figures_dir,
             config$paths$spatial_validation_dir, config$paths$logs_dir)) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    log_info("Created directory: {dir}")
  }
}

# ==============================================================================
# PIPELINE EXECUTION
# ==============================================================================

pipeline_start_time <- Sys.time()
log_info("=== PIPELINE EXECUTION STARTED ===")

# Track execution results
execution_results <- list()

# ------------------------------------------------------------------------------
# STEP 1: CORRECTED AREA WEIGHTING ANALYSIS
# ------------------------------------------------------------------------------

if(run_area_weighting) {
  step_start <- log_checkpoint("Starting Corrected Area Weighting Analysis")
  
  tryCatch({
    
    # Check if corrected data already exists
    if(file.exists(config$paths$corrected_timeseries)) {
      log_info("Corrected timeseries already exists: {config$paths$corrected_timeseries}")
      user_input <- readline("Overwrite existing corrected data? (y/n): ")
      
      if(tolower(user_input) != "y") {
        log_info("Skipping area weighting - using existing data")
        execution_results$area_weighting <- "Skipped - existing data"
      } else {
        log_info("Proceeding with area weighting analysis")
        source("ZooMSS_2300_4h_CorrectedAreaWeighting.R")
        execution_results$area_weighting <- "Completed - data overwritten"
      }
    } else {
      log_info("Running corrected area weighting analysis")
      source("ZooMSS_2300_4h_CorrectedAreaWeighting.R")
      execution_results$area_weighting <- "Completed successfully"
    }
    
    log_checkpoint("Corrected Area Weighting Analysis", step_start)
    
  }, error = function(e) {
    log_error_context(
      "Failed in corrected area weighting step",
      "Error: {e$message}",
      stop_execution = !config$processing$error_handling$continue_on_error
    )
    execution_results$area_weighting <- paste("Failed:", e$message)
  })
  
} else {
  log_info("Skipping corrected area weighting analysis")
  execution_results$area_weighting <- "Skipped by user"
}

# ------------------------------------------------------------------------------
# STEP 2: SPATIAL COVERAGE VALIDATION  
# ------------------------------------------------------------------------------

if(run_spatial_validation) {
  step_start <- log_checkpoint("Starting Spatial Coverage Validation")
  
  tryCatch({
    
    log_info("Running spatial coverage validation")
    source("validate_spatial_coverage_simple.R")
    execution_results$spatial_validation <- "Completed successfully"
    
    log_checkpoint("Spatial Coverage Validation", step_start)
    
  }, error = function(e) {
    log_error_context(
      "Failed in spatial validation step", 
      "Error: {e$message}",
      stop_execution = !config$processing$error_handling$continue_on_error
    )
    execution_results$spatial_validation <- paste("Failed:", e$message)
  })
  
} else {
  log_info("Skipping spatial coverage validation")
  execution_results$spatial_validation <- "Skipped by user"
}

# ------------------------------------------------------------------------------
# STEP 3: ENHANCED PLOTTING
# ------------------------------------------------------------------------------

if(run_enhanced_plotting) {
  step_start <- log_checkpoint("Starting Enhanced Plotting")
  
  tryCatch({
    
    # Verify corrected data exists
    if(!file.exists(config$paths$corrected_timeseries)) {
      log_error_context(
        "Corrected timeseries data not found",
        "File: {config$paths$corrected_timeseries}",
        stop_execution = TRUE
      )
    }
    
    log_info("Running enhanced plotting analysis")
    source("ZooMSS_2300_4g_EnhancedPlotting.R")
    execution_results$enhanced_plotting <- "Completed successfully"
    
    log_checkpoint("Enhanced Plotting", step_start)
    
  }, error = function(e) {
    log_error_context(
      "Failed in enhanced plotting step",
      "Error: {e$message}",
      stop_execution = !config$processing$error_handling$continue_on_error
    )
    execution_results$enhanced_plotting <- paste("Failed:", e$message)
  })
  
} else {
  log_info("Skipping enhanced plotting")
  execution_results$enhanced_plotting <- "Skipped by user"
}

# ------------------------------------------------------------------------------
# STEP 4: COMPREHENSIVE VALIDATION
# ------------------------------------------------------------------------------

if(run_full_validation) {
  step_start <- log_checkpoint("Starting Comprehensive Validation")
  
  tryCatch({
    
    log_info("Running comprehensive validation")
    source("validate_area_weighting.R")
    execution_results$comprehensive_validation <- "Completed successfully"
    
    log_checkpoint("Comprehensive Validation", step_start)
    
  }, error = function(e) {
    log_error_context(
      "Failed in comprehensive validation step",
      "Error: {e$message}",
      stop_execution = !config$processing$error_handling$continue_on_error
    )
    execution_results$comprehensive_validation <- paste("Failed:", e$message)
  })
  
} else {
  log_info("Skipping comprehensive validation")
  execution_results$comprehensive_validation <- "Skipped by user"
}

# ==============================================================================
# PIPELINE COMPLETION AND SUMMARY
# ==============================================================================

pipeline_end_time <- Sys.time()
total_duration <- difftime(pipeline_end_time, pipeline_start_time, units = "mins")

log_info("=== PIPELINE EXECUTION COMPLETED ===")
log_info("Total execution time: {round(total_duration, 2)} minutes")

# Generate execution summary
log_info("=== EXECUTION SUMMARY ===")
for(step in names(execution_results)) {
  status <- execution_results[[step]]
  log_info("{step}: {status}")
}

# Final memory cleanup
final_memory <- log_memory_usage("Pipeline completion")

# Check for any errors or warnings
error_count <- length(grep("ERROR", readLines(log_file)))
warning_count <- length(grep("WARN", readLines(log_file)))

if(error_count > 0) {
  log_error("Pipeline completed with {error_count} errors")
}

if(warning_count > 0) {
  log_warn("Pipeline completed with {warning_count} warnings")
}

# Generate processing summary report
summary_file <- create_processing_summary(log_file, config$paths$logs_dir)

# ==============================================================================
# FINAL VALIDATION AND OUTPUTS
# ==============================================================================

log_info("=== FINAL OUTPUTS VALIDATION ===")

# Check expected output files
expected_outputs <- list()

# Only check outputs that are defined in config
if(!is.null(config$paths$corrected_timeseries)) {
  expected_outputs[["Corrected Timeseries"]] <- config$paths$corrected_timeseries
}
if(!is.null(config$paths$spatial_validation_report)) {
  expected_outputs[["Spatial Validation Report"]] <- config$paths$spatial_validation_report
}
if(!is.null(config$paths$enhanced_figures_dir)) {
  expected_outputs[["Enhanced Figures Directory"]] <- config$paths$enhanced_figures_dir
}
if(!is.null(config$paths$spatial_validation_dir)) {
  expected_outputs[["Spatial Validation Directory"]] <- config$paths$spatial_validation_dir
}

for(output_name in names(expected_outputs)) {
  output_path <- expected_outputs[[output_name]]
  
  # Ensure output_path is not NULL or empty
  if(!is.null(output_path) && output_path != "") {
    if(file.exists(output_path) || dir.exists(output_path)) {
      log_info("✓ {output_name}: Available")
    } else {
      log_warn("✗ {output_name}: Missing ({output_path})")
    }
  } else {
    log_warn("✗ {output_name}: Path not configured")
  }
}

# Final success check
all_critical_steps_passed <- all(sapply(execution_results, function(x) !grepl("Failed", x)))

if(all_critical_steps_passed && error_count == 0) {
  log_info("🎉 PIPELINE COMPLETED SUCCESSFULLY!")
  cat("\n=== ZooMSS_2300 PIPELINE COMPLETED SUCCESSFULLY ===\n")
  cat("📊 Marine ecosystem analysis through 2300 complete\n")
  cat("📁 Results available in:", config$paths$output_dir, "\n")
  cat("📈 Figures available in:", config$paths$figures_dir, "\n")
  cat("📋 Processing log:", log_file, "\n")
  cat("📝 Summary report:", summary_file, "\n")
} else {
  log_error("❌ PIPELINE COMPLETED WITH ISSUES")
  cat("\n=== ZooMSS_2300 PIPELINE COMPLETED WITH ISSUES ===\n")
  cat("⚠️  Check log file for details:", log_file, "\n")
  cat("📝 Summary report:", summary_file, "\n")
}

# Session info for reproducibility
log_info("=== SESSION INFO ===")
session_info <- sessionInfo()
log_info("R version: {session_info$R.version$version.string}")
log_info("Platform: {session_info$platform}")

# Save session info for reproducibility
saveRDS(session_info, file.path(config$paths$logs_dir, 
                                paste0("session_info_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")))

log_info("=== PIPELINE SESSION ENDED ===")

# ==============================================================================
# PIPELINE USAGE INFORMATION
# ==============================================================================

if(length(args) == 0 || "--help" %in% args) {
  cat("\n=== ZooMSS_2300 Master Pipeline Usage ===\n")
  cat("Rscript ZooMSS_2300_MasterPipeline.R [OPTIONS]\n\n")
  cat("Options:\n")
  cat("  --skip-area-weighting     Skip corrected area weighting analysis\n")
  cat("  --skip-spatial-validation Skip spatial coverage validation\n") 
  cat("  --skip-plotting          Skip enhanced plotting\n")
  cat("  --skip-validation        Skip comprehensive validation\n")
  cat("  --area-weighting-only    Run only area weighting analysis\n")
  cat("  --help                   Show this help message\n\n")
  cat("Examples:\n")
  cat("  # Run complete pipeline:\n")
  cat("  Rscript ZooMSS_2300_MasterPipeline.R\n\n")
  cat("  # Run only area weighting:\n") 
  cat("  Rscript ZooMSS_2300_MasterPipeline.R --area-weighting-only\n\n")
  cat("  # Skip plotting:\n")
  cat("  Rscript ZooMSS_2300_MasterPipeline.R --skip-plotting\n\n")
}
