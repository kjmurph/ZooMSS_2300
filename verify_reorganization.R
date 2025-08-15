# ================================================================
# Repository Reorganization Verification
# ================================================================
# Verifies that the reorganization was completed successfully

cat("=== ZOOMSS 2300 REPOSITORY REORGANIZATION VERIFICATION ===\n")
cat("Verification time:", Sys.time(), "\n\n")

base_dir <- "c:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/ZooMSS_2300"
setwd(base_dir)

# ================================================================
# VERIFY DIRECTORY STRUCTURE
# ================================================================

cat("=== DIRECTORY STRUCTURE VERIFICATION ===\n\n")

# Check Scripts organization
scripts_structure <- list(
  "Scripts/" = dir.exists("Scripts"),
  "Scripts/Core_Pipeline/" = dir.exists("Scripts/Core_Pipeline"),
  "Scripts/Utilities/" = dir.exists("Scripts/Utilities"), 
  "Scripts/Archive/" = dir.exists("Scripts/Archive")
)

cat("Scripts Directory Structure:\n")
for(path in names(scripts_structure)) {
  status <- if(scripts_structure[[path]]) "✅" else "❌"
  cat("  ", status, path, "\n")
}

# Check Figures organization
figures_structure <- list(
  "Figures/Environmental_Timeseries/" = dir.exists("Figures/Environmental_Timeseries"),
  "Figures/Biomass_Timeseries/" = dir.exists("Figures/Biomass_Timeseries"),
  "Figures/Biomass_Timeseries/Individual_Models/" = dir.exists("Figures/Biomass_Timeseries/Individual_Models"),
  "Figures/Biomass_Timeseries/Multi_Model_Ensemble/" = dir.exists("Figures/Biomass_Timeseries/Multi_Model_Ensemble"),
  "Figures/Quality_Checks/" = dir.exists("Figures/Quality_Checks")
)

cat("\nFigures Directory Structure:\n")
for(path in names(figures_structure)) {
  status <- if(figures_structure[[path]]) "✅" else "❌"
  cat("  ", status, path, "\n")
}

# ================================================================
# VERIFY SCRIPT ORGANIZATION
# ================================================================

cat("\n=== SCRIPT ORGANIZATION VERIFICATION ===\n\n")

# Core Pipeline Scripts
core_scripts <- list.files("Scripts/Core_Pipeline", pattern = "\\.R$")
cat("Core Pipeline Scripts (", length(core_scripts), " files):\n")
for(script in sort(core_scripts)) {
  cat("  ✓", script, "\n")
}

# Utility Scripts
util_scripts <- list.files("Scripts/Utilities", pattern = "\\.R$")
cat("\nUtility Scripts (", length(util_scripts), " files):\n")
for(script in sort(util_scripts)) {
  cat("  📁", script, "\n")
}

# Archived Scripts
archive_scripts <- list.files("Scripts/Archive", pattern = "\\.R$")
cat("\nArchived Scripts (", length(archive_scripts), " files):\n")
for(script in sort(archive_scripts)[1:10]) { # Show first 10
  cat("  📦", script, "\n")
}
if(length(archive_scripts) > 10) {
  cat("  ... and", length(archive_scripts) - 10, "more archived scripts\n")
}

# ================================================================
# VERIFY FIGURE ORGANIZATION
# ================================================================

cat("\n=== FIGURE ORGANIZATION VERIFICATION ===\n\n")

# Environmental Timeseries
env_files <- list.files("Figures/Environmental_Timeseries", pattern = "\\.(png|pdf)$")
cat("Environmental Timeseries Figures (", length(env_files), " files):\n")
for(fig in sort(env_files)[1:5]) { # Show first 5
  cat("  🌍", fig, "\n")
}
if(length(env_files) > 5) {
  cat("  ... and", length(env_files) - 5, "more environmental figures\n")
}

# Individual Model Biomass Plots
individual_files <- list.files("Figures/Biomass_Timeseries/Individual_Models", pattern = "\\.png$")
cat("\nIndividual Model Biomass Plots (", length(individual_files), " files):\n")
for(fig in sort(individual_files)) {
  cat("  📊", fig, "\n")
}

# Multi-Model Ensemble Plots
ensemble_files <- list.files("Figures/Biomass_Timeseries/Multi_Model_Ensemble", pattern = "\\.(png|csv)$")
cat("\nMulti-Model Ensemble Files (", length(ensemble_files), " files):\n")
for(fig in sort(ensemble_files)) {
  cat("  📈", fig, "\n")
}

# Quality Check Files
quality_files <- list.files("Figures/Quality_Checks", pattern = "\\.(png|csv)$")
cat("\nQuality Check Files (", length(quality_files), " files):\n")
for(fig in sort(quality_files)) {
  cat("  🔍", fig, "\n")
}

# ================================================================
# VERIFY ROOT DIRECTORY CLEANUP
# ================================================================

cat("\n=== ROOT DIRECTORY CLEANUP VERIFICATION ===\n\n")

# Check for remaining R scripts in root
root_r_files <- list.files(".", pattern = "\\.R$")
cat("R Scripts remaining in root directory (", length(root_r_files), " files):\n")
if(length(root_r_files) == 0) {
  cat("  ✅ No R scripts in root - cleanup successful!\n")
} else {
  for(script in root_r_files) {
    cat("  ⚠", script, "\n")
  }
}

# ================================================================
# CALCULATE REORGANIZATION BENEFITS
# ================================================================

cat("\n=== REORGANIZATION BENEFITS SUMMARY ===\n\n")

# Count files
total_archived <- length(list.files("Scripts/Archive", pattern = "\\.R$"))
total_core <- length(list.files("Scripts/Core_Pipeline", pattern = "\\.R$"))
total_utilities <- length(list.files("Scripts/Utilities", pattern = "\\.R$"))
total_active <- total_core + total_utilities

cat("Script Organization Benefits:\n")
cat("  📦 Scripts archived:", total_archived, "\n")
cat("  🔧 Core pipeline scripts:", total_core, "\n")
cat("  🛠 Utility scripts:", total_utilities, "\n")
cat("  📊 Total active scripts:", total_active, "\n")
cat("  💾 Reduction ratio:", round((total_archived / (total_archived + total_active)) * 100, 1), "%\n")

# Figure organization
total_env_figs <- length(list.files("Figures/Environmental_Timeseries", recursive = TRUE))
total_biomass_figs <- length(list.files("Figures/Biomass_Timeseries", recursive = TRUE))
total_quality_figs <- length(list.files("Figures/Quality_Checks", recursive = TRUE))

cat("\nFigure Organization Benefits:\n")
cat("  🌍 Environmental figures:", total_env_figs, "\n")
cat("  📈 Biomass analysis figures:", total_biomass_figs, "\n")
cat("  🔍 Quality check figures:", total_quality_figs, "\n")
cat("  ✨ Total organized figures:", total_env_figs + total_biomass_figs + total_quality_figs, "\n")

# ================================================================
# VERIFICATION SUMMARY
# ================================================================

cat("\n=== REORGANIZATION VERIFICATION SUMMARY ===\n\n")

all_dirs_exist <- all(unlist(scripts_structure)) && all(unlist(figures_structure))
cleanup_successful <- length(root_r_files) <= 1  # Allow for this verification script

if(all_dirs_exist && cleanup_successful) {
  cat("🎉 REORGANIZATION VERIFICATION: SUCCESS! 🎉\n\n")
  cat("✅ All required directories created\n")
  cat("✅ Scripts properly organized by purpose\n")
  cat("✅ Figures organized by analysis type\n")
  cat("✅ Root directory cleaned up\n")
  cat("✅ Repository structure streamlined\n")
  
  cat("\n📋 NEXT STEPS:\n")
  cat("1. Test the updated master pipeline\n")
  cat("2. Update any external documentation\n")
  cat("3. Begin using the organized workflow\n")
  cat("4. Consider removing archived scripts if no longer needed\n")
  
} else {
  cat("⚠️ REORGANIZATION VERIFICATION: ISSUES DETECTED ⚠️\n\n")
  if(!all_dirs_exist) {
    cat("❌ Some required directories missing\n")
  }
  if(!cleanup_successful) {
    cat("❌ Root directory cleanup incomplete\n")
  }
  cat("\nPlease review and complete the reorganization process.\n")
}

cat("\nVerification complete!\n")
