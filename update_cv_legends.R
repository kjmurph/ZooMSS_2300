# Script to regenerate ensemble comparison figure with corrected CV legends
# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)
library(maps)

# Source the main script to get all functions
cat("Loading functions from ensemble analysis script...\n")
source('Scripts/Core_Pipeline/ZooMSS_2300_4k_MultiModelEnsemble.R')

# Check if ensemble data exists and load it
if(file.exists('Output/ensemble_ssp126_future.rds') && file.exists('Output/ensemble_ssp585_future.rds')) {
  cat("Loading ensemble data...\n")
  ssp126_future_ensemble <- readRDS('Output/ensemble_ssp126_future.rds')
  ssp585_future_ensemble <- readRDS('Output/ensemble_ssp585_future.rds')
  
  # Create figure directory if it doesn't exist
  figure_dir <- 'Figures/Spatial_Biomass/'
  if(!dir.exists(figure_dir)) {
    dir.create(figure_dir, recursive = TRUE)
  }
  
  # Recreate the custom ensemble comparison plot
  cat('Creating updated ensemble scenarios future comparison with fixed CV legends...\n')
  custom_future_comparison <- create_custom_ensemble_comparison(ssp126_future_ensemble, ssp585_future_ensemble, '2290s')
  
  # Save the updated figure
  ggsave(paste0(figure_dir, 'ensemble_scenarios_future_comparison.png'),
         custom_future_comparison, width = 18, height = 12, dpi = 300, bg = 'white')
  
  cat('✅ Updated figure saved: ensemble_scenarios_future_comparison.png\n')
  cat('CV legend titles should now be positioned at the top like other legends\n')
  cat('CV legend color scale should also look more consistent\n')
} else {
  cat('❌ Ensemble data files not found. Need to run ensemble analysis first.\n')
  cat('Available files in Output directory:\n')
  print(list.files('Output/', pattern = '*.rds'))
}
