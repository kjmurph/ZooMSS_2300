# Archived Core Pipeline Scripts

This directory contains deprecated or superseded scripts from the ZooMSS 2300 core pipeline.

## Archived Scripts

### Step 0: Preprocessing
- **`ZooMSS_2300_0a_ConvertPhycToChl.R`** → Superseded by `ZooMSS_2300_0a_ConvertPhycToChl_clean.R`
  - Original version with redundant code
  - Cleaned version is now the active script

### Step 2: Input Preparation
- **`ZooMSS_2300_2b_SetupInputs.R`** → Superseded by `ZooMSS_2300_2b_SetupInputs_Updated.R`
  - Original version before updates
  
- **`ZooMSS_2300_2d_CompileInputs.R`** → Functionality integrated into Step 2de
  - Merged into `ZooMSS_2300_2de_ProcessIndividualFiles.R`
  
- **`ZooMSS_2300_2e_SplitModels.R`** → Functionality integrated into Step 2de
  - Merged into `ZooMSS_2300_2de_ProcessIndividualFiles.R`

### Step 3: Biomass Projection
- **`ZooMSS_2300_3d_Experiments.R`** → Superseded by `ZooMSS_2300_3d_Experiments_Updated.R`
  - Original version before FishMIP protocol implementation
  - Did not include FishMIP format outputs
  - Used only 158 weight classes (not 191)
  
- **`ZooMSS_2300_3d_Reprocess_Overshoot.R`** → Temporary debugging script
  - Created to fix SSP5-3.4-OS initialization issues
  - Functionality incorporated into main Step 3d script

### Step 4: Visualization
- **`ZooMSS_2300_4h_CorrectedAreaWeighting.R`** → Testing script
  - Used to debug area weighting calculations
  - Not part of regular pipeline

### Master Pipeline
- **`ZooMSS_2300_MasterPipeline.R`** → Superseded by `ZooMSS_2300_MasterPipeline_Reorganized.R`
  - Original master script before reorganization
  - Updated version has better structure and documentation

## Reason for Archiving

These scripts were archived on **November 7, 2025** as part of pipeline cleanup to:
1. Remove redundant versions after updates
2. Consolidate functionality into fewer, more maintainable scripts
3. Streamline the pipeline to only essential steps
4. Reduce confusion about which scripts are current

## Recovery

If you need to reference or restore any of these scripts, they are preserved here with full functionality intact. The archive is version-controlled, so historical versions can always be recovered from Git history if needed.

---
Archived: November 7, 2025
