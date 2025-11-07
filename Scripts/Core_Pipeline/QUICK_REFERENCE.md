# ZooMSS 2300 Pipeline - Quick Reference

## Active Scripts (In Order)

### 🔧 Step 0: Preprocessing
```r
ZooMSS_2300_0_PreprocessUKESM_monthly.R      # Convert UKESM monthly → annual
ZooMSS_2300_0a_ConvertPhycToChl_clean.R      # Convert phyc → chl (if needed)
```

### 📊 Step 1: Quality Control
```r
ZooMSS_2300_1a_PlotGlobalTimeseriesTemp.R    # Global SST timeseries
ZooMSS_2300_1b_PlotGlobalTimeseriesChl.R     # Global Chl timeseries
ZooMSS_2300_1c_PlotSSTChlDistribution.R      # Novel climate states
```

### 🗂️ Step 2: Input Preparation
```r
ZooMSS_2300_2a_CreateEnviroMatrix.R          # Create KNN reference matrix
ZooMSS_2300_2de_ProcessIndividualFiles.R     # Process all model-scenario files
```

### 🎯 Step 3: Biomass Projection (MAIN)
```r
ZooMSS_2300_3d_Experiments_Updated.R         # ⭐ CORE PROCESSING
                                             # - KNN matching
                                             # - Standard biomass outputs
                                             # - FishMIP protocol outputs
```

### 📈 Step 4: Visualization & Export
```r
# Ensemble Analysis
ZooMSS_2300_4k_MultiModelEnsemble.R          # Spatial ensemble maps
ZooMSS_2300_4m_MultiModelMeanBiomass.R       # Timeseries ensemble

# FishMIP Protocol
ZooMSS_2300_4n_FishMIP_Plots.R               # FishMIP visualizations
ZooMSS_2300_4o_FishMIP_NetCDF_Export.R       # NetCDF export for ISIMIP

# Additional Plots (optional)
ZooMSS_2300_4i_SpatialPlotting.R             # Individual model maps
ZooMSS_2300_4l_SeparateBiomassPlots.R        # Functional group details
ZooMSS_2300_4_QAQC_SpatialPlots.R            # QA/QC checks
```

### 🔄 Master Pipeline
```r
ZooMSS_2300_MasterPipeline_Reorganized.R     # Integrated workflow
```

---

## Typical Workflow

### Full Pipeline (From Scratch)
```r
# 1. Preprocessing (if needed)
source("Scripts/Core_Pipeline/ZooMSS_2300_0a_ConvertPhycToChl_clean.R")

# 2. Create environmental matrix (once)
source("Scripts/Core_Pipeline/ZooMSS_2300_2a_CreateEnviroMatrix.R")

# 3. Process input files
source("Scripts/Core_Pipeline/ZooMSS_2300_2de_ProcessIndividualFiles.R")

# 4. Generate projections (⭐ MAIN STEP)
source("Scripts/Core_Pipeline/ZooMSS_2300_3d_Experiments_Updated.R")

# 5. Create visualizations
source("Scripts/Core_Pipeline/ZooMSS_2300_4k_MultiModelEnsemble.R")
source("Scripts/Core_Pipeline/ZooMSS_2300_4m_MultiModelMeanBiomass.R")
source("Scripts/Core_Pipeline/ZooMSS_2300_4n_FishMIP_Plots.R")

# 6. Export to NetCDF
source("Scripts/Core_Pipeline/ZooMSS_2300_4o_FishMIP_NetCDF_Export.R")
```

### Quick Analysis (Outputs Already Generated)
```r
# Just create new visualizations
source("Scripts/Core_Pipeline/ZooMSS_2300_4n_FishMIP_Plots.R")
source("Scripts/Core_Pipeline/ZooMSS_2300_4o_FishMIP_NetCDF_Export.R")
```

---

## Key Outputs

| Step | Output Location | Description |
|------|----------------|-------------|
| 3d | `Output/Step3d_ZooMSS_Biomass_Projections_2300/` | Standard biomass (15 files, ~44GB) |
| 3d | `Output/Step3d_FishMIP_Format/` | FishMIP protocol (15 files, ~47GB) |
| 4k | `Figures/FishMIP_2300_CORRECTED/ensemble_spatial_*.png` | Ensemble spatial maps |
| 4m | `Figures/FishMIP_2300_CORRECTED/ensemble_timeseries_*.png` | Ensemble timeseries |
| 4n | `Figures/FishMIP_Outputs/FishMIP_*_timeseries.png` | FishMIP visualizations |
| 4o | `Output/FishMIP_NetCDF/*.nc` | ISIMIP NetCDF files (165 files) |

---

## Processing Time Estimates

| Step | Time | Notes |
|------|------|-------|
| 2de | ~30 min | Process all 15 scenarios |
| 3d | ~2-3 hrs | Main KNN matching (all scenarios) |
| 4k | ~10 min | Ensemble spatial |
| 4m | ~5 min | Ensemble timeseries |
| 4n | ~2 min | FishMIP plots |
| 4o | ~30 min | NetCDF export (165 files) |

**Total**: ~3-4 hours for complete pipeline

---

## Troubleshooting

### Memory Issues
- **Step 3d**: Uses ~8-12GB RAM per scenario
- **Step 4n**: Process one model at a time if needed
- **Step 4o**: May need 16GB+ for large files

### Common Errors
1. **"File not found"**: Check that Step 2de completed successfully
2. **"KNN matching failed"**: Verify environmental matrix exists
3. **"NetCDF creation error"**: Ensure ncdf4 package installed
4. **Script crashes**: Check available RAM and disk space

---

Last Updated: November 7, 2025
