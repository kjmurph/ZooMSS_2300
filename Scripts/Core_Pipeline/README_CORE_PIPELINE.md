# ZooMSS 2300 Core Pipeline

## Overview
This directory contains the core processing pipeline for generating ZooMSS biomass projections to year 2300 using CMIP6 climate model outputs. The pipeline processes three Earth System Models (ESMs) - CESM2-WACCM, IPSL-CM6A-LR, and UKESM1-0-LL - across five scenarios: historical, picontrol, SSP1-2.6, SSP5-3.4-OS (overshoot), and SSP5-8.5.

## Pipeline Structure

### Step 0: Preprocessing
**Purpose**: Convert and prepare raw climate data files

- **`ZooMSS_2300_0_PreprocessUKESM_monthly.R`**: Converts UKESM monthly data to annual means
- **`ZooMSS_2300_0a_ConvertPhycToChl_clean.R`**: Converts phytoplankton carbon (phyc) to chlorophyll concentration 
  - Input: `Input/raw/phyc/` NetCDF files
  - Output: `Input/converted/chl/` NetCDF files
  - Note: Required because some models provide phyc instead of chl

### Step 1: Quality Control & Visualization
**Purpose**: Verify input data quality and coverage

- **`ZooMSS_2300_1a_PlotGlobalTimeseriesTemp.R`**: Global mean SST timeseries by model and scenario
- **`ZooMSS_2300_1b_PlotGlobalTimeseriesChl.R`**: Global mean chlorophyll timeseries by model and scenario  
- **`ZooMSS_2300_1c_PlotSSTChlDistribution.R`**: Spatial distribution of SST-Chl combinations
  - Identifies novel combinations outside current climate envelope
  - Creates plots showing all vs novel environmental states

### Step 2: Environmental Matrix & Input Preparation
**Purpose**: Create KNN lookup matrices and prepare model inputs

- **`ZooMSS_2300_2a_CreateEnviroMatrix.R`**: Build environmental state matrices
  - Loads ZooMSS control model trained on contemporary climate
  - Extracts SST-Chl combinations from model training data
  - Creates reference matrix for KNN matching
  - Output: `Enviro_Matrix/enviro_CMIP_Matrix_wPhyto.RDS`

- **`ZooMSS_2300_2de_ProcessIndividualFiles.R`**: Process each model-scenario combination
  - Reads SST and Chl NetCDF files
  - Combines into unified dataframes
  - Splits by model (cesm2-waccm, ipsl-cm6a-lr, ukesm1-0-ll)
  - Output: `Input/2300_processed/*.rds` (one file per model-scenario)

### Step 3: Biomass Projection
**Purpose**: Generate ZooMSS biomass projections using KNN matching

- **`ZooMSS_2300_3d_Experiments_Updated.R`**: **MAIN PROCESSING SCRIPT**
  - **Functionality**:
    - Loads ZooMSS control model (12 functional groups, 191 weight classes)
    - For each grid cell-year:
      1. Finds nearest neighbor in environmental matrix using KNN (k=1)
      2. Retrieves corresponding biomass from control model
      3. Applies FishMIP protocol calculations
    - Special handling for SSP5-3.4-OS (overshoot):
      - Uses only 2040-2300 data (no SSP5-8.5 prepending)
      - Ensures proper temporal continuity
  
  - **FishMIP Protocol Variables**:
    - `tcb`: Total consumer biomass (g m⁻²)
    - `tcblog10_0` to `tcblog10_5`: Biomass in log10 weight bins (0.1-1g, 1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg)
    - `tpb`: Total pelagic biomass (g m⁻²)
    - `bp30cm`: Biomass < 30cm body length (g m⁻²)
    - `bp30to90cm`: Biomass 30-90cm body length (g m⁻²)
    - `bp90cm`: Biomass > 90cm body length (g m⁻²)
  
  - **FishMIP Constants**:
    - Mixed Layer Depth: 60m (for m⁻³ to m⁻² conversion)
    - Weight-to-Length: W(g) = 0.01 × L(cm)³
    - Size thresholds: 30cm = 270g, 90cm = 7,290g
  
  - **Weight Classes**: Uses all 191 classes (1×10⁻¹²g to 10,000kg)
    - Previous version used 158 classes (max 5kg)
    - Extended to populate FishMIP bins up to >100kg
  
  - **Outputs** (with gzip compression):
    - Standard: `Output/Step3d_ZooMSS_Biomass_Projections_2300/ZooMSS_BiomassProjection_2300_[model]_[scenario].rds`
    - FishMIP: `Output/Step3d_FishMIP_Format/ZooMSS_FishMIP_2300_[model]_[scenario].rds`
  
  - **Processing Time**: ~110s per historical, ~360s per picontrol, ~185s per SSP scenario
  - **File Sizes**: Standard ~43.9GB total, FishMIP ~47GB total (all 15 scenarios)

### Step 4: Analysis & Visualization
**Purpose**: Create ensemble analyses and visualizations

#### Ensemble Analysis
- **`ZooMSS_2300_4k_MultiModelEnsemble.R`**: Multi-model spatial ensemble
  - Calculates ensemble mean, min, max across 3 ESMs
  - Creates spatial maps for key time periods (1990s, 2050s, 2090s, 2290s)
  - Generates difference maps (future - baseline)
  - Output: `Figures/FishMIP_2300_CORRECTED/ensemble_spatial_*.png`

- **`ZooMSS_2300_4m_MultiModelMeanBiomass.R`**: Multi-model timeseries
  - Global mean biomass trends by scenario
  - Ensemble mean with min-max range shading
  - Separate panels by functional group
  - Output: `Figures/FishMIP_2300_CORRECTED/ensemble_timeseries_*.png`

#### FishMIP Protocol Outputs
- **`ZooMSS_2300_4n_FishMIP_Plots.R`**: FishMIP variable visualization
  - Memory-efficient iterative processing (loads one file at a time)
  - Calculates spatial means immediately to reduce memory footprint
  - Creates timeseries plots:
    - Total Consumer Biomass (TCB) by scenario and model
    - Log10 size bins (6 bins) by scenario and model
    - Length-based bins (3 bins) by scenario and model
  - Percentage change from 1990s baseline
  - Output: `Figures/FishMIP_Outputs/FishMIP_*_timeseries.png`
  - Note: Processes all 15 scenarios including picontrol

- **`ZooMSS_2300_4o_FishMIP_NetCDF_Export.R`**: ISIMIP NetCDF export
  - Converts RDS files to CF-compliant NetCDF format
  - **ISIMIP File Naming**: `model_climate-forcing_scenario_soc_variable_global_timestep_start_end.nc`
    - Example: `zoomss_cesm2-waccm_historical_nat_tcb_global_annual_1850_2014.nc`
  - **NetCDF Specifications**:
    - Format: NETCDF4_CLASSIC
    - Compression: Level 5
    - Dimensions: lon (360), lat (168), time (varies by scenario)
    - Calendar: 365-day (noleap)
    - CF Conventions: 1.6
  - **Time Axis** (days since reference):
    - Historical: 1850-01-01
    - Picontrol: 1601-01-01
    - SSP1-2.6 & SSP5-8.5: 2015-01-01
    - SSP5-3.4-OS: 2040-01-01 (overshoot starts 2040)
  - **Output**: 165 NetCDF files (15 scenarios × 11 variables)
  - **Location**: `Output/FishMIP_NetCDF/`
  - **Target Upload**: ISIMIP server `/work/bb0820/scratch/FishMIP_2300_outputs`

#### Additional Visualization Scripts
- **`ZooMSS_2300_4i_SpatialPlotting.R`**: Individual model spatial maps
- **`ZooMSS_2300_4l_SeparateBiomassPlots.R`**: Functional group breakdowns
- **`ZooMSS_2300_4_QAQC_SpatialPlots.R`**: Quality assurance plots

## Spatial Coverage

**Grid Resolution**: 1° × 1°
- **Longitude**: 360 points (-179.5° to 179.5°)
- **Latitude**: 168 points (-77.5° to 89.5°)
  - Note: Excludes extreme polar regions (< -77.5° and > 89.5°)
  - This is the native resolution of the climate model ocean grids
  - Polar regions may not be fully resolved due to ice cover

## Model-Scenario Matrix

| Model | Historical | Picontrol | SSP1-2.6 | SSP5-3.4-OS | SSP5-8.5 |
|-------|-----------|-----------|----------|-------------|----------|
| CESM2-WACCM | 1850-2014 | 1601-2099 | 2015-2100 | 2040-2300 | 2015-2100 |
| IPSL-CM6A-LR | 1850-2014 | 1601-2099 | 2015-2100 | 2040-2300 | 2015-2100 |
| UKESM1-0-LL | 1850-2014 | 1601-2099 | 2015-2100 | 2040-2300 | 2015-2100 |

**Total Scenarios**: 15 (3 models × 5 scenarios)

## Key Protocol Updates

### SSP5-3.4-OS (Overshoot) Handling
- **Previous approach**: Prepended SSP5-8.5 for 2015-2039, used overshoot 2040-2100
- **Current approach**: Uses overshoot scenario 2040-2300 ONLY
  - No prepending from SSP5-8.5
  - Direct KNN matching for entire period
  - Ensures proper scenario independence

### Weight Class Extension
- **Previous**: 158 classes (max 5kg)
- **Current**: 191 classes (max 10,000kg)
- **Reason**: FishMIP protocol includes size bins up to >100kg
- **Impact**: Proper population of all FishMIP log10 bins

### Weight-to-Length Conversion
- **Formula**: W(g) = 0.01 × L(cm)³
- **Thresholds**: 
  - 30cm = 270g (not rounded from 251g)
  - 90cm = 7,290g (not rounded from 7,943g)
- **Method**: Direct calculation, no log10 rounding

## Data Flow Diagram

```
Raw NetCDF Files (SST, Chl)
    ↓
Step 0: Preprocessing (phyc→chl conversion if needed)
    ↓
Step 1: Quality Control Plots
    ↓
Step 2a: Environmental Matrix Creation
Step 2de: Process Individual Files → Combined RDS per model-scenario
    ↓
Step 3d: KNN Matching & Biomass Projection
    ├→ Standard Format (12 functional groups)
    └→ FishMIP Format (11 protocol variables)
    ↓
Step 4k,4m: Multi-model Ensemble Analysis
Step 4n: FishMIP Visualizations
Step 4o: NetCDF Export for ISIMIP
```

## Output Directories

```
Output/
├── Step3d_ZooMSS_Biomass_Projections_2300/  # Standard biomass outputs (15 files, ~43.9GB)
├── Step3d_FishMIP_Format/                    # FishMIP protocol outputs (15 files, ~47GB)
└── FishMIP_NetCDF/                           # NetCDF exports (165 files, ~2.8GB)

Figures/
├── FishMIP_2300_CORRECTED/                   # Ensemble spatial & timeseries plots
├── FishMIP_Outputs/                          # FishMIP protocol visualizations
├── Environmental_Timeseries/                  # SST & Chl timeseries
└── ZooMSS_CC_Figures/                        # Distribution analyses

Enviro_Matrix/
└── enviro_CMIP_Matrix_wPhyto.RDS             # Reference environmental matrix
```

## Dependencies

### R Packages
- **Core**: tidyverse (dplyr, ggplot2, purrr, readr, tidyr)
- **Spatial**: raster, ncdf4
- **Machine Learning**: yaImpute (KNN matching)
- **Visualization**: viridis, patchwork, RColorBrewer

### External Data
- **ZooMSS Control Model**: `Input/ZooMSS_enviro_matrix_results/ClimateChange/model_Control.RDS`
- **Climate Inputs**: CMIP6 NetCDF files for SST and chlorophyll

## Computational Requirements

- **RAM**: Minimum 16GB, recommended 32GB for NetCDF export
- **Storage**: ~200GB for all outputs
- **Processing Time**: ~2-3 hours for full pipeline (all 15 scenarios)

## Quality Assurance

1. **Overshoot Verification**: Check that SSP5-3.4-OS starts at 2040 with no SSP5-8.5 prepending
2. **Weight Class Range**: Verify 191 classes used (not 158)
3. **Spatial Coverage**: Confirm 360×168 grid (60,480 cells)
4. **Temporal Continuity**: No gaps in timeseries
5. **FishMIP Variables**: All 11 variables calculated correctly with proper units
6. **NetCDF Compliance**: CF-1.6 conventions, proper compression, correct calendar

## Recent Updates (November 2025)

1. **Overshoot Scenario Fix**: Removed SSP5-8.5 prepending, use 2040-2300 only
2. **FishMIP Protocol Implementation**: Added 11 ISIMIP-compliant variables
3. **Weight Class Extension**: Increased from 158 to 191 classes
4. **Compression**: Added gzip compression to all RDS outputs
5. **Picontrol Processing**: Re-enabled picontrol scenario processing
6. **Label Correction**: Fixed scenario labels in FishMIP plots (SSP5-8.5 vs SSP5-3.4-OS)
7. **Memory Optimization**: Implemented iterative file processing for large datasets

## Citation

When using these outputs, please cite:
- ZooMSS model: [Original ZooMSS citation]
- CMIP6 data: Individual model citations (CESM2-WACCM, IPSL-CM6A-LR, UKESM1-0-LL)
- FishMIP protocol: [FishMIP protocol paper]

## Contact

For questions about this pipeline, contact: [Your contact information]

---
Last Updated: November 7, 2025
Pipeline Version: 2.0 (FishMIP 2300 Protocol)
