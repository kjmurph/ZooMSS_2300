# ZooMSS 2300 Repository - REORGANIZED Structure

## Overview
This repository contains the ZooMSS (Zooplankton Model of Size Structure) 2300 project, which models marine ecosystem changes through the year 2300 using three Earth System Models (ESMs): CESM2-WACCM, IPSL-CM6A-LR, and UKESM1-0-LL.

**Repository Status: ✅ REORGANIZED** (August 2025)
- 134 scripts reduced to 19 core scripts + utilities
- Figures organized by analysis type
- Clear 6-stage workflow pipeline
- Redundant files archived

## 📁 Directory Structure

```
ZooMSS_2300/
├── Scripts/                           # All R scripts organized by purpose
│   ├── Core_Pipeline/                 # Main analysis workflow (15 scripts)
│   ├── Utilities/                     # Helper functions (4 scripts)
│   └── Archive/                       # Development/diagnostic scripts (35 scripts)
├── Figures/                           # All visualizations organized by type
│   ├── Environmental_Timeseries/      # Temperature & chlorophyll plots (17 files)
│   ├── Biomass_Timeseries/           # Marine ecosystem timeseries
│   │   ├── Individual_Models/         # Per-ESM biomass plots (3 files)
│   │   └── Multi_Model_Ensemble/      # Ensemble statistics (5 files)
│   ├── Spatial_Plots/                # Geographic biomass distributions
│   │   ├── Biomass_Maps/             # Spatial biomass patterns
│   │   └── Change_Maps/              # Spatial change patterns
│   ├── Quality_Checks/               # Validation plots (4 files)
│   └── [Legacy subdirectories preserved]
├── Data/
│   ├── Input/                        # Raw downloaded climate data
│   ├── Enviro_Matrix/               # Processed environmental matrices
│   └── Output/                      # Model results and processed data
├── Documentation/
└── Config/
```

## 🔄 Analysis Workflow

### **STAGE 1: Data Preprocessing**
- `ZooMSS_2300_0a_ConvertPhycToChl.R` - Convert phytoplankton carbon to chlorophyll

### **STAGE 2: Environmental Timeseries Analysis**
- `ZooMSS_2300_1a_PlotGlobalTimeseriesTemp.R` - Global temperature timeseries
- `ZooMSS_2300_1b_PlotGlobalTimeseriesChl.R` - Global chlorophyll timeseries  
- `ZooMSS_2300_1c_PlotSSTChlDistribution.R` - SST-Chlorophyll distributions

### **STAGE 3: Environmental Matrix Creation**
- `ZooMSS_2300_2a_CreateEnviroMatrix.R` - Create environmental forcing matrix
- `ZooMSS_2300_2b_SetupInputs.R` - Setup model input files
- `ZooMSS_2300_2d_CompileInputs.R` - Compile environmental inputs

### **STAGE 4: Model Execution**
- `ZooMSS_2300_2e_SplitModels.R` - Split data by ESM for parallel processing
- `ZooMSS_2300_3d_Experiments.R` - Run ZooMSS experiments

### **STAGE 5: Spatial Analysis**
- `ZooMSS_2300_4h_CorrectedAreaWeighting.R` - Apply area weighting corrections
- `ZooMSS_2300_4i_SpatialPlotting.R` - Generate spatial biomass maps

### **STAGE 6: Biomass Timeseries Analysis**
- `ZooMSS_2300_4k_MultiModelEnsemble.R` - Multi-model ensemble statistics
- `ZooMSS_2300_4l_SeparateBiomassPlots.R` - Individual ESM biomass plots
- `ZooMSS_2300_4m_MultiModelMeanBiomass.R` - Multi-model mean with uncertainty

## 🚀 Running the Analysis

### **Complete Pipeline**
```r
# Run the full reorganized pipeline
source("Scripts/Core_Pipeline/ZooMSS_2300_MasterPipeline_Reorganized.R")
```

### **Individual Stages**
```r
# Run specific analysis stages
setwd("c:/path/to/ZooMSS_2300")
source("Scripts/Core_Pipeline/ZooMSS_2300_4l_SeparateBiomassPlots.R")
source("Scripts/Core_Pipeline/ZooMSS_2300_4m_MultiModelMeanBiomass.R")
```

### **Utilities**
```r
# Load helper functions
source("Scripts/Utilities/setup_packages.R")
source("Scripts/Utilities/fZooMSS_Xtras.R")
```

## 📊 Key Outputs

### **Environmental Analysis**
- **Location**: `Figures/Environmental_Timeseries/`
- Global temperature and chlorophyll projections through 2300
- SST-Chlorophyll distribution analysis across scenarios

### **Biomass Projections**
- **Individual Models**: `Figures/Biomass_Timeseries/Individual_Models/`
  - Separate plots for Zooplankton, Fish, and Total Consumer Biomass
  - Each plot shows three ESM panels with scenario comparisons
  
- **Multi-Model Ensemble**: `Figures/Biomass_Timeseries/Multi_Model_Ensemble/`
  - Ensemble mean with uncertainty bands (IQR and full range)
  - Model agreement analysis and coefficient of variation

### **Spatial Analysis**
- **Location**: `Figures/Spatial_Plots/`
- Geographic patterns of biomass change
- Regional ecosystem responses

### **Quality Validation**
- **Location**: `Figures/Quality_Checks/`
- Area weighting validation
- Spatial coverage verification

## 🔬 Scientific Context

### **Marine Ecosystem Components**
- **Zooplankton**: Flagellates, Ciliates, Larvaceans, Copepods, Euphausiids, Chaetognaths, Salps, Jellyfish
- **Fish**: Small, Medium, and Large size classes
- **Total Consumer Biomass (TCB)**: Combined zooplankton and fish biomass

### **Climate Scenarios**
- **Historical**: 1850-2014 simulation
- **SSP1-2.6**: Low emissions pathway
- **SSP5-8.5**: High emissions pathway  
- **SSP5-3.4-overshoot**: Overshoot scenario

### **Earth System Models**
- **CESM2-WACCM**: Community Earth System Model
- **IPSL-CM6A-LR**: Institut Pierre-Simon Laplace model
- **UKESM1-0-LL**: UK Earth System Model

## 📈 Key Findings

### **Ensemble Projections (2100)**
- **SSP1-2.6**: -5% zooplankton, -6% fish, -6% total consumer biomass
- **SSP5-8.5**: -14% zooplankton, -18% fish, -16% total consumer biomass
- **SSP5-3.4-overshoot**: -8% zooplankton, -9% fish, -9% total consumer biomass

### **Long-term Projections (2300)**
- **SSP1-2.6**: Stabilization with potential recovery (-2% to -3%)
- **SSP5-8.5**: Continued decline (-26% to -36%)
- **SSP5-3.4-overshoot**: Recovery after initial decline (~-1%)

### **Model Agreement**
- **>99% agreement** across scenarios on direction of change
- High confidence in ensemble projections
- Robust uncertainty quantification

## 🗂️ Archive Information

### **Archived Scripts** (`Scripts/Archive/`)
The following script types have been archived:
- **Development versions** (multiple iterations of the same analysis)
- **Diagnostic scripts** (troubleshooting and validation)
- **Redundant analyses** (superseded by consolidated versions)
- **Experimental approaches** (exploratory methods)

These scripts are preserved for reference but are not part of the main workflow.

### **Legacy Figure Directories**
Some existing figure subdirectories are preserved for compatibility:
- `Figures/ZooMSS_CC_Figures/` - Climate change archive
- `Figures/IPSL_Individual/` - IPSL-specific plots
- `Figures/Biomass_Enhanced/` - Legacy enhanced plots (now relocated)

## ⚙️ Configuration

### **System Requirements**
- R 4.0+ with tidyverse, patchwork, ggplot2
- Sufficient memory for large climate datasets (>16GB recommended)
- Windows/Mac/Linux compatible

### **Path Configuration**
Update base paths in scripts as needed:
```r
base_dir <- "c:/your/path/to/ZooMSS_2300"
```

## 📝 Development History

### **Major Reorganization (August 2025)**
- ✅ Streamlined from 134 to 19 core scripts
- ✅ Organized figures by analysis type
- ✅ Created clear 6-stage pipeline
- ✅ Archived redundant development files
- ✅ Improved documentation and workflow clarity

### **Key Analysis Developments**
- Enhanced uncertainty quantification (IQR, CV, model agreement)
- Separate biomass plots by ESM and functional group
- Multi-model ensemble analysis with uncertainty bands
- Corrected area weighting methodology
- Comprehensive spatial coverage validation

## 🤝 Contributing

When adding new analyses:
1. Place core scripts in `Scripts/Core_Pipeline/`
2. Place utilities in `Scripts/Utilities/`
3. Organize outputs in appropriate `Figures/` subdirectories
4. Update the master pipeline as needed
5. Test with the complete workflow

## 📧 Contact

For questions about the ZooMSS 2300 project structure or methodology, please refer to the comprehensive project documentation or analysis scripts.

---

**Repository Status**: ✅ **STREAMLINED AND ORGANIZED**  
**Last Updated**: August 2025  
**Core Scripts**: 19 (down from 134)  
**Figure Organization**: Complete  
**Workflow Documentation**: Complete  
