# Roo Code App Prompt: ZooMSS_2300 Marine Ecosystem Analysis Enhancement

## Project Context
The ZooMSS_2300 project analyzes marine ecosystem responses to climate change through 2300 using Earth System Model outputs (CESM2-WACCM, IPSL-CM6A-LR, UKESM1-0-LL) across multiple climate scenarios. We have successfully implemented corrected area-weighted biomass analysis and need to enhance the codebase following Claude Opus 4 recommendations.

## Current Achievements ✅
- **Corrected Area Weighting**: Fixed critical methodology flaw ensuring consistent 348.9M km² ocean reference
- **Global Coverage Validation**: Confirmed complete spatial coverage (41,019 ocean locations)
- **Enhanced Analysis Pipeline**: Processed 51,480 time series points across 15 model-scenario combinations
- **Publication-Quality Figures**: Generated 4 comprehensive visualization outputs
- **Memory-Safe Processing**: Adaptive sampling strategies for >40GB datasets

## Implementation Tasks (Priority Order)

### PHASE 1: Documentation & Reproducibility (High Priority)

#### Task 1: Create Scientific Methods Documentation
```r
# Create: METHODS.md
# Document the corrected area weighting methodology
# Include scientific rationale for spatial sampling (15%/8%/2% fractions)
# Explain the critical fix: area calculation before vs after spatial sampling
# Add mathematical formulation of latitude-dependent grid cell areas
```

#### Task 2: Add Comprehensive Logging System
```r
# Enhance: ZooMSS_2300_4h_CorrectedAreaWeighting.R
# Add progress logging with timestamps
# Implement memory usage tracking
# Create processing checkpoints for recovery
# Log validation results and consistency checks
```

#### Task 3: Configuration Management
```r
# Create: config.yml
# Replace hard-coded paths with configurable parameters
# Add model/scenario selection options
# Configure sampling fractions and memory thresholds
# Set output directory structures
```

#### Task 4: Master Pipeline Script
```r
# Create: ZooMSS_2300_MasterPipeline.R
# Single entry point for complete analysis
# Orchestrate: corrected area weighting → validation → visualization
# Include parameter validation and error handling
# Implement reproducible seed setting
```

### PHASE 2: Code Organization & Testing (Medium Priority)

#### Task 5: Function Modularization
```r
# Create: biomass_analysis_utils.R
# Extract common functions from existing scripts:
#   - Area calculation functions
#   - Memory-safe data loading
#   - Spatial sampling utilities
#   - Validation check functions
# Add roxygen2 documentation for all functions
```

#### Task 6: Validation Framework
```r
# Create: tests/
# Unit tests for area calculation accuracy
# Integration tests for pipeline consistency
# Automated range checks for biomass values
# Comparison tests: old vs corrected methodology
```

#### Task 7: Script Consolidation
```r
# Consolidate key scripts:
#   - ZooMSS_2300_4h_CorrectedAreaWeighting.R (primary analysis)
#   - ZooMSS_2300_4g_EnhancedPlotting.R (visualization)
#   - validate_spatial_coverage_simple.R (spatial validation)
# Archive redundant versions
# Establish clear versioning system
```

### PHASE 3: Performance & Visualization (Lower Priority)

#### Task 8: Performance Optimization
```r
# Implement parallel processing for file operations
# Consider data.table for memory efficiency
# Add configurable chunk-based processing
# Profile code to identify bottlenecks
```

#### Task 9: Enhanced Visualizations
```r
# Create interactive plotly visualizations
# Add spatial biomass change maps
# Include uncertainty bands in time series
# Generate automated R Markdown reports
```

## Key Files and Structure

### Current Key Files:
- `ZooMSS_2300_4h_CorrectedAreaWeighting.R`: Corrected area weighting implementation
- `ZooMSS_2300_4g_EnhancedPlotting.R`: Publication-quality visualization
- `Output/combined_corrected_biomass_timeseries.rds`: Main processed dataset (51,480 points)
- `Figures/Biomass_Enhanced/`: Publication-ready figures
- `Figures/Spatial_Coverage_Validation/`: Spatial validation outputs

### Target Enhanced Structure:
```
ZooMSS_2300/
├── config.yml                           # Configuration management
├── ZooMSS_2300_MasterPipeline.R        # Main entry point
├── R/
│   ├── biomass_analysis_utils.R         # Core functions
│   ├── corrected_area_weighting.R       # Enhanced primary analysis
│   ├── enhanced_plotting.R              # Visualization pipeline
│   └── validation_framework.R           # Testing utilities
├── tests/                               # Unit and integration tests
├── docs/
│   ├── METHODS.md                       # Scientific methodology
│   ├── WORKFLOW.md                      # Technical workflow
│   └── API_REFERENCE.md                 # Function documentation
└── logs/                                # Processing logs
```

## Technical Requirements

### R Packages Needed:
```r
# Core analysis
library(tidyverse)
library(raster)
library(ncdf4)

# Enhancement packages
library(config)        # Configuration management
library(logger)        # Comprehensive logging
library(testthat)      # Testing framework
library(roxygen2)      # Documentation
library(plotly)        # Interactive visualizations
library(renv)          # Package management
```

### Key Functions to Preserve:
1. **Corrected area weighting calculation** with consistent ocean grid
2. **Memory-safe adaptive sampling** (15%/8%/2% strategy)
3. **Spatial validation functions** ensuring global coverage
4. **Enhanced visualization pipeline** for publication-quality outputs

## Critical Success Criteria

### Scientific Integrity:
- ✅ Maintain corrected area weighting methodology (348.9M km² reference)
- ✅ Preserve global spatial coverage validation
- ✅ Ensure reproducible biomass projections through 2300

### Technical Robustness:
- 📋 Implement comprehensive error handling and logging
- 📋 Add automated validation at each processing step
- 📋 Ensure memory-safe processing of >40GB datasets
- 📋 Create reproducible configuration management

### User Experience:
- 📋 Provide clear documentation of methods and workflow
- 📋 Implement single-command pipeline execution
- 📋 Add progress monitoring and error reporting
- 📋 Create modular, reusable function library

## Expected Outcomes

### Immediate (Phase 1):
- Comprehensive documentation of corrected methodology
- Robust error handling and logging system
- Reproducible configuration management
- Single-entry pipeline execution

### Medium-term (Phase 2):
- Clean, modular codebase with tested functions
- Automated validation framework
- Consolidated script structure
- Complete API documentation

### Long-term (Phase 3):
- High-performance parallel processing
- Interactive visualization capabilities
- Automated reporting system
- Enhanced scientific analysis features

## Implementation Notes

### Preserve Current Achievements:
- The corrected area weighting methodology is scientifically critical
- Spatial validation confirms global ocean coverage
- Enhanced figures are publication-ready
- Memory-safe processing enables large dataset analysis

### Build Upon Validated Foundation:
- Use existing validated results as test benchmarks
- Maintain backward compatibility with current outputs
- Enhance rather than replace proven methodologies
- Document all changes with scientific rationale

This enhanced ZooMSS_2300 project will provide a robust, reproducible, and user-friendly platform for marine ecosystem analysis under climate change, building upon the scientifically validated foundation we have established.
