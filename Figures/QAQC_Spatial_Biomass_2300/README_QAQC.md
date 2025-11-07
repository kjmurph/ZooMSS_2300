# QAQC Spatial Biomass Figures - ZooMSS 2300

**Quality Check Run - November 2025**

## Overview

This folder contains spatial biomass projection figures generated during the quality check (QAQC) run of the ZooMSS 2300 pipeline. These figures visualize marine consumer biomass changes from 1990s baseline through to 2300 under different climate scenarios.

## Data Source

- **Input**: Step 3d outputs (`Output/Step3d_ZooMSS_Biomass_Projections_2300/`)
- **Processing**: ZooMSS_2300_4_QAQC_SpatialPlots.R
- **Date Generated**: 2025-11-07

## Earth System Models

All figures include results from three Earth System Models:
1. **CESM2-WACCM** - Community Earth System Model
2. **IPSL-CM6A-LR** - Institut Pierre-Simon Laplace Climate Model
3. **UKESM1-0-LL** - UK Earth System Model

## Climate Scenarios

- **Historical**: 1850-2014 (baseline period 1990-1999)
- **SSP1-2.6**: Low emissions, sustainable development pathway
- **SSP5-3.4-OS**: Overshoot scenario (initialized from SSP5-8.5 2015-2039)
- **SSP5-8.5**: High emissions, fossil-fueled development

## Files Included

### Baseline
- `QAQC_historical_total_biomass_spatial.png`
  - Historical baseline Total Consumer Biomass (TCB) 1990-1999
  - Shows spatial distribution of marine consumer biomass during reference period

### Recent Projections (2090-2099)
- `QAQC_recent_biomass_change_ssp585_spatial.png`
  - TCB percentage change by 2090s under SSP5-8.5
  - Change relative to 1990-1999 historical baseline

### Future Projections (2290-2299)
- `QAQC_future_biomass_change_multimodel_ssp126_spatial.png`
  - Multi-model TCB change by 2290s under SSP1-2.6
  - Faceted by model (3 panels)

- `QAQC_future_biomass_change_multimodel_ssp585_spatial.png`
  - Multi-model TCB change by 2290s under SSP5-8.5
  - Faceted by model (3 panels)

- `QAQC_future_biomass_change_multimodel_ssp534over_spatial.png`
  - Multi-model TCB change by 2290s under SSP5-3.4-Overshoot
  - Faceted by model (3 panels)

### Functional Group Comparison
- `QAQC_zoop_fish_changes_comparison_spatial.png`
  - Zooplankton vs Fish biomass changes by 2090s (SSP5-8.5)
  - Side-by-side comparison of functional group responses

### Summary Statistics
- `QAQC_spatial_summary_statistics.csv`
  - Statistical summaries by model, scenario, and time period
  - Includes: mean, median, SD, and 10th/90th percentiles of TCB change

## Key Findings (Preliminary)

### 2090s Changes (vs 1990-1999 baseline)
- **CESM2-WACCM**: Mixed responses (+0.4% to +8.9% mean change)
- **IPSL-CM6A-LR**: Generally positive (+3.3% to +5.8% mean change)
- **UKESM1-0-LL**: Negative responses (-20.5% to -8.6% mean change)

### 2290s Changes (vs 1990-1999 baseline)
- **SSP1-2.6**: Relatively stable (-6.8% to +3.9% mean change)
- **SSP5-8.5**: Large declines, especially UKESM (-34.6% mean, -53.3% median)
- **SSP5-3.4-OS**: Varied responses (-7.0% to +14.0% mean change)

## Color Scales

- **Biomass**: Viridis (plasma) - purple to yellow
- **Change**: Diverging Red-Blue
  - Red = Decreases (negative change)
  - Blue = Increases (positive change)
  - White = No change

## Technical Notes

1. **Spatial Resolution**: 1° x 1° global ocean grid (~65,000 cells)
2. **Temporal Averaging**: 10-year means for each period
3. **Baseline Period**: 1990-1999 from historical scenario
4. **Overshoot Implementation**: SSP5-3.4-OS initialized from SSP5-8.5 2015-2039 per FishMIP 2300 protocol
5. **Missing Data**: Shown as gray (land masses as dark gray/black)

## Quality Checks Passed

✅ All 15 input files processed successfully
✅ Correct date ranges for each scenario
✅ Proper FishMIP 2300 overshoot initialization
✅ UKESM 2101-2300 segment successfully merged
✅ Multi-model spatial consistency verified

## Next Steps

1. Create biomass timeseries plots
2. Generate multi-model ensemble statistics
3. Produce comparison with FishMIP protocols
4. Validate against ISIMIP3a results

## Contact

For questions about these QAQC outputs, please refer to the main project documentation or the comprehensive project summary.

---
Generated: 2025-11-07  
Pipeline Version: QAQC Branch
