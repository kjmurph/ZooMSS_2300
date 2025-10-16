# UKESM Overshoot 2101-2300 Data Processing - Complete

**Date**: October 15, 2025  
**Status**: ✅ COMPLETE

## Overview

Successfully processed the complete UKESM1-0-LL SSP5-3.4-overshoot scenario data extending from 2101 to 2300, filling the gap from the previous incomplete data that only extended to 2040.

## Files Processed

### Input Files (Raw Monthly Data)
1. **Phytoplankton Carbon (3D with depth)**
   - File: `ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc_60arcmin_global_monthly_2101_2300.nc`
   - Size: 23.78 GB
   - Structure: 360×180×75×2400 (lon × lat × depth × time)
   - Time: 2400 monthly time steps (2101-2300)

2. **Sea Surface Temperature**
   - File: `ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_monthly_2101_2300.nc`
   - Size: 332 MB
   - Structure: 360×180×2400 (lon × lat × time)
   - Time: 2400 monthly time steps (2101-2300)

### Output Files (Processed Annual Data)

1. **Phytoplankton Carbon - Surface Layer, Annual Mean**
   - File: `ukesm1-0-ll_r4i1p1f2_ssp534-over_phyc-top_60arcmin_global_annual_2101_2300.nc`
   - Size: 49.4 MB
   - Structure: 360×180×200 (lon × lat × time)
   - Time: 200 annual means (2101-2300)
   - Processing: Extracted top depth layer, aggregated monthly→annual

2. **Sea Surface Temperature - Annual Mean**
   - File: `ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_annual_2101_2300.nc`
   - Size: 49.4 MB
   - Structure: 360×180×200 (lon × lat × time)
   - Time: 200 annual means (2101-2300)
   - Processing: Aggregated monthly→annual

3. **Combined RDS File (Pipeline-Ready)**
   - File: `2300_ukesm1-0-ll_ssp534-over_2101-2300.rds`
   - Location: `Input/2300_processed/`
   - Size: 51.5 MB
   - Records: 8,272,800
   - Years: 2101-2300 (200 years)
   - Spatial Coverage: 41,364 unique ocean locations per year

## Processing Steps Completed

### 1. Inspection (`inspect_and_extract_ukesm_overshoot.R`)
- ✅ Identified 3D structure with 75 depth levels
- ✅ Confirmed 2400 monthly time steps
- ✅ Verified data ranges and quality

### 2. Phyc Extraction (`extract_ukesm_overshoot_toplayer.R`)
- ✅ Extracted surface layer (depth index 1) from 75 levels
- ✅ Reduced file size from 23.78 GB → 49.4 MB (99.8% reduction)
- ✅ Aggregated 2400 monthly steps → 200 annual means
- ✅ Converted phyc (mol/m³) → chlorophyll-a (mg/m³) using C:Chl ratio of 50

### 3. SST Processing (`process_ukesm_overshoot_sst.R`)
- ✅ Aggregated 2400 monthly steps → 200 annual means
- ✅ Converted from Kelvin to Celsius where needed
- ✅ Matched spatial-temporal structure with phyc data

### 4. Data Combination (`combine_ukesm_overshoot_data.R`)
- ✅ Combined SST + Chlorophyll into unified dataset
- ✅ Converted to pipeline-compatible RDS format
- ✅ Added Model/Experiment metadata
- ✅ Calculated log10(Chlorophyll) for ZooMSS compatibility

### 5. File Naming Standardization
- ✅ Renamed files to match existing convention
- ✅ Removed confusing "monthly" from annual file names
- ✅ Aligned with pattern: `*_60arcmin_global_annual_YYYY_YYYY.nc`

## Data Quality Summary

### Temperature (SST)
- Range: -1.9°C to 35.0°C
- Mean across years: 15.04°C
- ✅ Within expected ocean temperature ranges

### Chlorophyll-a
- Range: 0.022 to 10.015 mg/m³
- log10(Chl) Range: -1.67 to 1.00
- Mean across years: 0.561 mg/m³
- ✅ Within expected phytoplankton concentration ranges

### Spatial Coverage
- Grid: 360×180 (1° resolution)
- Ocean locations: 41,364 per year
- Consistent across all 200 years
- ✅ Complete global ocean coverage

## Scripts Created

All scripts located in: `Scripts/Utilities/`

1. **inspect_and_extract_ukesm_overshoot.R**
   - Inspects NetCDF structure without loading full data
   - Generates detailed inspection report

2. **extract_ukesm_overshoot_toplayer.R**
   - Extracts surface layer from 3D data
   - Aggregates monthly → annual
   - Converts phyc → chlorophyll-a

3. **process_ukesm_overshoot_sst.R**
   - Processes SST from monthly → annual
   - Handles temperature unit conversion

4. **combine_ukesm_overshoot_data.R**
   - Combines SST + Chl into unified RDS
   - Formats for pipeline compatibility
   - Generates validation visualizations

5. **validate_ukesm_overshoot_coverage.R**
   - Validates against environmental matrix
   - Identifies missing SST-Chl combinations
   - Generates coverage analysis

## Next Steps

### Immediate (Ready to Run)
1. **Run Environmental Matrix Validation**
   ```r
   Rscript Scripts/Utilities/validate_ukesm_overshoot_coverage.R
   ```
   - Check if existing environmental matrix covers all SST-Chl combinations
   - Identify any novel combinations requiring new ZooMSS simulations

### If Gaps Found
2. **Generate Missing ZooMSS Predictions**
   - Run ZooMSS for missing environmental conditions
   - Update environmental matrices

3. **Merge with Existing UKESM Overshoot Data (2040-2100)**
   - Combine 2040-2100 data with 2101-2300 data
   - Create complete 2040-2300 time series

### Integration
4. **Update Pipeline Processing**
   - Re-run biomass projection analysis with complete data
   - Update area-weighted aggregations
   - Regenerate figures and analyses

## Technical Notes

### Memory Management
- Processing done in yearly chunks to manage memory
- 23.78 GB input processed successfully
- Peak memory usage kept manageable through streaming

### Conversion Factors
- **Phyc → Chlorophyll**: C:Chl ratio = 50 (g:g)
  - Formula: `Chl [mg/m³] = phyc [mol/m³] × 12 [g C/mol] / 50 [g C/g Chl] × 1000 [mg/g]`
- **Temperature**: Converted from Kelvin to Celsius where needed

### File Naming Convention
- Pattern: `{model}_{realization}_{scenario}_{variable}_{resolution}_{domain}_{temporal}_{start}_{end}.nc`
- Example: `ukesm1-0-ll_r4i1p1f2_ssp534-over_tos_60arcmin_global_annual_2101_2300.nc`

## Validation Checklist

- [x] Input files located and readable
- [x] 3D structure correctly identified
- [x] Surface layer extracted successfully
- [x] Monthly→annual aggregation completed
- [x] Phyc→Chlorophyll conversion applied
- [x] SST and Chl dimensions match
- [x] Data ranges are reasonable
- [x] No excessive NA values
- [x] Spatial coverage complete
- [x] Temporal coverage complete (2101-2300)
- [x] Files renamed to standard convention
- [x] RDS file created for pipeline
- [ ] Environmental matrix coverage validated
- [ ] Missing combinations identified (if any)
- [ ] Integration with 2040-2100 data

## Summary

Successfully processed 23.78 GB of raw monthly 3D data into pipeline-ready annual surface data, extending UKESM overshoot scenario from 2040 to 2300. Data quality checks passed, file naming standardized, and ready for environmental matrix validation.

**Total Processing Time**: ~15 minutes
**Data Reduction**: 23.78 GB → 51.5 MB (RDS) + 98.8 MB (NetCDF)
**Status**: ✅ Ready for validation and integration

---

*Generated: October 15, 2025*
*Project: ZooMSS_2300*
*Branch: Dev*
