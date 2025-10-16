# UKESM OVERSHOOT 2101-2300 VALIDATION COMPLETE

**Date:** October 15, 2025  
**Analysis:** Environmental Matrix Coverage Validation

---

## EXECUTIVE SUMMARY

✅ **Validation completed successfully for full UKESM overshoot scenario (2101-2300)**

### Coverage Results:
- **99.75% coverage** (50,178 of 50,305 unique SST-Chlorophyll combinations)
- **127 missing combinations** requiring new ZooMSS simulations
- All missing combinations fall within existing matrix bounds (no extreme values)

---

## DATA SUMMARY

### UKESM Overshoot Data (2101-2300):
- **Total records:** 8,272,800 (41,364 locations × 200 years)
- **Unique SST-Chl combinations:** 50,305
- **SST range:** -1.9 to 35.0°C
- **Chlorophyll range:** 0.022 to 10.015 mg/m³
- **Chl_log10 range:** -1.67 to 1.00

### Existing Environmental Matrix:
- **Total combinations:** 98,896
- **SST range:** -2.0 to 41.9°C
- **Chlorophyll range:** 0.0028 to 18.20 mg/m³
- **Chl_log10 range:** -2.56 to 1.26

---

## MISSING COMBINATIONS ANALYSIS

### Characteristics:
- **Count:** 127 combinations (0.25% of UKESM data)
- **SST range:** -1.9 to 31.9°C
- **Chl_log10 range:** 0.03 to 0.91 (Chl: 1.07 to 8.13 mg/m³)

### Distribution:
- Predominantly at **cold temperatures** (SST < 2°C): ~30 combinations
- Mid-range temperatures (SST 0-10°C): ~50 combinations  
- Moderate-high temperatures (SST 10-32°C): ~47 combinations
- All with **moderate chlorophyll** values (Chl_log10: 0.03 to 0.91)

### Sample Missing Combinations:
| SST (°C) | Chl_log10 | Chl (mg/m³) | Region Type |
|----------|-----------|-------------|-------------|
| -1.9     | 0.03      | 1.07        | Cold polar  |
| -1.8     | 0.12      | 1.32        | Cold polar  |
| -1.5     | 0.28      | 1.91        | Cold polar  |
| 0.5      | 0.27      | 1.86        | Cold temp   |
| 1.2      | 0.36      | 2.29        | Cool temp   |
| 10.5     | 0.32      | 2.09        | Warm temp   |
| 20.3     | 0.44      | 2.75        | Subtropical |
| 31.9     | 0.91      | 8.13        | Tropical    |

---

## EXTREME VALUE CHECK

✅ **No extreme values detected**
- No SST values exceed existing matrix maximum (41.9°C)
- No SST values below existing matrix minimum (-2.0°C)  
- No Chl values exceed existing matrix maximum (18.2 mg/m³)
- No Chl values below existing matrix minimum (0.0028 mg/m³)

All missing combinations are **interpolation issues**, not extrapolation.

---

## OUTPUT FILES

### Validation Results:
- **Summary:** `Output/ukesm_overshoot_validation_summary.txt`
- **Full Report (RDS):** `Output/ukesm_overshoot_validation_report.rds`
- **Missing Combinations (CSV):** `Output/ukesm_overshoot_missing_combinations.csv`

### Visualizations:
All figures saved to `Figures/UKESM_Validation/`:
1. `ukesm_overshoot_coverage_analysis.png` - Overlay of UKESM vs existing matrix
2. `ukesm_overshoot_density_comparison.png` - Density comparison plots
3. `ukesm_overshoot_hexbin_comparison.png` - Hexbin density plots
4. `ukesm_overshoot_2101-2300_combined_data.png` - Combined data visualization

---

## NEXT STEPS

### 1. Review Missing Combinations
   - Examine `ukesm_overshoot_missing_combinations.csv`
   - Assess spatial/temporal distribution of missing combinations
   - Determine if gaps are ecologically meaningful

### 2. Decision Point: Run Additional ZooMSS Simulations?

**Option A: Generate predictions for 127 missing combinations**
   - Provides complete coverage
   - Requires ZooMSS simulations for 127 conditions
   - May take additional computational time

**Option B: Proceed with 99.75% coverage**
   - Missing combinations represent 0.25% of data
   - Use nearest-neighbor interpolation for missing values
   - Assess if impact is negligible for analysis goals

### 3. If Running New Simulations:
   - Use `ukesm_overshoot_missing_combinations.csv` as input
   - Run ZooMSS model for each SST-Chl combination
   - Update environmental matrices:
     - `Enviro_Matrix/all_sst_chl_combinations_2300_wPhyto.rds`
     - `Enviro_Matrix/novel_sst_chl_combinations_2300_wPhyto.rds`

### 4. Re-process UKESM Overshoot Scenario:
   - Once matrix is updated, re-run processing pipeline
   - Generate complete biomass predictions for 2101-2300
   - Update `ClimateChange_2300_ukesm1-0-ll_ssp534-over.rds`

---

## TECHNICAL NOTES

### File Path Issues Resolved:
- Initial validation used wrong base directory (`R Projects` vs `GitHub`)
- Loaded old 61-year dataset (2101-2161) instead of full 200-year data
- Fixed by updating `base_dir` path in `validate_ukesm_overshoot_coverage.R`

### Namespace Conflicts Fixed:
- `raster::select()` vs `dplyr::select()` conflicts resolved
- All `select()` calls now use explicit `dplyr::select()`

### Processing Chain Verified:
1. ✅ Surface layer extraction from 3D monthly data (23.78GB → 49.4MB)
2. ✅ Monthly to annual aggregation (2400 → 200 timesteps)
3. ✅ Phyc to chlorophyll conversion (C:Chl = 50)
4. ✅ SST and Chl combination into unified RDS (8.27M records)
5. ✅ Validation against environmental matrix (127 gaps identified)

---

## VALIDATION SCRIPT

**Location:** `Scripts/Utilities/validate_ukesm_overshoot_coverage.R`

**Key Functions:**
- Loads existing environmental matrices
- Loads UKESM overshoot processed data
- Extracts unique SST-Chl combinations
- Identifies missing combinations via anti-join
- Analyzes coverage and extreme values
- Generates visualizations and reports

**Dependencies:**
- tidyverse, raster, viridis, patchwork

---

## CONCLUSION

The UKESM overshoot scenario (2101-2300) has been successfully validated against the existing environmental matrix. The analysis reveals:

✅ **High coverage:** 99.75% of combinations are covered  
⚠️ **Small gaps:** 127 missing combinations (0.25%)  
✅ **No extrapolation:** All missing values fall within existing bounds  
✅ **Quality data:** Full 200-year dataset processed successfully  

**Recommendation:** Review the 127 missing combinations to determine if additional ZooMSS simulations are necessary, or if nearest-neighbor interpolation is acceptable for this small proportion of the data.

---

**Analysis completed:** October 15, 2025  
**Analyst:** ZooMSS_2300 Analysis Team
