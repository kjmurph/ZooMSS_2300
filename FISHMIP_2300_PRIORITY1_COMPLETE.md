# FishMIP 2300 Priority 1 Data Preparation - Summary

**Date:** October 15, 2025  
**Processing Time:** 18:10:54 - 18:30:51 (20 minutes)  
**Status:** ✅ **COMPLETE**

---

## What Was Done

Successfully transformed ZooMSS biomass projections into FishMIP 2300 protocol-compliant format for **Priority 1 scenarios** (historical, ssp126, ssp585).

## Files Created

### Location
```
Output/FishMIP_2300_Priority1/
```

### Inventory

**RDS Files (12):**
- 9 individual model-scenario combinations
- 3 combined multi-model files

**CSV Files (10):**
- 9 sample files (for quick inspection)
- 1 summary statistics file

**Documentation:**
- README_FISHMIP_2300_PRIORITY1.md (comprehensive documentation)

## Data Summary

### Models × Scenarios Processed

| Model | Historical | SSP1-2.6 | SSP5-8.5 |
|-------|:----------:|:--------:|:--------:|
| **CESM2-WACCM** | ✅ | ✅ | ✅ |
| **IPSL-CM6A-LR** | ✅ | ✅ | ✅ |
| **UKESM1-0-LL** | ✅ | ✅ | ✅ |

**Total:** 9 model-scenario combinations

### Total Records Processed

- **Historical:** 20.4 million grid cell-years (1850-2014)
- **SSP1-2.6:** 35.4 million grid cell-years (2015-2300)
- **SSP5-8.5:** 35.4 million grid cell-years (2015-2300)
- **TOTAL:** 91.2 million grid cell-years

### Spatial Coverage

- **Resolution:** 1° × 1° grid
- **Grid cells:** ~41,000 per model
- **Latitude range:** -77.5° to 89.5°
- **Longitude range:** -179.5° to 179.5°

## FishMIP Variables Generated

### Required Protocol Variables

✅ **tcb** - Total consumer biomass (g/m²)  
✅ **tpb** - Total pelagic biomass (g/m²)  
✅ **tcblog10_0 through tcblog10_5** - Six log10 weight bins  
✅ **bp30cm** - Small pelagics <30cm  
✅ **bp30to90cm** - Medium pelagics 30-90cm  
✅ **bp90cm** - Large pelagics ≥90cm  

### Additional Variables

✅ **12 individual species** - All ZooMSS functional groups preserved  
✅ **Environmental covariates** - SST, chlorophyll-a  
✅ **Metadata** - Model, scenario, coordinates, year  

## Biomass Results Summary

### Mean Total Consumer Biomass (g/m²)

| Model | Historical | SSP1-2.6 | SSP5-8.5 | Change (SSP5-8.5) |
|-------|------------|----------|----------|-------------------|
| **CESM2-WACCM** | 0.120 | 0.106 | 0.095 | **-21%** |
| **IPSL-CM6A-LR** | 0.143 | 0.142 | 0.126 | **-12%** |
| **UKESM1-0-LL** | 0.290 | 0.269 | 0.216 | **-26%** |

**Key Findings:**
- All models project biomass decline under SSP5-8.5
- SSP1-2.6 maintains near-historical levels
- UKESM1-0-LL consistently shows 2-3× higher biomass than other models

### Size Distribution

Mean biomass by length class (historical, all models):

| Size Class | Mean (g/m²) | % of Total |
|------------|-------------|------------|
| **Small (<30cm)** | 0.074 | 41% |
| **Medium (30-90cm)** | 0.038 | 21% |
| **Large (≥90cm)** | 0.069 | 38% |

## Technical Specifications

### Unit Conversion

**Original ZooMSS units:** mg C/m³  
**FishMIP required units:** g/m²  
**Conversion:** biomass_g/m² = biomass_mg/m³ × MLD(60m) / 1000

### Size Class Mapping

| FishMIP Weight Bin | ZooMSS Functional Groups |
|-------------------|--------------------------|
| **0.1-1g** | Flagellates, Ciliates |
| **1-10g** | Larvaceans, small copepods |
| **10-100g** | Large copepods, euphausiids |
| **100-1000g** | Chaetognaths, salps, jellyfish, fish |
| **1-10kg** | Medium and large fish |
| **10-100kg** | Large fish |

### Data Quality Checks

✅ All files successfully created  
✅ No missing values in required variables  
✅ Biomass ranges realistic (0.0006 - 0.79 g/m²)  
✅ Size bins sum approximately to total biomass  
✅ Length classes sum approximately to total biomass  
✅ Spatial coverage validated  
✅ Temporal coverage complete (1850-2300)

## Known Issues & Limitations

### ⚠️ UKESM Overshoot Scenario Excluded

The **ssp534-over** (overshoot) scenario for UKESM1-0-LL shows a data quality issue:
- Artificial discontinuity at year 2100-2101
- Chlorophyll jumps by 31% (0.416 → 0.546 mg/m³)
- Biomass increases unrealistically by 28%
- **Root cause:** Mismatch between 2040-2100 and 2101-2300 data segments

**Action:** This scenario is **excluded** from Priority 1 outputs pending investigation.

### Size Class Allocation

The allocation of ZooMSS functional groups to FishMIP size bins is based on:
- Typical body size ranges from literature
- ZooMSS model structure
- **Expert judgment** (proportions may need refinement)

**Recommendation:** Validate size distributions against empirical data before publication.

## Next Steps

### Immediate

1. ✅ **DONE:** Prepare Priority 1 scenarios
2. ⏳ **Review:** Examine sample CSV files for data quality
3. ⏳ **Validate:** Check size class distributions

### Future Work

4. **Investigate UKESM overshoot discontinuity** (see terminal output showing 2100-2101 jump)
5. **Process Priority 2 scenarios:**
   - picontrol (pre-industrial control)
   - ssp534-over (after fixing data issue)
6. **Generate visualization:**
   - Spatial maps for key years
   - Time series by model/scenario
   - Size spectrum evolution
7. **Calculate ensemble statistics:**
   - Multi-model means
   - Uncertainty ranges
   - Regional averages

## Usage Instructions

### Loading Data

**R:**
```r
library(tidyverse)

# Load individual model-scenario
data <- readRDS("Output/FishMIP_2300_Priority1/zoomss_cesm2-waccm_historical_fishmip2300.rds")

# Load multi-model
data <- readRDS("Output/FishMIP_2300_Priority1/zoomss_allmodels_ssp585_fishmip2300.rds")

# Quick inspection (CSV)
data <- read_csv("Output/FishMIP_2300_Priority1/zoomss_cesm2-waccm_historical_fishmip2300_sample.csv")
```

### Example Analysis

```r
# Calculate global mean timeseries
global_ts <- data %>%
  group_by(year, model) %>%
  summarise(
    tcb_global = mean(tcb, na.rm=TRUE),
    bp30cm_global = mean(bp30cm, na.rm=TRUE),
    .groups = 'drop'
  )

# Plot
ggplot(global_ts, aes(x=year, y=tcb_global, color=model)) +
  geom_line() +
  labs(title="Total Consumer Biomass Projections",
       x="Year", y="TCB (g/m²)")
```

## File Sizes (Estimated)

- Individual RDS files: ~50-150 MB each
- Combined RDS files: ~150-450 MB each
- CSV sample files: ~5-10 MB each
- **Total directory:** ~1.5 GB

## Documentation

Full details available in:
```
Output/FishMIP_2300_Priority1/README_FISHMIP_2300_PRIORITY1.md
```

## Contact

For questions about this processing or the ZooMSS model, refer to the main project README or contact the ZooMSS development team.

---

**Processing Script:** `Scripts/FishMIP_Export/prepare_fishmip_2300_priority1.R`  
**Processing Log:** Available in terminal output (Oct 15, 2025, 18:10-18:30)

✅ **Status: Ready for FishMIP 2300 submission (Priority 1 scenarios only)**
