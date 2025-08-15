# Uncertainty Metrics Guide for Marine Biomass Projections

## Overview
This guide explains the different uncertainty metrics used to assess inter-model variability in the ZooMSS marine ecosystem projections through 2300.

## Standard Ensemble Metrics

### 1. Multi-Model Mean
- **Description**: Average change across all 3 Earth System Models
- **Units**: Percentage change relative to 1990-1999 baseline
- **Interpretation**: Central tendency of projected changes
- **Color Scale**: Blue = increases, Red = decreases

### 2. Multi-Model Median  
- **Description**: Middle value across all 3 Earth System Models
- **Units**: Percentage change relative to 1990-1999 baseline
- **Interpretation**: Robust central tendency less affected by extreme values
- **Color Scale**: Blue = increases, Red = decreases

## Enhanced Uncertainty Metrics (More Interpretable than Standard Deviation)

### 3. Inter-Quartile Range (IQR)
- **Description**: Difference between 75th and 25th percentiles of model projections
- **Units**: Percentage points
- **Interpretation**: 
  - Low IQR (< 10%): High model agreement
  - Medium IQR (10-30%): Moderate uncertainty
  - High IQR (30-60%): High uncertainty
  - Very High IQR (> 60%): Very high uncertainty
- **Advantages**: More robust than standard deviation, easier to interpret
- **Color Scale**: Purple gradient (low to high spread)

### 4. Model Agreement (%)
- **Description**: Percentage of models agreeing on direction of change
- **Calculation**: Models with changes > +5% (increase) vs < -5% (decrease)
- **Units**: Percentage (33-100%)
- **Interpretation**:
  - 100%: All models agree on direction
  - 67%: 2 out of 3 models agree
  - 33%: No consensus (equal split)
- **Color Scale**: Viridis (low agreement = dark, high agreement = bright)

### 5. Model Range  
- **Description**: Difference between maximum and minimum model projections
- **Units**: Percentage points
- **Interpretation**: Total spread of model projections
- **Advantages**: Simple, intuitive measure of projection spread
- **Color Scale**: Plasma gradient (low to high range)

### 6. Coefficient of Variation (CV)
- **Description**: Standard deviation divided by mean (normalized variability)
- **Units**: Dimensionless ratio
- **Interpretation**: 
  - CV < 0.5: Low relative variability
  - CV 0.5-1.0: Moderate relative variability  
  - CV > 1.0: High relative variability
- **Advantages**: Accounts for magnitude of changes (normalized uncertainty)
- **Limitations**: Undefined when mean ≈ 0, filtered for extreme values
- **Color Scale**: Inferno gradient

## Which Metric to Use?

### For Scientific Publications:
- **Primary**: IQR (most interpretable and robust)
- **Secondary**: Model Agreement (shows consensus level)

### For Policy Applications:
- **Model Agreement**: Shows where scientists agree/disagree
- **Range**: Shows worst-case vs best-case scenarios

### For Detailed Analysis:
- **Coefficient of Variation**: When you need normalized uncertainty
- **IQR + Range**: Complete picture of variability

## File Outputs

### Individual Ensemble Plots:
- `ensemble_ssp126_future_comparison.png`: SSP1-2.6 2290s (Mean | Median | IQR)
- `ensemble_ssp585_future_comparison.png`: SSP5-8.5 2290s (Mean | Median | IQR)

### Enhanced Uncertainty Plots:
- `uncertainty_ssp126_future.png`: SSP1-2.6 2290s (Agreement | Range | CV)
- `uncertainty_ssp585_future.png`: SSP5-8.5 2290s (Agreement | Range | CV)

### Summary Statistics:
- `ensemble_summary_statistics.csv`: Global-scale numerical summaries

## Technical Notes

- **Baseline**: All changes calculated relative to 1990-1999 historical mean
- **Models**: CESM2-WACCM, IPSL-CM6A-LR, UKESM1-0-LL
- **Grid Cells**: 40,508 marine grid cells with complete data from all models
- **Scenarios**: SSP1-2.6 (low emissions) and SSP5-8.5 (high emissions)
- **Time Periods**: 2090s (recent future) and 2290s (far future)

## Recommendations

1. **Use IQR instead of standard deviation** - more interpretable and robust
2. **Report model agreement** - shows scientific consensus level  
3. **Consider coefficient of variation** - for normalized uncertainty assessment
4. **Show range for context** - demonstrates full spread of projections
5. **Combine metrics** - no single metric captures all aspects of uncertainty
