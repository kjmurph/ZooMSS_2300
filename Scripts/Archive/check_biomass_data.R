# Check structure of combined biomass files
print('=== combined_biomass_timeseries.rds ===')
data1 <- readRDS('Output/combined_biomass_timeseries.rds')
print(paste('Class:', class(data1)))
if(is.data.frame(data1)) {
  print(paste('Dimensions:', nrow(data1), 'x', ncol(data1)))
  print('Column names:')
  print(colnames(data1))
  print('First few rows:')
  print(head(data1, 3))
} else {
  print('Data structure:')
  str(data1, max.level = 2)
}

print('=== combined_weighted_biomass_timeseries.rds ===')
data2 <- readRDS('Output/combined_weighted_biomass_timeseries.rds')
print(paste('Class:', class(data2)))
if(is.data.frame(data2)) {
  print(paste('Dimensions:', nrow(data2), 'x', ncol(data2)))
  print('Column names:')
  print(colnames(data2))
  print('First few rows:')
  print(head(data2, 3))
} else {
  print('Data structure:')
  str(data2, max.level = 2)
}

# Also check one of the individual biomass projection files
print('=== Individual biomass projection file ===')
file_path <- 'Output/Biomass_projections/Biomass_ClimateChange_Compiled_withZooMSS_cesm2-waccm_historical_Control.rds'
if(file.exists(file_path)) {
  data3 <- readRDS(file_path)
  print(paste('Class:', class(data3)))
  if(is.data.frame(data3)) {
    print(paste('Dimensions:', nrow(data3), 'x', ncol(data3)))
    print('Column names:')
    print(colnames(data3))
    print('First few rows:')
    print(head(data3, 3))
  } else {
    print('Data structure:')
    str(data3, max.level = 2)
  }
} else {
  print('File not found')
}
