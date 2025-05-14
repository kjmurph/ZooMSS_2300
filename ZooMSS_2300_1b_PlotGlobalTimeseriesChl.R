library(RNetCDF)
library(raster)
library(ggplot2)


cell_areas <- t(as.matrix(area(raster())*1e6))
plot(raster(cell_areas))

# Load all NetCDF files for chlorophyll (chla)

# CESM2
chla_hist_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/cesm2-waccm_r1i1p1f1_historical_chla-top_60arcmin_global_annual_1850_2014.nc"), "chla")
chla_picontrol_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/cesm2-waccm_r1i1p1f1_picontrol_chla-top_60arcmin_global_annual_1601_2100.nc"), "chla")
chla_ssp126_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/cesm2-waccm_r1i1p1f1_ssp126_chla-top_60arcmin_global_annual_2015_2299.nc"), "chla")
chla_ssp535_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/cesm2-waccm_r1i1p1f1_ssp534-over_chla-top_60arcmin_global_annual_2040_2299.nc"), "chla")
chla_ssp585_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/cesm2-waccm_r1i1p1f1_ssp585_chla-top_60arcmin_global_annual_2015_2299.nc"), "chla")

# IPSL
chla_hist_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ipsl-cm6a-lr_r1i1p1f1_historical_chla-top_60arcmin_global_annual_1850_2014.nc"), "chla")
chla_picontrol_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ipsl-cm6a-lr_r1i1p1f1_picontrol_chla-top_60arcmin_global_annual_1601_2100.nc"), "chla")
chla_ssp126_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ipsl-cm6a-lr_r1i1p1f1_ssp126_chla-top_60arcmin_global_annual_2015_2300.nc"), "chla")
chla_ssp535_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ipsl-cm6a-lr_r1i1p1f1_ssp534-over_chla-top_60arcmin_global_annual_2040_2300.nc"), "chla")
chla_ssp585_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ipsl-cm6a-lr_r1i1p1f1_ssp585_chla-top_60arcmin_global_annual_2015_2300.nc"), "chla")

# UKESM
chla_hist_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ukesm1-0-ll_r1i1p1f2_historical_chla-top_60arcmin_global_annual_1850_2014.nc"), "chla")
chla_picontrol_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ukesm1-0-ll_r1i1p1f2_picontrol_chla-top_60arcmin_global_annual_1601_2100.nc"), "chla")
chla_ssp126_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ukesm1-0-ll_r4i1p1f2_ssp126_chla-top_60arcmin_global_annual_2015_2300.nc"), "chla")
chla_ssp535_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ukesm1-0-ll_r4i1p1f2_ssp534-over_chla-top_60arcmin_global_annual_2040_2100.nc"), "chla")
chla_ssp585_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/chl/ukesm1-0-ll_r4i1p1f2_ssp585_chla-top_60arcmin_global_annual_2015_2300.nc"), "chla")

# Create ocean mask and areas for CESM2
# Cut out land cells from cell area matrix (using chlorophyll data to define ocean cells)
ocean_cells_cesm2 <- chla_ssp126_cesm2[,,1]
ocean_cells_cesm2[!is.na(ocean_cells_cesm2)] <- 1
ocean_areas_cesm2 <- ocean_cells_cesm2 * cell_areas
# Get total area of the ocean
tot_ocean_area_cesm2 <- sum(ocean_areas_cesm2, na.rm = TRUE)

# Create ocean mask and areas for IPSL
# Cut out land cells from cell area matrix
ocean_cells_ipsl <- chla_ssp126_ipsl[,,1]
ocean_cells_ipsl[!is.na(ocean_cells_ipsl)] <- 1
ocean_areas_ipsl <- ocean_cells_ipsl * cell_areas
# Get total area of the ocean
tot_ocean_area_ipsl <- sum(ocean_areas_ipsl, na.rm = TRUE)

# Create ocean mask and areas for UKESM
# Cut out land cells from cell area matrix
ocean_cells_ukesm <- chla_ssp126_ukesm[,,1]
ocean_cells_ukesm[!is.na(ocean_cells_ukesm)] <- 1
ocean_areas_ukesm <- ocean_cells_ukesm * cell_areas
# Get total area of the ocean
tot_ocean_area_ukesm <- sum(ocean_areas_ukesm, na.rm = TRUE)

# plot ocean masks to inspect differences between the models
plot(raster(ocean_areas_ipsl))
plot(raster(ocean_areas_cesm2))
plot(raster(ocean_areas_ukesm))

# Calculate area-weighted means for CESM2
chla_ssp126_cesm2_mean <- apply(sweep(chla_ssp126_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
chla_ssp535_cesm2_mean <- apply(sweep(chla_ssp535_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
chla_ssp585_cesm2_mean <- apply(sweep(chla_ssp585_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
chla_hist_cesm2_mean <- apply(sweep(chla_hist_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
chla_picontrol_cesm2_mean <- apply(sweep(chla_picontrol_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2

# Calculate area-weighted means for IPSL
chla_hist_ipsl_mean <- apply(sweep(chla_hist_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
chla_picontrol_ipsl_mean <- apply(sweep(chla_picontrol_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
chla_ssp126_ipsl_mean <- apply(sweep(chla_ssp126_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
chla_ssp535_ipsl_mean <- apply(sweep(chla_ssp535_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
chla_ssp585_ipsl_mean <- apply(sweep(chla_ssp585_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl

# Calculate area-weighted means for UKESM
chla_hist_ukesm_mean <- apply(sweep(chla_hist_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
chla_picontrol_ukesm_mean <- apply(sweep(chla_picontrol_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
chla_ssp126_ukesm_mean <- apply(sweep(chla_ssp126_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
chla_ssp535_ukesm_mean <- apply(sweep(chla_ssp535_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
chla_ssp585_ukesm_mean <- apply(sweep(chla_ssp585_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm

plot(chla_hist_ipsl_mean, type = "l")
plot(chla_picontrol_ipsl_mean, type = "l")
plot(chla_ssp126_ipsl_mean, type = "l")
plot(chla_ssp585_ipsl_mean, type = "l")

# Check the actual dimensions of your data and create appropriate year vectors
# Historical data: 1850-2014 (165 years)
years_hist <- seq(1850, 2014, by = 1)
print(paste("Length of years_hist:", length(years_hist)))
print(paste("Length of chla_hist_cesm2_mean:", length(chla_hist_cesm2_mean)))
print(paste("Length of chla_hist_ipsl_mean:", length(chla_hist_ipsl_mean)))
print(paste("Length of chla_hist_ukesm_mean:", length(chla_hist_ukesm_mean)))

# PI Control: Check actual length and create appropriate years
print(paste("Length of chla_picontrol_cesm2_mean:", length(chla_picontrol_cesm2_mean)))
print(paste("Length of chla_picontrol_ipsl_mean:", length(chla_picontrol_ipsl_mean)))
print(paste("Length of chla_picontrol_ukesm_mean:", length(chla_picontrol_ukesm_mean)))
# Create years based on actual data length (assuming it starts from 1601)
years_picontrol_cesm2 <- seq(1601, 1601 + length(chla_picontrol_cesm2_mean) - 1, by = 1)
years_picontrol_ipsl <- seq(1601, 1601 + length(chla_picontrol_ipsl_mean) - 1, by = 1)
years_picontrol_ukesm <- seq(1601, 1601 + length(chla_picontrol_ukesm_mean) - 1, by = 1)

# SSP scenarios: Check lengths
print(paste("Length of chla_ssp126_cesm2_mean:", length(chla_ssp126_cesm2_mean)))
print(paste("Length of chla_ssp126_ipsl_mean:", length(chla_ssp126_ipsl_mean)))
print(paste("Length of chla_ssp126_ukesm_mean:", length(chla_ssp126_ukesm_mean)))
print(paste("Length of chla_ssp535_cesm2_mean:", length(chla_ssp535_cesm2_mean)))
print(paste("Length of chla_ssp535_ipsl_mean:", length(chla_ssp535_ipsl_mean)))
print(paste("Length of chla_ssp535_ukesm_mean:", length(chla_ssp535_ukesm_mean)))
# Create years based on actual data length
years_ssp_cesm2 <- seq(2015, 2015 + length(chla_ssp126_cesm2_mean) - 1, by = 1)
years_ssp_ipsl <- seq(2015, 2015 + length(chla_ssp126_ipsl_mean) - 1, by = 1)
years_ssp_ukesm <- seq(2015, 2015 + length(chla_ssp126_ukesm_mean) - 1, by = 1)
# SSP534-over starts from 2040
years_ssp535_cesm2 <- seq(2040, 2040 + length(chla_ssp535_cesm2_mean) - 1, by = 1)
years_ssp535_ipsl <- seq(2040, 2040 + length(chla_ssp535_ipsl_mean) - 1, by = 1)
years_ssp535_ukesm <- seq(2040, 2040 + length(chla_ssp535_ukesm_mean) - 1, by = 1)


# Create comprehensive data frame with all scenarios
data_list <- list(
  # Historical data
  data.frame(Year = years_hist,
             Chlorophyll = chla_hist_cesm2_mean,
             Scenario = "Historical",
             Model = "CESM2"),
  data.frame(Year = years_hist,
             Chlorophyll = chla_hist_ipsl_mean,
             Scenario = "Historical",
             Model = "IPSL"),
  data.frame(Year = years_hist,
             Chlorophyll = chla_hist_ukesm_mean,
             Scenario = "Historical",
             Model = "UKESM"),

  # PI Control data (using model-specific year vectors)
  data.frame(Year = years_picontrol_cesm2,
             Chlorophyll = chla_picontrol_cesm2_mean,
             Scenario = "PI Control",
             Model = "CESM2"),
  data.frame(Year = years_picontrol_ipsl,
             Chlorophyll = chla_picontrol_ipsl_mean,
             Scenario = "PI Control",
             Model = "IPSL"),
  data.frame(Year = years_picontrol_ukesm,
             Chlorophyll = chla_picontrol_ukesm_mean,
             Scenario = "PI Control",
             Model = "UKESM"),

  # SSP scenarios
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll = chla_ssp126_cesm2_mean,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll = chla_ssp585_cesm2_mean,
             Scenario = "SSP585",
             Model = "CESM2"),
  data.frame(Year = years_ssp535_cesm2,
             Chlorophyll = chla_ssp535_cesm2_mean,
             Scenario = "SSP534-over",
             Model = "CESM2"),
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll = chla_ssp126_ipsl_mean,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll = chla_ssp585_ipsl_mean,
             Scenario = "SSP585",
             Model = "IPSL"),
  data.frame(Year = years_ssp535_ipsl,
             Chlorophyll = chla_ssp535_ipsl_mean,
             Scenario = "SSP534-over",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll = chla_ssp126_ukesm_mean,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll = chla_ssp585_ukesm_mean,
             Scenario = "SSP585",
             Model = "UKESM"),
  data.frame(Year = years_ssp535_ukesm,
             Chlorophyll = chla_ssp535_ukesm_mean,
             Scenario = "SSP534-over",
             Model = "UKESM")
)


# Combine all data
combined_data <- do.call(rbind, data_list)

# PLOT 1: All scenarios by model
p1 <- ggplot(combined_data, aes(x = Year, y = Chlorophyll, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_manual(values = c("Historical" = "black",
                                "PI Control" = "gray",
                                "SSP126" = "blue",
                                "SSP585" = "red",
                                "SSP534-over" = "purple")) +
  labs(title = "Global Ocean Chlorophyll by Model and Scenario",
       x = "Year",
       y = "Global Ocean Chlorophyll (mg/m³)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  coord_cartesian(xlim = c(1850, 2300))  # Focus on relevant time period

print(p1)

# PLOT 2: Focus on future scenarios and historical (zoom in)
future_data <- combined_data[combined_data$Scenario %in% c("Historical", "SSP126", "SSP585", "SSP534-over") &
                               combined_data$Year >= 1900, ]

p2 <- ggplot(future_data, aes(x = Year, y = Chlorophyll, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_manual(values = c("Historical" = "black",
                                "SSP126" = "blue",
                                "SSP585" = "red",
                                "SSP534-over" = "purple")) +
  labs(title = "Global Ocean Chlorophyll: Historical and Future Scenarios",
       x = "Year",
       y = "Global Ocean Chlorophyll (mg/m³)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p2)

# PLOT 3: Chlorophyll anomalies relative to historical baseline (30-year period)
# Calculate historical baseline using 30-year period (1985-2014)
# 1985 = index 136 (1985-1850+1), 2014 = index 165 (2014-1850+1)
hist_baseline_start <- 136  # 1985
hist_baseline_end <- 165    # 2014

hist_baseline_cesm2 <- mean(chla_hist_cesm2_mean[hist_baseline_start:hist_baseline_end])
hist_baseline_ipsl <- mean(chla_hist_ipsl_mean[hist_baseline_start:hist_baseline_end])
hist_baseline_ukesm <- mean(chla_hist_ukesm_mean[hist_baseline_start:hist_baseline_end])

print(paste("Historical baseline CESM2 (1985-2014):", round(hist_baseline_cesm2, 6), "mg/m³"))
print(paste("Historical baseline IPSL (1985-2014):", round(hist_baseline_ipsl, 6), "mg/m³"))
print(paste("Historical baseline UKESM (1985-2014):", round(hist_baseline_ukesm, 6), "mg/m³"))

# Create anomaly data using the new baseline
anomaly_data_list <- list(
  # CESM2 anomalies
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll_Anomaly = chla_ssp126_cesm2_mean - hist_baseline_cesm2,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll_Anomaly = chla_ssp585_cesm2_mean - hist_baseline_cesm2,
             Scenario = "SSP585",
             Model = "CESM2"),
  data.frame(Year = years_ssp535_cesm2,
             Chlorophyll_Anomaly = chla_ssp535_cesm2_mean - hist_baseline_cesm2,
             Scenario = "SSP534-over",
             Model = "CESM2"),

  # IPSL anomalies
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll_Anomaly = chla_ssp126_ipsl_mean - hist_baseline_ipsl,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll_Anomaly = chla_ssp585_ipsl_mean - hist_baseline_ipsl,
             Scenario = "SSP585",
             Model = "IPSL"),
  data.frame(Year = years_ssp535_ipsl,
             Chlorophyll_Anomaly = chla_ssp535_ipsl_mean - hist_baseline_ipsl,
             Scenario = "SSP534-over",
             Model = "IPSL"),

  # UKESM anomalies
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll_Anomaly = chla_ssp126_ukesm_mean - hist_baseline_ukesm,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll_Anomaly = chla_ssp585_ukesm_mean - hist_baseline_ukesm,
             Scenario = "SSP585",
             Model = "UKESM"),
  data.frame(Year = years_ssp535_ukesm,
             Chlorophyll_Anomaly = chla_ssp535_ukesm_mean - hist_baseline_ukesm,
             Scenario = "SSP534-over",
             Model = "UKESM")
)

anomaly_data <- do.call(rbind, anomaly_data_list)

p3 <- ggplot(anomaly_data, aes(x = Year, y = Chlorophyll_Anomaly, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_manual(values = c("SSP126" = "blue", "SSP585" = "red", "SSP534-over" = "purple")) +
  labs(title = "Global Ocean Chlorophyll Anomalies Relative to Historical Baseline (1985-2014)",
       x = "Year",
       y = "Chlorophyll Anomaly (mg/m³)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p3)

# PLOT 4: Difference between SSP scenarios and PI Control baseline
# Calculate PI Control baseline (10-year mean around 2014)
# For PI Control, 2014 corresponds to approximately year 414 in the simulation (1601+413)
# We'll use years 409-418 (2000-2018 equivalent) for a 10-year mean
pi_start_idx <- 409
pi_end_idx <- 418

pi_baseline_cesm2 <- mean(chla_picontrol_cesm2_mean[pi_start_idx:pi_end_idx])
pi_baseline_ipsl <- mean(chla_picontrol_ipsl_mean[pi_start_idx:pi_end_idx])
pi_baseline_ukesm <- mean(chla_picontrol_ukesm_mean[pi_start_idx:pi_end_idx])

print(paste("PI Control baseline CESM2 (2009-2018):", round(pi_baseline_cesm2, 6), "mg/m³"))
print(paste("PI Control baseline IPSL (2009-2018):", round(pi_baseline_ipsl, 6), "mg/m³"))
print(paste("PI Control baseline UKESM (2009-2018):", round(pi_baseline_ukesm, 6), "mg/m³"))

# Calculate anomalies relative to PI Control baseline
pi_anomaly_data_list <- list(
  # CESM2 anomalies
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll_Anomaly = chla_ssp126_cesm2_mean - pi_baseline_cesm2,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll_Anomaly = chla_ssp585_cesm2_mean - pi_baseline_cesm2,
             Scenario = "SSP585",
             Model = "CESM2"),
  data.frame(Year = years_ssp535_cesm2,
             Chlorophyll_Anomaly = chla_ssp535_cesm2_mean - pi_baseline_cesm2,
             Scenario = "SSP534-over",
             Model = "CESM2"),

  # IPSL anomalies
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll_Anomaly = chla_ssp126_ipsl_mean - pi_baseline_ipsl,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Chlorophyll_Anomaly = chla_ssp585_ipsl_mean - pi_baseline_ipsl,
             Scenario = "SSP585",
             Model = "IPSL"),
  data.frame(Year = years_ssp535_ipsl,
             Chlorophyll_Anomaly = chla_ssp535_ipsl_mean - pi_baseline_ipsl,
             Scenario = "SSP534-over",
             Model = "IPSL"),

  # UKESM anomalies
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll_Anomaly = chla_ssp126_ukesm_mean - pi_baseline_ukesm,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Chlorophyll_Anomaly = chla_ssp585_ukesm_mean - pi_baseline_ukesm,
             Scenario = "SSP585",
             Model = "UKESM"),
  data.frame(Year = years_ssp535_ukesm,
             Chlorophyll_Anomaly = chla_ssp535_ukesm_mean - pi_baseline_ukesm,
             Scenario = "SSP534-over",
             Model = "UKESM")
)

pi_anomaly_data <- do.call(rbind, pi_anomaly_data_list)

p4 <- ggplot(pi_anomaly_data, aes(x = Year, y = Chlorophyll_Anomaly, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_manual(values = c("SSP126" = "blue", "SSP585" = "red", "SSP534-over" = "purple")) +
  labs(title = "Global Ocean Chlorophyll Anomalies Relative to PI Control Baseline",
       x = "Year",
       y = "Chlorophyll Anomaly (mg/m³)",
       subtitle = "Baseline: PI Control 10-year mean (2009-2018 equivalent)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p4)

# PLOT 5: Combined plot showing all scenarios with different line types
combined_plot_data <- combined_data[combined_data$Scenario %in% c("Historical", "SSP126", "SSP585", "SSP534-over") &
                                      combined_data$Year >= 1900, ]

p5 <- ggplot(combined_plot_data, aes(x = Year, y = Chlorophyll,
                                     color = Scenario, linetype = Model)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Historical" = "black",
                                "SSP126" = "blue",
                                "SSP585" = "red",
                                "SSP534-over" = "purple")) +
  scale_linetype_manual(values = c("CESM2" = "solid", "IPSL" = "dashed", "UKESM" = "dotted")) +
  labs(title = "Global Ocean Chlorophyll: All Scenarios and Models",
       x = "Year",
       y = "Global Ocean Chlorophyll (mg/m³)",
       color = "Scenario",
       linetype = "Model") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p5)

# PLOT 6: Difference between SSP scenarios for each model
# Calculate differences (SSP585 - SSP126)
diff_cesm2 <- chla_ssp585_cesm2_mean - chla_ssp126_cesm2_mean
diff_ipsl <- chla_ssp585_ipsl_mean - chla_ssp126_ipsl_mean
diff_ukesm <- chla_ssp585_ukesm_mean - chla_ssp126_ukesm_mean

diff_data <- rbind(
  data.frame(Year = years_ssp_cesm2,
             Chlorophyll_Difference = diff_cesm2,
             Model = "CESM2"),
  data.frame(Year = years_ssp_ipsl[1:length(diff_ipsl)],
             Chlorophyll_Difference = diff_ipsl,
             Model = "IPSL"),
  data.frame(Year = years_ssp_ukesm[1:length(diff_ukesm)],
             Chlorophyll_Difference = diff_ukesm,
             Model = "UKESM")
)

p6 <- ggplot(diff_data, aes(x = Year, y = Chlorophyll_Difference, color = Model)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("CESM2" = "darkblue", "IPSL" = "darkgreen", "UKESM" = "darkred")) +
  labs(title = "Chlorophyll Difference Between SSP585 and SSP126 Scenarios",
       x = "Year",
       y = "Chlorophyll Difference (mg/m³)",
       subtitle = "Positive values indicate SSP585 has higher chlorophyll than SSP126",
       color = "Model") +
  theme_bw()

print(p6)

# PLOT 7: SSP534-over vs other scenarios comparison
ssp_comparison_data <- combined_data[combined_data$Scenario %in% c("SSP126", "SSP585", "SSP534-over") &
                                       combined_data$Year >= 2015, ]

p7 <- ggplot(ssp_comparison_data, aes(x = Year, y = Chlorophyll, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Model, ncol = 3) +
  scale_color_manual(values = c("SSP126" = "blue",
                                "SSP585" = "red",
                                "SSP534-over" = "purple")) +
  labs(title = "Global Ocean Chlorophyll: SSP Scenario Comparison",
       x = "Year",
       y = "Global Ocean Chlorophyll (mg/m³)",
       color = "Scenario",
       subtitle = "Note: SSP534-over starts from 2040") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p7)

# Save plots if desired

# ggsave("Figures/chla_all_scenarios_facet.png", p1, width = 12, height = 8, dpi = 300)
# ggsave("Figures/chla_all_scenarios_combined.png", p5, width = 12, height = 8, dpi = 300)
# ggsave("Figures/chla_difference_ssp126_ssp_585.png", p6, width = 12, height = 8, dpi = 300)