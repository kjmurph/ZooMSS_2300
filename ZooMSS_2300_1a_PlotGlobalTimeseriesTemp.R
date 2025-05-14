library(RNetCDF)
library(raster)
library(ggplot2)

cell_areas <- t(as.matrix(area(raster())*1e6))
plot(raster(cell_areas))

# Load all NetCDF files (as in your code)

# CESM2
tos_hist_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/cesm2-waccm_r1i1p1f1_historical_tos_60arcmin_global_annual_1850_2014.nc"), "tos")
tos_picontrol_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/cesm2-waccm_r1i1p1f1_picontrol_tos_60arcmin_global_annual_1601_2100.nc"), "tos")
tos_ssp126_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/cesm2-waccm_r1i1p1f1_ssp126_tos_60arcmin_global_annual_2015_2299.nc"), "tos")
tos_ssp585_cesm2 <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/cesm2-waccm_r1i1p1f1_ssp585_tos_60arcmin_global_annual_2015_2299.nc"), "tos")

# IPSL
tos_hist_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ipsl-cm6a-lr_r1i1p1f1_historical_tos_60arcmin_global_annual_1850_2014.nc"), "tos")
tos_picontrol_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ipsl-cm6a-lr_r1i1p1f1_picontrol_tos_60arcmin_global_annual_1601_2100.nc"), "tos")
tos_ssp126_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ipsl-cm6a-lr_r1i1p1f1_ssp126_tos_60arcmin_global_annual_2015_2300.nc"), "tos")
tos_ssp585_ipsl <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ipsl-cm6a-lr_r1i1p1f1_ssp585_tos_60arcmin_global_annual_2015_2300.nc"), "tos")

# UKESM
tos_hist_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ukesm1-0-ll_r4i1p1f2_historical_tos_60arcmin_global_annual_1850_2014.nc"), "tos")
tos_picontrol_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ukesm1-0-ll_r1i1p1f2_picontrol_tos_60arcmin_global_annual_1601_2100.nc"), "tos")
tos_ssp126_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ukesm1-0-ll_r4i1p1f2_ssp126_tos_60arcmin_global_annual_2015_2300.nc"), "tos")
tos_ssp585_ukesm <- var.get.nc(open.nc("~/R Projects/ZooMSS_2300/Input/tos/ukesm1-0-ll_r4i1p1f2_ssp585_tos_60arcmin_global_annual_2015_2300.nc"), "tos")

# Create ocean mask and areas for CESM2
# Cut out land cells from cell area matrix
ocean_cells_cesm2 <- tos_ssp126_cesm2[,,1]
ocean_cells_cesm2[!is.na(ocean_cells_cesm2)] <- 1
ocean_areas_cesm2 <- ocean_cells_cesm2 * cell_areas
# Get total area of the ocean
tot_ocean_area_cesm2 <- sum(ocean_areas_cesm2, na.rm = TRUE)

# Create ocean mask and areas for IPSL
# Cut out land cells from cell area matrix
ocean_cells_ipsl <- tos_ssp126_ipsl[,,1]
ocean_cells_ipsl[!is.na(ocean_cells_ipsl)] <- 1
ocean_areas_ipsl <- ocean_cells_ipsl * cell_areas
# Get total area of the ocean
tot_ocean_area_ipsl <- sum(ocean_areas_ipsl, na.rm = TRUE)

# Create ocean mask and areas for UKESM
# Cut out land cells from cell area matrix
ocean_cells_ukesm <- tos_ssp126_ukesm[,,1]
ocean_cells_ukesm[!is.na(ocean_cells_ukesm)] <- 1
ocean_areas_ukesm <- ocean_cells_ukesm * cell_areas
# Get total area of the ocean
tot_ocean_area_ukesm <- sum(ocean_areas_ukesm, na.rm = TRUE)

# plot ocean masks to inspect differences between the models
plot(raster(ocean_areas_ipsl))
plot(raster(ocean_areas_cesm2))
plot(raster(ocean_areas_ukesm))

# Calculate area-weighted means for CESM2
tos_ssp126_cesm2_mean <- apply(sweep(tos_ssp126_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
tos_ssp585_cesm2_mean <- apply(sweep(tos_ssp585_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
tos_hist_cesm2_mean <- apply(sweep(tos_hist_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2
tos_picontrol_cesm2_mean <- apply(sweep(tos_picontrol_cesm2, c(1,2), ocean_areas_cesm2, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_cesm2

# Calculate area-weighted means for IPSL
tos_hist_ipsl_mean <- apply(sweep(tos_hist_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
tos_picontrol_ipsl_mean <- apply(sweep(tos_picontrol_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
tos_ssp126_ipsl_mean <- apply(sweep(tos_ssp126_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl
tos_ssp585_ipsl_mean <- apply(sweep(tos_ssp585_ipsl, c(1,2), ocean_areas_ipsl, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ipsl

# Calculate area-weighted means for UKESM
tos_hist_ukesm_mean <- apply(sweep(tos_hist_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
tos_picontrol_ukesm_mean <- apply(sweep(tos_picontrol_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
tos_ssp126_ukesm_mean <- apply(sweep(tos_ssp126_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm
tos_ssp585_ukesm_mean <- apply(sweep(tos_ssp585_ukesm, c(1,2), ocean_areas_ukesm, "*"),3,sum,na.rm=TRUE)/tot_ocean_area_ukesm

plot(tos_hist_ipsl_mean, type = "l")
plot(tos_picontrol_ipsl_mean, type = "l")
plot(tos_ssp126_ipsl_mean, type = "l")
plot(tos_ssp585_ipsl_mean, type = "l")

# Check the actual dimensions of your data and create appropriate year vectors
# Historical data: 1850-2014 (165 years)
years_hist <- seq(1850, 2014, by = 1)
print(paste("Length of years_hist:", length(years_hist)))
print(paste("Length of tos_hist_cesm2_mean:", length(tos_hist_cesm2_mean)))
print(paste("Length of tos_hist_ipsl_mean:", length(tos_hist_ipsl_mean)))
print(paste("Length of tos_hist_ukesm_mean:", length(tos_hist_ukesm_mean)))

# PI Control: Check actual length and create appropriate years
print(paste("Length of tos_picontrol_cesm2_mean:", length(tos_picontrol_cesm2_mean)))
print(paste("Length of tos_picontrol_ipsl_mean:", length(tos_picontrol_ipsl_mean)))
print(paste("Length of tos_picontrol_ukesm_mean:", length(tos_picontrol_ukesm_mean)))
# Create years based on actual data length (assuming it starts from 1601)
years_picontrol_cesm2 <- seq(1601, 1601 + length(tos_picontrol_cesm2_mean) - 1, by = 1)
years_picontrol_ipsl <- seq(1601, 1601 + length(tos_picontrol_ipsl_mean) - 1, by = 1)
years_picontrol_ukesm <- seq(1601, 1601 + length(tos_picontrol_ukesm_mean) - 1, by = 1)

# SSP scenarios: Check lengths
print(paste("Length of tos_ssp126_cesm2_mean:", length(tos_ssp126_cesm2_mean)))
print(paste("Length of tos_ssp126_ipsl_mean:", length(tos_ssp126_ipsl_mean)))
print(paste("Length of tos_ssp126_ukesm_mean:", length(tos_ssp126_ukesm_mean)))
# Create years based on actual data length
years_ssp_cesm2 <- seq(2015, 2015 + length(tos_ssp126_cesm2_mean) - 1, by = 1)
years_ssp_ipsl <- seq(2015, 2015 + length(tos_ssp126_ipsl_mean) - 1, by = 1)
years_ssp_ukesm <- seq(2015, 2015 + length(tos_ssp126_ukesm_mean) - 1, by = 1)


# Create comprehensive data frame with all scenarios
data_list <- list(
  # Historical data
  data.frame(Year = years_hist,
             Temperature = tos_hist_cesm2_mean,
             Scenario = "Historical",
             Model = "CESM2"),
  data.frame(Year = years_hist,
             Temperature = tos_hist_ipsl_mean,
             Scenario = "Historical",
             Model = "IPSL"),
  data.frame(Year = years_hist,
             Temperature = tos_hist_ukesm_mean,
             Scenario = "Historical",
             Model = "UKESM"),

  # PI Control data (using model-specific year vectors)
  data.frame(Year = years_picontrol_cesm2,
             Temperature = tos_picontrol_cesm2_mean,
             Scenario = "PI Control",
             Model = "CESM2"),
  data.frame(Year = years_picontrol_ipsl,
             Temperature = tos_picontrol_ipsl_mean,
             Scenario = "PI Control",
             Model = "IPSL"),
  data.frame(Year = years_picontrol_ukesm,
             Temperature = tos_picontrol_ukesm_mean,
             Scenario = "PI Control",
             Model = "UKESM"),

  # SSP scenarios
  data.frame(Year = years_ssp_cesm2,
             Temperature = tos_ssp126_cesm2_mean,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Temperature = tos_ssp585_cesm2_mean,
             Scenario = "SSP585",
             Model = "CESM2"),
  data.frame(Year = years_ssp_ipsl,
             Temperature = tos_ssp126_ipsl_mean,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Temperature = tos_ssp585_ipsl_mean,
             Scenario = "SSP585",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ukesm,
             Temperature = tos_ssp126_ukesm_mean,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Temperature = tos_ssp585_ukesm_mean,
             Scenario = "SSP585",
             Model = "UKESM")
)


# Combine all data
combined_data <- do.call(rbind, data_list)

# PLOT 1: All scenarios by model
p1 <- ggplot(combined_data, aes(x = Year, y = Temperature, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Model, ncol = 3) +  # Changed from ncol = 2 to ncol = 3
  scale_color_manual(values = c("Historical" = "black",
                                "PI Control" = "gray",
                                "SSP126" = "blue",
                                "SSP585" = "red")) +
  labs(title = "Global Ocean Temperature by Model and Scenario",
       x = "Year",
       y = "Global Ocean Temperature (°C)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  coord_cartesian(xlim = c(1850, 2300))  # Focus on relevant time period

print(p1)

# PLOT 2: Focus on future scenarios and historical (zoom in)
future_data <- combined_data[combined_data$Scenario %in% c("Historical", "SSP126", "SSP585") &
                               combined_data$Year >= 1900, ]

p2 <- ggplot(future_data, aes(x = Year, y = Temperature, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Model, ncol = 3) +  # Changed from ncol = 2 to ncol = 3
  scale_color_manual(values = c("Historical" = "black",
                                "SSP126" = "blue",
                                "SSP585" = "red")) +
  labs(title = "Global Ocean Temperature: Historical and Future Scenarios",
       x = "Year",
       y = "Global Ocean Temperature (°C)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p2)

# PLOT 3: Temperature anomalies relative to historical baseline (30-year period)
# Calculate historical baseline using 30-year period (1985-2014)
# 1985 = index 136 (1985-1850+1), 2014 = index 165 (2014-1850+1)
hist_baseline_start <- 136  # 1985
hist_baseline_end <- 165    # 2014

# Alternative: you can try even larger periods by changing these indices
# For 50-year baseline (1965-2014): hist_baseline_start <- 116
# For full historical period (1850-2014): hist_baseline_start <- 1

hist_baseline_cesm2 <- mean(tos_hist_cesm2_mean[hist_baseline_start:hist_baseline_end])
hist_baseline_ipsl <- mean(tos_hist_ipsl_mean[hist_baseline_start:hist_baseline_end])
hist_baseline_ukesm <- mean(tos_hist_ukesm_mean[hist_baseline_start:hist_baseline_end])

print(paste("Historical baseline CESM2 (1985-2014):", round(hist_baseline_cesm2, 3), "°C"))
print(paste("Historical baseline IPSL (1985-2014):", round(hist_baseline_ipsl, 3), "°C"))
print(paste("Historical baseline UKESM (1985-2014):", round(hist_baseline_ukesm, 3), "°C"))

# Create anomaly data using the new baseline
anomaly_data_list <- list(
  # CESM2 anomalies
  data.frame(Year = years_ssp_cesm2,
             Temperature_Anomaly = tos_ssp126_cesm2_mean - hist_baseline_cesm2,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Temperature_Anomaly = tos_ssp585_cesm2_mean - hist_baseline_cesm2,
             Scenario = "SSP585",
             Model = "CESM2"),

  # IPSL anomalies
  data.frame(Year = years_ssp_ipsl,
             Temperature_Anomaly = tos_ssp126_ipsl_mean - hist_baseline_ipsl,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Temperature_Anomaly = tos_ssp585_ipsl_mean - hist_baseline_ipsl,
             Scenario = "SSP585",
             Model = "IPSL"),

  # UKESM anomalies
  data.frame(Year = years_ssp_ukesm,
             Temperature_Anomaly = tos_ssp126_ukesm_mean - hist_baseline_ukesm,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Temperature_Anomaly = tos_ssp585_ukesm_mean - hist_baseline_ukesm,
             Scenario = "SSP585",
             Model = "UKESM")
)

anomaly_data <- do.call(rbind, anomaly_data_list)

p3 <- ggplot(anomaly_data, aes(x = Year, y = Temperature_Anomaly, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Model, ncol = 3) +  # Changed from ncol = 2 to ncol = 3
  scale_color_manual(values = c("SSP126" = "blue", "SSP585" = "red")) +
  labs(title = "Global Ocean Temperature Anomalies Relative to Historical Baseline (1985-2014)",
       x = "Year",
       y = "Temperature Anomaly (°C)",
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

pi_baseline_cesm2 <- mean(tos_picontrol_cesm2_mean[pi_start_idx:pi_end_idx])
pi_baseline_ipsl <- mean(tos_picontrol_ipsl_mean[pi_start_idx:pi_end_idx])
pi_baseline_ukesm <- mean(tos_picontrol_ukesm_mean[pi_start_idx:pi_end_idx])

print(paste("PI Control baseline CESM2 (2009-2018):", round(pi_baseline_cesm2, 3), "°C"))
print(paste("PI Control baseline IPSL (2009-2018):", round(pi_baseline_ipsl, 3), "°C"))
print(paste("PI Control baseline UKESM (2009-2018):", round(pi_baseline_ukesm, 3), "°C"))

# Calculate anomalies relative to PI Control baseline
pi_anomaly_data_list <- list(
  # CESM2 anomalies
  data.frame(Year = years_ssp_cesm2,
             Temperature_Anomaly = tos_ssp126_cesm2_mean - pi_baseline_cesm2,
             Scenario = "SSP126",
             Model = "CESM2"),
  data.frame(Year = years_ssp_cesm2,
             Temperature_Anomaly = tos_ssp585_cesm2_mean - pi_baseline_cesm2,
             Scenario = "SSP585",
             Model = "CESM2"),

  # IPSL anomalies
  data.frame(Year = years_ssp_ipsl,
             Temperature_Anomaly = tos_ssp126_ipsl_mean - pi_baseline_ipsl,
             Scenario = "SSP126",
             Model = "IPSL"),
  data.frame(Year = years_ssp_ipsl,
             Temperature_Anomaly = tos_ssp585_ipsl_mean - pi_baseline_ipsl,
             Scenario = "SSP585",
             Model = "IPSL"),

  # UKESM anomalies
  data.frame(Year = years_ssp_ukesm,
             Temperature_Anomaly = tos_ssp126_ukesm_mean - pi_baseline_ukesm,
             Scenario = "SSP126",
             Model = "UKESM"),
  data.frame(Year = years_ssp_ukesm,
             Temperature_Anomaly = tos_ssp585_ukesm_mean - pi_baseline_ukesm,
             Scenario = "SSP585",
             Model = "UKESM")
)

pi_anomaly_data <- do.call(rbind, pi_anomaly_data_list)

p4 <- ggplot(pi_anomaly_data, aes(x = Year, y = Temperature_Anomaly, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Model, ncol = 3) +  # Changed from ncol = 2 to ncol = 3
  scale_color_manual(values = c("SSP126" = "blue", "SSP585" = "red")) +
  labs(title = "Global Ocean Temperature Anomalies Relative to PI Control Baseline",
       x = "Year",
       y = "Temperature Anomaly (°C)",
       subtitle = "Baseline: PI Control 10-year mean (2009-2018 equivalent)",
       color = "Scenario") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p4)

# PLOT 5: Combined plot showing all scenarios with different line types
combined_plot_data <- combined_data[combined_data$Scenario %in% c("Historical", "SSP126", "SSP585") &
                                      combined_data$Year >= 1900, ]

p5 <- ggplot(combined_plot_data, aes(x = Year, y = Temperature,
                                     color = Scenario, linetype = Model)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Historical" = "black",
                                "SSP126" = "blue",
                                "SSP585" = "red")) +
  scale_linetype_manual(values = c("CESM2" = "solid", "IPSL" = "dashed", "UKESM" = "dotted")) +  # Added UKESM
  labs(title = "Global Ocean Temperature: All Scenarios and Models",
       x = "Year",
       y = "Global Ocean Temperature (°C)",
       color = "Scenario",
       linetype = "Model") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p5)

# PLOT 6: Difference between SSP scenarios for each model (original p4)
# Calculate differences (SSP585 - SSP126)
diff_cesm2 <- tos_ssp585_cesm2_mean - tos_ssp126_cesm2_mean
diff_ipsl <- tos_ssp585_ipsl_mean - tos_ssp126_ipsl_mean
diff_ukesm <- tos_ssp585_ukesm_mean - tos_ssp126_ukesm_mean

diff_data <- rbind(
  data.frame(Year = years_ssp_cesm2,
             Temperature_Difference = diff_cesm2,
             Model = "CESM2"),
  data.frame(Year = years_ssp_ipsl[1:length(diff_ipsl)],
             Temperature_Difference = diff_ipsl,
             Model = "IPSL"),
  data.frame(Year = years_ssp_ukesm[1:length(diff_ukesm)],
             Temperature_Difference = diff_ukesm,
             Model = "UKESM")
)

p6 <- ggplot(diff_data, aes(x = Year, y = Temperature_Difference, color = Model)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("CESM2" = "darkblue", "IPSL" = "darkgreen", "UKESM" = "darkred")) +  # Added UKESM color
  labs(title = "Temperature Difference Between SSP585 and SSP126 Scenarios",
       x = "Year",
       y = "Temperature Difference (°C)",
       subtitle = "Positive values indicate SSP585 is warmer than SSP126",
       color = "Model") +
  theme_bw()

print(p6)

# Save plots if desired

# ggsave("Figures/temp_all_scenarios_facet.png", p1, width = 12, height = 8, dpi = 300)
# ggsave("Figures/temp_all_scenarios_combined.png", p5, width = 12, height = 8, dpi = 300)
# ggsave("Figures/temp_difference_ssp126_ssp585_scenarios.png", p6, width = 12, height = 8, dpi = 300)

