# Install required packages if not already installed
# install.packages(c("ncdf4", "raster"))

# Load libraries
library(ncdf4)
library(raster)

# Open the NetCDF file
nc_file <- nc_open("~/R Projects/ZooMSS_2300/ZooMSS_2300_forcings/phyc/cesm2-waccm_r1i1p1f1_picontrol_phyc-top_60arcmin_global_annual_1601_2100.nc")

# Print information about the file
print(nc_file)

# Get specific information about the phyc variable
phyc_var <- nc_file$var$phyc  # Adjust variable name if different
print(phyc_var)

# Check the units attribute
phyc_units <- ncatt_get(nc_file, "phyc", "units")
print(paste("Phytoplankton biomass units:", phyc_units$value))

# You can also check other attributes
phyc_longname <- ncatt_get(nc_file, "phyc", "long_name")
print(paste("Long name:", phyc_longname$value))

# Close the NetCDF file
nc_close(nc_file)