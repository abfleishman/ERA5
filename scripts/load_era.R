library(raster)
library(ncdf4)
library(dplyr)
u10<-stack("data/era5_2017_03_N23_W161_S18_E154.nc",varname="u10")
v10<-stack("data/era5_2017_03_N23_W161_S18_E154.nc",varname="v10")
wind<-stack("data/era5_2017_03_N23_W161_S18_E154.nc",varname="wind")
plot(wind[[1]])
plot(wind[[3]])