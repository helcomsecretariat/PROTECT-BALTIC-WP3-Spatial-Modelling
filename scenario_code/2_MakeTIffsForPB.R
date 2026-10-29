setwd("") # your wd 

library(terra)
library(ncdf4)
library(stringr)

## Get lat lon
load("Data/Input/RCO_Coordinates.RData")


file_paths <- list.files(path = "CalcEnvLayers/RCP45_currentNutrients_2080_2099/", pattern = "*.nc", full.names = TRUE)
startfiles <- file_paths

get_resampled <- function(ncfile,varname,  outputfilename, rco.lon, rco.lat ){
  
  mean_SSS <- ncvar_get(ncfile, varname)#'mean')
  
  rownames(mean_SSS) <- rco.lon
  colnames(mean_SSS) <- rco.lat
  XYZ <- as.data.frame(as.table(as.matrix(mean_SSS)))
  r <- rast(XYZ)
  projrco <- "+proj=longlat +zone=35 +north +ellps=WGS84 +datum=WGS84 +no_defs"
  crs(r) <- projrco 
  writeRaster(r, paste0("ReadyTiffs/RCP45_currentNutrients_2080_2099/PB_2NM_",outputfilename), overwrite = TRUE, 
              datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"))
  
  
}

for(jj in 1:length(startfiles)){
  ncfile <- nc_open(startfiles[jj])
  print(startfiles[jj])
  outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_mean.tif")
  get_resampled(ncfile,"mean",  outputfilename, rco.lon, rco.lat)
  
  outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_sd.tif")
  get_resampled(ncfile,"sd",  outputfilename, rco.lon, rco.lat)
  
  outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_max.tif")
  get_resampled(ncfile,"max",  outputfilename, rco.lon, rco.lat)
  
  outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_min.tif")
  get_resampled(ncfile,"min",  outputfilename, rco.lon, rco.lat)
  
  
  nc_close(ncfile)
  
  
}
