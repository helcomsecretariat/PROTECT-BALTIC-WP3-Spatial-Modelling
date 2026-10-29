# creating stacks for delta variables to be plugged into SDM scenario prediction

# 0. libraries, layer names etc 
# 1. fix unit problem with sig wave height + recalculate delta layer 
# 2. pre-process scenario data (neg values to current 0 etc)
# 3. produce ready raster stacks for scenario modelling 

# 0. libraries etc ---- 
library(terra)
library(tidyverse) 

setwd("")

# current env 
current.env <- rast("Current_env/env_stack_19_11_2025_updated_names.tif")


# 1. fix wave height (a unit problem) -----

# from tiffsto250m folder 

# RCP85
wh.rcp85.10.23 <- rast("TiffsTo250/RCP85_currentNutrients_2010_2023/PB_2NM_SigWaveHeight_PB_2010-20231-12_depth_Surface_mean.tif")
wh.rcp85.40.60 <- rast("TiffsTo250/RCP85_currentNutrients_2040_2059/PB_2NM_SigWaveHeight_PB_2040-20591-12_depth_Surface_mean.tif")
wh.rcp85.80.00 <- rast("TiffsTo250/RCP85_currentNutrients_2080_2099/PB_2NM_SigWaveHeight_PB_2080-20991-12_depth_Surface_mean.tif")

# RCP45
wh.rcp45.10.23 <- rast("TiffsTo250/RCP45_currentNutrients_2010_2023/PB_2NM_SigWaveHeight_PB_2010-20231-12_depth_Surface_mean.tif")
wh.rcp45.40.60 <- rast("TiffsTo250/RCP45_currentNutrients_2040_2059/PB_2NM_SigWaveHeight_PB_2040-20591-12_depth_Surface_mean.tif")
wh.rcp45.80.00 <- rast("TiffsTo250/RCP45_currentNutrients_2080_2099/PB_2NM_SigWaveHeight_PB_2080-20991-12_depth_Surface_mean.tif")

# copernicus current wave height 
current.wh.copernicus <- current.env[["wave_significant_height_mean"]]

# convert the units cm -> m 

wh.rcp45.10.23 <- wh.rcp45.10.23/100
wh.rcp45.40.60 <- wh.rcp45.40.60/100
wh.rcp45.80.00 <- wh.rcp45.80.00/100

wh.rcp85.10.23 <- wh.rcp85.10.23/100
wh.rcp85.40.60 <- wh.rcp85.40.60/100
wh.rcp85.80.00 <- wh.rcp85.80.00/100

# calculate difference layers 
wh.diff.rcp45.40.60 <- wh.rcp45.40.60 - wh.rcp45.10.23
wh.diff.rcp45.80.00 <- wh.rcp45.80.00 - wh.rcp45.10.23

wh.diff.rcp85.40.60 <- wh.rcp85.40.60 - wh.rcp85.10.23
wh.diff.rcp85.80.00 <- wh.rcp85.80.00 - wh.rcp85.10.23

# calculate the delta layers 
wh.delta.rcp45.40.60 <- current.wh.copernicus + wh.diff.rcp45.40.60
wh.delta.rcp45.80.00 <- current.wh.copernicus + wh.diff.rcp45.80.00
wh.delta.rcp85.40.60 <- current.wh.copernicus + wh.diff.rcp85.40.60
wh.delta.rcp85.80.00 <- current.wh.copernicus + wh.diff.rcp85.80.00

# write fixed tiffs to delta folder 
writeRaster(wh.delta.rcp45.40.60, 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_SigWaveHeight_PB1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
            )
writeRaster(wh.delta.rcp45.80.00, 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_SigWaveHeight_PB1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(wh.delta.rcp85.40.60, 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_SigWaveHeight_PB1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

writeRaster(wh.delta.rcp85.80.00, 
            "Delta_layers/RCP85_currentNutrients_20802099//PB_2NM_SigWaveHeight_PB1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

# 2. fix layers ---- 

# fixes: 
# surface salinity: neg values -> current min
# bottom salinity: neg values -> current min
# sea ice concentration: neg values -> 0, rename variable -> sea_ice_fraction_mean
# nitrate: neg values -> current min
# phosphate: neg values -> current min
# ammonium: neg values -> current min


# current env min values for selected variables 

salinity.surf.min <- current.env[["salinity_surface_mean"]] %>% minmax() %>% unlist() %>% as_tibble() %>% slice(1) %>% as.numeric()
salinity.bottom.min <- current.env[["salinity_bottom_mean"]] %>% minmax() %>% unlist() %>% as_tibble() %>% slice(1) %>% as.numeric()
nitrate.min <- current.env[["nitrate_surface_mean"]] %>% minmax() %>% unlist() %>% as_tibble() %>% slice(1) %>% as.numeric()
phospate.min <- current.env[["phosphate_surface_mean"]] %>% minmax() %>% unlist() %>% as_tibble() %>% slice(1) %>% as.numeric() 
ammonium.min <- current.env[["ammonium_surface_mean"]] %>% minmax() %>% unlist() %>% as_tibble() %>% slice(1) %>% as.numeric() 


# fix surface salinity 
sur.salinity.raster <- c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta.tif") # RCP45 2080-2100
)

sur.salinity.raster[sur.salinity.raster < 0] <- salinity.surf.min

# write to file 
writeRaster(sur.salinity.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(sur.salinity.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(sur.salinity.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(sur.salinity.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)


# fix bottom salinity 
bottom.salinity.raster <- c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta.tif") # RCP45 2080-2100
)

bottom.salinity.raster[bottom.salinity.raster < 0] <- salinity.bottom.min

# write to file 
writeRaster(bottom.salinity.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(bottom.salinity.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(bottom.salinity.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(bottom.salinity.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

# fix nitrate 

nitrate.raster <- c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta.tif") # RCP45 2080-2100
)

nitrate.raster[nitrate.raster < 0] <- nitrate.min

# write to file 
writeRaster(nitrate.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(nitrate.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(nitrate.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(nitrate.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

# fix phosphate

phosphate.raster <- c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta.tif") # RCP45 2080-2100
)

phosphate.raster[phosphate.raster < 0] <- phospate.min

# write to file 
writeRaster(phosphate.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(phosphate.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(phosphate.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(phosphate.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

# fix sea ice
sea.ice.raster <-  c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_SeaIceConc_PB1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_SeaIceConc_PB1-12_depth_Surface_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_SeaIceConc_PB1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_SeaIceConc_PB1-12_depth_Surface_mean_delta.tif") # RCP45 2080-2100
)

sea.ice.raster[sea.ice.raster < 0] <- 0
names(sea.ice.raster) <- c(rep("sea_ice_fraction_mean", 4))

# write to file 
writeRaster(sea.ice.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_SeaIceConc_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite=TRUE
)
writeRaster(sea.ice.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_SeaIceConc_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite=TRUE
)
writeRaster(sea.ice.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_SeaIceConc_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite=TRUE
)
writeRaster(sea.ice.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_SeaIceConc_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite=TRUE
)

# fix ammonium 
ammonium.raster <-  c(
  rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_Ammonimum_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_Ammonimum_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2080-2100
  rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_Ammonimum_PB_1-12_depth_Surface_mean_delta.tif"), # RCP45 2040-2060
  rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_Ammonimum_PB_1-12_depth_Surface_mean_delta.tif") # RCP45 2080-2100
)

ammonium.raster[ammonium.raster < 0] <- ammonium.min

# write to file 
writeRaster(ammonium.raster[[1]], 
            "Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_Ammonium_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(ammonium.raster[[2]], 
            "Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_Ammonium_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(ammonium.raster[[3]], 
            "Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_Ammonium_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)
writeRaster(ammonium.raster[[4]], 
            "Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_Ammonium_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2")
)

# chlorophyll ok
# rast("Delta_layers/RCP45_currentNutrients_20402059/PB_2NM_chlorophyll_PB_1-12_depth_Surface_mean_delta.tif") %>% minmax()
# rast("Delta_layers/RCP45_currentNutrients_20802099/PB_2NM_chlorophyll_PB_1-12_depth_Surface_mean_delta.tif")%>% minmax()
# rast("Delta_layers/RCP85_currentNutrients_20402059/PB_2NM_chlorophyll_PB_1-12_depth_Surface_mean_delta.tif")%>% minmax()
# rast("Delta_layers/RCP85_currentNutrients_20802099/PB_2NM_chlorophyll_PB_1-12_depth_Surface_mean_delta.tif") %>% minmax()


# 3. raster stacks for scenario modelling

# candidate layers for scenario modelling
layer.name <- c(
  "PB_2NM_Ammonium_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_chlorophyll_PB_1-12_depth_Surface_mean_delta.tif",
  "PB_2NM_nitrate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_oxygen_PB_1-12_depth_Bottom_mean_delta.tif",
  "PB_2NM_phosphate_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_salinity_PB_1-12_depth_Bottom_mean_delta_ATfix14072026.tif",
  "PB_2NM_salinity_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_SeaIceConc_PB_1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_SigWaveHeight_PB1-12_depth_Surface_mean_delta_ATfix14072026.tif",
  "PB_2NM_temp_PB_1-12_depth_Bottom_mean_delta.tif" ,
  "PB_2NM_temp_PB_1-12_depth_Surface_mean_delta.tif"
)

# RCp 85
env.rcp85.80.00 <- rast(
  paste("Delta_layers/RCP85_currentNutrients_20802099", layer.name, sep= "/")
)

writeRaster(env.rcp85.80.00, 
            "delta_stacks/env_stack_rcp85_2080_2099.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), 
            overwrite = T
)

env.rcp85.40.60 <- rast(
  paste("Delta_layers/RCP85_currentNutrients_20402059/", layer.name, sep= "/")
)

writeRaster(env.rcp85.40.60, 
            "delta_stacks/env_stack_rcp85_2040_2059.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), 
            overwrite = T
)

# RCP45 

env.rcp45.80.00 <- rast(
  paste("Delta_layers/RCP45_currentNutrients_20802099", layer.name, sep= "/")
)

writeRaster(env.rcp45.80.00, 
            "delta_stacks/env_stack_rcp45_2080_2099.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), 
            overwrite = T
)

env.rcp45.40.60 <- rast(
  paste("Delta_layers/RCP45_currentNutrients_20402059/", layer.name, sep= "/")
)

writeRaster(env.rcp45.40.60, 
            "delta_stacks/env_stack_rcp45_2040_2059.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), 
            overwrite = T
)

# fixed variables 

fut.pred <- read.csv2("D:/Protect_BALTIC/WP3/Future/r_code/future_predictors.csv")
const.pred <- fut.pred %>% filter(predtype == "constant") %>% pull(short_name)

const.stack <- current.env[[const.pred]]

# mask out cells for which we dont have future data 

const.stack.masked <- mask(const.stack, mask = env.rcp45.40.60[[1]], maskvalue = NA)

# plot(const.stack[[1]])
# plot(const.stack.masked[[1]])

writeRaster(const.stack.masked, 
            "delta_stacks/fixed_var_stack.tif",
            datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), 
            overwrite = T
)














