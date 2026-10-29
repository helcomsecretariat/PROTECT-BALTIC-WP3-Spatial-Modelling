# Calculate the Delta layers:
# Set the working directory to the project's data folder
setwd("")

library(terra)
library(stringr)

env_current <- rast("Current_env/env_stack_19_11_2025_updated_names.tif")
variables <- c( "ammonium_surface_mean", "chloro_a_mean", "nitrate_surface_mean",
                "oxygen_bottom_mean",  "oxygen_surface_mean", "phosphate_surface_mean", 
                "salinity_bottom_mean", "salinity_surface_mean", "sea_ice_fraction_mean",
                "sea_surface_height_mean","temperature_bottom_mean","temperature_surface_mean",
                "wave_significant_height_mean")

# Select only the variables listed in `variables`
selected_vars <- env_current[[variables]]

selected_var_names <- names(selected_vars)

# Downscaled diff layers
file_path <- c("Diff_to_250m/")
output_file <- c("Delta_layers/")

# This pattern filters for files ending with ".tif"
file_list <- list.files(
  path = file_path,
  recursive =TRUE,
  pattern = "*.tif$",
  full.names = TRUE
)

# Calculate difference
# Create a data frame to store the pairs and differences

pairs_df <- data.frame(variable = character(),
                       paired_file = character(),
                       stringsAsFactors = FALSE)

base_names <- basename(file_list)

for (var in selected_var_names) {
  
  # Extract key parts
  var_lower <- tolower(var)
  
  # Detect variable type
  if (str_detect(var_lower, "ammonium")) keyword <- "ammon"
  else if (str_detect(var_lower, "chloro")) keyword <- "chlorophyll"
  else if (str_detect(var_lower, "nitrate")) keyword <- "nitrate"
  else if (str_detect(var_lower, "oxygen")) keyword <- "oxygen"
  else if (str_detect(var_lower, "phosphate")) keyword <- "phosphate"
  else if (str_detect(var_lower, "salinity")) keyword <- "salinity"
  else if (str_detect(var_lower, "temperature")) keyword <- "temp"
  else if (str_detect(var_lower, "wave")) keyword <- "sigwave"
  else if (str_detect(var_lower, "sea_surface_height")) keyword <- "sea_surface_height"
  else if (str_detect(var_lower, "sea_ice")) keyword <- "	seaiceconc"
  else next
  
  # Detect layer type
  layer_type <- if (str_detect(var_lower, "bottom")) "bottom" else "surface"
  
  # Find matching files (case-insensitive + flexible spelling)
  matching_idx <- str_detect(tolower(base_names), keyword) &
    str_detect(tolower(base_names), layer_type)
  
  matching_files <- file_list[matching_idx]
  
  for (f in matching_files) {
    diff_rast <- rast(f)
    target_rast <- selected_vars[[var]]
    
    # 1. Reproject if CRS differs
    if (crs(diff_rast) != crs(target_rast)) {
      diff_rast <- project(diff_rast, target_rast)
    }
    
    # 2. Align resolution + extent + grid
    # (resample handles this properly)
    if (!all(res(diff_rast) == res(target_rast)) ||
        !ext(diff_rast) == ext(target_rast)) {
      
      diff_rast <- resample(diff_rast, target_rast, method = "bilinear")
    }
    # 3. subtraction
    difference <- target_rast - diff_rast
    
    # save as .tif
    folder_name <- basename(dirname(f))
    writeRaster(difference, paste0(output_file,folder_name,"/",basename(f)), overwrite = TRUE,
                datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"))
  }
  
  # Store results
  if (length(matching_files) > 0) {
    temp_df <- data.frame(variable = var,
                          paired_file = matching_files)
    pairs_df <- rbind(pairs_df, temp_df)
  }
}


