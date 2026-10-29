# Calc change
setwd("")
# Load required libraries

library(terra)

# List all .tif files

Current_env <- list.files("ReadyTiffs/RCP85_currentNutrients_2010_2023", pattern = "*.tif", full.names = TRUE)

Future_env <- list.files("ReadyTiffs/RCP85_currentNutrients_2040_2059", pattern = "*.tif", full.names = TRUE)

# Save the result
output_dir <- "Difference_layers/RCP85_currentNutrients_20402059"
future_common <- "_2040-2059"

# # Load the rasters into terra objects
Current_rasters <- lapply(Current_env, rast)
Future_rasters <- lapply(Future_env, rast)
# 
# # Ensure the rasters are in the same order
# names(Current_rasters) <- basename(Current_env)
# names(Future_rasters) <- basename(Future_env)



# Extract the common part of the basenames
extract_common_part <- function(basename) {
  gsub("_2010-2023", "", basename)
}

extract_common_part_future <- function(basename) {
  gsub(future_common, "", basename)
}
# Ensure the rasters are in the same order
names(Current_rasters) <- basename(sapply(Current_env, extract_common_part))
names(Future_rasters) <- basename(sapply(Future_env, extract_common_part_future))

# Find the common basenames
common_basenames <- intersect(names(Current_rasters), names(Future_rasters))

# Perform the subtraction
Diff_rasters <- lapply(common_basenames, function(basename) {
  Future_rasters[[basename]] - Current_rasters[[basename]]
})

names(Diff_rasters) <- common_basenames

for (basename in common_basenames) {
  output_file <- file.path(output_dir, paste0(gsub(".tif", "", basename), "_diff.tif"))
  writeRaster(Diff_rasters[[basename]], output_file)
}




