library(terra)

output_dir <- "E:/Protect Baltic - WP3/outputs"

# Identify seasonal raster files
files_bred <- list.files(path = output_dir, pattern = "_breeding\\.tif$",
                         recursive = TRUE, full.names = TRUE)

files_nbred <- list.files(path = output_dir, pattern = "_nonbreeding\\.tif$",
                          recursive = TRUE, full.names = TRUE)

# Extract species names
spp_bred <- basename(dirname(files_bred))
spp_nbred <- basename(dirname(files_nbred))

# Match species present in both lists
common_spp <- intersect(spp_bred, spp_nbred)

# Report missing seasonal models
missing_nbred <- setdiff(spp_bred, spp_nbred)
missing_bred  <- setdiff(spp_nbred, spp_bred)

if (length(missing_nbred) > 0) 
  {
  message("Missing non-breeding models: ",
          paste(missing_nbred, collapse = ", "))
  }

if (length(missing_bred) > 0) 
  {
  message("Missing breeding models: ",
          paste(missing_bred, collapse = ", "))
  }

# Process each species
for (spp_name in common_spp)
  {

  print(spp_name)

  # Retrieve corresponding files
  bred_file <- files_bred[spp_bred == spp_name]
  nbred_file <- files_nbred[spp_nbred == spp_name]

  # Check for unique files
  if (length(bred_file) != 1 || length(nbred_file) != 1) 
    {
    warning("Multiple or missing files for: ", spp_name)
    next
    }

  # Load rasters
  bred  <- rast(bred_file)
  nbred <- rast(nbred_file)

  # Calculate maximum suitability
  merged <- max(bred, nbred, na.rm = TRUE)

  # Define output directory
  spp_dir <- file.path(output_dir, spp_name)

  # dir.create(spp_dir, recursive = TRUE, showWarnings = FALSE)

  # Export merged raster
  out_file <- file.path(spp_dir, paste0(spp_name, "_merged.tif"))

  writeRaster(merged, out_file, overwrite = TRUE, 
              filetype = "GTiff", datatype = "FLT4S")
  gc()
  }
