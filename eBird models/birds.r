# This script does the following:
# 1. Based on the bird species Protect Baltic aimed to model and the species modeled by eBird, generate a final species list to work with.  
# 2. Based on the species list, download the species data models from eBird. 
# 3. Downscale the maps to 250 m and produce presence absence-maps from range maps

remotes::install_github("ebird/ebirdst")

library(ebirdst)
library(tidyverse)
library(sf)
library(terra)
library(fs)
library(readxl)
library(writexl)
library(taxize)

set_ebirdst_access_key("") # access key to download species data from eBird

# species list ----

species_list <- read.csv("bird_species_list.csv", sep=",", dec=".")
ebird_list <- as.data.frame(ebirdst_runs$scientific_name)
colnames(ebird_list) <- "species"
species <- inner_join(species_list, ebird_list, by = "species")
species$species <- as.character(species$species)

sp <- classification(species$species, db = "itis") # itis or gbif are better than worms for birds

families <- map_chr(sp, ~ {if (is.data.frame(.x)) {fam <- .x %>%
      filter(rank == "family") %>%
      pull(name)
    if (length(fam) > 0) fam else NA_character_} else {NA_character_}})

orders <- map_chr(sp, ~ {if (is.data.frame(.x)) {ord <- .x %>%
  filter(rank == "order") %>%
  pull(name)
if (length(ord) > 0) ord else NA_character_} else {NA_character_}})

sp1 = tibble(order = orders, family = families, species = species$species) %>%
  as.data.frame()

write_xlsx(sp1, "eBird_species.xlsx")

# download species data from eBird ---- 

for (i in 1:length(species[,1])) 
  {

  sp <- species[i, 1]
  sp_clean <- gsub(" ", "_", sp)

  # temporary download directory
  tmp_dir <- "D:/Nextcloud/WP3/Modelling/Birds/eBird/R_implementation/data/temp"
  dir_create(tmp_dir)

  # download the species data to temporary location
  ebirdst_download_status(species = sp, path = tmp_dir, download_all = T)

  # the downloaded species folder is inside: tmp_dir/2023/species/
  year_folder <- dir_ls(tmp_dir, type = "directory")       # should be "2023"
  species_folder <- dir_ls(year_folder, type = "directory")

  # final destination
  final_folder <- file.path("D:/Nextcloud/WP3/Modelling/Birds/eBird/R_implementation/data",
                            sp_clean)
  # move the species folder from temp_dir to the main data directory
  dir_create(final_folder)
  file.copy(from = dir_ls(species_folder), to = final_folder, recursive = TRUE)
  
  # remove temp directory
  if (dir_exists(year_folder)) 
    {
      # Remove all contents of the year_folder first
      #file.remove(dir_ls(year_folder, all.files = TRUE, full.names = TRUE))
    
      Sys.sleep(10) # put the system to sleep to give some time for files to be deleted
    
      # Now remove the year_folder itself
      dir_delete(dir_ls(year_folder, all.files = TRUE, full.names = TRUE))
      dir_delete(tmp_dir)
  }
  
  rm(year_folder, species_folder, final_folder)
}

# load species data ----

datadir <- "E:/Protect Baltic - WP3/eBird models"
setwd("D:/Nextcloud/WP3/Modelling/Birds/eBird/R_implementation")

grid <- rast("D:/Nextcloud/WP3/Modelling/Grid/grid_marine_250m_v3.tif")

# Occurrence: The likelihood of seeing at least one individual of the species during a survey
# i.e., probability of occurrence

# 1. Load all the species in a files dir.files object 
files <- list.files(path = datadir,  
                    pattern = "occurrence_full-year_mean_3km_2023\\.tif$",
                    recursive = TRUE, full.names = TRUE)

# Downscale probability maps ----

for (i in 69:length(files)) 
  {
  spp <- rast(files[i])
  spp_name <- basename(dirname(dirname(files[i])))
  print(spp_name)
  
  # Reproject to grid CRS to 250 m
  spp_proj <- project(spp, grid, method="bilinear", mask=T)
  spp_proj <- mask(spp_proj, grid)
  
  # save probability map
  out_dir <- file.path("E:/Protect Baltic - WP3/outputs", spp_name)
  # dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(out_dir, paste0(spp_name, "_probability", ".tif"))
  writeRaster(spp_proj, out_file, overwrite = TRUE, 
              filetype = "GTiff", datatype = "FLT4S")
  
  gc()
}


# Downscale to produce presence-absence maps ----

# 2. Load all the species in a files dir.files object 
files <- list.files(path = datadir,  
                    pattern = "occurrence_seasonal_mean_3km_2023\\.tif$",
                    recursive = TRUE, full.names = TRUE)

for (i in 1:length(files)) 
  {
  spp <- vect(files[i], layer = "range")
  spp_name <- basename(dirname(dirname(files[i])))
  print(spp_name)
  
  # Reproject to grid CRS
  spp_proj <- project(spp, crs(grid))
  
  # Breeding layers #
  
  breed_poly <- spp_proj[spp_proj$season == "breeding", ]
  
  breed_raster <- rasterize(breed_poly, grid, field = 1, background = 0)
  breed_raster <- mask(breed_raster, grid)
  
  names(breed_raster) <- "breeding"
  
  # Non-breeding layers #
  
  nonbreed_poly <- spp_proj[spp_proj$season != "breeding", ]
  
  # merge all polygons together (molting, post-breeding, etc.)
  nonbreed_poly <- aggregate(nonbreed_poly) # dissolve 
  
  nonbreed_raster <- rasterize(nonbreed_poly, grid, field = 1, background = 0)
  nonbreed_raster <- mask(nonbreed_raster, grid)
  
  names(nonbreed_raster) <- "non_breeding"
  
  # save breeding
  out_dir <- file.path("E:/Protect Baltic - WP3/outputs", spp_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(out_dir, paste0(spp_name, "_breeding", ".tif"))
  writeRaster(breed_raster, out_file, overwrite = TRUE, 
              filetype = "GTiff", datatype = "FLT4S")
  
  # save non-breeding
  out_file <- file.path(out_dir, paste0(spp_name, "_nonbreeding", ".tif"))
  writeRaster(nonbreed_raster, out_file, overwrite = TRUE, 
              filetype = "GTiff", datatype = "FLT4S")
  gc()
}


# end of script ---- 
