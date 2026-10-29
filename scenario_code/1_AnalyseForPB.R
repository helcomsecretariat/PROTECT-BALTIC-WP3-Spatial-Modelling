
setwd("") # your wd 

# Load required libraries

library(ncdf4)
library(abind, include.only = c("abind"))
library(stringr, include.only = c("str_extract"))

# scripts are located in "Scripts" folder 
# input data in "Data/Input"
# raw data files are in "RawData/scenario_time"

source("Scripts/ProcessDataForPB.R") # Script for processing
source("Scripts/save_to_nc_forPB.R") # Script for saving
source("Scripts/Bottom_values.R") # Script for bottom values

load("Data/Input/depth_indices.RData")
load("Data/Input/NAids.RData")

# List all .nc files. change the scenario name/ path to point to correct scen
file_paths <- list.files(path = "RawData/RCP45_currentNutrients_2080_2099/", pattern = "*.nc", full.names = TRUE)
check_nc <- c('.nc')

files <- file_paths[grepl(paste0("(", paste(check_nc, collapse="|"), ")$"), basename(file_paths))]

# Assuming your list of files is stored in `files`
# group_files <- str_extract(basename(files), "^[aA-zZ]+")

# # Define the prefixes you want to group together
# prefix_groups <- c("AH" = c("AH_file_", "fileA_"), 
#                    "BH" = c("BH_file_", "fileB_"), 
#                    "CH" = c("CH_file_", "fileC_"), 
#                    "DH" = c("DH_file_", "fileD_"))
# 
# 
# # Group the files based on the defined prefixes
# grouped_files <- setNames(split(group_files, sapply(group_files, function(x) {
#   for (prefix in names(prefix_groups)) {
#     if (any(grepl(paste(prefix_groups[[prefix]], collapse = "|"), x))) {
#       return(prefix)
#     }
#   }
#   return("Other")  # Default group if no match is found
# })), names(prefix_groups))
# 

# Combine files from each group
# combined_groups <- list()
# for (group in names(grouped_files)) {
#   combined_groups[[group]] <- c(grouped_files[[group]])
# }
# 
# # Print the combined groups
# print(combined_groups)
# 
# # Print the grouped files
# print(grouped_files)

# group_files <- str_extract(basename(files), "[aA-zZ]+")
# # Identify unique groups
# unique_groups <- unique(group_files)

# Define variables to process and their conditions
variables <- list(
  VAR165 = "SigWaveHeight",
  VAR004 = "temp",
  VAR005 = "salinity",
  VAR016 = "nitrate",
  VAR017 = "phosphate",
  # 
  # # VAR012 = "chlorophyll2",
  # # VAR013 = "chlorophyll3",
  VAR051 = "SeaIceThick",
  VAR050 = "SeaIceConc",
  #
  VAR018 = "oxygen",
  VAR011 = "chlorophyll",

  VAR001 = "FreeSurfHeight",
  VAR015 = "Ammonimum"
)

# Define months for selection (summer months for example)

annual <- c(1:12)

# depths for surface
selected_depth <- 1
# Iterate over each unique group

# for (group in names(grouped_files)) {
#   files_in_group <- grouped_files[[group]]
#   print(paste("Processing group:", group))
#   
# for (group in unique_groups) {
#   # Filter files by the current group
#   group_files_filtered <- files[group_files == group]

for (ii in 1:length(variables)) {
  
  var <- names(variables)[ii]
  var_name <- variables[var]
  print(var)
  if (var %in% c("VAR165","VAR051","VAR050","VAR166","VAR001")){
    ## Surface
    data_out <- process_data_forPB(file_path4=files, varname=var, time_period = annual, depth = selected_depth,naids=naids)
    # Save output with appropriate filename
    output_filename <- paste0("./CalcEnvLayers/RCP45_currentNutrients_2080_2099/",var_name, "_PB_",
                              min(data_out$years), "-", max(data_out$years),
                              # "_",
                              min(data_out$months), "-", max(data_out$months),
                              "_depth_Surface.nc")
    print(output_filename)
    save_to_nc_forPB(data_out,output_filename, var)
  }else{

    data_out <- process_data_forPB(file_path4=files, varname=var, time_period = annual, depth = selected_depth,naids=naids)
    # Save output with appropriate filename
    output_filename <- paste0("./CalcEnvLayers/RCP45_currentNutrients_2080_2099/",var_name, "_PB_",
                              min(data_out$years), "-", max(data_out$years),
                              "_",
                              min(data_out$months), "-", max(data_out$months),
                              "_depth_Surface.nc")
    print(output_filename)
    save_to_nc_forPB(data_out,output_filename, var)

    # # Bottom
    data_out2 <- process_data_forPB(file_path4=files, varname=var, time_period = annual, depth = depth_indices,naids=naids)
    # Save output with appropriate filename
    output_filename2 <- paste0("./CalcEnvLayers/RCP45_currentNutrients_2080_2099/",var_name, "_PB_",
                               min(data_out2$years), "-", max(data_out2$years),
                               "_",
                               min(data_out2$months), "-", max(data_out2$months),
                               "_depth_Bottom.nc")
    print(output_filename2)
    save_to_nc_forPB(data_out2,output_filename2, var)
  }
  
}

# }



