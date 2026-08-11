# Protect Baltic future SDM modelling 
# Antti Takolander, Finnish Environment Institute 
# 08 / 2026

library(tidyverse)

# sp_group <- "invertebrates"
# sp_group <- "macrophytes"
sp_group <- "fish"

# reading the species and species-specific parameters from automation file 
# and producing species-specific R and sh files for Puhti 

# 1. read automation template from csv (dat file) + templates for R and sh files
# 2. create sh-file for each species within a loop  
# 3. create R-file for each species within a loop 
if(sp_group == "fish") {
  dat = read_csv("/scratch/project_2017703/WP3/SDM/input_files/spec_list_fish.csv")  
}
if(sp_group == "macrophytes") {
  dat = read_csv("/scratch/project_2017703/WP3/SDM/input_files/spec_list_macrophytes_update.csv")  
}
if(sp_group == "invertebrates") {
  dat = read_csv("/scratch/project_2017703/WP3/SDM/input_files/spec_list_invertebrates.csv")  
}

dat$sp_workname = gsub(x = dat$scientific_name, pattern = " ", replacement = "_")
dat$sp_workname = gsub(x = dat$scientific_name, pattern = ".", replacement = "_", fixed = T)

# remove species with less than 150 obs 
dat = dat %>% filter(quantity >= 150)


# R template ; placeholder: str_a - str_l
r.template = read_lines(
   file = "/scratch/project_2017703/WP3/SDM/R_scripts/GAM_modelling_current.R"
)

# sbatch template; placeholder: str_a
sbatch.template = read_lines(
  file = "/scratch/project_2017703/WP3/SDM/R_scripts/batch_new.sh"
)

model.range = 171:203
# model.range = c(47, 148, 145, 181, 189, 197, 209, 189, 129, 95)
# model.range = 1:5
# model.range = c(5,6,26,75,89,65, 105, 286, 350) # invertebrates
# model.range = c(13, 4, 17, 29, 34, 98, 77) # macrophytes
# model.range = 2 # macrophytes
# model.range = c(1:4, 7:10) # macrophytes

# dat = dat %>% filter(use_method_cov == "TRUE")
# model.range <- 1:5 # Zostera delta conversion test
# model.range <- 1:nrow(dat)'
# dat$use_method_cov <- FALSE

# loop through species 
for (s in model.range) { # first row is a column description
  print(paste(s, dat$scientific_name[s]))
  
  # modify sbatch 
  s.temp = sbatch.template
  
  s.temp = gsub(
    pattern = "str_a", 
    replacement = 
      dat$sp_workname[s],
    x = s.temp
  )
  
  # write sbatch file ... 
  write_lines(
    s.temp, 
    file = paste0(
      "/scratch/project_2017703/WP3/SDM/models/", 
      sp_group, "/",
      dat$sp_workname[s], 
      ".sh"
    )
  )
  
  # R template 
  r.temp = r.template
  
  # species workname 
  r.temp = gsub(
    pattern = 'species_name_placeholder', 
    replacement = dat$scientific_name[s], 
    x = r.temp
  )
  
  r.temp = gsub(
    pattern = 'species_group_placeholder', 
    replacement = sp_group, 
    x = r.temp
  )
  
  # method (country) variable?
  r.temp = gsub(
    pattern = 'method_var_placeholder',
    replacement = dat$use_method_cov[s],
    x = r.temp
  )
  
    # write R code to file ... 
  write_lines(
    r.temp, 
    file = paste0(
      "/scratch/project_2017703/WP3/SDM/models/",
      sp_group, "/",
      dat$sp_workname[s], 
      ".R"
    )
  )
  
}

# jäätiin kaloihin nro 44 Callionymus. tästä eteenpäin ei vielä submittoitu jobeja. filet luotu 70een saakka.
stringr::str_flatten(string = paste0("sbatch ", dat$sp_workname[model.range], ".sh", collapse = ";"))



