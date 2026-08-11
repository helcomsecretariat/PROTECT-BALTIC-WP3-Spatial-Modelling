
# Protect Baltic future SDM modelling 
# Antti Takolander, Finnish Environment Institute 
# 08 / 2026

# 0. set model run parameters ---- 

## 0.0 libraries ---- 

library(tidyverse)
library(mgcv)
library(terra)
library(caret)
library(ggsci)

sessionInfo()

## 0.1 main (adjustable) parameters ----
testing = FALSE 
species_name = 'species_name_placeholder'
species_group = 'species_group_placeholder'
use_method = "method_var_placeholder"

cv_perc = 0.8
n_rep = 10
k.fixed = 7 # fixed: substrates, bathymetry, dist to coast etc 
k.scen = 4 # "climate" vars: O2, salinity, temperature, nutrients

terraOptions(memmax = 32)
run.no = 1 # change if multiple runs on the same day 

## 0.2 set run-specific etc parameters ----

set.seed(1345)
run.start.time = Sys.time()

sp_workname = gsub(pattern = " ", replacement = "_", x = species_name)
sp_workname = gsub(x = sp_workname, pattern = ".", fixed = T, replacement = "_")

model.dir = paste0("/scratch/project_2017703/WP3/SDM/models/", species_group)
modelling.tag = paste0(sp_workname, "--", as.Date(run.start.time), "-run", run.no)

# paths 
env.stack.path = "/scratch/project_2017703/WP3/SDM/predictors/env_stack_19_11_2025_updated_names.tif"
predictor.path = "/scratch/project_2017703/WP3/SDM/R_scripts/future_predictors.csv"

if (species_group == "macrophytes") {
  obs.data.path = paste0("/scratch/project_2017703/WP3/SDM/occurrence_data/macrophytes/", species_name, ".rds") # add the data path 
}
if (species_group == "fish") {
  obs.data.path = paste0("/scratch/project_2017703/WP3/SDM/occurrence_data/fish/", species_name, ".rds") # add the data path 
}
if (species_group == "invertebrates") {
  obs.data.path = paste0("/scratch/project_2017703/WP3/SDM/occurrence_data/invertebrates/", species_name, ".rds") # add the data path 
}

pb.grid.path = "/scratch/project_2017703/WP3/SDM/predictors/grid_marine_250m_v2.tif"

source("/scratch/project_2017703/WP3/SDM/R_scripts/plotting_scripts.R")
source("/scratch/project_2017703/WP3/SDM/R_scripts/helper_functions.R")

# scenario paths 
rcp45.curnutr.2040_2059.path = "/scratch/project_2017703/WP3/SDM/predictors/env_stack_rcp45_2040_2059.tif"
rcp45.curnutr.2080_2099.path = "/scratch/project_2017703/WP3/SDM/predictors/env_stack_rcp45_2080_2099.tif"
rcp85.curnutr.2040_2059.path = "/scratch/project_2017703/WP3/SDM/predictors/env_stack_rcp85_2040_2059.tif"
rcp85.curnutr.2080_2099.path = "/scratch/project_2017703/WP3/SDM/predictors/env_stack_rcp85_2080_2099.tif"
fixed.stack.path = "/scratch/project_2017703/WP3/SDM/predictors/fixed_var_stack.tif"

## 0.3 load data and predictors ---- 

# load and preprocess data 
data.in = readRDS(obs.data.path)
data.in = as.data.frame(data.in)
data.in = data.in[complete.cases(data.in),] # discard rows with NAs 
data.in$y <- data.in$quantity
data.in$y[data.in$y > 0] <-1
if(species_group != "invertebrates") {
  data.in$method <- gsub(pattern = " ", replacement = "", x = data.in$method)  
}
env.stack = rast(env.stack.path)
pb.grid = rast(pb.grid.path)

# preprocess predictor lists 
pred.list = read.csv2(predictor.path) # species-group specific predictor lists

if (species_group == "macrophytes") {
  predictors = pred.list %>% 
    filter(macrophytes == "x") %>% 
    select(vartype, predtype, short_name)
  cont.preds.fixed = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "constant"]
  cont.preds.scen = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "environment"]
  bin.preds = predictors$short_name[predictors$vartype == "binary"]
} 

if (species_group == "fish") {
  predictors = pred.list %>% 
    filter(fish == "x") %>% 
    select(vartype, predtype, short_name)
  cont.preds.fixed = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "constant"]
  cont.preds.scen = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "environment"]
  bin.preds = predictors$short_name[predictors$vartype == "binary"]
} 

if (species_group == "invertebrates") {
  predictors = pred.list %>% 
    filter(invertebrates == "x") %>% 
    select(vartype, predtype, short_name)
  cont.preds.fixed = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "constant"]
  cont.preds.scen = predictors$short_name[predictors$vartype == "continuous" & predictors$predtype == "environment"]
  bin.preds = predictors$short_name[predictors$vartype == "binary"]
} 

## 0.4 if method = "TRUE" calculate the most prevalent method and add this into stacks ----
# in scenario prediction 

if (use_method == "TRUE") {
  method_tab <- data.in %>% group_by(method) %>%
    summarise(num_samples = n(),
              num_presence = sum(y),
              perc_presence = mean(y)*100)
  
  # Identify method with highest prevalence
  if (species_group == "fish") {
    method_most <- method_tab$method[which.max(method_tab$perc_presence)]
  } 
  if (species_group == "macrophytes") {
    method_most <- method_tab$method[which.max(method_tab$num_presence)]
  }
  data.in$method <- as.factor(data.in$method)
  data.in$method <- droplevels(data.in$method)
  method_levels <- levels(data.in$method)
  
  if(length(method_levels) == 1){use_method <- "FALSE"} # Only use method as a predictor if more than 1 method in the data
  
  if(use_method == "TRUE"){
    
    dummies <- model.matrix(~ method, data = data.in)
    dummies <- as.data.frame(dummies)
    dummies <- dummies[,-1, drop = FALSE]
    
    data.in <- cbind(data.in, dummies)
    
    
    for(i in 2:length(method_levels)){
      r <- pb.grid
      r[] <- 0 # Defaults all method layers to zero (which represents the intercept)
      if(method_most == method_levels[i]){r[] <- 1}
      
      if(i == 2){dummy_preds <- r}
      if(i > 2){dummy_preds <- c(dummy_preds, r)}
    }
    
    names(dummy_preds) <- colnames(dummies)
    varnames(dummy_preds) <- colnames(dummies)
    
    bin.preds <- c(bin.preds, colnames(dummies))
   
  }
}

## 0.5 create modelling formula ----

preds = c(cont.preds.fixed, cont.preds.scen, bin.preds)
data.in <- data.in[,c("y", preds)]

# template for fixed vars
cont.template.fixed =  paste0(
  "s(", 
  cont.preds.fixed, 
  ", k = ",
  k.fixed,
  ", bs = 'ts')", 
  collapse = " + "
)

# template for scenario vars 
cont.template.scen =  paste0(
  "s(", 
  cont.preds.scen, 
  ", k = ",
  k.scen,
  ", bs = 'ts')", 
  collapse = " + "
)

# combined continuous variables 
cont.template <- paste0(
  cont.template.fixed, 
  " + ",
  cont.template.scen 
)

# binary + cont 
if (length(bin.preds) > 0 | use_method == "TRUE") {
  bin.template <- paste0(bin.preds, collapse = " + ")
  formula.str <- paste0("y ~ ", cont.template, " + ", bin.template)
  mdl_formula <- as.formula(formula.str)
} else { # only cont pred
  formula.str <- paste0("y ~ ", cont.template)
  mdl_formula <- as.formula(formula.str)
}

## 0.6 create subdirectories and set wd ----

# create directories 

# main modelling dir 
if(!exists(paste0(model.dir, "/", modelling.tag))) {
  dir.create(
    paste0(model.dir, "/", modelling.tag)
  )
} else {
  
  print(
    paste("Directory", paste0(model.dir, "/", modelling.tag), "exists, update run number")
  )
  stop()
}

setwd(paste0(model.dir, "/", modelling.tag))

dir.create("plots")
dir.create("prediction")
dir.create("validation")

# 1. n-fold cross-validation ----

part <- caret::createDataPartition(factor(data.in$y), times = n_rep, list = FALSE, p = cv_perc)
calib <- matrix(nrow = length(data.in$y), ncol = n_rep)
calib <- as.data.frame(calib)
colnames(calib) <- paste0("cv_run_", 1:n_rep)

# set test/training indexes
for(i in 1:n_rep){
  calib[,i] <- FALSE
  calib[part[,i],i] <- TRUE
}

# data frame for cv scores
cv.scores = tibble(
  auc = rep(NA_real_, n_rep),
  tjurr2 = rep(NA_real_, n_rep),
  i = rep(NA_integer_, n_rep), 
  sensitivity = rep(NA_real_, n_rep), 
  specificity = rep(NA_real_, n_rep),
  tss = rep(NA_real_, n_rep) 
)

# data frame for variable importance 
varimp.results <- tibble(
  variable = NA_character_,
  delta.tss = NA_real_,
  delta.sens = NA_real_,
  delta.spec = NA_real_
) %>% 
  slice(0)

# loop through nrows (i cv runs) of data frame
for(i in 1:n_rep) {
  print(paste("cross-validation cv run", i))

  index.i = calib[,i]

  train.data.i = data.in[index.i,]
  test.data.i = data.in[!index.i,]

  mdl.i = fit.gam(data_gam = train.data.i, mdl_formula = mdl_formula)

  # validation scores
  cv.scores$i[i] <- i
  cv.scores$auc[i] <- calc.auc(model.in = mdl.i, test.data = test.data.i)
  cv.scores$tjurr2[i] <- calc.tjurr2(model.in = mdl.i, test.data = test.data.i)
  
  cv.second.i <- calc.sens.spec.tss(model.in = mdl.i, test.data = test.data.i)
  
  cv.scores$tss[i] <- cv.second.i$tss
  cv.scores$sensitivity[i] <- cv.second.i$sensitivity
  cv.scores$specificity[i] <- cv.second.i$specificity
  
  varimp.results <- rbind(varimp.results, 
                          calc.varimportance(
                            model.in = mdl.i, 
                            test.data = test.data.i, 
                            var.list = preds
                          )
                          )
  
}

cv.scores$species <- species_name
cv.scores$model_tag <- modelling.tag

write_csv2(cv.scores, file = "validation/cv_scores.csv")

# average varimportance scores

varimp.results = varimp.results %>% 
  group_by(variable) %>% 
  summarise(
    tss.drop.mean = mean(delta.tss) %>% round(digits = 4), 
    tss.drop.sd = sd(delta.tss) %>% round(digits = 4), 
    sens.drop.mean = mean(delta.sens) %>% round(digits = 4), 
    sens.drop.sd = sd(delta.sens) %>% round(digits = 4), 
    spec.drop.mean = mean(delta.spec) %>% round(digits = 4), 
    spec.drop.sd = sd(delta.spec) %>% round(digits = 4)
  ) %>% 
  arrange(desc(tss.drop.mean))

varimp.results$species <- species_name
varimp.results$model_tag <- modelling.tag

# remove method variables 
varimp.results <- varimp.results[grep(x = varimp.results$variable, pattern = "method", invert = T),]
write_csv2(varimp.results, file = "validation/variable_importance.csv")

# 2. fit final model and save the outputs----

mdl.final = fit.gam(data_gam = data.in, mdl_formula = mdl_formula)

mdl_pred <- predict(mdl.final,
                    newdata = data.in,
                    type = "response",
                    se.fit = TRUE)


data.in$f_pred <- mdl_pred$fit
data.in$f_se <- mdl_pred$se.fit

## evaluate
e <- dismo::evaluate(
  p = data.in$f_pred[data.in$y== 1] %>% as.vector(),
  a = data.in$f_pred[data.in$y == 0] %>% as.vector()
)

# final thresholds 
thresh.final <- dismo::threshold(e) %>% 
  as_tibble() %>% 
  rename_with(
    ~paste0("th_", .x)
  )

modelfitsummary <- data.frame(model = "final",
             k.fixed = k.fixed,
             k.scen = k.scen,
             P = e@np,
             A = e@na,
             AUC = e@auc)

# tjur's R2 
tjur.final <- calc.tjurr2(mdl.final, data.in)
modelfitsummary <- cbind(modelfitsummary, tjur.final)

# TSS, sensitivity, specificity of the final model
tss.final <- calc.sens.spec.tss(mdl.final, data.in)
modelfitsummary <- cbind(modelfitsummary, tss.final)

# save thresholds if needed later
modelfitsummary = cbind(modelfitsummary, thresh.final)

modelfitsummary$species <- species_name
modelfitsummary$model_tag <- modelling.tag

write.csv(modelfitsummary,
          row.names = FALSE,
          paste0("validation/", "final_model_statistics_and_thresholds.csv")
          )

capture.output(
  summary(mdl.final), 
  file = paste0("validation/summary_final_model.txt")
)

capture.output(
  gam.check(mdl.final), 
  file = paste0("validation/gam_check_final_model.txt")
)

# concurvity 
ccc.full = concurvity(mdl.final, full = T) %>% as.data.frame() 
ccc.pairwise = concurvity(mdl.final, full = F) %>% as.data.frame() 

write_csv2(ccc.full, file = "validation/concurvity_full.csv")
write_csv2(ccc.pairwise, file = "validation/concurvity_pairwise.csv")

# save the final model 
saveRDS(mdl.final, file = paste0(modelling.tag, "_final_model", ".rds"))

## 2.1 plot model response and diagnostics ----

### 2.1.1. scaled response plot ----
pdf(paste0("plots/", sp_workname, "_responseplot_final_model_scaled.pdf"), height = 8, width = 12)
plot(
  mdl.final, 
  pages = 1, 
  rug = T, 
  residuals = T, 
  se = T, 
  shade = T, 
  seWithMean = T,
  shift = coef(mdl.final)[1], 
  ylab = "", 
  trans = plogis
)
dev.off()

### 2.1.2. unscaled response plot ----
pdf(paste0("plots/", sp_workname, "_responseplot_final_model_unscaled.pdf"), height = 8, width = 12)
plot(
  mdl.final, 
  pages = 1, 
  ylab = "", 
  trans = plogis
)
dev.off()

### 2.1.3. calibration plot ----
p.calib <- calibration_plot(obs = data.in$y, pred_prob = data.in$f_pred)
ggsave(paste0("plots/", sp_workname, "_predicted_probabilities_in_bins.pdf"), 
       plot = p.calib,
       height = 6, width = 9, units = "in")

### 2.1.4. a density threshold plot ----

p.density <- plot_density_threshold(
  data.in = data.in %>% select(y, f_pred) %>% rename(pred = f_pred), 
  threshold = thresh.final$th_spec_sens
  )

ggsave(paste0("plots/", sp_workname, "_p_densities.pdf"), 
       plot = p.density,
       height = 9, width = 6, units = "in")

### 2.1.5. variable importance plot ----
p.varimp <- plot_variable_importance(varimp.results)

ggsave(
  p.varimp, 
  file = paste0(
    "plots/varimportanceTSS_",
    sp_workname, 
    ".pdf"
  ), 
  height = 6, width = 9,  units = "in"
)


# 3. scenario prediction ----

## 3.1. load scenario data sets ----

env.stack.rcp45.2040.2059 <- rast(rcp45.curnutr.2040_2059.path)
env.stack.rcp45.2080.2099 <- rast(rcp45.curnutr.2080_2099.path)
env.stack.rcp85.2040.2059 <- rast(rcp85.curnutr.2040_2059.path)
env.stack.rcp85.2080.2099 <- rast(rcp85.curnutr.2080_2099.path)
fixed.stack <- rast(fixed.stack.path)

env.stack.rcp45.2040.2059 <- c(env.stack.rcp45.2040.2059, fixed.stack)
env.stack.rcp45.2080.2099 <- c(env.stack.rcp45.2080.2099, fixed.stack)
env.stack.rcp85.2040.2059 <- c(env.stack.rcp85.2040.2059, fixed.stack)
env.stack.rcp85.2080.2099 <- c(env.stack.rcp85.2080.2099, fixed.stack)

# if method == TRUE add dummy predictors 
if (use_method == "TRUE") {
  
  env.stack <- c(env.stack, dummy_preds)
  env.stack.rcp45.2040.2059 <- c(env.stack.rcp45.2040.2059, dummy_preds)
  env.stack.rcp45.2080.2099 <- c(env.stack.rcp45.2080.2099, dummy_preds)
  env.stack.rcp85.2040.2059 <- c(env.stack.rcp85.2040.2059, dummy_preds)
  env.stack.rcp85.2080.2099 <- c(env.stack.rcp85.2080.2099, dummy_preds)

}

## 3.2. current prediction ---- 

prediction.current = terra::predict(object = env.stack,
                           model = mdl.final,
                           cores = 1,
                           se.fit = FALSE,
                           type = "response",
                           cpkgs = "mgcv",
                           na.rm = TRUE)

# PA equal sensitivity - specificity 
equal_sens_spec <- modelfitsummary$th_equal_sens_spec

m_main <- c(0, equal_sens_spec, 0,
            equal_sens_spec, 1, 1)

rclmat_main <- matrix(m_main, ncol = 3, byrow = TRUE)

prediction.current.pa <- classify(prediction.current,
                       rclmat_main,
                       include.lowest = TRUE)

writeRaster(prediction.current,
            file = paste0("prediction/current_p_",  sp_workname, ".tif"),
            gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
            NAflag = -3.4e+38,
            overwrite=TRUE)

writeRaster(prediction.current.pa,
            file = paste0("prediction/current_bin_equal_sens_spec_",  sp_workname, ".tif"),
            datatype = "INT1U",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
            overwrite=TRUE)

## 3.3. scenario prediction ----

### 3.3.1. RCP45 2040-2059 current nutrients ----

prediction.rcp45.2040_2059.curnutr = terra::predict(
  object = env.stack.rcp45.2040.2059,
                           model = mdl.final,
                           cores = 1,
                           se.fit = FALSE,
                           type = "response"
  )

prediction.rcp45.2040_2059.curnutr.pa <- classify(prediction.rcp45.2040_2059.curnutr,
                                  rclmat_main,
                                  include.lowest = TRUE)

# save the spatrasters
writeRaster(prediction.rcp45.2040_2059.curnutr,
            file = paste0("prediction/prediction_RCP45_2040_2059_curnutr_p_",  sp_workname, ".tif"),
            gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
            NAflag = -3.4e+38,
            overwrite=TRUE)

writeRaster(prediction.rcp45.2040_2059.curnutr.pa,
            file = paste0("prediction/prediction_RCP45_2040_2059_curnutr_bin_",  sp_workname, ".tif"),
            datatype = "INT1U",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
            overwrite=TRUE)


### 3.3.2. RCP45 2080-2099 current nutrients ----

prediction.rcp45.2080_2099.curnutr = terra::predict(
  object = env.stack.rcp45.2080.2099,
  model = mdl.final,
  cores = 1,
  se.fit = FALSE,
  type = "response",
  cpkgs = "mgcv"
)

prediction.rcp45.2080_2099.curnutr.pa <- classify(prediction.rcp45.2080_2099.curnutr,
                                                  rclmat_main,
                                                  include.lowest = TRUE)

# save the spatrasters
writeRaster(prediction.rcp45.2080_2099.curnutr,
            file = paste0("prediction/prediction_RCP45_2080_2099_curnutr_p_",  sp_workname, ".tif"),
            gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
            NAflag = -3.4e+38,
            overwrite=TRUE)

writeRaster(prediction.rcp45.2080_2099.curnutr.pa,
            file = paste0("prediction/prediction_RCP45_2080_2099_curnutr_bin_",  sp_workname, ".tif"),
            datatype = "INT1U",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
            overwrite=TRUE)


### 3.3.3. RCP85 2040-2059 current nutrients ----

prediction.rcp85.2040_2059.curnutr = terra::predict(
  object = env.stack.rcp85.2040.2059,
  model = mdl.final,
  cores = 1,
  se.fit = FALSE,
  type = "response",
  cpkgs = "mgcv"
)

prediction.rcp85.2040_2059.curnutr.pa <- classify(prediction.rcp85.2040_2059.curnutr,
                                                  rclmat_main,
                                                  include.lowest = TRUE)

# save the spatrasters
writeRaster(prediction.rcp85.2040_2059.curnutr,
            file = paste0("prediction/prediction_RCP85_2040_2059_curnutr_p_",  sp_workname, ".tif"),
            gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
            NAflag = -3.4e+38,
            overwrite=TRUE)

writeRaster(prediction.rcp85.2040_2059.curnutr.pa,
            file = paste0("prediction/prediction_RCP85_2040_2059_curnutr_bin_",  sp_workname, ".tif"),
            datatype = "INT1U",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
            overwrite=TRUE)

### 3.3.4. RCP85 2080-2099 current nutrients ----

prediction.rcp85.2080_2099.curnutr = terra::predict(
  object = env.stack.rcp85.2080.2099,
  model = mdl.final,
  cores = 1,
  se.fit = FALSE,
  type = "response",
  cpkgs = "mgcv"
)

prediction.rcp85.2080_2099.curnutr.pa <- classify(prediction.rcp85.2080_2099.curnutr,
                                                  rclmat_main,
                                                  include.lowest = TRUE)

# save the spatrasters
writeRaster(prediction.rcp85.2080_2099.curnutr,
            file = paste0("prediction/prediction_RCP85_2080_2099_curnutr_p_",  sp_workname, ".tif"),
            gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
            NAflag = -3.4e+38,
            overwrite=TRUE)

writeRaster(prediction.rcp85.2080_2099.curnutr.pa,
            file = paste0("prediction/prediction_RCP85_2080_2099_curnutr_bin_",  sp_workname, ".tif"),
            datatype = "INT1U",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
            overwrite=TRUE)

### 3.3.5. plot the binary predictions as tiffs ----

### current prediction----- 

p.current.bin <- plot_bin_map(prediction.current.pa, title = "Current")

ggsave(
  plot = p.current.bin, 
  filename = paste0("plots/", species_name, "_current_bin_prediction.tiff"), 
  width = 15,
  height = 10, 
  compression = "lzw",
  units = "in", 
  dpi = 300
)

#### RCP45 2040-2060 curnutr ----

p.rcp45.40.60.bin <- plot_bin_map(prediction.rcp45.2040_2059.curnutr.pa, title = "RCP45 2040-2060")

ggsave(
  plot = p.rcp45.40.60.bin, 
  filename = paste0("plots/", species_name, "_RCP45_2040_2060_bin_prediction.tiff"), 
  width = 15,
  height = 10, 
  compression = "lzw",
  units = "in", 
  dpi = 300
)

#### RCP45 2080-2099 curnutr ----

p.rcp45.80.99.bin <- plot_bin_map(prediction.rcp45.2080_2099.curnutr.pa, title = "RCP45 2080-2099")

ggsave(
  plot = p.rcp45.80.99.bin, 
  filename = paste0("plots/", species_name, "_RCP45_2080_2099_bin_prediction.tiff"), 
  width = 15,
  height = 10, 
  compression = "lzw",
  units = "in", 
  dpi = 300
)

#### RCP85 2040-2060 curnutr ----

p.rcp85.40.60.bin <- plot_bin_map(prediction.rcp85.2040_2059.curnutr.pa, title = "RCP85 2040-2060")

ggsave(
  plot = p.rcp85.40.60.bin, 
  filename = paste0("plots/", species_name, "_RCP85_2040_2060_bin_prediction.tiff"), 
  width = 15,
  height = 10, 
  compression = "lzw",
  units = "in", 
  dpi = 300
)

#### RCP85 2080-2099 curnutr ----

p.rcp85.80.99.bin <- plot_bin_map(prediction.rcp85.2080_2099.curnutr.pa, title = "RCP85 2080-2099")

ggsave(
  plot = p.rcp85.80.99.bin, 
  filename = paste0("plots/", species_name, "_RCP85_2080_2099_bin_prediction.tiff"), 
  width = 15,
  height = 10, 
  compression = "lzw",
  units = "in", 
  dpi = 300
)

# 4. delta conversion of the probabilities ----

delta.conversion(
  prob.in = prediction.current,
  bin.in = prediction.current.pa,
  pb.grid = pb.grid,
  scen.name = "current"
)

delta.conversion(
  prob.in = prediction.rcp45.2040_2059.curnutr,
  bin.in = prediction.rcp45.2040_2059.curnutr.pa,
  pb.grid = pb.grid,
  scen.name = "RCP45_2040_2060"
)

delta.conversion(
  prob.in = prediction.rcp45.2080_2099.curnutr,
  bin.in = prediction.rcp85.2080_2099.curnutr.pa,
  pb.grid = pb.grid,
  scen.name = "RCP45_2080_2099"
)

delta.conversion(
  prob.in = prediction.rcp85.2040_2059.curnutr,
  bin.in = prediction.rcp85.2040_2059.curnutr.pa,
  pb.grid = pb.grid,
  scen.name = "RCP85_2040_2060"
)

delta.conversion(
  prob.in = prediction.rcp85.2080_2099.curnutr,
  bin.in = prediction.rcp85.2080_2099.curnutr.pa,
  pb.grid = pb.grid,
  scen.name = "RCP85_2080_2099"
)


# 5. modelling end, move log, R and sh files to out folder ----
# the run folder is the models folder 

run.end.time = Sys.time()

print(
  paste(
    "Modeling run ended, run duration", run.end.time - run.start.time
  )
)

if (!testing) {
  file.copy(from = paste0(model.dir,"/", sp_workname, ".R"), to = paste0(model.dir, "/", modelling.tag, "/", sp_workname, ".R"))
  file.copy(from = paste0(model.dir, "/",sp_workname, ".sh"), to = paste0(model.dir,  "/", modelling.tag, "/", sp_workname, ".sh"))
  file.copy(from = paste0(model.dir, "/",sp_workname, "_ERROR.txt"), to = paste0(model.dir, "/", modelling.tag, "/", sp_workname, "_ERROR.txt"))
  file.copy(from = paste0(model.dir,"/", sp_workname, "_OUT.txt"), to = paste0(model.dir, "/",  modelling.tag, "/", sp_workname, "_OUT.txt"))
  
  file.remove(paste0(model.dir, "/", sp_workname, ".R"))
  file.remove(paste0(model.dir, "/", sp_workname, ".sh"))
  file.remove(paste0(model.dir, "/", sp_workname, "_ERROR.txt"))
  file.remove(paste0(model.dir, "/", sp_workname, "_OUT.txt"))
}

