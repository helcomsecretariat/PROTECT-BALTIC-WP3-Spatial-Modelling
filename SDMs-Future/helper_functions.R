# Protect Baltic future SDM modelling 
# Antti Takolander, Finnish Environment Institute 
# 08 / 2026


fit.gam <- function(data_gam, mdl_formula, gamma = 1.7) {

    mdl <- mgcv::gam(
      formula = mdl_formula,
      data = data_gam,
      family = binomial(link = "logit"),
      method = "REML",
      select = T,
      gamma = gamma)
    return(mdl)
}

# fit.gam <- function(data_gam, mdl_formula, sal_offset = FALSE, gamma = 1.7) {
# 
#   if(sal_offset) {
#     mdl <- mgcv::gam(
#       formula = mdl_formula,
#       data = data_gam,
#       family = binomial(link = "logit"),
#       method = "REML",
#       select = T,
#       gamma = gamma,
#       offset = sal_offset) # predicted into the data by salinity model
#     return(mdl)
#   }
# 
#   else {
#     mdl <- mgcv::gam(
#       formula = mdl_formula,
#       data = data_gam,
#       family = binomial(link = "logit"),
#       method = "REML",
#       select = T,
#       gamma = gamma)
#     return(mdl)
#   }
# }

calc.auc <- function(model.in, test.data) {
  
  temp.validation = data.frame(
    y = test.data$y, 
    pred = predict(
      model.in,
      newdata = test.data,
      type = "response"
    )
  )
  
  e <- dismo::evaluate(
    p = temp.validation[temp.validation$y == 1, "pred"],
    a = temp.validation[temp.validation$y == 0, "pred"]
  )
  
  return(e@auc)
}

calc.tjurr2 = function(model.in, test.data) {
  
  temp.validation = data.frame(
    y = test.data$y, 
    pred = predict(
      model.in,
      newdata = test.data,
      type = "response"
    )
  )
  
  preds.at.obs1 = temp.validation[temp.validation$y == 1, "pred"]
  preds.at.obs0 = temp.validation[temp.validation$y == 0, "pred"]
  
  if (length(preds.at.obs1) == 0 || length(preds.at.obs0) == 0)
    stop("Test data contains only one class — Tjur R2 undefined")
  
  tr2 = sum(preds.at.obs1) / length(preds.at.obs1) - sum(preds.at.obs0) /
    length(preds.at.obs0)
  return(tr2)
}

# calculate sensitivity, specificity, TSS score
calc.sens.spec.tss <- function(model.in, test.data){
  
  temp.validation = data.frame(
    y = test.data$y, 
    pred = predict(
      model.in,
      newdata = test.data,
      type = "response"
    )
  )
  
  e <- dismo::evaluate(
    p = temp.validation[temp.validation$y == 1, "pred"] %>% as.vector(),
    a = temp.validation[temp.validation$y == 0, "pred"] %>% as.vector()
  )
  
  #threshold which maximizes sensitivity and specificity
  th.max.sens.spec <- dismo::threshold(e)[2] %>% unlist()
  
  # convert to binary prediction 
  temp.validation$pred.bin <- ifelse(temp.validation$pred >= th.max.sens.spec, 1, 0)
  
  # N true positives
  n.t.pos <- temp.validation %>% filter(y ==1 & pred.bin == 1) %>% nrow()
  # N true negatives 
  n.t.neg <- temp.validation %>% filter(y ==0 & pred.bin == 0) %>% nrow()
  # N false positives
  n.f.pos <- temp.validation %>% filter(y == 0 & pred.bin == 1) %>% nrow()
  # N false negatives 
  n.f.neg <- temp.validation %>% filter(y == 1 & pred.bin == 0) %>% nrow()
  
  # sensitivity
  sens.temp <- n.t.pos / (n.t.pos + n.f.neg)
  
  # specificity
  spec.temp <- n.t.neg / (n.t.neg + n.f.pos)
  
  # TSS 
  tss.temp <- sens.temp + spec.temp -1
  
  # return data 
  out.data <- data.frame(
    sensitivity = sens.temp, 
    specificity = spec.temp, 
    tss = tss.temp
  )
  
  return(out.data)
  
}

calc.varimportance <- function(model.in, test.data, var.list) {
  
  varimp.out <- tibble(
    variable = var.list, 
    delta.tss = rep(NA_real_, times = length(var.list)), 
    delta.sens = rep(NA_real_, times = length(var.list)),
    delta.spec = rep(NA_real_, times = length(var.list))
  )
  
  full.model.stats <- calc.sens.spec.tss(model.in, test.data)
  
  for(v in seq_along(var.list)) {
    
    variable.v <- var.list[v]
    
    print(paste("Permuting variable", v, variable.v))
    
    test.data.v = test.data
    test.data.v[[variable.v]] <- sample(test.data[[variable.v]]) # shuffle var v 
    
    stats.v <- calc.sens.spec.tss(model.in, test.data.v)
    
    # save the results
    varimp.out$variable[v] <- variable.v
    varimp.out$delta.tss[v]  <- full.model.stats$tss - stats.v$tss
    varimp.out$delta.sens[v] <- full.model.stats$sensitivity - stats.v$sensitivity
    varimp.out$delta.spec[v] <- full.model.stats$specificity - stats.v$specificity
    
  }
  
  return(varimp.out)
  
}

# delta conversion 
delta.conversion <- function(prob.in , bin.in, pb.grid, scen.name = "") {
  print("Starting delta conversion ---- ")
  
  prob.in <- prob.in*1000
  prob.in <- round(prob.in, digits = 0)
  
  out_delta <- prob.in
  out_delta[bin.in == 0] <- 0 # binary mask
  out_delta_prob <- out_delta
  out_delta_prob <- out_delta_prob*pb.grid
  out_delta[out_delta == 0] <- NA
  
  out_delta <- ((out_delta - minmax(out_delta)[1,]) / (minmax(out_delta)[2,] - minmax(out_delta)[1,])) * (1000 - 1) + 1
  out_delta <- round(out_delta)
  out_delta[is.na(out_delta)] <- 0
  out_delta <- out_delta*pb.grid
  
  # Export delta model
  writeRaster(
    x = out_delta_prob, 
    filename = paste0("prediction/", scen.name, "_delta_", sp_workname, ".tif"), 
    gdal = c("COMPRESS = DEFLATE", "TILED = YES"),
    datatype = "INT2U",
    overwrite=TRUE
  )
  
  writeRaster(
    x = out_delta, 
    filename = paste0("prediction/", scen.name, "_delta_standardized_", sp_workname, ".tif"), 
    datatype = "INT2U",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
    overwrite=TRUE
  )
}





