# Protect Baltic future SDM modelling 
# Antti Takolander, Finnish Environment Institute 
# 08 / 2026

calibration_plot <- function(obs, pred_prob, n.bins = 10) {
  
  breaks = quantile(pred_prob, probs = seq(0, 1, by = 1/n.bins))
  
  # are breaks unique?
  if(length(unique(breaks)) != length(breaks)) {
    
    return(ggplot() + theme_void() + ggtitle("Non-unique breaks"))
    
  } else {
    
    # Bin predicted probabilities into 10 equal-frequency bins
    bins <- cut(pred_prob, 
                breaks = breaks, 
                include.lowest = TRUE, 
                labels = paste0("bin", 1:n.bins))
    
    # Summarise per bin
    df <- data.frame(pred = pred_prob, obs = obs, bin = bins) %>%
      group_by(bin) %>%
      summarise(
        mean_pred = mean(pred),   # mean predicted probability
        obs_frac  = mean(obs),    # observed fraction of presences
        n         = n()           # number of observations per bin
      )
    
    df.raw = data.frame(pred = pred_prob, obs = obs, bin = bins) 
    
    # Plot 1 
    p1 = ggplot(df, aes(x = mean_pred, y = obs_frac)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
      scale_size_continuous(name = "n obs") +
      labs(
        x     = "Mean predicted probability",
        y     = "Observed fraction of presences",
        title = "Calibration plot"
      ) +
      theme_bw()
    
    p2 = ggplot(data = df) +
      geom_bar(aes(x = as.numeric(bin) + 0.25, y = obs_frac, fill = "Proportion of presences"), 
               stat = "identity", col = "lightskyblue", width = 0.5) +
      geom_boxplot(
        data = df.raw, 
        aes(x = as.numeric(bin) -0.25, y = pred, group = bin, color = "Model prediction"), width = 0.5) + 
      # geom_point(aes(x = as.numeric(p.bin), y = pred.bin, color = "Model prediction")) +
      # geom_line(aes(x = as.numeric(p.bin), y = pred.bin, color = "Model prediction")) +
      scale_fill_manual(values = c("Proportion of presences" = "lightskyblue")) +
      scale_color_manual(values = c("Model prediction" = "black")) +
      scale_x_continuous(breaks = 1:10, labels = levels(df$bin)) +
      labs(fill = NULL, color = NULL) +
      theme_bw() + 
      ylab("Proportion 1 | mean p in bin") + 
      xlab("Probability bin")
    
    return(gridExtra::grid.arrange(p1, p2))
    
  }
}

plot_density_threshold <- function(data.in, threshold) {
  
  # data.in needs to have y and pred columns 
  temp.validation = data.in
  
  e <- dismo::evaluate(
    p = temp.validation[temp.validation$y == 1, "pred"] %>% unlist() %>% as.vector(),
    a = temp.validation[temp.validation$y == 0, "pred"] %>% unlist() %>% as.vector()
  )
  
  #threshold which maximizes sensitivity and specificity
  th.max.sens.spec <- dismo::threshold(e)[2] %>% unlist()
  
  # convert to binary prediction 
  temp.validation$pred.bin <- ifelse(temp.validation$pred >= th.max.sens.spec, 1, 0)
  
  
  p.dens = ggplot(
    data = temp.validation, 
    aes(y = pred, fill = factor(y))
  ) + 
    geom_density() + 
    geom_hline(yintercept = th.max.sens.spec, lty = 2) +
    scale_fill_manual(values = c("orange", "lightskyblue")) + 
    coord_flip() +
    ylab("prediction (p)") + 
    guides(fill = guide_legend("Observed"))
  
  p.hist = ggplot(
    data = temp.validation, 
    aes(y = pred, fill = factor(y))
  ) + 
    geom_histogram() + 
    geom_hline(yintercept = th.max.sens.spec, lty = 2) +
    scale_fill_manual(values = c("orange", "lightskyblue")) + 
    coord_flip() +
    ylab("prediction (p)") + 
    guides(fill = guide_legend("Observed"))
  
  p.both <- gridExtra::grid.arrange(p.dens, p.hist)
  
  return(p.both)
}

plot_bin_map <- function(bin.pred, title = "") {
  
  # bin.pred = as.factor(bin.pred)
  # levels(bin.pred) <- data.frame(
  #   ID = 0:1,  # must match the internal integer codes of the raster
  #   category = c("Absence", "Presence")
  # )
  # 
  # p.bin <- ggplot2::ggplot() + 
  #   geom_spatraster(
  #     data = bin.pred, 
  #     aes(fill = category)
  #   ) + 
  #   scale_fill_manual(
  #     values = c("orangered1", "limegreen"), 
  #     na.value = "transparent", 
  #     drop = TRUE,  
  #     breaks = c("Absence", "Presence"),
  #     labels = c("Absence", "Presence"),
  #     name = "") + 
  #   theme_void()
  #  return(p.bin)
  
  df <- as.data.frame(bin.pred, xy = TRUE)
  colnames(df) <- c("x", "y", "z")
  df$z = factor(df$z)
  plot1 <- ggplot(df, aes(x = x, y = y, fill = z)) +
    geom_raster() +
    scale_fill_manual(name = "Presence", values = c("white", "red")) +
    coord_equal() +
    theme(
      panel.background = element_rect(fill = "grey95"), 
      panel.grid = element_blank(),
      legend.position = "none"
    ) + 
    ggtitle(paste(species_name, title)) + 
    ylab("") + xlab("")
  
  return(plot1)
  
}

plot_cont_map <- function(cont.pred) {
  
  p.cont <- ggplot2::ggplot() + 
    geom_spatraster(
      data = cont.pred, 
      aes(fill = fit)
    ) + 
    scale_fill_gradient2(na.value = "gray90") + 
    theme_void()
  
  return(p.cont)
  
}

plot_variable_importance <- function(varimportance) {

  p.varimp <- ggplot(
    data = varimportance, 
    aes(x = reorder(variable, -tss.drop.mean), y = tss.drop.mean, fill = variable)
  ) + 
    geom_col() + 
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "none"
      ) +
    xlab("") + ylab("cv TSS drop") + 
    ggtitle(unique(varimportance$species)) + 
    scale_fill_igv() 
  
  return(p.varimp)
}

