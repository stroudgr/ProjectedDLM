buffalo_one_step = function(model, time_steps,  params) {
  
  buffalo_data = load_dataset("buffalo")
  x = buffalo_data$x
  a = buffalo_data$a
  
  m=model_indices[[model]]
  
  {  
  root_path = "WindSpeed/experiments/model_testing/visualize_1_step_forecast_dists/1_buffalo/"
    
  
  
  
  for (time_of_interest in time_steps) {
      
      png(paste0(root_path, "data_figures/",
                 "buff_speed_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
      plot(x, type = "l",
           main = "Buffalo Wind Speed over time",
           xlab = "Time (hours)",
           ylab = "mph")
      
      abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
      dev.off()
      
      png(paste0(root_path,"data_figures/",
                 "buff_dir_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
      plot(a, type = "l",
           main = "Buffalo Wind direction over time",
           xlab = "Time (hours)",
           ylab = "Angle (radians)")
      abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
      dev.off()
  }
  }
  
  
  buffalo_data = load_dataset("buffalo")
  x = buffalo_data$x
  logx = log(x+1)
  a = buffalo_data$a
  
  MCMC_path = paste0("WindSpeed/experiments/MCMC/saved_MCMC/", model, "/forecast_samples/buffalo/forecast.Rdata")
  
  root_path = "WindSpeed/experiments/model_testing/visualize_1_step_forecast_dists/1_buffalo/results/"
  #for (model in models) {
  
  forecast_samples = get(load(MCMC_path))
  #cat(paste0(model, time_steps, " loaded for Buffalo\n"))
    
  p_max=1
  
  #
  # Histogram
  #
  
  #ggplot(forecast_samples$speed_forecasts[,TT], aes(x=weight)) + geom_histogram()
  
  for (t in time_steps) { 
    
    speed_forecast_samples = forecast_samples$speed_forecasts[,t]
    direction_forecast_samples = forecast_samples$direction_forecasts[,,t]
    
    angle_forecast_samples = unitcircle2radians(direction_forecast_samples)
    
    png(paste0(root_path,
               t, "/",
               "model_", model_alphanumeric_identifiers[m],"_forecast_angle_hist_time", t, ".png"), width = 800, height = 600, res = 100)
    
    blah = paste0(root_path,
                  t, "/",
                  "model_", model_alphanumeric_identifiers[m],"_forecast_angle_hist_time", t, ".png")
    cat(paste0(blah, "\n"))
    
    hist(angle_forecast_samples,
         main = paste("Buffalo : One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
         xlab = "angle",
         ylab = "Frequency"
    )
    legend("topleft", c(paste("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
    abline(v = a[t-1], col = "red", lty = 2, lwd = 2)
    abline(v = a[t], col = "green", lty = 2, lwd = 2)
    
    
    dev.off()
    
    png(paste0(root_path,
               t, "/",
               "model_", model_alphanumeric_identifiers[m],"_forecast_speed_hist_time", t, ".png"), width = 800, height = 600, res = 100)
    
    hist(speed_forecast_samples,
         main = paste("One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
         xlab = "log(Speed+1) (mph)",
         ylab = "Frequency"
    )
    legend("topleft", c(paste("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
    abline(v = logx[t-1], col = "red", lty = 2, lwd = 2)
    abline(v = logx[t], col = "green", lty = 2, lwd = 2)
    
    dev.off()
  } # end time loop
  
  #} # end model for loop
  
  
}

santa_ana_one_step = function(model, time_steps,  params){
  
  stop("Not implemented error for santa_ana_one_step")
  
  {
  root_path = "WindSpeed/experiments/model_testing/visualize_1_step_forecast_dists/2_santa_ana/"
  
    for (time_of_interest in time_steps) {
      
      png(paste0(root_path, "data_figures/",
                 "santa_ana_speed_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
      plot(x, type = "l",
           main = "Santa Wind Speed over time",
           xlab = "Time (minutes)",
           ylab = "mph")
      
      abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
      dev.off()
      
      png(paste0(root_path, "data_figures/",
                 "santa_ana_dir_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
      plot(a, type = "l",
           main = "Santa Ana Wind direction over time",
           xlab = "Time (minutes)",
           ylab = "Angle (radians)")
      abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
      dev.off()
    }
  
  }# end grouping

  
  santa_ana_data = load_dataset("santa_ana")
  x = santa_ana_data$x
  logx = log(x+1)
  a = santa_ana_data$a
  
  MCMC_path = paste0("WindSpeed/experiments/MCMC/saved_MCMC/", model, "/forecast_samples/santa_ana/forecast.Rdata")
  
  root_path = "WindSpeed/experiments/model_testing/visualize_1_step_forecast_dists/2_santa_ana/results/"
  #for (model in models) {
  
  forecast_samples = get(load(MCMC_path))
  cat(paste0(model, time_steps, " loaded for Santa Ana\n"))
  
  p_max=1
  
  for (t in time_steps) { 
    
    speed_forecast_samples = forecast_samples$speed_forecasts[,t]
    direction_forecast_samples = forecast_samples$direction_forecasts[,,t]
    
    angle_forecast_samples = unitcircle2radians(direction_forecast_samples)
    
    png(paste0(root_path,
               t, "/",
               "model_", model_alphanumeric_identifiers[m],"_forecast_angle_hist_time", t, ".png"), width = 800, height = 600, res = 100)
    
    blah = paste0(root_path,
                  t, "/",
                  "model_", model_alphanumeric_identifiers[m],"_forecast_angle_hist_time", t, ".png")
    cat(paste0(blah, "\n"))
    
    hist(angle_forecast_samples,
         main = paste("Santa Ana : One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
         xlab = "angle",
         ylab = "Frequency"
    )
    legend("topleft", c(paste0("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
    abline(v = a[t-1], col = "red", lty = 2, lwd = 2)
    abline(v = a[t], col = "green", lty = 2, lwd = 2)
    
    
    dev.off()
    
    png(paste0(root_path,
               t, "/",
               "model_", model_alphanumeric_identifiers[m],"_forecast_speed_hist_time", t, ".png"), width = 800, height = 600, res = 100)
    
    hist(speed_forecast_samples,
         main = paste("Santa Ana: One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
         xlab = "log(Speed+1) (mph)",
         ylab = "Frequency"
    )
    legend("topleft", c(paste("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
    abline(v = logx[t-1], col = "red", lty = 2, lwd = 2)
    abline(v = logx[t], col = "green", lty = 2, lwd = 2)
    
    dev.off()
  } # end time loop
  
  
  
  
  
}



visualize_one_step_ahead = function(models, datasets, times_list, params) {
  
  root_path = "WindSpeed/experiments/MCMC/saved_MCMC/"
  
  
  
  verbose = TRUE
  
  forecast_samples(models=models, datasets=datasets, last_data_times = times_list, params=params)
  
  for (model in models) {
    for (dataset in datasets){
      
      #folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      #save_path = paste0(folder_name, "post.Rdata")
      #post_samples = get(load(save_path))
      
      if (dataset == "buffalo") {
        
        buffalo_one_step(model, time_steps = times_list[["buffalo"]], params)
        
      } else if (dataset == "santa_ana") {
        
        santa_ana_one_step(model, time_steps = times_list[["santa_ana"]], params)
        
      } else {
        stop("No such dataset")
      }
      
    }
  }
  
  
  
}