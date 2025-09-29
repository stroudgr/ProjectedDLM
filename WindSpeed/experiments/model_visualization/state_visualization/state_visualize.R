# Load ggplot2
library(ggplot2)

bv_Sigma = function(model, post_samples) {
  
  #cat("\n\nCallIT\n\n")
  
  city = "Buffalo"
  city_fname = "1_buffalo"
  TT = 128
  
  root_folder = "WindSpeed/experiments/model_visualization/Sigma_visualization/"
  
  m=model_indices[[model]]
  #for (m in c(1,3,4,5)) 
  
  
  cat("ECHO")
  cat(m)
  
  if (m==1) {
    S_draws = post_samples$Sigma_draws
  } else if (m==3) {
    S_draws = post_samples$direction_forecasts$Sigma
  } else if (m==4) {
    S_draws = post_samples$Sigma_draws
  } else if (m==5) {
    S_draws = post_samples$Sigma_draws
  } else if (m == 6) {
    S_draws = post_samples$Sigma_draws
  }
  Sigma_draws = S_draws
  model_id = model_alphanumeric_identifiers[m]
  
  for (S_index in 1:2) {
    
    
    S_i_draws = Sigma_draws[, S_index,]
    
    n_samples = dim(S_i_draws)[2]
    
  }

}

visualize_states_helper = function(dataset, model, post_samples) {
  
  # TODO
  verbose = TRUE
  root_folder = "WindSpeed/experiments/model_visualization/state_visualization/results/"
  
  city = ""
  city_fname = ""
  TT = dataset_lengths[[dataset]]
  restricted_times = TT
  
  if (dataset == "buffalo"){
    city = "Buffalo"
    city_fname = "1_buffalo"
    
  } else if (dataset == "santa_ana") {
    city = "Santa Ana"
    city_fname = "2_santa_ana"
    
    restricted_times = 1000 # Restrict times to get clearer uncertainty visualization.
    
  }
  
  model_id = get_index_by_dataset_name(dataset)
  
  S_draws = post_samples$S_draws
  
  for (S_index in 1:2) {
    
    S_i_draws = S_draws[, S_index,]
    if (restricted_times <  TT){ 
      #cat("restricting, ", dataset, " to 1:", restricted_times)
      S_i_draws = S_i_draws[1:restricted_times,]
    }
    
    n_samples = dim(S_i_draws)[2]
    
    # Compute quantiles at each time step
    quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    quantiles <- t(apply(S_i_draws, 1, quantile, probs = quantile_levels))
    
    # Create data frame for ggplot
    df <- data.frame(
      time = 1:restricted_times,
      q05 = quantiles[, 1],
      q25 = quantiles[, 2],
      q50 = quantiles[, 3],
      q75 = quantiles[, 4],
      q95 = quantiles[, 5]
    )
    
    
    
    # Fan plot using shaded ribbons
    p=ggplot(df, aes(x = time)) +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.5) +
      geom_line(aes(y = q50), color = "black", size = 1) +
      labs(
        title = paste0("Model ", model_id, " for ", city, ": ", "Fan Plot of state S_" , S_index, " Distribution Over Time"),
        x = "Time",
        y = paste0("S_", S_index , " Value")
      ) +
      theme_minimal()
    
    state_file_name = paste0(root_folder, city_fname, "/", model, "/s", S_index )
    
    if (verbose){
      #cat(paste0("Saving to ", state_file_name, ". \n"))
    }
    
    suppressMessages(
    ggsave(paste0(state_file_name, ".png"), plot = p, width = 6, height = 4, dpi = 300)
    )
  }# end for loop over which dimension of the state to visualize.
  
}


buffalo_visualize_states = function(model, post_samples){
  
  city = "Buffalo"
  city_fname = "1_buffalo"
  TT = dataset_lengths[["buffalo"]]
  
  
  root_folder = "WindSpeed/experiments/model_visualization/state_visualization/"
  
  #m=model_indices[[model]]
  #for (m in c(1,3,4,5)) 
  #cat(m)
  
  S_draws = post_samples$S_draws
  
  #if ()
  
  #if (m==1) {
  #  S_draws = post_samples$S_draws
  #} else if (m==3) {
  #  S_draws = post_samples$pdlm_draws$S
  #} else if (m==4) {
  #  S_draws = post_samples$S_draws
  #} else if (m==5) {
  #  S_draws = post_samples$S_draws
  #} else if (m == 6) {
  #  S_draws = post_samples$S_draws
  #}
  
  #model_id = model_alphanumeric_identifiers[m]
  
  for (S_index in 1:2) {
  
  
  S_i_draws = S_draws[, S_index,]

  n_samples = dim(S_i_draws)[2]
  
  # Compute quantiles at each time step
  quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  quantiles <- t(apply(S_i_draws, 1, quantile, probs = quantile_levels))
  
  # Create data frame for ggplot
  df <- data.frame(
    time = 1:TT,
    q05 = quantiles[, 1],
    q25 = quantiles[, 2],
    q50 = quantiles[, 3],
    q75 = quantiles[, 4],
    q95 = quantiles[, 5]
  )
  
  
  
  # Fan plot using shaded ribbons
  p=ggplot(df, aes(x = time)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.5) +
    geom_line(aes(y = q50), color = "black", size = 1) +
    labs(
      title = paste0("Model ", model_id, " in ", city, " ", "Fan Plot of S_" , S_index, " Distribution Over Time"),
      x = "Time",
      y = paste0("S_", S_index , " Value")
    ) +
    theme_minimal()
  
  state_file_name = paste0(root_folder, city_fname, "/", model_id, "/s", S_index )
  
  
  cat(paste0("Saving to ", state_file_name))
  
  ggsave(paste0(state_file_name, ".png"), plot = p, width = 6, height = 4, dpi = 300)
  }
  
  
  if (m == 3) {
    
    beta_draws = post_samples$pdlm_draws$beta
    
    # Compute quantiles at each time step
    quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    quantiles <- t(apply(beta_draws, 1, quantile, probs = quantile_levels))
    
    beta1 = beta_draws[1,]
    beta2 = beta_draws[2,]
    
    
    # First, speed time series plot.
    png(paste0(root_folder, city_fname, "/", model_id, "/beta_1.png" ), width = 800, height = 600, res = 100)
    hist(beta1)
    dev.off()
    
    png(paste0(root_folder, city_fname, "/", model_id, "/beta_1.png" ), width = 800, height = 600, res = 100)
    hist(beta2)
    dev.off()
  }

}















santa_ana_visualize_states = function(model, post_samples){
  city = "Santa Ana"
  city_fname = "2_santa_ana"
  TT = 8253
  
  root_folder = "WindSpeed/experiments/model_visualization/state_visualization/"
  
  m=model_indices[[model]]
  
  if (m==1) {
      S_draws = post_samples$S_draws
  } else if (m==3) {
      S_draws = post_samples$pdlm_draws$S
  } else if (m==4) {
      S_draws = post_samples$S_draws
  } else if (m==5) {
      S_draws = post_samples$S_draws
  } else if (m == 6) {
      S_draws = post_samples$S_draws
  } 
    
  model_id = model_alphanumeric_identifiers[m]
    
    
  for (S_index in 1:2) {
  
      #restricted_times = 1:1000
      restricted_times = 1:TT
      
      
      S_i_draws = S_draws[, S_index,]
      #S_i_draws = S_i_draws[restricted_times,]
      
      n_samples = dim(S_i_draws)[2]
      
      # Compute quantiles at each time step
      quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      quantiles <- t(apply(S_i_draws, 1, quantile, probs = quantile_levels))
      
      # Create data frame for ggplot
      df <- data.frame(
        time = restricted_times,
        q05 = quantiles[, 1],
        q25 = quantiles[, 2],
        q50 = quantiles[, 3],
        q75 = quantiles[, 4],
        q95 = quantiles[, 5]
      )
      
      
      # Fan plot using shaded ribbons
      p=ggplot(df, aes(x = time)) +
        geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
        geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.5) +
        geom_line(aes(y = q50), color = "black", size = 0.3) +
        labs(
          title = paste0("Model ", model_id, " in ", city, " ", "Fan Plot of S_" , S_index, " Distribution Over Time"),
          x = "Time",
          y = paste0("S_", S_index , " Value")
        ) +
        theme_minimal()
      
      state_file_name = paste0(root_folder, city_fname, "/", model_id, "/s", S_index )
      #ggsave(paste0(state_file_name, ".png"), plot = p, width = 6, height = 4, dpi = 300)
      
      if (m != 3 | m!= 6){
        next
      }
      
      if (m==6){
        
      }
      
      
      if (m == 3) {
        
        beta_draws = post_samples$pdlm_draws$beta
        
        # Compute quantiles at each time step
        quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
        quantiles <- t(apply(beta_draws, 1, quantile, probs = quantile_levels))
        
        beta1 = beta_draws[1,]
        beta2 = beta_draws[2,]
        
        
        #hist(beta1)
        #hist(beta2)
        
      }
      
    }
}