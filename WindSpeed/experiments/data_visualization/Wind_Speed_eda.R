source("WindSpeed/helpers.R")

create_dataset_figures = function(datasets, params){
  
  root_path = "WindSpeed/experiments/data_visualization/"
  
  
  impute = list()
  if (params[["impute"]] == TRUE | params[["impute"]] == FALSE) {
    for (dataset in datasets) {
      impute[dataset] = params["impute"] 
    }
  } else { # If I pass in a custom list to specify for each dataset.
    impute = params["impute"] 
    # TODO check validity of above. Add to helpers.R?
  }
  
  for (dataset in datasets){ 
  
    # ----------------------------------------------------------------------------
    # 1. Angle v speed
    # ----------------------------------------------------------------------------
  
    {
      data = load_dataset(dataset, impute[[dataset]])
      a = data$a
      x = data$x
    
      angle_v_speed_title = ""
      angle_v_speed_fname = ""
      
      if (dataset == "buffalo") {
        angle_v_speed_title = "Buffalo angle over speed"
        angle_v_speed_fname = paste0(root_path, "1. buffalo/buffalo_angle_v_speed")
        
      } else if (dataset == "santa_ana") {
        angle_v_speed_title = "Santa Ana angle over speed"
        angle_v_speed_fname = paste0(root_path, "2. santa_ana/santa_ana_angle_v_speed")
      }
      
      #smaller_indices = seq(from = 1, to=TT, by=10)
      #x = x[smaller_indices]
      #a = a[smaller_indices]
      
      TT = length(x)
      
      
      # Load ggplot2
      library(ggplot2)
      
      # Create data frame
      # Add a little noise for less overall of points.
      df <- data.frame(x = x + rnorm(TT, 0, 0.2), y = a, index = 1:TT)
      #sub_index = seq(1, nrow(df), by = 5)
      sub_index = 1:nrow(df)
      #df <- df[sub_index, ]
      
      # Plot: color by index (time)
      p = ggplot(df, aes(x = x[sub_index], y = a[sub_index], color = 1:nrow(df))) +
        geom_point(size = 0.5) +
        scale_color_gradient(low = "blue", high = "red") +
        #geom_path(color = "blue")+
        labs(title = paste0(angle_v_speed_title, ", Colored by Index"), x="Speed", y="Angle", color = "Time (index)") +
        theme_minimal()
      
      
      
      #ggsave(paste0(angle_v_speed_fname, ".png"), plot = p, width = 6, height = 4, dpi = 300)
      
      ggsave(paste0(angle_v_speed_fname, ".png"), plot = p, width = 6, height = 4, dpi = 300)
      
      
    }
    
    
    
    
  }
}

