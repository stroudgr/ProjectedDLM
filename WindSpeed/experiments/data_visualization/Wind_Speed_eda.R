source("WindSpeed/helpers.R")

create_dataset_figures = function(datasets, params){
  
  root_path = "WindSpeed/experiments/data_visualization/"
  
  
  impute = list()
  
  if (!("impute" %in% names(params))){
    params["impute"] = TRUE
  }
  
  if (params[["impute"]] == TRUE | params[["impute"]] == FALSE) {
    for (dataset in datasets) {
      impute[dataset] = params[["impute"]]
    }
  } else { # If I pass in a custom list to specify for each dataset.
    impute = params["impute"] 
    # TODO check validity of above. Add to helpers.R?
  }
  
  for (dataset in datasets){ 
  
    {
      data = load_dataset(dataset, impute[[dataset]])
      a = data$a
      x = data$x
    }
    
    # --------------------------------------------------------------------------
    # 1. Angle v speed
    # --------------------------------------------------------------------------
    create_angle_v_speed_figure(dataset, root_path, a, x)
      
    # --------------------------------------------------------------------------
    # 2. Time plots
    # --------------------------------------------------------------------------
    create_time_plots(dataset, root_path, a, x)
    
    # --------------------------------------------------------------------------
    # 3. OLS for different bases
    # --------------------------------------------------------------------------
    
    
    generate_design_matrix <- function(x, knot_vector, degree){
      return(cbind(outer(x,1:degree,"^"),outer(x,knot_vector,">")*outer(x,knot_vector,"-")^degree))
    }
    
    gdm2 = function(x){
      return(cbind(x, (x>10)*1))
    }
    
    gdm3 = function(x){
      return(cbind(x, log(x+1), (x>10)*1))
    }
    
    
    # TODO, these should depend on dataset I think.
    design_matrix <- generate_design_matrix(degree = 1, knot_vector = c(10,20, 30), x = x)
    basis_name = "Linear splines"
    create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)
    
    
    
    design_matrix = gdm2(x)
    basis_name = "Indicator"
    create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)
    
    design_matrix = gdm3(x)
    basis_name = "Indicator and log"
    create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)
    
    
    design_matrix <- generate_design_matrix(degree = 3, knot_vector = c(10,20, 30), x = x)
    basis_name = "cubic splines"
    create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)
    
    
  }
}

# ------------------------------------------------------------------------------
# 1. Angle v speed
# ------------------------------------------------------------------------------
create_angle_v_speed_figure = function(dataset, root_path, a, x) {
  
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


# --------------------------------------------------------------------------
# 2. Time plots
# --------------------------------------------------------------------------
create_time_plots = function(dataset, root_path, a, x) {
  
  title = ""
  
  if (dataset == "buffalo") {
    title = "Buffalo time versus "
    folder_name = paste0(root_path, "1. buffalo/")
    xlab = "Time (hours)"
    speed_lab = "knots"
    
  } else if (dataset == "santa_ana") {
    title = "Santa Ana time versus "
    folder_name = paste0(root_path, "2. santa_ana/")
    xlab = "Time (minutes)"
    speed_lab = "mph"
  }
  
  a_lab = "Angle (radians)"
  
  
  # First, speed time series plot.
  png(paste0(folder_name, "speed.png"), width = 800, height = 600, res = 100)
  plot(x, type = "l",
       main = paste0(title, "speed"),
       xlab = xlab,
       ylab = speed_lab)
  
  dev.off()
  
  # Angle over time
  png(paste0(folder_name, "angle.png"), width = 800, height = 600, res = 100)
  plot(a, type = "l",
       main = paste0(title, "angle"),
       xlab = xlab,
       ylab = a_lab)
  
  dev.off()
  
}


create_OLS_regression_plot = function(dataset, root_path, a, x, basis_name, design_matrix) {
  "
  design_matrix: an T x L matrix, where T = length(x) and the ith row, jth column 
                 of the design_matrix is the application of basis j to data 
                 vector xi.
  "
  
  
  if (dataset == "buffalo") {
    #title = "Buffalo angle over speed"
    fname = paste0(root_path, "1. buffalo/")
    
  } else if (dataset == "santa_ana") {
    #title = "Santa Ana angle over speed"
    fname = paste0(root_path, "2. santa_ana/")
  }
  
  mod_spline <- lm(a~design_matrix)
  #mod_spline <- lm(U~design_matrix)
  
  prediction = predict(mod_spline)
  
  # Use U or a, doesn't seem to make a difference.
  #if (dim(prediction)[2] == 2) {
  #  a_pred = unitcircle2radians(prediction)
  #} else {
    a_pred = prediction
  #}
  
  p= ggplot() +
    geom_point(aes(x = x, y = a), color = "black", alpha = .5) +
    geom_line(aes(x = x, y = a_pred), color = "red", linewidth=1.5)+
    labs(title= paste0("OLS for direction on ", basis_name, " for ", dataset))
  ggsave(paste0(fname, "OLS_", basis_name, ".png"),  plot=p)
  
  
}

