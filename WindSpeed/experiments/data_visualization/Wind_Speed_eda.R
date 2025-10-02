source("WindSpeed/helpers.R")
source("WindSpeed/initialization.R")
source("WindSpeed/models/4A spline/bases_initialization.R")

#' create_dataset_figures
#' 
#' More each dataset and model requested, creates multiple visualizations of the
#' data. 
#' 
#' @param datasets A list of datasets to create figures for. 
#' @param params A list of optional parameters to provide
#'                - impute: - either a named list with where impute[[d]] = TRUE
#'                            if and only if we want to impute dataset d.
#'                            If dataset is missing in this list, default is TRUE.
#'                          - or TRUE/FALSE if we want to impute/not impute all
#'                            datasets. 
#' @examples
#' create_dataset_figures(list("buffalo"), list("1A"))
create_dataset_figures = function(datasets, params){
  
  # Removes any datasets that are invalid. 
  datasets = valid_datasets(datasets, print_output = TRUE)
  
  root_path = "WindSpeed/experiments/data_visualization/results/"
  
  
  # ----------------------------------------------------------------------------
  # Optional parameter processing
  # ----------------------------------------------------------------------------
  impute = get_impute_list(datasets, params)
  
  # ----------------------------------------------------------------------------
  # Create figures for each dataset.
  # ----------------------------------------------------------------------------
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
    
    bases = c(1,2,3, 4)
    create_OLS_regression_plots(dataset, root_path, bases = bases)
    
  }
}




# 
#
# Helper functions for each individual plot below:
#
#

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
  } else {
    stop("create_OLS_regression_plots : no such dataset ", dataset, "\n")
  }
  
  #smaller_indices = seq(from = 1, to=TT, by=10)
  #x = x[smaller_indices]
  #a = a[smaller_indices]
  
  TT = length(x)
  
  # Create data frame.
  # Add a little noise for less overlapping of points.
  df <- data.frame(x = x + rnorm(TT, 0, 0.2), y = a, index = 1:TT)
  #sub_index = seq(1, nrow(df), by = 5)
  sub_index = 1:nrow(df)
  #df <- df[sub_index, ]
  
  p = ggplot(df, aes(x = x[sub_index], y = a[sub_index], color = 1:nrow(df))) +
    geom_point(size = 0.5) +
    scale_color_gradient(low = "blue", high = "red") +
    #geom_path(color = "blue")+
    labs(title = paste0(angle_v_speed_title, ", Colored by Index"), 
         x="Speed (mph)", y="Angle (radians)", color = "Time (index)") +
    theme_minimal()
  
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
    speed_lab = "Speed (knots)"
    
  } else if (dataset == "santa_ana") {
    title = "Santa Ana time versus "
    folder_name = paste0(root_path, "2. santa_ana/")
    xlab = "Time (minutes)"
    speed_lab = "Speed (mph)"
  } else {
    stop("create_OLS_regression_plots : no such dataset ", dataset, "\n")
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


# ------------------------------------------------------------------------------
# 3. OLS plot
# ------------------------------------------------------------------------------

create_OLS_regression_plots = function(dataset, root_path, bases=c(1,2,3), verbose=FALSE) {
  
  
  if (dataset == "buffalo") {
    fname = paste0(root_path, "1. buffalo/")
    xa = load_dataset("buffalo")
    x = xa$x
    a = xa$a
    U = radians2unitcircle(a)
    
  } else if (dataset == "santa_ana") {
    
    fname = paste0(root_path, "2. santa_ana/")
    xa = load_dataset("santa_ana")
    x = xa$x
    a = xa$a
    U = radians2unitcircle(a)
    
  } else {
    stop("create_OLS_regression_plots : no such dataset ", dataset, "\n")
  }
  
  for (basis in bases) {
  
  
  
  if (!(basis %in% 1:get_num_bases())){
    cat(paste0("Invalid basis num ", basis  , "\n"))
    next 
  }
    
  a_pred = get_OLS_angle_predictions_for_basis(basis, x, a, U)
  basis_name = bases_names[[basis]]
  
  p = ggplot() +
    geom_point(aes(x = x, y = a), color = "black", alpha = .5) +
    geom_line(aes(x = x, y = a_pred), color = "red", linewidth=1.5)+
    labs(title= paste0("OLS for direction on ", basis_name, " for ", dataset),
         x="Speed (mph)", y="Angle (radians)")
  suppressMessages(
  ggsave(paste0(fname, "OLS_", basis_name, ".png"),  plot=p)
  )
  
  }
}



