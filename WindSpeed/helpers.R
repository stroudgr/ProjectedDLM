source("WindSpeed/initialization.R")


# A helper function to load the dataset of the given name.
load_dataset = function(dataset, impute=TRUE) {
  
  data_path = all_data_paths[[dataset]]
  
  speed_dir_data = read.csv(data_path)
  
  # ----------------------------------------------------------------------------
  # 1. Buffalo Airport dataset.
  # ----------------------------------------------------------------------------
  if (dataset == "buffalo") {
    degrees = speed_dir_data[["wind_direction"]]
    x = speed_dir_data[["wind_speed"]]
    
    TT = length(x)
    
    if (TT > 128) {
      # Part of dataset with no missing values.
      period = 38 + 1:128
      x = x[period]
      degrees = degrees[period]
      TT = length(x)
    }
    
    a = degrees2radians(degrees)
    # U = radians2unitcircle(a)
    
    if (impute) {
      x = na_interpolation(x)
      a = na_interpolation(a)
      # U = radians2unitcircle(a)
    }
    
    return(list(a=a, x=x))
    
    
  } 
  # ----------------------------------------------------------------------------
  # 2. Santa Ana airport during the January 2025 wildfires.
  # ----------------------------------------------------------------------------
  else if (dataset == "santa_ana") {
    degrees = speed_dir_data[["drct"]]
    x = speed_dir_data[["sknt"]]
    a = degrees2radians(degrees)
    # U = radians2unitcircle(a)
    
    if (impute) {
      x = na_interpolation(x) 
      a = na_interpolation(a)
      # U = radians2unitcircle(a)
    }
    
    return(list(a=a, x=x))
    
  }
  
  stop("Error in helper.R function load_dataset: Not a valid dataset.")
  
}
