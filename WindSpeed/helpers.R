source("WindSpeed/initialization.R")

load_dataset = function(dataset, impute=TRUE) {
   
  #get_index_by_name(dataset)
  
  data_path = all_data_paths[[dataset]]
  
  speed_dir_data = read.csv(data_path)
  
  if (dataset == "buffalo") {
    degrees = speed_dir_data[["wind_direction"]]
    x = speed_dir_data[["wind_speed"]]
    
    TT = length(x)
    
    if (TT > 128) {
      
      period = 38 + 1:128
      x = x[period]
      degrees = degrees[period]
      TT = length(x)
    }
    
    a = degrees2radians(degrees)
    #U = radians2unitcircle(a)
    
    if (impute) {
      x = na_interpolation(x)
      #logx=log(x+1)
      a = na_interpolation(a)
      #U = radians2unitcircle(a)
    }
    
    return(list(a=a, x=x))
    
    
  } else if (dataset == "santa_ana") {
    degrees = speed_dir_data[["drct"]]
    x = speed_dir_data[["sknt"]]
    #TT = length(x)
    a = degrees2radians(degrees)
    
    if (impute) {
      x = na_interpolation(x)
      #logx=log(x+1)
      a = na_interpolation(a)
      #U = radians2unitcircle(a)
    }
    
    return(list(a=a, x=x))
    
  }
  
  stop("Error in helper.R function load_dataset: Not a valid dataset.")
  
}
