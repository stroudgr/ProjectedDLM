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


get_impute_list = function(datasets, params){
  impute = list()
  if (!("impute" %in% names(params))){
    params["impute"] = TRUE #default
  }
  
  if (params[["impute"]] == TRUE | params[["impute"]] == FALSE) {
    for (dataset in datasets) {
      impute[dataset] = params[["impute"]]
    }
  } else { # If I pass in a custom list to specify for each dataset.
    impute = params[["impute"]] 
    
    if (!is.list(impute) | is.null(names(impute))) {
      stop("Error: impute in parameter list must be boolean or a named list.\n")
    }
    
    for (dataset in datasets){
      if (!(dataset %in% names(impute))){
        stop("Error: impute list must specify for all datasets. Missing for dataset ", dataset, ".\n")
      }
    }
  }
  return(impute)
}


get_optional_boolean_variable = function(var, lst, default_value) {
  
  value = default_value
  if ( var %in% names(lst)) {
    
    if (!is.logical(lst[[var]]) ) {
      stop("Error: ", var, " should be set to a boolean value.")
    } else if (!is.logical(default_value)){
      stop("Error: default_value must be boolean value.")
    }
    
    # This if shouldn't be necessary, but it sometimes didn't work without it.
    if (lst[[var]] == !default_value ) {
      value = !default_value
    }
  }
  return(value)
  
}