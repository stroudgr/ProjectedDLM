source("WindSpeed/models/1A/speed_rw.R")
source("WindSpeed/models/dlm/DLM.R")
source("WindSpeed/models/2A/speed_pdlm_regression.R")
source("WindSpeed/models/1B/speed_TVAR.R")
source("WindSpeed/models/indep/indep.R")
source("WindSpeed/models/4A spline/spline_cond.R")

# TODO get code location for all models...
#TODO infinite recursion here
#source("WindSpeed/models/model.R")

source("WindSpeed/helpers.R")
source("WindSpeed/initialization.R")



#' run_MCMC
#'
#' Run and save MCMC samples for the model list provided on the datasets provided.
#'
#' @param models Description of the first argument, including its type and purpose.
#' @param datasets A list of datasets to create figures for.
#' @param params A list of optional parameters to provide
#'                - root_path - where the data is located. Default to MCMC_PATH
#'                              relative path located in initialization.R
#'                - impute: - either a named list with where impute[[d]] = TRUE
#'                            if and only if we want to impute dataset d.
#'                            If dataset is missing in this list, default is TRUE.
#'                          - or TRUE/FALSE if we want to impute/not impute all
#'                            datasets. 
#'                - verbose - print verbose output or not. For debugging.
#'                            Default is FALSE.
#'                - end_times - A named list, where each entry end_times[d]=t 
#'                              tells to use the first t times points of the 
#'                              time series dataset d. Default is to use the 
#'                              full data for all datasets.
#'                - diagnostics - TRUE/FALSE whether to save MCMC diagnostics 
#'                                figures (traceplots) and whether to view
#'                                Stan output. Default is FALSE.
#'                - rerun - TRUE/FALSE whether to run MCMC even if we have found
#'                          the data saved. Default is FALSE.
#'                          
#' @examples
#' params = list()
#' run_MCMC(models=list("1A"), datasets=list("buffalo"), params)
run_MCMC = function(models, datasets, params=list()) {
  
  root_path = MCMC_PATH
  
  # ----------------------------------------------------------------------------
  # Optional parameter processing
  # ----------------------------------------------------------------------------
  
  if ("root_path" %in% names(params) ) {
    root_path = params[["root_path"]]
  }
  
  verbose = FALSE
  if ( "verbose" %in% names(params)) {
    if (params[["verbose"]] == TRUE ) {
      verbose = TRUE
    }
  }
  
  end_times = list()
  if ("end_times" %in% names(params)) {
    
    end_times = params[["end_times"]]
    if (!is.list(end_times)){
      stop("Error in joint_model_mcmc: end_times must be a list.")
    }
    
  }
  
  # Helper in initialization.R
  impute = extract_impute_list(params, datasets)
  
  diagnostics = FALSE
  if ("diagnostics" %in% names(params)){
    if (params[["diagnostics"]]) {
      diagnostics = TRUE
    }
  }
  
  rerun = FALSE 
  if ("rerun" %in% params(params)) {
    if (params[["rerun"]]) {
      rerun = TRUE
    }
  }
  
  
  if (verbose) {
    cat("Running MCMC with the following params: \n")
    cat(paste0("diagnostics=", diagnostics, "\n"))
    cat(paste0("impute=", impute, "\n"))
    cat(paste0("rerun=", rerun, "\n"))
    cat(paste0("root_path=", root_path, "\n"))
    cat(paste0("end_times=", end_times, "\n"))
  }
  
  MCMC_params = list(diagnostics = diagnostics, verbose=verbose, stan_output=diagnostics)
  # ----------------------------------------------------------------------------
  # Run MCMC for each dataset and model.
  # ----------------------------------------------------------------------------
  for (dataset in datasets) {
    
    # Load data
    {
      data = load_dataset(dataset, impute[[dataset]])
      a = data$a
      x = data$x
      TT = length(x)
      
      # If no entry in end_times
      if (!(dataset %in% names(end_times))){
        end_times[dataset] = TT
      }
      
      # If entry in end_times, only looks at data up to this time.
      # Beneficial for model testing and testing runtime as function of data length.
      end_time = end_times[[dataset]]
      x = x[1:end_time]
      a = a[1:end_time]
      
    }
    
    
    for (model in models) {
      
      # Where to load or save MCMC samples for this model.
      folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      save_path = ifelse((end_times[[dataset]] == TT), "post", paste0("post_Time_", end_times[[dataset]]) )
      save_path = paste0(folder_name, save_path ,".Rdata")
      
      # Don't rerun in this case.
      if (!rerun & file.exists(save_path)) {
        if (verbose){
          cat(paste0("Found saved data for ", model, " on dataset ", dataset, "[1:", TT, "] \n"))
          # post_samples = get(load(save_path))
        }
        next
      }
      
      
      if (verbose) {
        cat(paste0("Running model ", model, " on dataset ", dataset, "[1:", end_time, "] \n"))
      }
      
      
      if (model == "1A") {
        post_samples = speed_rw_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
    
      } else if (model == "1B") {
        post_samples = speed_tvar_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
        
      } else if (model == "2A") {
        post_samples = speed_pdlm_regression_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
        
      } else if (model == "3A") {
        post_samples = indep_posterior_samples(a,x, replicates=TRUE, params=MCMC_params)
        
      } else if (model == "4A") {
        post_samples = spline_posterior_samples(a, x, replicates=TRUE, params=MCMC_params)
        
      } else if (model == "4Aii") {
        post_samples = spline_posterior_samples(a, x, replicates=TRUE, params=MCMC_params, spatial_confound=TRUE)
        
      } else if (model == "dlm") { 
        post_samples = dlm_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
      
      } else {
        stop("No model of name ", model  , " found, might still need to be implemented.")
        
      }
      
      save(post_samples, file = save_path)
    
    } # End for loop over models
  
  } # End for loop over datasets
  
} # End function




#' forecast_samples
#'
#' Create and save forecast samples
#'
#' @param models Description of the first argument, including its type and purpose.
#' @param datasets A list of datasets to create figures for.
#' @param H number of steps ahead to forecast. Default to 1.
#' @param params A list of optional parameters to provide
#'                - root_path - where the data is located. Default to MCMC_PATH
#'                              in initialization.R
#'                - impute: - either a named list with where impute[[d]] = TRUE
#'                            if and only if we want to impute dataset d.
#'                            If dataset is missing in this list, default is TRUE.
#'   TODO!                       - or TRUE/FALSE if we want to impute/not impute all
#'                            datasets. 
#'  not same as              - verbose - print verbose output or not. For debugging.
#'  1st function!                          Default is FALSE.
#'                - end_times - A named list, where each entry end_times[d]=t 
#'                              tells to use the first t times points of the 
#'                              time series dataset d. Default is to use the 
#'                              full data for all datasets.
#'                - diagnostics - TRUE/FALSE whether to save MCMC diagnostics 
#'                                figures (traceplots) and whether to view
#'                                Stan output. Default is FALSE.
#'                - rerun - TRUE/FALSE whether to run MCMC even if we have found
#'                          the data saved. Default is FALSE.
#'                          
#' @examples
#' params = list()
#' forecast_samples(models=list("1A"), datasets=list("buffalo"), H=1, params=params)
forecast_samples = function(models, datasets, last_data_times, H=1, params=list()){
  
  root_path = MCMC_PATH
  
  # ----------------------------------------------------------------------------
  # Optional parameter processing
  # ----------------------------------------------------------------------------
  verbose = TRUE # TODO handle all optional parameters
  
  rerun = TRUE
  
  if ("root_path" %in% names(params) ) {
    root_path = params[["root_path"]]
  }
  
  verbose = FALSE
  if ( "verbose" %in% names(params)) {
    if (params[["verbose"]] == TRUE ) {
      verbose = TRUE
    }
  }
  
  #times = params[["time_range"]][[dataset]]
  
  for (dataset in datasets) {
    if (!( dataset %in% names(last_data_times))){
      stop(paste0("Need a list of times for each dataset. Missing for ", dataset, " dataset."))
    }
  }
  
  # Helper in initialization.R
  impute = extract_impute_list(params, datasets)
  
  diagnostics = FALSE
  if ("diagnostics" %in% names(params)) {
    if (params[["diagnostics"]]) {
      diagnostics = TRUE
    }
  }
  
  rerun = FALSE 
  if ("rerun" %in% names(params)) {
    if (params[["rerun"]]==TRUE) {
      rerun = TRUE
    }
  }
  
  
  if (verbose) {
    cat("Running MCMC with the following params: \n")
    cat(paste0("diagnostics=", diagnostics, "\n"))
    cat(paste0("impute=", impute, "\n"))
    cat(paste0("rerun=", rerun, "\n"))
    cat(paste0("root_path=", root_path, "\n\n"))
    # cat(paste0("last_data_times=", last_data_times, "\n"))
  }
  
  
  
  
  # ----------------------------------------------------------------------------
  # Run or load MCMC, and use it to forecast from the posterior predicitive
  # distribution.
  # ----------------------------------------------------------------------------
  for (dataset in datasets) {
    
    # Load dataset
    data_contents = load_dataset(dataset)
    x = data_contents$x
    a = data_contents$a
    TT = length(x)
    
    #TODO this never gets used.
    params_copy = params
    
    times = last_data_times[[dataset]]
    
    if (max(times) > TT) {
      stop(paste0("Times included for dataset ", dataset," exceed size of dataset.\n"))
    }
    
    
    for (model in models) {
        
        # Where the MCMC is located for this model, dataset pair.
        save_path = paste0(root_path, "/", model, "/forecast_samples/", dataset, "/forecast.Rdata")
        
        
        if (!rerun & file.exists(save_path)) {
          
          if (verbose){
            cat(paste0("Found saved data for ", model, " on dataset ", dataset, "[1:", TT, "] \n"))
            # f_samples = get(load(save_path))
          }
          next
        }
        
        
        
        if (model == "1A") {
          f_samples = speed_rw_forecast_samples(x, a, custom_times = times) 
          
        } else if (model == "1B") {
          f_samples = speed_tvar_forecast_samples(x, a, custom_times = times)
          
        } else if (model == "2A") {
          f_samples = speed_pdlm_regression_forecast_samples(x, a, custom_times = times)
          
        } else if (model == "2Ai"){
          #TODO
          f_samples = two_A_i(x, a, custom_times = times, verbose = verbose)
          
        } else if (model == "3A") {
          f_samples = indep_forecast_samples(x,a, custom_times = times)
          
        } else if (model == "4A") {
          f_samples = spline_forecast_samples(x,a, custom_times = times)
          
        } else if (model == "4Aii"){
          f_samples = spline_forecast_samples(x,a, custom_times = times, spatial_confound = TRUE)
          
        } else if (model == "dlm") { 
          f_samples = dlm_forecasting(x, a, custom_times = times)
          
        } else {
          stop("No model of name ", model  , " found, might still need to be implemented.")
          
        }
        
        save(f_samples, file = save_path)
      
      } # end for model loop
    } # end dataset loop
} #end function

