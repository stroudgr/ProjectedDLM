#source("WindSpeed/initialization.R")
#source("WindSpeed/helpers.R")

# TODO get code location for all models...
#source("WindSpeed/models/model.R")


source("WindSpeed/models/1A/speed_rw.R")
source("WindSpeed/models/dlm/DLM.R")
source("WindSpeed/models/2A/speed_pdlm_regression.R")
source("WindSpeed/models/1B/speed_TVAR.R")
source("WindSpeed/models/indep/indep.R")

#TODO infinite recursion here
#source("WindSpeed/models/model.R")



source("WindSpeed/models/4A spline/spline_cond.R")
source("WindSpeed/spline_gibbs.R")



run_MCMC = function(models, datasets, params) {
  
  root_path = "WindSpeed/experiments/MCMC/saved_MCMC/" #TODO
  
  if ("root_path" %in% names(params) ) {
    root_path = params[["root_path"]]
  }
  
  verbose = FALSE
  
  if (params[["verbose"]]){
    verbose = TRUE
  }
  
  end_times = list()
  if ("end_times" %in% names(params)) {
    
    # TODO ensure this is a non-empty list
    end_times = params[["end_times"]]
  }
  
  #TODO make this impute thing a function
  impute = list()
  
  # Default is to impute
  if (!("impute" %in% names(params))){
    params["impute"] = TRUE
  }
  
  # If binary, then impute all or none of the data
  if (params[["impute"]] == TRUE | params[["impute"]] == FALSE) {
    for (dataset in datasets) {
      impute[dataset] = params[["impute"]]
    }
  } else { # If I pass in a custom list to specify for each dataset.
    impute = params["impute"] 
    # TODO check validity of above. Add to helpers.R?
  }
  
  diagnostics = FALSE
  if (params[["diagnostics"]]) {
    diagnostics = TRUE
  }
  
  rerun = FALSE 
  if (params[["rerun"]]) {
    rerun = TRUE
  }
  
  
  if (verbose) {
    cat("Running with the following params: \n")
    cat(paste0("diagnostics=", diagnostics, "\n"))
    cat(paste0("impute=", impute, "\n"))
    cat(paste0("rerun=", rerurn, "\n"))
  }
  
  
  for (dataset in datasets) {
    
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
      
      folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      
      # TODO Don't always suppress Stan output.
      MCMC_params = list(diagnostics = diagnostics, verbose=verbose, stan_output=FALSE)
      
      save_path = ifelse((end_times[[dataset]] == TT), "post", paste0("post_Time_", end_times[[dataset]]) )
      save_path = paste0(folder_name, save_path ,".Rdata")
      
      if (!rerun & file.exists(save_path)) {
        # post_samples = get(load(save_path))
        if (verbose){
          cat(paste0("Found saved data for ", model, " on dataset ", dataset, "[1:", TT, "] \n"))
        }
        next
      }
      
      if (verbose) {
        cat(paste0("Running model ", model, " on dataset ", dataset, "[1:", TT, "] \n"))
      }
      
      if (model == "1A") {
        #TODO get rid of hardcoded stan_output
        post_samples = speed_rw_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
        
      } else if (model == "1B") {
        
        post_samples = speed_tvar_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
        
      } else if (model == "2A") {
        post_samples = speed_pdlm_regression_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
        
      } else if (model == "3A") {
        post_samples = indep_posterior_samples(a,x, replicates=TRUE, params=MCMC_params)
        
      } else if (model == "4A") {
        post_samples = spline_posterior_samples(a, x, replicates=TRUE, params=MCMC_params)
        
      } else if (model == "dlm") { 
        post_samples = dlm_posterior_samples(a, x, replicates = TRUE, params=MCMC_params)
      
      } else {
        stop("No model of name ", model  , " found, might still need to be implemented.")
      }
      
      save(post_samples, file = save_path)
     
      
      
    }
  
  }
  
}



