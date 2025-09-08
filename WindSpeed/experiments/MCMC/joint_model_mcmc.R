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
  
  if ("root_path" %in% params ) {
    root_path = params[["root_path"]]
  }
  
  verbose = FALSE
  
  if (params[["verbose"]]){
    verbose = TRUE
  }
  
  end_times = list()
  if ("end_times" %in% params) {
    
    # TODO ensure this is a non-empty list
    end_times = params[["end_times"]]
    
  }
  
  #TODO make this impute thing a function
  impute = list()
  
  if (!("impute" %in% params)){
    params["impute"] = TRUE
  }
  
  if (params[["impute"]] == TRUE | params[["impute"]] == FALSE) {
    for (dataset in datasets) {
      impute[dataset] = params["impute"] 
    }
  } else { # If I pass in a custom list to specify for each dataset.
    impute = params["impute"] 
    # TODO check validity of above. Add to helpers.R?
  }
  
  
  for (dataset in datasets) {
    
    {
      data = load_dataset(dataset, impute[[dataset]])
      a = data$a
      x = data$x
    }
    
    
    for (model in models) {
      
      folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      
      if (model == "1A") {
        #TODO get rid of hardcoded stan_output
        post_samples = speed_rw_posterior_samples(a, x, replicates = TRUE, verbose=verbose, stan_output=FALSE)
        
      } else if (model == "1B") {
        
        post_samples = speed_tvar_posterior_samples(a, x, replicates = TRUE)
        
      } else if (model == "2A") {
        speed_pdlm_regression_posterior_samples(a, x, replicates = TRUE)
        
      } else if (model == "3A") {
        post_samples = indep_posterior_samples(a,x, replicates=TRUE)
        
      } else if (model == "4A") {
        post_samples = spline_posterior_samples(a, x, replicates=TRUE)
        
      } else if (model == "dlm") { 
        post_samples = dlm_posterior_samples(a, x, replicates = TRUE)
      
      } else {
        stop("No model of name ", model  , " found, might still need to be implemented.")
      }
      
      if (dataset %in% end_times) {
        save(post_samples, file = paste0(folder_name, "post.Rdata"))
      } else {
        save(post_samples, file = paste0(folder_name, "post_Time_", end_times[[dataset]], ".Rdata"))
      }
      
    }
  
  }
  
}