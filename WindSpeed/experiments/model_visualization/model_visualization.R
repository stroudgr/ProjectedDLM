source("WindSpeed/experiments/model_visualization/state_visualization/state_visualize.R")
source("WindSpeed/experiments/model_visualization/non_parametric_function_visualization/non_parametric_function_visualization.R")



#' visualize_states
#'
#' Description
#'
#' @param models A list of models .
#' @param datasets A list of datasets to create figures for.
#' 
#' !!!
#' TODO which params are needed?
#' !!!  
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
#' visualize_states(models=list("1A"), datasets=list("buffalo"), params)
visualize_states = function(models, datasets, params) {
  
  root_path = "WindSpeed/experiments/MCMC/saved_MCMC/"
  
  verbose = FALSE
  if ( "verbose" %in% names(params)) {
    if (params[["verbose"]] == TRUE ) {
      verbose = TRUE
    }
  }
  
  for (model in models) {
    for (dataset in datasets){
      
      # Loads MCMC posterior data.
      folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      save_path = paste0(folder_name, "post.Rdata")
      post_samples = get(load(save_path))
      
      if (verbose) {
        cat(paste0("Loading MCMC for model ", model, " dataset ", dataset,  "."))
      }
      
      visualize_states_helper(dataset, model, post_samples)
      
      if (dataset == "buffalo") {
        #bv_Sigma(model, post_samples)
      }
      
    }
  }
  
  
  
  
}




nonparametric_function_visualization = function(datasets, params) {
  
  
  
  
}