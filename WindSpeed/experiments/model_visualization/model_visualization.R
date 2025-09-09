source("WindSpeed/experiments/model_visualization/state_visualization/state_visualize.R")

visualize_states = function(models, datasets, params) {
  
  root_path = "WindSpeed/experiments/MCMC/saved_MCMC/"
  
  verbose = TRUE
  
  for (model in models) {
    for (dataset in datasets){
      
      folder_name = paste0(root_path, "/", model, "/post_samples/", dataset, "/")
      save_path = paste0(folder_name, "post.Rdata")
      post_samples = get(load(save_path))
      
      if (verbose) {
        cat(paste0("Model ", model, " dataset ", dataset,  "  "))
      }
      
      if (dataset == "buffalo") {
        
        buffalo_visualize_states(model, post_samples)
        
      } else if (dataset == "santa_ana") {
        
        santa_ana_visualize_states(model, post_samples)
        
      } else {
        stop("No such dataset")
      }
      
    }
  }
  
  
  
  
}