# ==============================================================================
# Load dataset.
# ==============================================================================


#rm(list = ls())

# source("_packages.R")
# source("helpers/_helpers.R")
# 
# library(Rfast)
# library(tvReg)
# library(rstan)

# source("WindSpeed/models/1A/speed_rw.R")
# source("WindSpeed/models/dlm/DLM.R")
# source("WindSpeed/models/2A/speed_pdlm_regression.R")
# source("WindSpeed/models/1B/speed_TVAR.R")
# source("WindSpeed/models/indep/indep.R")
# source("WindSpeed/models/model.R")
# source("WindSpeed/models/4A spline/spline_cond.R")
# source("WindSpeed/spline_gibbs.R")
# source("WindSpeed/launch_experiment.R")

#set.seed(8675309)

source("_packages.R")
source("helpers/_helpers.R")
source("WindSpeed/_helpers.R")
#

RUN_EXPERIMENT_1 = FALSE
RUN_EXPERIMENT_2 = TRUE
RUN_EXPERIMENT_3 = FALSE

# ------------------------------------------------------------------------------
# Experiment 1
# ------------------------------------------------------------------------------

if (RUN_EXPERIMENT_1) {
local({
  # The experiment is Data visualization: all subexperiments (Should be fast to run all at once)
  datasets = list("buffalo", "santa_ana")
  
  params = list(impute = TRUE)
  create_dataset_figures(datasets, params)
})
}





# ------------------------------------------------------------------------------
# Experiment 2
# ------------------------------------------------------------------------------
# The experiment is Model inference: posterior sampling
if (RUN_EXPERIMENT_2) {
local({

  # params should include:
  # - save diagnostics or not 
  # - whether to include Stan output
  #        -> Eg: fit <- stan(model_code = "...", data = "...", refresh = 0) 
  # - May as well always save posterior samples.
  # - General progress output: Y/N
  params = list(verbose = TRUE, stan_output=FALSE, diagnostics = TRUE, impute = TRUE, end_times = list(santa_ana=10))
  params["rerun"] = FALSE
  
  
  models = list("1A", "2A", "3A", "4A", "dlm")
  
  datasets = list("buffalo", "santa_ana")
  datasets = list("buffalo")
  
  models = list("1A")
  
  #start_time <- Sys.time()
  
  
  run_MCMC(models, datasets, params) 
  
  #end_time <- Sys.time()
  #time_taken <- end_time - start_time
  #print(time_taken)
  
  
  # Runtimes:
  #Running model 2A on dataset santa_ana[1:4000] 
  #Time difference of 25.04377 mins
                                   
})
}



# ------------------------------------------------------------------------------
# Experiment 3
# ------------------------------------------------------------------------------
# The experiment is Model inference: posterior predictive forecasting
if (RUN_EXPERIMENT_3) {
local({
  params = list()
  
  # Params should include:
  # - For what subinterval of data am I experimenting on?
  # - runMCMC if no data saved?
  # - For how long do I forecast for?
  
  forecast_samples(models, datasets, params)
})
}

