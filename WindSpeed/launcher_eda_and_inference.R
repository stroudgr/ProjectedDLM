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


source("WindSpeed/experiments/data_visualization/Wind_Speed_eda.R")

# ------------------------------------------------------------------------------
# Experiment 1
# ------------------------------------------------------------------------------
local({
# The experiment is Data visualization: all subexperiments (Should be fast to run all at once)
datasets = list("buffalo", "santa_ana")

params = list(impute_missing = TRUE)
create_dataset_figures(datasets, params)
})


# ------------------------------------------------------------------------------
# Experiment 2
# ------------------------------------------------------------------------------
# The experiment is Model inference: posterior sampling
local({

# params should include:
# - save diagnostics or not 
# - whether to include Stan output
#        -> Eg: fit <- stan(model_code = "...", data = "...", refresh = 0) 
# - May as well always save posterior samples.
# - General progress output: Y/N
params = list()
run_MCMC(models, datasets, params) 
                                   
})


# ------------------------------------------------------------------------------
# Experiment 3
# ------------------------------------------------------------------------------
# The experiment is Model inference: posterior predictive forecasting
local({
  params = list()
  
  # Params should include:
  # - For what subinterval of data am I experimenting on?
  # - runMCMC if no data saved?
  # - For how long do I forecast for?
  
  forecast_samples(models, datasets, params)
})



# ------------------------------------------------------------------------------
# Experiment 4
# ------------------------------------------------------------------------------
# The experiment is data visualization: basis OLS visualization
local({
  # Visualize what the OLS solution looks like for the provided basis.
  
  # TODO should this just be in #1? Probably, although we probably want to easily make some diff choices
  # of what the basis could/should be.
  
  # TODO params?
  params = list()
  
  # Lengths should match
  basis_choice_names = list()
  basis_choice = list()
  
  forecast_samples(models, datasets, params)

})


