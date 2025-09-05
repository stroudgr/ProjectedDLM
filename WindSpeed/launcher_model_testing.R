

# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model testing: one step ahead distribution visualization : histograms
    # Visualize u_{t+1}, x_{t+1} given 
    
    # params should include
    # - Whether to runMCMC if there is no saved data (how do I save MCMC samples for subsets?).
    # - Mandatory param: Which time steps to create histograms for.
    # - Anything else?
    
    models = list()
    datasets = list("buffalo", "santa_ana")
    
    buffalo_list = c(4,5,6)
    santa_ana_list = c(7,8,9) #change these
    
    
    times_list = list(buffalo_list, santa_ana_list)
    params = list(runMCMC = FALSE)
    
    visualize_states(models, datasets, times_list=times_list, params=params)
    
  }
)



# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model testing: fan plots for H step forecast ahead.
    # Visualize u_{t+1}, x_{t+1}, ...., u_{t+H}, x_{t+h} with fan plot,
    # given MCMC samples on data U_{1:t},x_{1:t}.
    
    # params should include
    # - Whether to runMCMC if there is no saved data (how do I save MCMC samples for subsets?).
    # - Mandatory param: t, how much data is known.
    # - Mandatory param: H, how far the horizon is.
    # - Anything else?
    
    models = list()
    datasets = list("buffalo", "santa_ana")
    
    # Same length as datasets
    t = list(buffalo = 100, santa_ana = 5000)
    H = list(buffalo = 20, santa_ana = 20)
    
    
    params = list(runMCMC = FALSE)
    
    visualize_states(models, datasets, t, H, params=params)
    
  }
)



# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
# The experiment is Model testing: table of forecasting metrics.
local(
  {
    
    models = list() 
    datasets = list()
    
    # This list should be the same length as the number of datasets.
    # For datasets[t], averaging_time_frames[t] should have a range (u,l) where 1<=u<=l <= number of time steps of curr dataset.
    averaging_time_frames = list()
    
    # TODO what are valid params to consider, beyond runMCMC?
    params = list(runMCMC = FALSE)
    
    
    visualize_regression_coefficients(models, datasets, averaging_time_frames, params)
    
  }
)



















