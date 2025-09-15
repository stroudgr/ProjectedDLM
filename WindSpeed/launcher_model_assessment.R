source("_packages.R")
source("WindSpeed/_packages.R")
source("helpers/_helpers.R")
source("WindSpeed/_helpers.R")


# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model testing: visualize one step ahead forecasts
    
    models = list("1A", "2A", "3A", "4A")
    datasets = list("buffalo", "santa_ana")
    datasets = list("buffalo")
    
    buffalo_list = c(29, 44,53,109, 127, 128)
    
    santa_ana_list = c(7,8,9) #change these
    
    times_list = list(buffalo=buffalo_list, santa_ana = santa_ana_list)
    params = list(rerun = FALSE)
    
    visualize_one_step_ahead(models, datasets, times_list = times_list, params= params)
    
  }
)


