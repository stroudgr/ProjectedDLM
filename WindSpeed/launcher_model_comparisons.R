# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model comparison: spatial confounding.
  
    #See what happens when spatial confounding is turned on/off.
    
    valid_models = list()
    models = list() # just spline is valid for now
    datasets = list("buffalo", "santa_ana")
    
    # TODO decide on what the output should be.
    spatial_confounding(models, datasets)
    
  }
)
