

# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
{
# The experiment is Model Visualization: state visualization
# For models that take the form y_t = s_t + f(x_t) + noise
#                           or  y_t = s_t*beta_t + noise
# we want to visualize the time dependent latent state variables.

# params should include
# - Whether to runMCMC if there is no saved data (shouldn't be any issue).
# - Mandatory param: Which states to create visuals for.
# - Anything else?
  
models = list()
datasets = list("buffalo", "santa_ana")

buffalo_list = c(4,5,6)
santa_ana_list = c(7,8,9) #change these


states_list = list(buffalo_list, santa_ana_list)
params = list(runMCMC = FALSE)

visualize_states(models, datasets, states_list=states_list, params=params)
  
}
)


# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
# The experiment is Model visulization: regression coefficient visualization.
local(
{

  # Non-parametric models or models without non-time varying coefficients have nothing to visualize.
  valid_models = list()
  invalid_models = list()
  
  models = list()
  datasets = list()
  
  # TODO what are valid params to consider, beyond runMCMC?
  params = list(runMCMC = FALSE)
  
  visualize_regression_coefficients(models, datasets, params)
  
}
)

# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
# The experiment is Model visualization: non-parametric function distribution visualization.
local({
  
  # Non-parametric models only.
  valid_models = list()
  invalid_models = list()
  
  models = list()
  datasets = list()
  
  # TODO what are valid params to consider, beyond runMCMC?
  params = list()
  
  nonparametric_function_visualization(models, datasets, params)
  
})


















