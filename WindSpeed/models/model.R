# S3 classes
# Model interface

get_posterior_samples = function(model, ...){
  UseMethod("get_posterior_samples")
}

get_point_estimation = function(model, ...){
  UseMethod("get_point_estimation")
}

get_forecast_samples = function(model, ...){
  UseMethod("get_forecast_samples")
}

get_forecast_ahead_samples = function(model, ...){
  UseMethod("get_forecast_ahead_samples")
}





# ==============================================================================
# Model 1A : 
# ==============================================================================

Speed_Rw = function(name = "Unnamed") {
  structure(
    list(name=name),
    class = "Speed_Rw" # should match class name
  )
}

get_posterior_samples.Speed_Rw = function(model, ...) {speed_rw_posterior_samples(...)}

get_point_estimation.Speed_Rw = function(model, ...) {speed_rw_point_estimation(...)}

get_forecast_samples.Speed_Rw = function(model, ...) {speed_rw_forecast_samples(...)}

get_forecast_ahead_samples.Speed_Rw = function(model, ...) {speed_rw_forecast_ahead_samples(...)}


# Create instance
model_1A = Speed_Rw()







# ==============================================================================
# Model 1B : Copy idea from above
# ==============================================================================

