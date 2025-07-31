# Generating posterior predictive boxplots for various models

rm(list = ls())

source("_packages.R")
source("helpers/_helpers.R")


library(Rfast)
library(tvReg)
library(rstan)


source("WindSpeed/models/1A/speed_rw.R")
source("WindSpeed/models/dlm/DLM.R")
source("WindSpeed/models/2A/speed_pdlm_regression.R")
source("WindSpeed/models/1B/speed_TVAR.R")
source("WindSpeed/models/indep/indep.R")
source("WindSpeed/models/model.R")

set.seed(8675309)

# ==============================================================================
# Load dataset.
# ==============================================================================

path = "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv"
path = "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv"

speed_dir_data = read.csv(path)

degrees = speed_dir_data[["wind_direction"]]
x = speed_dir_data[["wind_speed"]]


degrees = speed_dir_data[["drct"]]
x = speed_dir_data[["sknt"]]

logx = log(x+1)




if (path == "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv") {
  period = 38 + 1:128
  x = x[period]
  degrees = degrees[period]
}

a = degrees2radians(degrees)

TT = length(x)

buffalo_time_steps = c(29, 44, 53, 109, TT)

santa_ana_time_steps = c(300, 1500, 2500, 3500, 5000, TT)

if (path == "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv") {
  
  x = na_interpolation(x)
  logx=log(x+1)
  a = na_interpolation(a)
  U = radians2unitcircle(a)
  
  for (time_of_interest in santa_ana_time_steps) {
  
    png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/data_figures/",
               "santa_ana_speed_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
    plot(x, type = "l",
         main = "Santa Wind Speed over time",
         xlab = "Time (minutes)",
         ylab = "mph")
    
    abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
    dev.off()
    
    png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/data_figures/",
               "santa_ana_dir_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
    plot(a, type = "l",
         main = "Santa Ana Wind direction over time",
         xlab = "Time (minutes)",
         ylab = "Angle (radians)")
    abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
    dev.off()
  }
  
}

library(imputeTS)

if (path == "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv") {
  
 
  
  
  for (time_of_interest in buffalo_time_steps) {
  
    png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/data_figures/",
               "buff_speed_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
    plot(x, type = "l",
         main = "Buffalo Wind Speed over time",
         xlab = "Time (hours)",
         ylab = "mph")
    
    abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
    dev.off()
    
    png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/data_figures/",
               "buff_dir_", time_of_interest, ".png"), width = 800, height = 600, res = 100)
    plot(a, type = "l",
         main = "Buffalo Wind direction over time",
         xlab = "Time (hours)",
         ylab = "Angle (radians)")
    abline(v = time_of_interest, col = "red", lty = 2, lwd = 2)
    dev.off()
  }
}
# ==============================================================================
# Graphing code
# ==============================================================================

replicate_plot = function (name, y, mu, postY, y_true = NULL, t01 = NULL, include_joint_bands = FALSE) 
{
  T = length(y)
  if (is.null(t01)) 
    t01 = seq(0, 1, length.out = T)
  dcip = dcib = t(apply(postY, 2, quantile, c(0.05/2, 1 - 
                                                0.05/2)))
  if (include_joint_bands) 
    dcib = credBands(postY)
  dev.new()
  par(mfrow = c(1, 1), mai = c(1, 1, 1, 1))
  
  plot(t01, y, type = "n", ylim = range(dcib, y, na.rm = TRUE), 
       xlab = "t", ylab = expression(paste("angle"[t])), main = paste("Replicate point estimates (", name, ")"), 
       cex.lab = 2, cex.main = 2, cex.axis = 2)
  polygon(c(t01, rev(t01)), c(dcib[, 2], rev(dcib[, 1])), 
          col = "gray50", border = NA)
  polygon(c(t01, rev(t01)), c(dcip[, 2], rev(dcip[, 1])), 
          col = "grey", border = NA)
  if (!is.null(y_true)) 
    lines(t01, y_true, lwd = 8, col = "black", lty = 6)
  lines(t01, y, type = "p")
  lines(t01, mu, lwd = 8, col = "cyan")
}








# ==============================================================================
# Run models
# ==============================================================================


model_pretty_names = c("speed random walk + noise", "DLM", "speed RWN, direction PDLM + regression", "speed TVAR", "speed RWN, PDLM indep")
model_names = c("speed_rw", "DLM", "speed_pdlm_regression", "speed_tvar", "independ")

# TODO: I should want to delete these three lists. 
post_samp_functions = c("speed_rw_posterior_samples", "dlm_posterior_samples", "speed_pdlm_regression_posterior_samples", "speed_tvar_posterior_samples")
points_est_functions = c("speed_rw_point_estimation", "dlm_point_estimation", "speed_pdlm_regression_point_estimation", "speed_tvar_point_estimation")
forecast_samp_functions = c("speed_rw_forecast_samples", "dlm_forecasting", "speed_pdlm_regression_forecast_samples", "speed_tvar_forecast_samples")

# 
model_alphanumeric_identifiers = c("1A", "two_dlms", "2A", "1B", "3A")

num_models = length(model_alphanumeric_identifiers)

{
H_MAX = 10
post_samples = NULL
points_forecasts = NULL 
forecast_samples = NULL
ahead_forecast_samples = NULL

post_samples1 = NULL
points_forecasts1 = NULL 
forecast_samples1 = NULL
ahead_forecast_samples1 = NULL

post_samples2 = NULL
points_forecasts2 = NULL 
forecast_samples2 = NULL
ahead_forecast_samples2 = NULL

post_samples3 = NULL
points_forecasts3 = NULL 
forecast_samples3 = NULL
ahead_forecast_samples3 = NULL

post_samples4 = NULL
points_forecasts4 = NULL 
forecast_samples4 = NULL
ahead_forecast_samples4 = NULL

}

# for (m in 1:num_models) {
#   post_samples = get(post_samp_functions[m])(a, x, replicates=TRUE)
#   points_forecasts = get(points_est_functions[m])(post_samples)
#   forecast_samples = get(forecast_samp_functions[m])(x, a) 
#   
#   if (m==1) {
#     post_samples1 = post_samples
#     points_forecasts1 = points_forecasts 
#     forecast_samples1 = forecast_samples
#   } else if (m==2) {
#     post_samples2 = post_samples
#     points_forecasts2 = points_forecasts 
#     forecast_samples2 = forecast_samples
#   } else{
#     post_samples3 = post_samples
#     points_forecasts3 = points_forecasts 
#     forecast_samples3 = forecast_samples
#   }
# } 

post_samples_1A = speed_rw_posterior_samples(a, x, replicates = TRUE)
#post_samples_1A = get_posterior_samples(model_1A, a, x, replicates= TRUE) # method(model_2A), etc...
post_samples1 = post_samples_1A

post_samples2 = dlm_posterior_samples(a, x, replicates = TRUE)
post_samples3 = speed_pdlm_regression_posterior_samples(a, x, replicates = TRUE)
post_samples4 = speed_tvar_posterior_samples(a, x, replicates = TRUE)
post_samples5 = indep_posterior_samples(a,x, replicates=TRUE)
post_samples_E = extra_posterior_samples(a,x, replicates=TRUE)



points_estimation_1A = speed_rw_point_estimation(post_samples_1A)
points_estimation1 = points_estimation_1A
points_estimation2 = dlm_point_estimation(post_samples2)
points_estimation3 = speed_pdlm_regression_point_estimation(post_samples3)
points_estimation4 = speed_tvar_point_estimation(post_samples4)
points_estimation5 = indep_point_estimation(post_samples5)
points_estimation_E = extra_point_estimation(post_samples_E)


custom_times = buffalo_time_steps
santa_ana_time_steps_fewer = c(300, 1500, 2500)

custom_times = santa_ana_time_steps_fewer

start_time <- Sys.time()
forecast_samples_1A = speed_rw_forecast_samples(x, a, custom_times = custom_times) 
forecast_samples1 = forecast_samples_1A
end_time <- Sys.time()

time_taken <- end_time - start_time
time_in_hours <- as.numeric(time_taken, units = "hours")

print(time_taken)
print(paste0(time_in_hours, " hours"))

forecast_samples2 = dlm_forecasting(x, a, custom_times = custom_times) 
forecast_samples3 = speed_pdlm_regression_forecast_samples(x, a, custom_times = custom_times) 
forecast_samples4 = speed_tvar_forecast_samples(x, a, custom_times = custom_times) 
forecast_samples5 = indep_forecast_samples(x,a, custom_times = custom_times)
forecast_samples_E = extra_forecast_samples(x,a, custom_times = custom_times)


ahead_forecast_samples_1A = speed_rw_forecast_ahead_samples(x,a, h=10)
ahead_forecast_samples1 = ahead_forecast_samples_1A 
ahead_forecast_samples2 = dlm_forecasting_ahead(x,a, h=10)
ahead_forecast_samples3 = speed_pdlm_regression_forecast_ahead_samples(x,a, h=10)
ahead_forecast_samples4 = speed_tvar_forecast_ahead_samples(x, a, h=10)
ahead_forecast_samples5 = indep_forecast_ahead_samples(x, a, h=10)
ahead_forecast_samples_E = extra_forecast_ahead_samples(x,a,h=1)

#post_samples_list = list(post_samples1, post_samples2, post_samples3)
#points_estimation_list = list(points_estimation1, points_estimation2, points_estimation3)
#forecast_samples_list = list(forecast_samples1, forecast_samples2, forecast_samples3)









