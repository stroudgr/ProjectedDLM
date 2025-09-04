library(ggplot2)

logx = log(x+1)
m=6
post_samples = post_samples6
points_forecasts = points_estimation5
forecast_samples = forecast_samples5
ahead_forecast_samples = ahead_forecast_samples5



a_med = points_forecasts$a_med
a_rep = post_samples$a_rep

p_max=1

#
# Histogram
#

#ggplot(forecast_samples$speed_forecasts[,TT], aes(x=weight)) + geom_histogram()

for (t in custom_times) { 

  speed_forecast_samples = forecast_samples$speed_forecasts[,t]
  direction_forecast_samples = forecast_samples$direction_forecasts[,,t]
  
  angle_forecast_samples = unitcircle2radians(direction_forecast_samples)
  
  png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/results/",
             t, "/",
             "model_", model_alphanumeric_identifiers[m],"_forecast_angle_hist_time", t, ".png"), width = 800, height = 600, res = 100)
  
  hist(angle_forecast_samples,
       main = paste("One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
       xlab = "angle",
       ylab = "Frequency"
  )
  legend("topleft", c(paste("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
  abline(v = a[t-1], col = "red", lty = 2, lwd = 2)
  #if (t!=TT)
  abline(v = a[t], col = "green", lty = 2, lwd = 2)
  
  
  dev.off()
  
  png(paste0("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/results/",
             t, "/",
             "model_", model_alphanumeric_identifiers[m],"_forecast_speed_hist_time", t, ".png"), width = 800, height = 600, res = 100)
  
  hist(speed_forecast_samples,
       main = paste("One-Step-Ahead distribution for model ", model_alphanumeric_identifiers[m]),
       xlab = "log(Speed+1) (mph)",
       ylab = "Frequency"
  )
  legend("topleft", c(paste("Model ", model_alphanumeric_identifiers[m]), "current", "next"), col=c("grey", "red", "green"), lwd=10)
  abline(v = logx[t-1], col = "red", lty = 2, lwd = 2)
  #if (t!=TT)
  abline(v = logx[t], col = "green", lty = 2, lwd = 2)
  
  dev.off()
}








