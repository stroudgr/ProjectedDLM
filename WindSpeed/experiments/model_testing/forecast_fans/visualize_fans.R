


m=1
post_samples = post_samples_E
points_forecasts = points_estimation_E
forecast_samples = forecast_samples_E
ahead_forecast_samples = ahead_forecast_samples_E



a_med = points_forecasts$a_med
a_rep = post_samples$a_rep

p_max=1
#replicate_plot(model_pretty_names[m], a[-(1:p_max)], mu=a_med[-(1:p_max)], postY=t(a_rep)[,-(1:p_max)], y_true=NULL, t01 = (1+p_max):TT)



#
# Histogram
#

speed_forecast_samples = forecast_samples$speed_forecasts[,TT]
direction_forecast_samples = forecast_samples$direction_forecasts[,,TT]

angle_forecast_samples = unitcircle2radians(direction_forecast_samples)

#########################











# ==============================================================================
#
# FAn plot
#
# ==============================================================================

h=10
start = max(1, TT-h+1)

logx_forcast = array(NA, dim=c(1000, TT)) 
logx_forcast[, start:TT] = ahead_forecast_samples$speed_forecasts[, start:TT]

a_forecast = array(NA, dim=c(1000, TT)) 

for (t in start:TT) {
  a_forecast[, t] = unitcircle2radians(ahead_forecast_samples$direction_forecasts[,,t])
}

for (t in 1:(start-1)) {
  logx_forcast[, t] = log(x[t] + 1)
  a_forecast[,t] = a[t]
}



plot(logx, main=paste0("logSpeed Forecasting, model ", model_alphanumeric_identifiers[m]), xlim = c(1, 130), ylim = c(-1.5, 3.5))
fan( logx_forcast)
lines(1:TT, logx, col = "red", lty = 1, lwd = 2)
points(1:TT, logx, bg="white", col = "black", pch = 21, cex=0.75)



plot(NA, main=paste0("Speed Forecasting, model ", model_alphanumeric_identifiers[m]), xlim = c(1, 130), ylim = c(0, 15), ylab="Speed")
fan( exp(logx_forcast)-1)
lines(1:TT, exp(logx)-1, col = "red", lty = 1, lwd = 2)
points(1:TT, exp(logx)-1, bg="white", col = "black", pch = 21, cex=0.75)


plot(a, main=paste0("Angle Forecasting, model ", model_alphanumeric_identifiers[m]), xlim = c(1, 130), ylim = c(0, 6.5))
fan( a_forecast)
lines(1:TT, a, col = "red", lty = 1, lwd = 2)
points(1:TT, a, bg="white", col = "black", pch = 21, cex=0.75)











