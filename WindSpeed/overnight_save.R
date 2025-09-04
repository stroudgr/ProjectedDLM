fs1$direction_forecasts[,,custom_times]= forecast_samples1$direction_forecasts[,,custom_times]
fs1$speed_forecasts[,custom_times]= forecast_samples1$speed_forecasts[,custom_times]
forecast_samples1 = fs1

fs2$direction_forecasts[,,custom_times]= forecast_samples2$direction_forecasts[,,custom_times]
fs2$speed_forecasts[,custom_times]= forecast_samples2$speed_forecasts[,custom_times]
forecast_samples2 = fs2

fs3$direction_forecasts[,,custom_times]= forecast_samples3$direction_forecasts[,,custom_times]
fs3$speed_forecasts[,custom_times]= forecast_samples3$speed_forecasts[,custom_times]
forecast_samples3 = fs3


fs4$direction_forecasts[,,custom_times]= forecast_samples4$direction_forecasts[,,custom_times]
fs4$speed_forecasts[,custom_times]= forecast_samples4$speed_forecasts[,custom_times]
forecast_samples4 = fs4


fs5$direction_forecasts[,,custom_times]= forecast_samples5$direction_forecasts[,,custom_times]
fs5$speed_forecasts[,custom_times]= forecast_samples5$speed_forecasts[,custom_times]
forecast_samples5 = fs5





if (TT == 8253) {
  save(forecast_samples1, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model1.Rdata")
  save(forecast_samples2, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model2.Rdata")
  save(forecast_samples3, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model3.Rdata")
  save(forecast_samples4, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model4.Rdata")
  save(forecast_samples5, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model5.Rdata")
}

if(TT == 8253){
  save(post_samples1, file = "WindSpeed/models/saved_MCMC/post_samples/2_santa_ana/model1.Rdata")  
  save(post_samples2, file = "WindSpeed/models/saved_MCMC/post_samples/2_santa_ana/model2.Rdata")
  save(post_samples3, file = "WindSpeed/models/saved_MCMC/post_samples/2_santa_ana/model3.Rdata")
  save(post_samples4, file = "WindSpeed/models/saved_MCMC/post_samples/2_santa_ana/model4.Rdata")
  save(post_samples5, file = "WindSpeed/models/saved_MCMC/post_samples/2_santa_ana/model5.Rdata")
}


if (TT == 128) {
  save(forecast_samples1, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model1.Rdata")
  save(forecast_samples2, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model2.Rdata")
  save(forecast_samples3, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model3.Rdata")
  save(forecast_samples4, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model4.Rdata")
  save(forecast_samples5, file = "WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model5.Rdata")

  save(post_samples1, file = "WindSpeed/models/saved_MCMC/post_samples/1_buffalo/model1.Rdata")  
  save(post_samples2, file = "WindSpeed/models/saved_MCMC/post_samples/1_buffalo/model2.Rdata")
  save(post_samples3, file = "WindSpeed/models/saved_MCMC/post_samples/1_buffalo/model3.Rdata")
  save(post_samples4, file = "WindSpeed/models/saved_MCMC/post_samples/1_buffalo/model4.Rdata")
  save(post_samples5, file = "WindSpeed/models/saved_MCMC/post_samples/1_buffalo/model5.Rdata")
  
  
  
}




