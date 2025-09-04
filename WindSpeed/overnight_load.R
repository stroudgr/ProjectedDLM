if (TT == 128) {
  fs1 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model1.Rdata"))
  fs2 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model2.Rdata"))
  fs3 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model3.Rdata"))
  fs4 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model4.Rdata"))
  fs5 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/1_buffalo/models_R/model5.Rdata"))
}

if (TT == 8253) {
  fs1 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model1.Rdata"))
  fs2 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model2.Rdata"))
  fs3 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model3.Rdata"))
  fs4 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model4.Rdata"))
  fs5 = get(load("WindSpeed/model_testing/visualize_1_step_forecast_dists/2_santa_ana/models_R/model5.Rdata"))
}



forecast_samples1 = fs1
forecast_samples2 = fs2
forecast_samples3 = fs3
forecast_samples4 = fs4
forecast_samples5 = fs5

