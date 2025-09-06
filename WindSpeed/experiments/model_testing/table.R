period = 50:TT
alpha = 0.1
U = radians2unitcircle(a)

{
n=2
pdlm_median = matrix(0, TT, n)
point_err = matrix(0, TT, 4)
set_summary = matrix(0, TT, 4)
set_cov = matrix(0, TT, 4)
set_size = matrix(0, TT, 4)
kernel_scores = matrix(0, TT, 4)
}


m=1
pdlm_forecasts = forecast_samples = forecast_samples1
pdlm_forecasts = pdlm_forecasts$direction_forecasts

for(t in period){
  
  obs = U[t, ]
  
  # posterior predictive draws as unit vectors
  
  pdlm_draws <- pdlm_forecasts[, , t]
  
  # compute point forecasts
  
  pdlm_median[t, ] = mediandir(pdlm_draws)
  
  # compute errors
  
  point_err[t, 1] = acos(c(t(obs) %*% pdlm_median[t, ]))
  
  
  # compute sets
  
  set_summary[t, 1] = quantile(c(pdlm_draws %*% pdlm_median[t, ]), alpha)
  
  set_cov[t, 1] = c(t(pdlm_median[t, ]) %*% obs) >= set_summary[t, 1]
  
  set_size[t, 1] = quantile_cap_size(n, set_summary[t, 1])
  
  kernel_scores[t, 1] = sample_kernel_score_on_sphere(obs, pdlm_draws)
  
}

colMeans(point_err[period,])
colMeans(set_cov[period,])
colMeans(set_size[period,])
colMeans(kernel_scores[period,])

