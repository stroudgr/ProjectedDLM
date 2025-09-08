
dlm_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform = function(x){log(x+1)}, params) {

  
  verbose = FALSE
  stan_output = TRUE
  diagnostics = FALSE  
  
  if (params[["verbose"]]) {
    verbose = TRUE
  }
  
  if (params[["stan_output"]] == FALSE) {
    stan_output = FALSE
  }
  
  if (params[["diagnostics"]]) {
    diagnostics = TRUE
  }
  
  
  logx = xtransform(x)
  TT = length(x)
  n = 2
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }
  
  U = radians2unitcircle(a)
  
  
  
  data_list <- list(
    T = TT,
    x = logx,
    y = a
  )
  
  # Compile the model
  model <- stan_model("WindSpeed/models/dlm/speed_dlm.stan")

  refresh = ifelse(stan_output, max(ndraw/10, 1), 0)
  # Sample from the posterior
  fit <- sampling(model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42, refresh=refresh)
  
  posterior <- rstan::extract(fit)

  
  if(!replicates) {
    return(posterior)
  } 
  
  #sigma_x_w_samples <- posterior$sigma_x_w
  #sigma_y_e_samples <- posterior$sigma_y_e
  #s_x_samples <- posterior$s
  logx_rep <- posterior$x_rep
  a_rep <- posterior$y_rep
  a_rep <- t(a_rep)

  # ==============================================================================
  # Posterior predictive (replicate) draws
  # ==============================================================================

  u_rep = array(0, dim = c(TT, n, ndraw))
  p_max=1

  for (draw in 1:ndraw){
    for (t in (1+p_max):TT) {
      a_to_u = matrix(a_rep[t,draw], nrow=1, ncol=1)
      u_rep[t, , draw] <- radians2unitcircle(a_to_u)
    }
  }
  
  return(list(posterior=posterior, u_rep=u_rep, a_rep=a_rep, logx_rep=posterior$x_rep))

}


dlm_point_estimation = function(post_samps) {

  n=2
  p=2
  u_rep = post_samps$u_rep

  u_med = array(0, dim = c(TT, n))
  a_med = rep(0, TT)
  median_state = array(0, dim=c(TT,n))
  median_angle_from_state = rep(0, TT)
  for (t in 2:TT) {
    #mediandir(t(u_rep[t,,]))
    #t( mediandir( t(state[t,,])  ))
    u_med[t,] = t(  mediandir( t(u_rep[t,,]) )  )
    #median_state[t,] = t( mediandir( t(state[t,,])  ))
  }
  a_med = unitcircle2radians(u_med)
  #median_angle_from_state = unitcircle2radians(median_state)
  
  #a_mean = apply(a_)
  return(list(u_med=u_med,a_med=a_med))
}


dlm_forecasting = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1, custom_times = NA) {

  n=2
  p=2
  #
  # Forecasting
  #
  logx = xtransform(x)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }
  
  U = radians2unitcircle(a)
  
  dlm_forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  dlm_median = matrix(0, TT, n)
  
  wind_speed_model = stan_model("WindSpeed/models/dlm/speed_dlm.stan")
  
  start = max(1, TT-h+1)
  #start = TT
  
  outer_interval = start:TT
  
  if (sum(!is.na(custom_times)) > 0){
    print("Using custom times")
    outer_interval = custom_times
    
  }
  
  for (t in outer_interval) {
    
    data_list <- list(
      T = t-1,
      x = logx[1:(t-1)],
      y = a[1:(t-1)]
    )
    
    # Sample from the posterior
    fit <- sampling(wind_speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
    
    posterior <- rstan::extract(fit)
    
    sigma_x_w_samples <- posterior$sigma_x_w
    sigma_x_e_samples <- posterior$sigma_x_e
    
    sigma_y_w_samples <- posterior$sigma_y_w
    sigma_y_e_samples <- posterior$sigma_y_e
    
    s_x_samples <- posterior$s_x
    s_y_samples <- posterior$s_y
    

    for(m in 1:ndraw){
      new_s_x = rnorm(1, mean = s_x_samples[m, t-1], sd = sqrt(sigma_x_w_samples[m]))
      new_x = rnorm(1, mean = new_s_x, sd = sqrt(sigma_x_e_samples[m]))
      
      speed_forecasts[m, t] = new_x
      
      new_s_y = rnorm(1, mean = s_y_samples[m, t-1], sd = sqrt(sigma_y_w_samples[m]))
      
      new_y = rnorm(1, mean = new_x*new_s_y, sd = sqrt(sigma_y_e_samples[m]))
      
      dlm_forecasts[m, , t] = radians2unitcircle(new_y) 
      
    }
    
    
  }

  return(list(direction_forecasts=dlm_forecasts, speed_forecasts=speed_forecasts))
  
}


dlm_forecasting_ahead = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1) {
  
  n=2
  p=2
  #
  # Forecasting
  #
  logx = xtransform(x)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }
  
  U = radians2unitcircle(a)
  
  dlm_forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  dlm_median = matrix(0, TT, n)
  
  wind_speed_model = stan_model("WindSpeed/models/dlm/speed_dlm.stan")
  
  start = max(1, TT-h+1)
  #start = TT
  t=start
  
  data_list <- list(
    T = t-1,
    x = logx[1:(t-1)],
    y = a[1:(t-1)]
  )
  
  # Sample from the posterior
  fit <- sampling(wind_speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
  
  posterior <- rstan::extract(fit)
  
  sigma_x_w_samples <- posterior$sigma_x_w
  sigma_x_e_samples <- posterior$sigma_x_e
  
  sigma_y_w_samples <- posterior$sigma_y_w
  sigma_y_e_samples <- posterior$sigma_y_e
  
  s_x_samples <- posterior$s_x
  s_y_samples <- posterior$s_y
  
  
  old_s_x =  s_x_samples[, start-1]
  old_s_y = s_y_samples[, start-1]
  for (t in start:TT) {
    
    
    for(m in 1:ndraw){
      new_s_x = rnorm(1, mean = old_s_x[m], sd = sqrt(sigma_x_w_samples[m]))
      new_x = rnorm(1, mean = new_s_x, sd = sqrt(sigma_x_e_samples[m]))
      
      old_s_x[m] = new_s_x 
      
      speed_forecasts[m, t] = new_x
      
      new_s_y = rnorm(1, mean = old_s_y[m], sd = sqrt(sigma_y_w_samples[m]))
      
      new_y = rnorm(1, mean = new_x*new_s_y, sd = sqrt(sigma_y_e_samples[m]))
      
      old_s_y[m] = new_s_y
      
      dlm_forecasts[m, , t] = radians2unitcircle(new_y) 
      
    }
    
    
  }
  
  return(list(direction_forecasts=dlm_forecasts, speed_forecasts=speed_forecasts))
  
}








posterior_samples.dlm_model = dlm_posterior_samples
point_estimation.dlm_model = dlm_point_estimation
forecasting.dlm_model = dlm_forecasting


create_dlm_model <- function() {
  structure("dlm", class = "dlm_model")
}

