#source("_packages.R")
#source("helpers/_helpers.R")
library(Rfast)
library(tvReg)
library(rstan)

# Prereq: a should be radians.
indep_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform=function(x){log(x+1)}, params) {
  
  
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
  
  
  
  TT = length(x)
  n = 2
  p = 2
  
  logx = xtransform(x)
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n)
  }
  
  TT = length(x)
  n = 2
  
  U = radians2unitcircle(a)
  
  
  # ==============================================================================
  # Step 1: Instantiate random walk plus noise model.
  # ==============================================================================
  
  
  p_max = 1
  
  data_list <- list(
    T = TT,
    x = logx
  )
  
  # Compile the model
  speed_model <- stan_model("WindSpeed/models/1A/rw.stan")
  
  
  
  # ==============================================================================
  # Step 2: Fit speed model, generate samples (parameters, posterior predictive).
  # ==============================================================================
  
  
  refresh = ifelse(stan_output, max(ndraw/10, 1), 0)
  # Sample from the posterior
  fit <- sampling(speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42, refresh=refresh)
  
  # Check summary
  #print(fit, pars = c("sigma_w", "sigma_e"))
  
  posterior <- rstan::extract(fit)
  
  sigma_w_samples <- posterior$sigma_w
  sigma_e_samples <- posterior$sigma_e
  s_samples <- posterior$s
  logx_rep <- posterior$x_rep
  
  
  
  
  
  
  # ==============================================================================
  # Fit U|X model
  # ==============================================================================
  
  
  # MCMC params
  pdlm_burn = 1000
  pdlm_thin = 1
  #pdlm_draws = gibbs_pdlm_basic(U, FF, V, G, W, s1, P1, r0, ndraw, pdlm_burn, pdlm_thin)
  pdlm_draws = gibbs_pdlm(U[1:TT, ], FF[, , 1:TT], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)
  
  
  
  S_draws = pdlm_draws$S#[TT,,]
  Sigma_draws = pdlm_draws$Sigma
  G_draws = pdlm_draws$G
  W_draws = pdlm_draws$W
  
  if (!replicates) {
    return(list(S_draws=S_draws, G_draws=G_draws, W_draws=W_draws, Sigma_draws=Sigma_draws, sigma_w_samples=sigma_w_samples, sigma_e_samples=sigma_e_samples, s_samples=s_samples))
  }
  
  # ==============================================================================
  # Optional: Posterior predictive (replicate) draws
  # ==============================================================================
  
  y_rep = array(0, dim = c(TT, n, ndraw))
  u_rep = array(0, dim = c(TT, n, ndraw))
  z_rep = array(0, dim = c(TT, n, ndraw))
  
  a_rep = array(0, dim=c(TT, ndraw))
  
  angle_from_state = array(0, dim = c(TT, ndraw))
  state = array(0, dim = c(TT, n, ndraw))
  
  ######
  #rep_x = log(rep_x)
  
  for (draw in 1:ndraw){
    for (t in (1+p_max):TT) {
      noise = mvrnorm(1, mu=c(0,0), Sigma=Sigma_draws[,,draw])
      
      y_rep[t, , draw] = diag(n) %*% S_draws[t,,draw] + noise
      
      vec <- y_rep[t, , draw]
      u_rep[t, , draw] <- vec / sqrt(sum(vec^2))
      
      u_to_angle = matrix(u_rep[t, , draw], nrow=1, ncol=n)
      a_rep[t, draw] = unitcircle2radians(u_to_angle)
      
      
    }
    #y_rep[,draw,] = diag(n) %x% t(rep_x[draw]) * S_draws[,,ndraw] + mvrnorm(TT, mu=c(0,0), Sigma=Sigma_draws[,,draw])
  }
  
  return(list(S_draws=S_draws, G_draws=G_draws, W_draws=W_draws, Sigma_draws=Sigma_draws, sigma_w_samples=sigma_w_samples, sigma_e_samples=sigma_e_samples, s_samples=s_samples, logx_rep=logx_rep, u_rep=u_rep, a_rep=a_rep))
}


indep_point_estimation = function(posterior_samples){
  
  n=2
  p=2
  
  u_rep = posterior_samples$u_rep
  
  
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
  return(list(u_med=u_med, a_med=a_med))
}

indep_forecast_samples = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1, custom_times = NA) {
  
  n=2
  p=2
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n)
  }
  
  # ==============================================================================
  # Forecasting
  # ==============================================================================
  
  forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  model_median = matrix(0, TT, n)
  
  #forecast_sigma_w_samples = array(0, dim=c(ndraw, TT))
  #forecast_sigma_e_samples = array(0, dim=c(ndraw, TT))
  #forecast_s_samples = array(0, dim=c(ndraw, TT, n))
  
  #forecast_G_draws = array(0, dim=c(TT, p, p, ndraw ))
  
  
  start = max(1, TT-h+1)
  #start=TT
  # Compile the model
  speed_model <- stan_model("WindSpeed/models/1A/rw.stan")
  
  outer_interval =  start:TT
  
  if (sum(!is.na(custom_times)) > 0){
    print("Using custom times")
    outer_interval = custom_times
    
  }
  
  for (t in outer_interval) {
    
    data_list <- list(
      T = t-1,
      x = logx[1:(t-1)]
    )
    
    # Sample from the posterior
    fit <- sampling(speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
    
    posterior <- rstan::extract(fit)
    
    #forecast_sigma_w_samples[,t] <- posterior$sigma_w
    sigma_w_samples <- posterior$sigma_w
    #forecast_sigma_e_samples[,t] <- posterior$sigma_e
    sigma_e_samples <- posterior$sigma_e
    s_samples <- posterior$s
    
    pdlm_burn = 1000
    pdlm_thin = 1
    pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)
    
    #forecast_G_draws[t,,,] = pdlm_draws$G
    G_draws = pdlm_draws$G
    #forecast_W_draws[t,,,] = pdlm_draws$W
    W_draws = pdlm_draws$W
    #forecast_V_draws[t,,,] = pdlm_draws$Sigma
    V_draws = pdlm_draws$Sigma
    #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
    S_draws = pdlm_draws$S[t-1,,]
    
    
    for(m in 1:ndraw){
      new_s = rnorm(1, mean = s_samples[m, t-1], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s, sd = sqrt(sigma_e_samples[m]))
      
      speed_forecasts[m, t] = new_x
      
      old_s = S_draws[ , m]
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n) #logx[t]
      
      # TODO: Reuse variable name new_s?  
      new_s = G %*% old_s + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_s + mvrnorm(n = 1, mu = numeric(n), V) 
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
    }
    
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts) )
}



indep_forecast_ahead_samples = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1) {
  
  n=2
  p=2
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n)
  }
  
  # ==============================================================================
  # Forecasting
  # ==============================================================================
  
  forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  model_median = matrix(0, TT, n)
  
  #forecast_sigma_w_samples = array(0, dim=c(ndraw, TT))
  #forecast_sigma_e_samples = array(0, dim=c(ndraw, TT))
  #forecast_s_samples = array(0, dim=c(ndraw, TT, n))
  
  #forecast_G_draws = array(0, dim=c(TT, p, p, ndraw ))
  
  
  start = max(1, TT-h+1)
  t= start
  #start=TT
  # Compile the model
  speed_model <- stan_model("WindSpeed/models/1A/rw.stan")
  
  
  data_list <- list(
    T = t-1,
    x = logx[1:(t-1)]
  )
  
  # Sample from the posterior
  fit <- sampling(speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
  
  posterior <- rstan::extract(fit)
  
  #forecast_sigma_w_samples[,t] <- posterior$sigma_w
  sigma_w_samples <- posterior$sigma_w
  #forecast_sigma_e_samples[,t] <- posterior$sigma_e
  sigma_e_samples <- posterior$sigma_e
  s_samples <- posterior$s
  
  pdlm_burn = 1000
  pdlm_thin = 1
  pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)
  
  #forecast_G_draws[t,,,] = pdlm_draws$G
  G_draws = pdlm_draws$G
  #forecast_W_draws[t,,,] = pdlm_draws$W
  W_draws = pdlm_draws$W
  #forecast_V_draws[t,,,] = pdlm_draws$Sigma
  V_draws = pdlm_draws$Sigma
  #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
  S_draws = pdlm_draws$S[t-1,,]
  
  
  old_s = s_samples[, t-1]
  old_S = S_draws
  for (t in start:TT) {
    
    for(m in 1:ndraw){
      new_s = rnorm(1, mean = old_s[m], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s, sd = sqrt(sigma_e_samples[m]))
      old_s[m] = new_s
      
      speed_forecasts[m, t] = new_x
      
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n) #logx[t]
      
      new_S = G %*% old_S[,m] + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_S + mvrnorm(n = 1, mu = numeric(n), V) 
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
      old_S[,m] = new_S
    }
    
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts) )
}



