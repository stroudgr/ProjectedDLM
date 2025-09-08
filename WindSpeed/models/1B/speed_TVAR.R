speed_tvar_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform=function(x){log(x+1)}, params) {

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
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }

  U = radians2unitcircle(a)

  

  # ==============================================================================
  # TVAR model
  # ==============================================================================

  p_max = 1
  #out = tvar(logx, p_max=p_max, D=1, nsave=ndraw)
  #logx_rep = out$yhat
  #names(out)

  model <- stan_model("WindSpeed/models/1B/speed_tvar.stan")

  data_list <- list(
    T = TT,
    x = logx,
    H = 1
  )

  refresh = ifelse(stan_output, max(ndraw/10, 1), 0)
  
  # Sample from the posterior
  fit <- sampling(model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42, refresh=refresh)
  
  posterior <- rstan::extract(fit)
  
  logx_rep = posterior$x_rep



  # ==============================================================================
  # U|X model posterior samples
  # ==============================================================================
  
  
  # MCMC params
  pdlm_burn = 1000
  pdlm_thin = 1
  
  
  #pdlm_draws = gibbs_pdlm_basic(U, FF, V, G, W, s1, P1, r0, ndraw, pdlm_burn, pdlm_thin)
  
  pdlm_draws = gibbs_pdlm(U[1:TT, ], FF[, , 1:TT], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)


  if(!replicates) {
    return(pdlm_draws)
  } 

  # ==============================================================================
  # Posterior predictive (replicate) draws
  # ==============================================================================
  
  S_draws = pdlm_draws$S#[TT,,]
  Sigma_draws = pdlm_draws$Sigma
  G_draws = pdlm_draws$G
  W_draws = pdlm_draws$W

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
      
      y_rep[t, , draw] = (diag(n) %x% t(logx_rep[draw,t-p_max])) %*% S_draws[t,,draw] + noise
      
      #z_rep[t, , draw] = S_draws[t,,draw] + noise
      
      #state[t,,draw] = (diag(n) %x% t(logx_rep[draw,t])) %*% S_draws[t,,draw]
      #st = (diag(n) %x% t(logx_rep[draw,t])) %*% S_draws[t,,draw]
      #st = matrix(st, nrow=1, ncol=length(st))
      #angle_from_state[t, draw] = unitcircle2radians(st)
      
      
      vec <- y_rep[t, , draw]
      u_rep[t, , draw] <- vec / sqrt(sum(vec^2))
      
      u_to_angle = matrix(u_rep[t, , draw], nrow=1, ncol=n)
      a_rep[t, draw] = unitcircle2radians(u_to_angle)
      
      
    }
    #y_rep[,draw,] = diag(n) %x% t(rep_x[draw]) * S_draws[,,ndraw] + mvrnorm(TT, mu=c(0,0), Sigma=Sigma_draws[,,draw])
  }

  return(list(S_draws=S_draws, G_draws=G_draws, W_draws=W_draws, Sigma_draws=Sigma_draws, logx_rep=logx_rep, u_rep=u_rep, a_rep=a_rep))
}



speed_tvar_point_estimation = function(posterior_samples) {

  n=2
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



speed_tvar_forecast_samples = function(x,a, ndraw=1000, xtransform = function(x){log(x+1)}, h=1, custom_times=NA){

  n=2
  p=2
  p_max=1
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }
  
  #
  # Forecasting
  #
  
  forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  model_median = matrix(0, TT, n)

  pdlm_burn = 1000
  pdlm_thin = 1
  
  start = TT-h+1
  
  model <- stan_model("WindSpeed/models/1B/speed_tvar.stan")
  
  outer_interval = start:TT
  
  if (sum(!is.na(custom_times)) > 0){
    print("Using custom times")
    outer_interval = custom_times
    
  }
  
  for (t in outer_interval) {
    
    p_max = 1
    #out = tvar(logx[1:(t-1)], p_max=p_max, D=1, nsave=ndraw)
    #names(out)
    
    data_list <- list(
      T = t-1,
      x = logx[1:(t-1)],
      H = 1
    )
    
    
    
    # Sample from the posterior
    fit <- sampling(model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
    
    posterior <- rstan::extract(fit)
    
    sigma_w_samples <- posterior$sigma_w
    sigma_e_samples <- posterior$sigma_e
    
    s_samples <- posterior$s
  
    
    pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)
    
    G_draws = pdlm_draws$G
    W_draws = pdlm_draws$W
    V_draws = pdlm_draws$Sigma
    S_draws = pdlm_draws$S[t-1,,]
    
    old_x = logx[t-1]
    for(m in 1:ndraw){
      new_s = rnorm(1, mean = s_samples[m, t-1], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s*old_x, sd = sqrt(sigma_e_samples[m]))
      
      speed_forecasts[m, t] = new_x
      
      
      old_s = S_draws[ , m]
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n) * new_x #logx[t]
      
      # TODO: Reuse variable name new_s?  
      new_s = G %*% old_s + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_s + mvrnorm(n = 1, mu = numeric(n), V) 
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
    }
  }

  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts))
}







speed_tvar_forecast_ahead_samples = function(x,a, ndraw=1000, xtransform = function(x){log(x+1)}, h=1){
  
  n=2
  p=2
  p_max=1
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
  }
  
  #
  # Forecasting
  #
  
  forecasts = array(0, dim = c(ndraw, n, TT))
  speed_forecasts = array(0, dim = c(ndraw, TT))
  model_median = matrix(0, TT, n)
  
  pdlm_burn = 1000
  pdlm_thin = 1
  
  start = TT-h+1
  t=start
  
  model <- stan_model("WindSpeed/models/1B/speed_tvar.stan")
  
  p_max = 1
  #out = tvar(logx[1:(t-1)], p_max=p_max, D=1, nsave=ndraw)
  #names(out)
  
  data_list <- list(
    T = t-1,
    x = logx[1:(t-1)],
    H = 1
  )
  
  
  
  # Sample from the posterior
  fit <- sampling(model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
  
  posterior <- rstan::extract(fit)
  
  sigma_w_samples <- posterior$sigma_w
  sigma_e_samples <- posterior$sigma_e
  
  s_samples <- posterior$s
  
  
  pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin)
  
  G_draws = pdlm_draws$G
  W_draws = pdlm_draws$W
  V_draws = pdlm_draws$Sigma
  S_draws = pdlm_draws$S[t-1,,]
  
  old_s = s_samples[, t-1]
  old_S = S_draws
  for (t in start:TT) {
    
    old_x = logx[t-1]
    for(m in 1:ndraw){
      new_s = rnorm(1, mean = old_s[m], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s*old_x, sd = sqrt(sigma_e_samples[m]))
      
      speed_forecasts[m, t] = new_x
      
      
      
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n) * new_x #logx[t]
      
      new_S = G %*% old_S[, m] + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_S + mvrnorm(n = 1, mu = numeric(n), V) 
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
      old_S[,m] = new_S
    }
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts))
}








