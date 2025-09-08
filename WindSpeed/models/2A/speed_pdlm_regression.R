
speed_pdlm_regression_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform=function(x){log(x+1)}, params) {

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
  p_max=1
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) #* logx[t] # diag(n) %x% t(x[t])
  }

  U = radians2unitcircle(a)

  logx = xtransform(x)
  
  # ==============================================================================
  # Speed model
  # ==============================================================================
  
  #d = 1
  #speed_model = SSModel(logx ~ -1 + SSMcustom(Z=diag(d),
  #                                                T=diag(d),
  #                                                R=diag(d),
  #                                                Q=diag(d),
  #                                                a1=rep(0,d),
  #                                                P1=diag(1),
  #                                                P1inf=matrix(0, d, d) ), H=diag(1)) 

  #  #alpha = simulateSSM(speed_model, type="states", nsim = ndraw)
  
  #speed_model_modified <- speed_model
  #speed_model_modified$y[] = NA
  
  #logx_rep = simulateSSM(speed_model_modified, type="obs", nsim = ndraw, conditional=TRUE)
  #logx_rep = logx_rep[,1,]
  #logx_rep = t(logx_rep)
  
  
  
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
  # PDLM and regression model
  # ==============================================================================


  # MCMC params
  pdlm_burn = 1000
  pdlm_thin = 1


  #pdlm_draws = gibbs_pdlm(U[1:TT, ], FF[, , 1:TT], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, regress=FALSE, x=logx)
  
  pdlm_draws = gibbs_pdlm(U[1:TT, ], FF[, , 1:TT], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, regress=TRUE, x=log(x+1))

  # ==============================================================================
  # Posterior predictive (replicate) draws
  # ==============================================================================

  S_draws = pdlm_draws$S#[TT,,]
  Sigma_draws = pdlm_draws$Sigma
  G_draws = pdlm_draws$G
  W_draws = pdlm_draws$W
  beta_draws = pdlm_draws$beta

  if (!replicates) {
    return(pdlm_draws)
  }
  
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
      
      y_rep[t, , draw] = xtransform(x[t])*beta_draws[, draw] + (diag(n) %x% t(logx_rep[draw,t-p_max])) %*% S_draws[t,,draw] + noise
      
      vec <- y_rep[t, , draw]
      u_rep[t, , draw] <- vec / sqrt(sum(vec^2))
      
      u_to_angle = matrix(u_rep[t, , draw], nrow=1, ncol=n)
      a_rep[t, draw] = unitcircle2radians(u_to_angle)
      
      
    }
    #y_rep[,draw,] = diag(n) %x% t(rep_x[draw]) * S_draws[,,ndraw] + mvrnorm(TT, mu=c(0,0), Sigma=Sigma_draws[,,draw])
  }
  
  return(list(pdlm_draws=pdlm_draws, u_rep=u_rep, a_rep=a_rep, logx_rep = logx_rep))
  
}

mediandirALT = function (x) 
{
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  u1 <- Rfast::colMedians(x)
  u1 <- u1/sqrt(sum(u1^2))
  ww <- 1/as.vector(sqrt(abs(1 - (x %*% u1)^2)))
  if (is.infinite(max(ww))) {
    u2 <- u1
  }
  else u2 <- Rfast::colsums(x * ww)
  u2 <- u2/sqrt(sum(u2^2))
  while (sum(abs(u2 - u1)) > 1e-04) {
    u1 <- u2
    ww <- 1/as.vector(sqrt(abs(1 - (x %*% u1)^2)))
    if (is.infinite(max(ww))) {
      u2 <- u1
    }
    else u2 <- Rfast::colsums(x * ww)
    u2 <- u2/sqrt(sum(u2^2))
  }
  u2
}


speed_pdlm_regression_point_estimation = function(posterior_samples){
  p_max=1
  n=2
  p=2
  
  u_rep = posterior_samples$u_rep
  
  u_med = array(0, dim = c(TT, n))
  a_med = rep(0, TT)
  median_state = array(0, dim=c(TT,n))
  median_angle_from_state = rep(0, TT)
  
  f = function(t) {
    tryCatch({
      u_med[t,] = t(  mediandir( t(u_rep[t,,]) )  )
    }, TimeoutException = function(e) {
      message(paste("Timeout on mediandir at time ", t))
    })
  }
  
  for (t in (1+p_max):TT) {
    #evalWithTimeout(fn(t), timeout = 1, onTimeout = "error")
    u_med[t,] = t(  mediandirALT( t(u_rep[t,,]) )  )
    #cat(t, "is done")
  }
  a_med = unitcircle2radians(u_med)
  
  return(list(u_med=u_med, a_med=a_med))
}






speed_pdlm_regression_forecast_samples = function(x,a, ndraw=1000, xtransform = function(x){log(x+1)}, h=1, custom_times=NA)  {

  n=2
  p=2
  p_max=1
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) #* logx[t] # diag(n) %x% t(x[t])
  }
  
  # ==============================================================================
  # Forecasting
  # ==============================================================================
  
  speed_forecasts = array(0, dim= c(ndraw, TT))
  direction_forecasts = array(0, dim = c(ndraw, n, TT))
  model_median = matrix(0, TT, n)
  
  #forecast_sigma_w_samples = array(0, dim=c(ndraw, TT))
  #forecast_sigma_e_samples = array(0, dim=c(ndraw, TT))
  #forecast_s_samples = array(0, dim=c(ndraw, TT, n))
  
  #forecast_G_draws = array(0, dim=c(TT, p, p, ndraw ))
  
  speed_model <- stan_model("WindSpeed/models/1A/rw.stan")
  
  #start = TT
  start = max(1, TT-h+1)
  partial_x = logx
  partial_x[start:TT] = NA
  
  outer_interval = start:TT
  
  if (sum(!is.na(custom_times)) > 0){
    print("Using custom times")
    outer_interval = custom_times
    
  }
  
  for (t in outer_interval) {
    
    #d = 1
    #speed_model = SSModel(partial_x ~ -1 + SSMcustom(Z=diag(d),
    #                                            T=diag(d),
    #                                            R=diag(d),
    #                                            Q=diag(d),
    #                                            a1=rep(0,d),
    #                                            P1=diag(1),
    #                                            P1inf=matrix(0, d, d) ), H=diag(1)) 
    
    #partial_x[t] = logx[t]
    
    #logx_one_step_forecast = simulateSSM(speed_model, type="obs", nsim = ndraw, conditional=TRUE)#[,1,]
    #logx_one_step_forecast = logx_one_step_forecast[t,1,]
    
    
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
    pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, regress = TRUE, x=logx[1:(t-1)])
    
    #forecast_G_draws[t,,,] = pdlm_draws$G
    G_draws = pdlm_draws$G
    #forecast_W_draws[t,,,] = pdlm_draws$W
    W_draws = pdlm_draws$W
    #forecast_V_draws[t,,,] = pdlm_draws$Sigma
    V_draws = pdlm_draws$Sigma
    #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
    S_draws = pdlm_draws$S[t-1,,]
    
    beta_draws = pdlm_draws$beta
    
    for(m in 1:ndraw){
      
      new_s = rnorm(1, mean = s_samples[m, t-1], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s, sd = sqrt(sigma_e_samples[m]))
      #new_x = logx_one_step_forecast[m]
      
      speed_forecasts[m,t] = new_x
      
      old_s = S_draws[ , m]
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      beta = beta_draws[,m]
      
      Ft = diag(n)
      
      # TODO: Reuse variable name new_s?  
      new_s = G %*% old_s + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = new_x*beta +  Ft %*% new_s + mvrnorm(n = 1, mu = numeric(n), V) 
      direction_forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
    }
    
  }
  
  return(list(speed_forecasts = speed_forecasts, direction_forecasts=direction_forecasts))
}





speed_pdlm_regression_forecast_ahead_samples = function(x,a, ndraw=1000, xtransform = function(x){log(x+1)}, h=1)  {
  
  n=2
  p=2
  p_max=1
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) #* logx[t] # diag(n) %x% t(x[t])
  }
  
  # ==============================================================================
  # Forecasting
  # ==============================================================================
  
  speed_forecasts = array(0, dim= c(ndraw, TT))
  direction_forecasts = array(0, dim = c(ndraw, n, TT))
  model_median = matrix(0, TT, n)
  
  
  speed_model <- stan_model("WindSpeed/models/1A/rw.stan")
  
  #start = TT
  start = max(1, TT-h+1)
  partial_x = logx
  partial_x[start:TT] = NA
  
  t=start
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
  pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, regress = TRUE, x=logx[1:(t-1)])
  
  #forecast_G_draws[t,,,] = pdlm_draws$G
  G_draws = pdlm_draws$G
  #forecast_W_draws[t,,,] = pdlm_draws$W
  W_draws = pdlm_draws$W
  #forecast_V_draws[t,,,] = pdlm_draws$Sigma
  V_draws = pdlm_draws$Sigma
  #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
  S_draws = pdlm_draws$S[t-1,,]
  
  beta_draws = pdlm_draws$beta
  
  
  s_old = s_samples[, start-1]
  old_S = S_draws
  for (t in start:TT) {
    
    
    for(m in 1:ndraw){
      
      new_s = rnorm(1, mean = s_old[m], sd = sqrt(sigma_w_samples[m]))
      new_x = rnorm(1, mean = new_s, sd = sqrt(sigma_e_samples[m]))
      #new_x = logx_one_step_forecast[m]
      s_old[m] = new_s
      
      speed_forecasts[m,t] = new_x
      
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      beta = beta_draws[,m]
      
      Ft = diag(n)
       
      new_S = G %*% old_S[,m] + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = new_x*beta +  Ft %*% new_S + mvrnorm(n = 1, mu = numeric(n), V) 
      direction_forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
      old_S[,m] = new_S
    }
    
  }
  
  return(list(speed_forecasts = speed_forecasts, direction_forecasts=direction_forecasts))
}

