#source("_packages.R")
#source("helpers/_helpers.R")
library(Rfast)
library(tvReg)
library(rstan)

# Prereq: a should be radians.
speed_rw_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform=function(x){log(x+1)}) {
  
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
  # Step 1: Instantiate random walk plus noise model.
  # ==============================================================================
  
  
  # Compile the model
  speed_model <- SSModel(logx ~ -1 + SSMcustom(
                                            Z = array(1, dim = c(1, 1, TT)),     # Observation matrix Z_t = 1
                                            T = array(1, dim = c(1, 1, TT)), # Transition matrix T_t = 1
                                            R = array(1, dim = c(1, 1, TT)),
                                            Q = matrix(1),
                                            a1 = matrix(0),
                                            P1 = matrix(1000),
                                            P1inf = matrix(1)), H = matrix(1))
  
  speed_model =  SSModel(logx ~ SSMtrend(degree = 1, Q = list(NA)), H = NA)
  
  # MLE so should not be used....
  speed_model_fit = fitSSM(speed_model, logx,
                           method = "BFGS")$model
  
  # ==============================================================================
  # Step 2: Fit speed model, generate samples (parameters, posterior predictive).
  # ==============================================================================
  
  fitted_model <- speed_model_fit$model
  
  
  kfs <- KFS(fitted_model, filtering = "state", smoothing = "state")
  
  # Filtered estimates (predictions based only on past)
  filtered <- kfs$a[-1]  # drop first NA
  filtered_var <- kfs$P[-1]  # variance of filtered state
  
  # Smoothed estimates (use full data for each time point)
  smoothed <- kfs$alphahat
  smoothed_var <- kfs$V
  
  # Filtered predictions (one-step ahead)
  logx_filt <- predict(fitted_model, interval = "prediction", type = "response", filtered = TRUE)
  
  # Smoothed predictions (retrospective fit)
  logx_smooth <- predict(fitted_model, interval = "prediction", type = "response", filtered = FALSE)
  #logx_rep <- posterior$x_rep
  
  
  
  
  
  
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
    return(pdlm_draws)
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
      
      y_rep[t, , draw] = (diag(n) %x% t(logx_rep[draw,t-p_max])) %*% S_draws[t,,draw] + noise
      
      vec <- y_rep[t, , draw]
      u_rep[t, , draw] <- vec / sqrt(sum(vec^2))
      
      u_to_angle = matrix(u_rep[t, , draw], nrow=1, ncol=n)
      a_rep[t, draw] = unitcircle2radians(u_to_angle)
      
      
    }
    #y_rep[,draw,] = diag(n) %x% t(rep_x[draw]) * S_draws[,,ndraw] + mvrnorm(TT, mu=c(0,0), Sigma=Sigma_draws[,,draw])
  }
  
  #TODO not everything
  return(list(pdlm_draws, u_rep))
}


speed_rw_point_estimation = function(posterior_samples){
  
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

speed_rw_forecast_samples = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}) {
  
  n=2
  p=2
  #TT = dim(post_samples$S_draws)[1]
  TT = length(x)
  
  logx=xtransform(x)
  U = radians2unitcircle(a)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n) * logx[t] # diag(n) %x% t(x[t])
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
  
  
  start = max(1, TT-20)
  start = TT
  
  
  
  for (t in start:TT) {
    
    speed_model <- SSModel(logx[1:(t-1)] ~ -1 + SSMcustom(Z = 1,
                                                 T = 1,
                                                 R = 1,
                                                 Q = 1,
                                                 a1 = 0,
                                                 P1 = 1,
                                                 P1inf = matrix(0, p, p)), H = 1)
    
    
    # MLE so should not be used....
    speed_model_fit = fitSSM(speed_model, logx[1:(t-1)],
                             method = "BFGS")$model
    fit <- sampling(speed_model, data = data_list, chains = 4, iter = ndraw, warmup = round(ndraw/2), seed = 42)
    fitted_model <- fit$model
    
    
    kfs <- KFS(fitted_model, filtering = "state", smoothing = "state")
    
    
    
    
    
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
      
      Ft = diag(n) * new_x #logx[t]
      
      # TODO: Reuse variable name new_s?  
      new_s = G %*% old_s + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_s + mvrnorm(n = 1, mu = numeric(n), V) 
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
    }
    
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts) )
}





