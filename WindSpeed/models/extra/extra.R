extra_posterior_samples = function(a, x, ndraw=1000, replicates=FALSE, xtransform=function(x){log(x+1)}) {
  
  U = radians2unitcircle(a)
  n=2
  TT = length(a)
  
  logx = xtransform(x)
  
  FF = array(0, c(n, n, TT))
  for (t in 1:TT) {
    FF[, , t] = diag(n)
  }
  
  pdlm_draws = gibbs_pdlm_extra(U, FF, x=logx)
  
  if (!replicates) {
    return(pdlm_draws)
  }
  
  S_draws = pdlm_draws$S#[TT,,]
  Sigma_draws = pdlm_draws$Sigma
  G_draws = pdlm_draws$G
  W_draws = pdlm_draws$W
  
  # ==============================================================================
  # Optional: Posterior predictive (replicate) draws
  # ==============================================================================
  
  y_rep = array(0, dim = c(TT, n, ndraw))
  u_rep = array(0, dim = c(TT, n, ndraw))
  z_rep = array(0, dim = c(TT, n, ndraw))
  logx_rep = array(0, dim = c(ndraw, TT))
  
  a_rep = array(0, dim=c(TT, ndraw))
  
  angle_from_state = array(0, dim = c(TT, ndraw))
  state = array(0, dim = c(TT, n, ndraw))
  
  ######
  #rep_x = log(rep_x)
  
  p_max = 1
  for (draw in 1:ndraw){
    for (t in (1+p_max):TT) {
      noise = mvrnorm(1, mu=c(0,0), Sigma=Sigma_draws[,,draw])
      
      y_rep[t, , draw] = (diag(n) %x% t(logx_rep[draw,t-p_max])) %*% S_draws[t,,draw] + noise
      
      
      vec <- y_rep[t, , draw]
      logx_rep[draw,t] = sqrt(sum(vec^2))
      u_rep[t, , draw] <- vec / sqrt(sum(vec^2))
      
      u_to_angle = matrix(u_rep[t, , draw], nrow=1, ncol=n)
      a_rep[t, draw] = unitcircle2radians(u_to_angle)
      
      
    }
    #y_rep[,draw,] = diag(n) %x% t(rep_x[draw]) * S_draws[,,ndraw] + mvrnorm(TT, mu=c(0,0), Sigma=Sigma_draws[,,draw])
  }
  
  return(list(S_draws=S_draws, G_draws=G_draws, W_draws=W_draws, Sigma_draws=Sigma_draws, logx_rep=logx_rep, u_rep=u_rep, a_rep=a_rep))
  
  
  
}

extra_point_estimation = function(posterior_samples){
    
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


extra_forecast_samples = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1) {
  
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
  
  start = min(max(1, TT-h+1), TT)
  outer_interval = start:TT
  
  for (t in outer_interval) {
    
    pdlm_burn = 1000
    pdlm_thin = 1
    pdlm_draws = gibbs_pdlm_extra(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, x=logx[1:(t-1)])
    
    #forecast_G_draws[t,,,] = pdlm_draws$G
    G_draws = pdlm_draws$G
    #forecast_W_draws[t,,,] = pdlm_draws$W
    W_draws = pdlm_draws$W
    #forecast_V_draws[t,,,] = pdlm_draws$Sigma
    V_draws = pdlm_draws$Sigma
    #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
    S_draws = pdlm_draws$S[t-1,,]
    
    prev_S = S_draws
    
    for(m in 1:ndraw){
      
      old_S = prev_S[, m]
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n)
      
      new_S = G %*% prev_S[,m] + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_S + mvrnorm(n = 1, mu = numeric(n), V) 
      
      speed_forecasts[m, t] <- sqrt(sum(new_y^2))
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
    }
    
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts) )
  
}


extra_forecast_ahead_samples = function(x,a, ndraw=1000, xtransform=function(x){log(x+1)}, h=1) {
  
  n=2
  p=2
  
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

  start = min(max(1, TT-h+1), TT)
  t= start 
  
  pdlm_burn = 1000
  pdlm_thin = 1
  pdlm_draws = gibbs_pdlm(U[1:(t-1), ], FF[, , 1:(t-1)], ndraw = ndraw, burn = pdlm_burn, thin = pdlm_thin, x=logx[1:(t-1)])
  
  #forecast_G_draws[t,,,] = pdlm_draws$G
  G_draws = pdlm_draws$G
  #forecast_W_draws[t,,,] = pdlm_draws$W
  W_draws = pdlm_draws$W
  #forecast_V_draws[t,,,] = pdlm_draws$Sigma
  V_draws = pdlm_draws$Sigma
  #forecast_S_draws[t,1:(t-1),,] = pdlm_draws$S[t-1,,]
  S_draws = pdlm_draws$S[t-1,,]
  
  #start=TT
  outer_interval = start:TT
  
  prev_S = S_draws # note [t-1,] indexing is already done above
  
  for (t in outer_interval) {
    
    for(m in 1:ndraw){
      
      old_S = prev_S[, m]
      G = G_draws[, , m]
      W = W_draws[, , m]
      V = V_draws[, , m]
      
      Ft = diag(n)
      
      # TODO: Reuse variable name new_s?  
      new_S = G %*% prev_S[,m] + mvrnorm(n = 1, mu = numeric(p), W)
      new_y = Ft %*% new_S + mvrnorm(n = 1, mu = numeric(n), V) 
      
      speed_forecasts[m, t] = sqrt(sum(new_y^2))
      
      forecasts[m, , t] = new_y / sqrt(sum(new_y^2))
      
      prev_S[,m] = new_S
      
    }
    
  }
  
  return(list(direction_forecasts=forecasts, speed_forecasts=speed_forecasts) )
  
  
}




# ==============================================================================
# ==============================================================================
# Gibbs sampler for (r, S, G, W, V)
# ==============================================================================
# ==============================================================================

gibbs_pdlm_extra <- function(U, FF, prior = NULL, init = NULL, ndraw = 1000, burn = 0, thin = 1, x=NULL){
  # ----------------------------------------------------------------------------
  # dimensions
  # ----------------------------------------------------------------------------
  
  TT = nrow(U)
  n = ncol(U)
  p = dim(FF)[2]
  M = burn + thin * ndraw
  
  if(is.null(prior)){
    s1 = numeric(p)
    P1 = diag(p)
    gamma_prmean = numeric(n - 1)
    gamma_prvar = diag(n - 1)
    Gamma_prdf = n - 1 + 2
    Gamma_prscale = (Gamma_prdf - (n - 1) - 1) * diag(n - 1)
    v0 = p + 2
    V0 = (v0 - p - 1) * diag(p)
    B0 = matrix(0, p, p) #diag(p)
    invO0 = diag(p)
  }else{
    s1 = prior$s1
    P1 = prior$P1
    gamma_prmean = prior$gamma_prmean
    gamma_prvar = prior$gamma_prvar
    Gamma_prdf = prior$Gamma_prdf
    Gamma_prscale = prior$Gamma_prscale
    v0 = prior$v0
    V0 = prior$V0
    B0 = prior$B0
    invO0 = prior$invO0
  }
  GW_prior = list(v = v0, P = V0, B = B0, invO = invO0)
  
  # ----------------------------------------------------------------------------
  # create or unpack MCMC initialization
  # ----------------------------------------------------------------------------
  
  if(is.null(init)){
    G = matrix(0, p, p)
    W = diag(p)
    r = rep(1, TT)
    Gamma = diag(n - 1)
    gamma = numeric(n - 1)
    beta = array(0, n)
  }
  
  Mu = matrix(0, TT, n)
  Sigma = gams2Sigma(Gamma, gamma)
  
  mu_beta = rep(0, n) 
  Sigma_beta = diag(n)
  Sigma_beta_inv = diag(n)
  
  # ----------------------------------------------------------------------------
  # preallocate storage
  # ----------------------------------------------------------------------------
  
  S_draws = array(numeric(TT * p * ndraw), c(TT, p, ndraw))
  Sigma_draws = array(0, c(n, n, ndraw))
  G_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  W_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  r_draws = matrix(0, TT, ndraw)
  beta_draws = array(0, c(n, ndraw))
  
  
  a = unitcircle2radians(U)
  a_na_indices = which(is.na(a))
  U_na_indices = which(is.na(U), arr.ind = TRUE)
  #print(a_na_indices)
  
  # ----------------------------------------------------------------------------
  # off to war
  # ----------------------------------------------------------------------------
  
  draw = 0
  
  for (m in 1:M) {
    
    # --------------------------------------------------------------------------
    # draw from p(states | ...)
    # --------------------------------------------------------------------------
    
    Y = U * x
    
    model = SSModel(Y ~ -1 + SSMcustom(Z = FF,
                                       T = G,
                                       R = diag(p),
                                       Q = W,
                                       a1 = s1,
                                       P1 = P1,
                                       P1inf = matrix(0, p, p)),
                    H = Sigma)
    S = simulateSSM(model, type = "states", nsim = 1)[, , 1]
    
    for (t in 1:TT){
      Mu[t, ] = FF[, , t] %*% S[t, ]
    }
    
    
    for (t in a_na_indices) {
      Y[t,] = FF[, , t] %*% S[t, ] + mvrnorm(n = 1, mu = numeric(p), Sigma)
    }
    
    # --------------------------------------------------------------------------
    # draw from p(Sigma | ...)
    # --------------------------------------------------------------------------
    
    gamma = draw_gamma_given_else(Y, Mu, Gamma, gamma_prmean, gamma_prvar)
    
    Gamma = draws_Gamma_given_else(Y, Mu, gamma, Gamma_prdf, Gamma_prscale)
    
    Sigma = gams2Sigma(Gamma, gamma)
    
    # --------------------------------------------------------------------------
    # draw from p(G, W | ...)
    # --------------------------------------------------------------------------
    
    GW = sample_conjugate_posterior_varp(S, 1, FALSE, GW_prior, TRUE)
    G = t(GW$B)
    W = GW$S
    
    # --------------------------------------------------------------------------
    # draw from p(r | ...)
    # --------------------------------------------------------------------------
    
    #r = draw_lengths_given_else_FFS(U, FF, Sigma, r, S)
    
    if ( retain_draw(m, burn, thin) ) {
      draw = draw + 1
      S_draws[, , draw] = S
      G_draws[, , draw] = G
      W_draws[, , draw] = W
      #r_draws[, draw] = r
      Sigma_draws[, , draw] = Sigma
    }
  }
  
  #if (!regress) {
  #  draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws)
  #} else {
  draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws, beta=beta_draws)
  #} 
  return(draws)
}

