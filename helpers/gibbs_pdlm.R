# ==============================================================================
# ==============================================================================
# Gibbs sampler for (r, S) with (V, G, W) fixed
# ==============================================================================
# ==============================================================================

gibbs_pdlm_basic <- function(U, FF, V, G, W, s1, P1, r0, ndraw, burn, thin){
  # U:  TT x n matrix
  # FF: n x p x TT array
  # V:  n x n matrix
  # G:  p x p matrix
  # W:  p x p matrix
  # s1: p-vector
  # P1: p x p matrix
  # r0: TT-vector
  # ndraw, burn, thin: integers
  
  # ----------------------------------------------------------------------------
  # dimensions
  # ----------------------------------------------------------------------------
  
  TT = nrow(U)
  n = ncol(U)
  p = dim(FF)[2]
  M = burn + thin * ndraw
  
  # ----------------------------------------------------------------------------
  # preallocate storage
  # ----------------------------------------------------------------------------
  
  S_draws = array(numeric(TT * p * ndraw), c(TT, p, ndraw))
  r_draws = matrix(0, TT, ndraw)
  
  # ----------------------------------------------------------------------------
  # initialize
  # ----------------------------------------------------------------------------
  
  r = r0
  draw = 0
  
  for (m in 1:M) {
    
    #---------------------------------------------------------------------------
    #  draw from p(states | ...)
    #---------------------------------------------------------------------------
    
    Y = U * r
    
    model = SSModel(Y ~ -1 + SSMcustom(Z = FF,
                                       T = G,
                                       R = diag(p),
                                       Q = W,
                                       a1 = s1,
                                       P1 = P1,
                                       P1inf = matrix(0, p, p)),
                    H = V)
    S = simulateSSM(model, type = "states", nsim = 1)[, , 1]
    
    #---------------------------------------------------------------------------
    #  draw from p(r | ...)
    #---------------------------------------------------------------------------
    
    r = draw_lengths_given_else_FFS(U, FF, V, r, S)
    
    #---------------------------------------------------------------------------
    #  store draw
    #---------------------------------------------------------------------------
    
    if ( retain_draw(m, burn, thin) ) {
      draw = draw + 1
      S_draws[, , draw] = S
      r_draws[, draw] = r
    }
  }
  
  draws = list(S = S_draws, r = r_draws)
  
  return(draws)
  
}

# ==============================================================================
# ==============================================================================
# Gibbs sampler for (r, S, G, W) with V fixed
# ==============================================================================
# ==============================================================================

gibbs_pdlm_intermediate <- function(U, FF, V, priorparams, init, ndraw, burn, thin){
  # ----------------------------------------------------------------------------
  # dimensions
  # ----------------------------------------------------------------------------
  
  TT = nrow(U)
  n = ncol(U)
  p = dim(FF)[2]
  M = burn + thin * ndraw
  
  # ----------------------------------------------------------------------------
  # preallocate storage
  # ----------------------------------------------------------------------------
  
  S_draws = array(numeric(TT * p * ndraw), c(TT, p, ndraw))
  r_draws = matrix(0, TT, ndraw)
  G_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  W_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  
  # ----------------------------------------------------------------------------
  # unpack prior
  # ----------------------------------------------------------------------------
  
  GW_prior = list(v = priorparams$v, P = priorparams$P, 
                  B = priorparams$B, invO = priorparams$invO)
  s1 = priorparams$s1
  P1 = priorparams$P1
  
  # ----------------------------------------------------------------------------
  # initialize
  # ----------------------------------------------------------------------------
  
  r = init$r
  G = init$G
  W = init$W
  draw = 0
  
  for (m in 1:M) {
    
    #---------------------------------------------------------------------------
    #  draw from p(states | ...)
    #---------------------------------------------------------------------------
    
    Y = U * r
    
    model = SSModel(Y ~ -1 + SSMcustom(Z = FF,
                                       T = G,
                                       R = diag(p),
                                       Q = W,
                                       a1 = s1,
                                       P1 = P1,
                                       P1inf = matrix(0, p, p)),
                    H = V)
    S = simulateSSM(model, type = "states", nsim = 1)[, , 1]
    
    #---------------------------------------------------------------------------
    #  draw from p(G, W | ...)
    #---------------------------------------------------------------------------
    
    GW = sample_conjugate_posterior_varp(S, 1, FALSE, GW_prior, TRUE)
    G = t(GW$B)
    W = GW$S
    
    #---------------------------------------------------------------------------
    #  draw from p(r | ...)
    #---------------------------------------------------------------------------
    
    r = draw_lengths_given_else_FFS(U, FF, V, r, S)
    
    #---------------------------------------------------------------------------
    #  store draw
    #---------------------------------------------------------------------------
    
    if ( retain_draw(m, burn, thin) ) {
      draw = draw + 1
      S_draws[, , draw] = S
      r_draws[, draw] = r
      G_draws[, , draw] = G
      W_draws[, , draw] = W
    }
  }
  
  draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws)
  
  return(draws)
}

# ==============================================================================
# ==============================================================================
# Gibbs sampler for (r, S, G, W, V)
# ==============================================================================
# ==============================================================================

gibbs_pdlm <- function(U, FF, prior = NULL, init = NULL, ndraw = 1000, burn = 0, thin = 1, regress=FALSE, logx=NULL, params=list()){
  
  # params 
  #speed_model="NA", miss_speed_post=NA, speed_post_samples = NA, verbose=FALSE, spatial_confound=FALSE
  
  speed_model = NA
  if ("speed_model" %in% params) {
    speed_model = params[["speed_model"]]
  }
  speed_model = ifelse(speed_model=="NA", NA, speed_model)
  if (!(speed_model %in% c(NA, "NA", "A", "B"))) {
    stop("Error in gibbs_pdlm: invalid speed_model parameter. Must be one of NA, A, B")
  }
  
  miss_speed_post = params[["miss_speed_post"]]
  if (!is.na(speed_model) & !is.function(miss_speed_post)){
    stop("miss_speed_post must be a function")
  }
  
  speed_post_samples = params[["speed_post_samples"]]
  if (!is.na(speed_model) & is.null(speed_post_samples)){
    stop("speed_post_samples must be data")
  }
  
  #TODO check dimension of speed_post_samples.
  # 
  #if (!is.na(speed_model) & ){
    #stop("speed_post_samples must be data")
  #}
  
  verbose = FALSE
  if ( "verbose" %in% params) {
    if (params[["verbose"]] == TRUE ) {
      verbose = TRUE
    }
  }
  
  spatial_confound = FALSE
  if ("spatial_confound" %in% params) {
    if (  params[["spatial_confound"]] == TRUE ) {
      
      if (regress == FALSE) {
        stop("Spatial confound error: orthoganlization cannot be done when there are no regression parameters")
      }
      
      spatial_confound = TRUE
    }
  }
  
  speed_model = ifelse(is.na(speed_model), "NA", speed_model)
  
  
  
  prior = NULL #TODO!
  
  x = exp(logx)-1
  
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
  
  logx_na_indices = NA
  
  psi_draws = NA
  sigma_sq = NA
  
  if (!is.null(logx)) {
    logx_na_indices = which(is.na(logx))
    logx_not_na_indices = which(!is.na(logx))
    
    
    if (speed_model == "A") {
      psi_draws = speed_post_samples$psi
      sigma_sq = speed_post_samples$sigma_sq
    }
  }
  
  #print(a_na_indices)
  
  # ----------------------------------------------------------------------------
  # off to war
  # ----------------------------------------------------------------------------
  
  #TODO: not working
  #proj_X = x %*% solve(out(x,x)) %*% t(X)
  
  draw = 0
  ten_percents = round(seq(1, M, length.out = 10))
  #ten_percents = 1:M
  
  for (m in 1:M) {
    
    if (verbose){
      if (m %in% ten_percents){
        cat(paste0("PDLM Iteration ", m, " / ", M, "\n"))
      }
    }
    
    # --------------------------------------------------------------------------
    # draw from p(states | ...)
    # --------------------------------------------------------------------------
    
    Y = U * r
    
    if (regress) {
      for (i in 1:n) {
        Y[,i] = Y[,i] + beta[i]*x
      }
    } 
    
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
    #Y[a_na_indices, ] = FF[, , a_na_indices] %*% S[a_na_indices, ] + mvrnorm(n = length(a_na_indices), mu = numeric(p), Sigma)
    
    for (t in logx_na_indices) {
      if (speed_model == "A") {
        logx = speed_rw_noise_miss_full_posterior_helper(Y, logx, S, psi_draws, solve(Sigma), sigma_sq, logx_not_na_indices, logx_na_indices)
      }
    }
    
    
    
    # --------------------------------------------------------------------------
    # draw beta | ...
    # --------------------------------------------------------------------------
    if (regress) {
      
      synth_data = c(t(Y - Mu))
      V = kronecker(logx, diag(n)) # %x%
      inv_O = diag(TT) %x% solve(Sigma) 
      
      Q = Sigma_beta_inv + t(V) %*% inv_O %*% V
      Q_inv = solve(Q)
      
      ell = Sigma_beta_inv %*% mu_beta + t(V) %*% inv_O %*% (synth_data)
      beta = mvrnorm(1,Q_inv %*% ell, Sigma= Q_inv)
      
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
    
    
    r = draw_lengths_given_else_FFS(U, FF, Sigma, r, S)
    
    
    
    if ( retain_draw(m, burn, thin) ) {
      draw = draw + 1
      S_draws[, , draw] = S
      G_draws[, , draw] = G
      W_draws[, , draw] = W
      r_draws[, draw] = r
      Sigma_draws[, , draw] = Sigma
      if (regress) {
        beta_draws[,draw] = beta
      } 
    }
  }
  
  if (!regress) {
    draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws)
  } else {
    draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws, beta=beta_draws)
  } 
  return(draws)
}

