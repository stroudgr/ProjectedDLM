source("WindSpeed/models/4A spline/bases_initialization.R")





gibbs_pdlm_splines <- function(num_basis=3, U, FF, prior = NULL, init = NULL, ndraw = 1000, burn = 0, thin = 1, x, speed_model = NA, miss_speed_post=NA, speed_post_samples = NA, basis=3, spatial_confound=FALSE){
  # ----------------------------------------------------------------------------
  # dimensions
  # ----------------------------------------------------------------------------
  logx = log(x+1)
  TT = nrow(U)
  n = ncol(U)

  p = dim(FF)[2]
  M = burn + thin * ndraw
  
  
  #L = get_num_basis_functions(basis)
  # This is TT x L
  # and BX[i,j] = bj(xi)
  basis=2
  BX <- get_design_matrix(2, x)
  L <- ncol(BX)
  lambda=0.5
  
  # Penalty matrix (ridge on coefficients for simplicity)
  Omega <- diag(L)
  
  # Storage for posterior samples of coefficients
  #coef_samples <- matrix(NA, nrow = n_iter, ncol = L)
  
  # Precompute cross-products
  BtB <- t(BX) %*% BX
  BtB_inv = solve(BtB + lambda * Omega)
  #BtY <- t(B) %*% y
  
  # Initialize
  #c_current <- solve(BtB + lambda * Omega, BtY)
  
  
  
  
  
  #BX = get_design_matrix(basis, x) # TODO Do I have to transpose it?
  #L = dim(BX)[2]
  
  library("corpcor")
  
  # This is T x T
  proj_BX = BX %*% solve(t(BX) %*% BX) %*% t(BX)
  
  orthogonalizer = diag(1, nrow = TT) - proj_BX
  
  #proj_BX_list = array(0, dim=c(TT, L,L))
  
  #for (t in 1:TT) {
    # OR outerproduct in solve(...)
  #  proj_BX_list[t,,] = BX[t,] %*% solve(t(BX[t,])  %*% BX[t,] ) %*% t(BX[t,])
  #}
  
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
    
    #beta = array(0, c(n,L))
    beta = array(0, c(L,n))
    
  }
  
  Mu = matrix(0, TT, n)
  Sigma = gams2Sigma(Gamma, gamma)
  
  #mu_beta = rep(0, n*L) 
  #Sigma_beta = diag(n*L)
  #Sigma_beta_inv = diag(n*L)
  
  # ----------------------------------------------------------------------------
  # preallocate storage
  # ----------------------------------------------------------------------------
  
  #S_draws = array(numeric(TT * p * ndraw*L), c(TT, p, L, ndraw))
  S_draws = array(numeric(TT * p * ndraw*L), c(TT, p, L, ndraw))
  
  
  # Intercept
  S_draws = array(numeric(TT * p * ndraw), c(TT, p, ndraw))
  
  
  Sigma_draws = array(0, c(n, n, ndraw))
  G_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  W_draws = array(numeric(p * p * ndraw), c(p, p, ndraw))
  r_draws = matrix(0, TT, ndraw)
  
  beta_draws = array(0, c(L, n, ndraw))
  
  
  a = unitcircle2radians(U)
  a_na_indices = which(is.na(a))
  U_na_indices = which(is.na(U), arr.ind = TRUE)
  
  logx_na_indices = NA
  
  psi_draws = NA
  sigma_sq = NA
  
  if (!is.null(logx)) {
    logx_na_indices = which(is.na(logx))
    logx_not_na_indices = which(!is.na(logx))
    
    
    if (speed_model == "A" & !is.na(speed_post_samples)) {
      psi_draws = speed_post_samples$psi
      sigma_sq = speed_post_samples$sigma_sq
    }
  }
  
  #print(a_na_indices)
  
  # ----------------------------------------------------------------------------
  # off to war
  # ----------------------------------------------------------------------------
  
  draw = 0
  
  for (m in 1:M) {
    
    # --------------------------------------------------------------------------
    # draw from p(states | ...)
    # --------------------------------------------------------------------------
    
    Y = U * r
    Y_backup = Y
    
    # U*r = X*beta + s + noise
    
    # Spatial confounding:
    # U*r = X*beta + (I-P_X)s + noise 
    
    for (i in 1:n) {
      for (j in 1:L) {
        Y[,i] = Y[,i] - beta[j,i]* BX[,L] #bases[[j]](x)
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
    
    # S is dimension TT x p
    S_copy = S
    if (spatial_confound)
      S = orthogonalizer %*% S
    
    
    for (t in 1:TT) {
      Mu[t, ] = FF[, , t] %*% S[t, ] # F is Identity
    }
    
    Y = Y_backup
    for (t in a_na_indices) {
      Y[t,] = FF[, , t] %*% S[t, ] + mvrnorm(n = 1, mu = numeric(p), Sigma)
      
      for (i in 1:n) {
        for (j in 1:L) {
          Y[t,i] = Y[t,i] + beta[j,i]*bases[[j]](x[t])
        }
      }
      
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
    #if (regress) {
    synth_data = c(t(Y - Mu))
    
    #BX is TT x L
    V = kronecker(BX, diag(n)) # %x%
    inv_O = diag(TT) %x% solve(Sigma) 
    
    #Q = Sigma_beta_inv + t(V) %*% inv_O %*% V
    Q = t(V) %*% inv_O %*% V
    
    Q_inv = solve(Q + diag(0.001, nrow=dim(Q)[1]))
    
    #ell = Sigma_beta_inv %*% mu_beta + t(V) %*% inv_O %*% (synth_data)
    ell =  t(V) %*% inv_O %*% (synth_data)
    
    
    beta_t = mvrnorm(1,Q_inv %*% ell, Sigma= Q_inv)
    
    beta_t = matrix(beta_t, nrow = n, ncol=L, byrow=FALSE)
    
    beta = t(beta_t)
        
      
    #}
    
    # --------------------------------------------------------------------------
    # draw from p(Sigma | ...)
    # --------------------------------------------------------------------------
    
    if (any(is.na(Y))) {
      #print("Y")
      #print(Y)
    }
    if (any(is.na(Mu))) {
      #print("Mu")
      #print(Mu)
    }
    
      
    gamma = draw_gamma_given_else(Y, Mu, Gamma, gamma_prmean, gamma_prvar)
    Gamma = draws_Gamma_given_else(Y, Mu, gamma, Gamma_prdf, Gamma_prscale)
    
    Sigma = gams2Sigma(Gamma, gamma)
    
    # --------------------------------------------------------------------------
    # draw from p(G, W | ...)
    # --------------------------------------------------------------------------
    
    GW = sample_conjugate_posterior_varp(S, 1, FALSE, GW_prior, TRUE)
    if (!is.null(GW)){
      G = t(GW$B)
      W = GW$S
    }
    
    # --------------------------------------------------------------------------
    # draw from p(r | ...)
    # --------------------------------------------------------------------------
    
    r = draw_lengths_given_else_FFS(U, FF, Sigma, r, S)
    
    
    S = S_copy
    if ( retain_draw(m, burn, thin) ) {
      draw = draw + 1
      S_draws[, , draw] = S
      G_draws[, , draw] = G
      W_draws[, , draw] = W
      r_draws[, draw] = r
      Sigma_draws[, , draw] = Sigma
      #if (regress) {
        beta_draws[,,draw] = beta
      #} 
    }
  }
  
  #if (!regress) {
  #  draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws)
  #} else {
    draws = list(S = S_draws, r = r_draws, G = G_draws, W = W_draws, Sigma = Sigma_draws, beta=beta_draws)
  #} 
  return(draws)
}

