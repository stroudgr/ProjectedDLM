speed_rw_noise_miss_full_posterior_helper = function(y, logx, s, psi, Sigma_inv, sigma_sq, obs_indices, miss_indices) {
  
  TT = length(logx)
  n=2
  
  Q_inv = array(data=0, dim = c(TT))
  ell = array(data=0, dim=c(TT))
  
  T_miss = length(miss_indices)
  i=1
  
  print(s[4,])
  
  for( t in miss_indices) {
    Q_inv[i] = 1 / (t(s[t,]) %*% Sigma_inv %*% s[t,] + 1/sigma_sq)
    ell[i] = t(y[t,]) %*% Sigma_inv %*% s[t] + psi[t]
    i = i+1
  }
  
  #Q_inv = 1/( t(s[miss_indices]) %*% Sigma_inv %*% s[miss_indices] + 1/sigma_sq)
  #ell = t(y[miss_indices]) %*% Sigma_inv %*% s[miss_indices] + psi[miss_indices]
  
  logx_sim = array(data=0, dim=c(TT))
  
  logx_sim[obs_indices] = logx[obs_indices]
  logx_sim[miss_indices] = rnorm(T_miss, Q_inv*ell, sqrt(Q_inv))
  
  return(logx_sim)
}
