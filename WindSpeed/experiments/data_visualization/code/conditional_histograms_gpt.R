


a_0_5 = a[x <= 5]

a_5_10 = a[x> 5 & x <= 10]

a_10_20 = a[x> 10 & x <= 20]

a_20 = a[x> 20]


angle_breaks = c(c(0:(4*pi))/2, 6.3) 

hist(a_0_5, breaks = angle_breaks, main="Angle data histogram, 0-5 mph" , xlab = "angle (radians)" )
hist(a_5_10, breaks = angle_breaks, main="Angle data histogram, 5-10 mph", xlab = "angle (radians)"  )
hist(a_10_20, breaks = angle_breaks, main="Angle data histogram, 10-20 mph", xlab = "angle (radians)" )
hist(a_20, breaks = angle_breaks, main="Angle data histogram, 20+ mph", xlab = "angle (radians)" )


n = length(a_0_5)
mu = c(1,1)



# helper: pdf and cdf of standard normal
phi <- function(x) dnorm(x)
Phi <- function(x) pnorm(x)

# truncated normal moments for N(m, s^2) truncated to (0, +inf)
trunc_normal_moments <- function(m, s) {
  alpha <- -m / s
  surv <- 1 - Phi(alpha)
  surv <- max(surv, 1e-300)
  lam <- phi(alpha) / surv
  ER <- m + s * lam
  ER2 <- s^2 + m^2 + m * s * lam
  return(list(mean = ER, second = ER2))
}

# negative Q-function for optimization of Sigma parameters
negQ_params <- function(params, W, n) {
  t <- params[1]; b <- params[2]
  a <- b^2 + 1e-9 + exp(t)
  Sigma <- matrix(c(a, b, b, 1), 2, 2)
  detS <- a*1 - b^2
  if (detS <= 0) return(1e12 + 1e6*abs(detS))
  invS <- matrix(c(1, -b, -b, a), 2, 2) / detS
  logdet <- log(detS)
  trace_term <- sum(invS * W)   # trace(invS %*% W)
  Q <- -0.5 * n * (logdet + trace_term)
  return(-Q)  # we minimize
}

em_projected_normal_learn_sigma <- function(Y, mu0=NULL, Sigma0=NULL, 
                                            max_iter=200, tol=1e-6, verbose=FALSE) {
  n <- nrow(Y)
  
  # initialization
  if (is.null(mu0)) {
    mu <- colMeans(Y)
  } else {
    mu <- mu0
  }
  if (is.null(Sigma0)) {
    Sigma <- diag(2)
    Sigma[2,2] <- 1
  } else {
    Sigma <- Sigma0
    Sigma[2,2] <- 1
  }
  
  for (it in 1:max_iter) {
    mu_old <- mu
    Sigma_old <- Sigma
    
    invS <- solve(Sigma)
    ER <- ER2 <- numeric(n)
    
    for (i in 1:n) {
      Yi <- Y[i,]
      Ai <- as.numeric(t(Yi) %*% invS %*% Yi)
      Bi <- as.numeric(t(Yi) %*% invS %*% mu)
      m <- Bi / Ai
      s <- sqrt(1/Ai)
      mom <- trunc_normal_moments(m, s)
      ER[i] <- mom$mean
      ER2[i] <- mom$second
    }
    
    # sufficient statistics
    S1 <- colSums(ER * Y)
    S2 <- matrix(0,2,2)
    for (i in 1:n) {
      S2 <- S2 + ER2[i] * (Y[i,] %o% Y[i,])
    }
    
    # M-step for mu
    mu <- S1 / n
    
    # W matrix
    W <- S2/n - mu %o% mu
    W <- 0.5 * (W + t(W))  # symmetrize
    
    # initial params for optimizer
    b0 <- W[1,2]
    a0 <- max(W[1,1], b0^2 + 1e-3)
    t0 <- log(max(1e-6, a0 - b0^2 - 1e-9))
    res <- optim(c(t0, b0), negQ_params, W=W, n=n, method="L-BFGS-B",
                 control=list(fnscale=1, factr=1e7))
    if (res$convergence != 0) {
      # fallback: projection
      a_hat <- W[1,1]; b_hat <- W[1,2]
      if (a_hat <= b_hat^2 + 1e-9) a_hat <- b_hat^2 + 1e-6
      Sigma <- matrix(c(a_hat, b_hat, b_hat, 1), 2, 2)
    } else {
      t_opt <- res$par[1]; b_opt <- res$par[2]
      a_opt <- b_opt^2 + 1e-9 + exp(t_opt)
      Sigma <- matrix(c(a_opt, b_opt, b_opt, 1), 2, 2)
    }
    
    dmu <- sqrt(sum((mu - mu_old)^2))
    dSigma <- sqrt(sum((Sigma - Sigma_old)^2))
    if (verbose) cat(sprintf("iter %3d: dmu=%.3e dSigma=%.3e\n", it, dmu, dSigma))
    if (dmu < tol && dSigma < tol) break
  }
  
  return(list(mu=mu, Sigma=Sigma))
}

# Example usage:
set.seed(42)
sample_projected_normal <- function(mu, Sigma, n) {
  X <- MASS::mvrnorm(n, mu, Sigma)
  Y <- X / sqrt(rowSums(X^2))
  return(Y)
}

true_mu <- c(2,1)
true_Sigma <- matrix(c(2,0.5,0.5,1),2,2) # bottom-right fixed to 1
Y <- sample_projected_normal(true_mu, true_Sigma, 500)

Y = U_0_5

fit <- em_projected_normal_learn_sigma(Y, verbose=TRUE)
print(fit$mu)
print(fit$Sigma)

library(circular)
discrete_angles <- seq(0, 2 * pi, length.out = 100)

density_values = dpnorm(discrete_angles, mu=fit$mu, sigma=fit$Sigma)


plot(discrete_angles, density_values, type="l")
