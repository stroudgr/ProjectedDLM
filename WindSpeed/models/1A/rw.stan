data {
  int<lower=1> T;          // Number of time steps
  vector[T] x;             // Observed data
}

parameters {
  vector[T] s;             // Latent states
  real<lower=0> sigma_w;   // Std dev of process noise
  real<lower=0> sigma_e;   // Std dev of observation noise
}

model {
  // Priors (can be adjusted or made hierarchical)
  sigma_w ~ normal(0, 1);
  sigma_e ~ normal(0, 1);
  s[1] ~ normal(0, 10);  // Prior for initial state

  // State evolution
  for (t in 2:T) {
    s[t] ~ normal(s[t - 1], sigma_w);
  }

  // Observation model
  for (t in 1:T) {
    x[t] ~ normal(s[t], sigma_e);
  }
}

generated quantities {
  vector[T] x_rep;
  real x_forecast;
  real s_forecast;

  for (t in 1:T) {
    x_rep[t] = normal_rng(s[t], sigma_e);  // Simulate new x from posterior s[t]
  }
  
  s_forecast = normal_rng(s[T], sigma_w);
  x_forecast = normal_rng(s_forecast, sigma_e);
  //x_forecast = normal_rng(normal_rng(s[T], sigma_w), sigma_e);
  
}

