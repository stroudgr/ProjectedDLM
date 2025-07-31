data {
  int<lower=1> T;          // Number of time steps
  vector[T] x;             // Observed data
  vector[T] y;
}

parameters {
  vector[T] s_x;
  vector[T] s_y;             // Latent states
  real<lower=0> sigma_x_w;   // Std dev of process noise
  real<lower=0> sigma_x_e;   // Std dev of observation noise
  real<lower=0> sigma_y_w;   // Std dev of process noise
  real<lower=0> sigma_y_e;   // Std dev of observation noise
}

model {
  sigma_x_w ~ normal(0, 1);
  sigma_x_e ~ normal(0, 1);
  sigma_y_w ~ normal(0, 1);
  sigma_y_e ~ normal(0, 1);
  s_x[1] ~ normal(0, 10);  // Prior for initial state
  s_y[1] ~ normal(0, 10);

  // State evolution
  for (t in 2:T) {
    s_x[t] ~ normal(s_x[t - 1], sigma_x_w);
    s_y[t] ~ normal(s_y[t - 1], sigma_y_w);
  }

  // Observation model
  for (t in 1:T) {
    x[t] ~ normal(s_x[t], sigma_x_e);
    y[t] ~ normal(x[t]*s_y[t], sigma_y_e);
  }
}

generated quantities {
  vector[T] x_rep;
  vector[T] y_rep;

  for (t in 1:T) {
    x_rep[t] = normal_rng(s_x[t], sigma_x_e);
    y_rep[t] = normal_rng(x_rep[t]*s_y[t], sigma_y_e);
  }
}

