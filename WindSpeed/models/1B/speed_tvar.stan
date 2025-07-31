data {
  int<lower=2> T;          // Number of observations
  vector[T] x;             // Time series data
  int<lower=1> H;          // Forecast horizon
}

parameters {
  real<lower=0> sigma_w;                // Observation noise
  real<lower=0> sigma_e;           // Evolution noise
  vector[T] s;                     // Time-varying AR(1) coefficients
}

model {
  // Priors
  sigma_w ~ normal(0, 1);
  sigma_e ~ normal(0, 1);
  s[1] ~ normal(0, 1);

  for (t in 2:T) {
    s[t] ~ normal(s[t - 1], sigma_w);
  }

  // Observation model
  for (t in 2:T) {
    x[t] ~ normal(s[t] * x[t - 1], sigma_e);
  }
}

generated quantities {
  vector[T] x_rep;
  
  vector[H] x_forecast;
  vector[H] s_forecast;

  # TODO no use in H
  for (t in 1:T) {
    x_rep[t] = normal_rng(s[t], sigma_e);  // Simulate new x from posterior s[t]
  }

  // Initialize with last known values
  real x_prev = x[T];
  real s_prev = s[T];

  for (h in 1:H) {
    // Evolve s and generate forecast
    s_forecast[h] = normal_rng(s_prev, sigma_w);
    x_forecast[h] = normal_rng(s_forecast[h] * x_prev, sigma_e);

    // Update for next step
    s_prev = s_forecast[h];
    x_prev = x_forecast[h];
  }
}
