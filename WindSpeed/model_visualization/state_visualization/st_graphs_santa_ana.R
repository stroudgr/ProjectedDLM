# Load ggplot2
library(ggplot2)

city = "Santa Ana"
city_fname = "2_santa_ana"

for (m in c(6)) {

if (m==1) {
  S_draws = post_samples1$S_draws
} else if (m==3) {
  S_draws = post_samples3$pdlm_draws$S
} else if (m==4) {
  S_draws = post_samples4$S_draws
} else if (m==5) {
  S_draws = post_samples5$S_draws
} else if (m == 6) {
  S_draws = post_samples6$S_draws
}

model_id = model_alphanumeric_identifiers[m]


#S_index = 2
for (S_index in 1:2) {


#restricted_times = 1:1000
restricted_times = 1:TT


S_i_draws = S_draws[, S_index,]

S_i_draws = S_i_draws[restricted_times,]


n_samples = dim(S_i_draws)[2]



# Compute quantiles at each time step
quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
quantiles <- t(apply(S_i_draws, 1, quantile, probs = quantile_levels))

# Create data frame for ggplot
df <- data.frame(
  time = restricted_times,
  q05 = quantiles[, 1],
  q25 = quantiles[, 2],
  q50 = quantiles[, 3],
  q75 = quantiles[, 4],
  q95 = quantiles[, 5]
)


# Fan plot using shaded ribbons
p=ggplot(df, aes(x = time)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = q50), color = "black", size = 0.3) +
  labs(
    title = paste0("Model ", model_id, " in ", city, " ", "Fan Plot of S_" , S_index, " Distribution Over Time"),
    x = "Time",
    y = paste0("S_", S_index , " Value")
  ) +
  theme_minimal()

state_file_name = paste0("WindSpeed/model_visualization/state_visualization/", city_fname, "/", model_id, "/s", S_index )
#ggsave(paste0(state_file_name, ".png"), plot = p, width = 6, height = 4, dpi = 300)


other_filename = "WindSpeed/basis/st"
ggsave(paste0(other_filename, ".png"), plot = p, width = 6, height = 4, dpi = 300)


if (m != 3 | m!= 6){
  next
}

if (m==6){
  
}


if (m == 3) {
  
  beta_draws = post_samples3$pdlm_draws$beta
  
  # Compute quantiles at each time step
  quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  quantiles <- t(apply(beta_draws, 1, quantile, probs = quantile_levels))
  
  beta1 = beta_draws[1,]
  beta2 = beta_draws[2,]
  
  
  hist(beta1)
  hist(beta2)
  
  
  beta1_times_speed = array(0, dim=c(TT, n_samples))
  beta2_times_speed = array(0, dim=c(TT, n_samples))
  
  for (t in 1:TT) {
    for (i in 1:n_samples) {
      beta1_times_speed[t, i] =  beta1[i] * logx[t]
      beta2_times_speed[t, i] =  beta2[i] * logx[t]
    }
  }
}



# Compute quantiles at each time step
quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
quantiles <- t(apply(beta2_times_speed, 1, quantile, probs = quantile_levels))
b_index=2

# Create data frame for ggplot
df <- data.frame(
  time = 1:TT,
  q05 = quantiles[, 1],
  q25 = quantiles[, 2],
  q50 = quantiles[, 3],
  q75 = quantiles[, 4],
  q95 = quantiles[, 5]
)



# Fan plot using shaded ribbons
p=ggplot(df, aes(x = time)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = q50), color = "black", size = 1) +
  labs(
    title = paste0("Model ", model_id, " in ", city, " ", " B_" , b_index, "* log(speed+1) Distribution Over Time"),
    x = "Time",
    y = paste0("B_", b_index , " Value")
  ) +
  theme_minimal()

state_file_name = paste0("WindSpeed/model_visualization/state_visualization/", city_fname, "/", model_id, "/beta_speed", b_index )
#ggsave(paste0(state_file_name, ".png"), plot = p, width = 6, height = 4, dpi = 300)

}
}
