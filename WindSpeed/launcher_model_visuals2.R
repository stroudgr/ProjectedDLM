source("_packages.R")
source("WindSpeed/_packages.R")
source("helpers/_helpers.R")
source("WindSpeed/_helpers.R")


#local(
  {
    
    # Visualize fan plots for the non-parametric functions we have posterior samples of.
    
    models = list("4A", "4Aii")
    
    datasets = list("buffalo")
    
    root_path = "WindSpeed/experiments/MCMC/saved_MCMC/"
    dataset = "buffalo"
    
    folder_name = paste0(root_path, "/", "4A", "/post_samples/", dataset, "/")
    save_path = paste0(folder_name, "post.Rdata")
    post_samples4A = get(load(save_path))
    
    
    folder_name = paste0(root_path, "/", "4Aii", "/post_samples/", dataset, "/")
    save_path = paste0(folder_name, "post.Rdata")
    post_samples4A = get(load(save_path))
    
    post_samples4Aii = get(load(save_path))
    
    buff = load_dataset("buffalo")
    x=buff$x
    a=buff$a
    TT = length(x)
    L=3
    
    bases = list(
      function(x) {x}, 
      function(x) {log(x+1)}, 
      function(x){ (x>10)*1 }
    )
    bases = bases[1:L]
    
    beta_draws = post_samples4A$beta_draws
    beta_draws_4Aii = post_samples4Aii$beta_draws
    
    
    x_axis = seq(from=1, to=12, by=0.1)
    #x_axis=x
    
    TTT = length(x_axis)
    
    XB = sapply(bases, function(f) sapply(x_axis, f))
    f = rowSums(XB)
    
    n_draw = dim(beta_draws)[3]
    vecs = array(NA, dim=c(TTT, 2, n_draw))
    angs = array(NA, dim=c(TTT, n_draw))
    for (n_s in 1:n_draw) {
      vecs[,,n_s] = XB%*% beta_draws[,,n_s]
      angs[,n_s] = unitcircle2radians(vecs[,,n_s])
    }
    
    n_samples = n_draw
    
    # Compute quantiles at each time step
    quantile_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    quantiles <- t(apply(angs, 1, quantile, probs = quantile_levels))
    
    # Create data frame for ggplot
    df <- data.frame(
      time = x_axis,
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
        title = paste0("title"),
        x = "Time",
        y = paste0("", " Value")
      ) +
      theme_minimal()
    
    #plot(x_axis, ?)
    #lines(x_axis, f, col = "red", lty = 2)
    
    
    
  }
#)










# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model testing: one step ahead distribution visualization : histograms
    # Visualize u_{t+1}, x_{t+1} given 
    
    # params should include
    # - Whether to runMCMC if there is no saved data (how do I save MCMC samples for subsets?).
    # - Mandatory param: Which time steps to create histograms for.
    # - Anything else?
    
    models = list("4A", "4Aii")
    datasets = list("buffalo")
    
    buffalo_list = c(44, 109, 128)
    
    
    times_list = list(buffalo=buffalo_list)
    params = list(runMCMC = FALSE)
    
    visualize_one_step_ahead(models, datasets, times_list=times_list, params=params)
    
  }
)






# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
local(
  {
    # The experiment is Model testing: fan plots for H step forecast ahead.
    # Visualize u_{t+1}, x_{t+1}, ...., u_{t+H}, x_{t+h} with fan plot,
    # given MCMC samples on data U_{1:t},x_{1:t}.
    
    # params should include
    # - Whether to runMCMC if there is no saved data (how do I save MCMC samples for subsets?).
    # - Mandatory param: t, how much data is known.
    # - Mandatory param: H, how far the horizon is.
    # - Anything else?
    
    models = list()
    datasets = list("buffalo")
    
    # Same length as datasets
    t = list(buffalo = 100, santa_ana = 5000)
    H = list(buffalo = 20, santa_ana = 20)
    
    
    params = list(runMCMC = FALSE)
    
    #
    
  }
)



# ------------------------------------------------------------------------------
# Experiment 
# ------------------------------------------------------------------------------
# The experiment is Model 
local(
  {
    
    models = list() 
    datasets = list()
    
    # This list should be the same length as the number of datasets.
    # For datasets[t], averaging_time_frames[t] should have a range (u,l) where 1<=u<=l <= number of time steps of curr dataset.
    averaging_time_frames = list()
    
    # TODO what are valid params to consider, beyond runMCMC?
    params = list(runMCMC = FALSE)
    
    
    visualize_regression_coefficients(models, datasets, averaging_time_frames, params)
    
  }
)



















