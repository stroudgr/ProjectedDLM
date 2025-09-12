
# Includes constants with information about the datasets, including dataset name, 
# data file location. Also contains information about models, including giving
# each model a name, a shorter string name, and a identifier.
source("WindSpeed/initialization.R")

# Includes any function that needs to be used in multiple parts of the code, 
# e.g. opening a file and converting to a consistent format.
# TODO what else?
# TODO I don't think you want to give the user this ability, so I don't think this belongs here.
# Same for initialization?
source("WindSpeed/helpers.R")


# The functions from these sources are ones that the user 

# Functions to generate figures for exploratory data analysis
source("WindSpeed/experiments/data_visualization/Wind_Speed_eda.R")

# Functions to run MCMC for the requested datasets and models.
source("WindSpeed/experiments/MCMC/joint_model_mcmc.R")

# Functions to generate plots for parameters of the model for the requested
# 
source("WindSpeed/experiments/model_visualization/model_visualization.R")

# 
source("WindSpeed/experiments/model_testing/model_testing.R")
source("WindSpeed/experiments/model_testing/model_comparison.R")
