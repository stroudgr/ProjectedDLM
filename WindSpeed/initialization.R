set.seed(8675309)

# ------------------------------------------------------------------------------
# Datasets
# ------------------------------------------------------------------------------

# Give each dataset a number.
buffalo_index = 1

# Relative file location for dataset.
buffalo_path = "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv"

# Longer descriptive name of dataset. Can safely modify to be more specific if
# similar datasets come into play.
buffalo_pretty_name = "Buffalo"

# Dataset #2
santa_ana_index = 2
santa_ana_path = "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv"
santa_ana_pretty_name = "Santa Ana"


# Gathering all information into lists.
all_datasets = list("buffalo", "santa_ana")
pretty_names = list(buffalo=buffalo_pretty_name, santa_ana=santa_ana_pretty_name)
all_data_paths = list(buffalo=buffalo_path, santa_ana=santa_ana_path)


all_indices = list(buffalo=buffalo_index, santa_ana=santa_ana_index)
# Helper to dataset index by name.
get_index_by_name = function(name) {
  return(all_indices[[name]])
}


# A helper function that ensures the user inputted list of datasets is valid.
valid_datasets = function(datasets, print_output=TRUE) {
  
  datasets = unique(datasets)
  is_subset <- function(x, y) { all(x %in% y) }
  
  removals = setdiff(datasets, all_datasets)
  
  if (print_output) {
    for (r in removals) {
      cat(paste0(r, " is not the name of a dataset."))
    }
  }
  
  keepers = setdiff(datasets, removals)
  
  return(keepers)
}



# ------------------------------------------------------------------------------
# Models
# ------------------------------------------------------------------------------

# Model identifiers.
model_alphanumeric_identifiers = c("1A", "two_dlms", "2A", "1B", "3A", "4A")

# Give each model a number.
model_indices = list("1A" = 1, "two_dlms"=2, "2A"=3, "1B"=4, "3A"=5, "4A"=6)
get_model_index_by_name = function(id) {
  return(model_indices[[id]])
}


# Give each model a long descriptive name.
model_pretty_names = list("1A" = "speed random walk + noise", 
                          "two_dlms" = "DLM", 
                          "2A" = "speed RW, direction PDLM + regression", 
                          "1B" = "speed TVAR", 
                          "3A" = "speed RWN, PDLM indep", 
                          "4A" = "basis functions")

# Give each model a shorter name
model_names = list("1A" = "speed_rw", 
                   "two_dlms" = "DLM", 
                   "2A" = "speed_pdlm_regression", 
                   "1B" = "speed_tvar", 
                   "3A" = "independ", 
                   "4A" = "basis")


num_models = length(model_alphanumeric_identifiers)


# A helper function that ensures the user inputted list of models is valid.
valid_models = function(models, print_output=TRUE) {
  
  models = unique(models)
  is_subset <- function(x, y) { all(x %in% y) }
  
  removals = setdiff(models, model_alphanumeric_identifiers)
  
  if (print_output) {
    for (r in removals) {
      cat(paste0(r, " is not the name of a model."))
    }
  }
  
  keepers = setdiff(models, removals)
  
  return(keepers)
}



# ----------------------------------------------------------------------------
# Other (default) file paths.
# ----------------------------------------------------------------------------
MCMC_PATH = "WindSpeed/experiments/MCMC/saved_MCMC/"




