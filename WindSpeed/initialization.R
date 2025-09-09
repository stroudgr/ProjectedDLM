set.seed(8675309)


# Give each dataset a number
buffalo_index = 1
buffalo_path = "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv"
buffalo_pretty_name = "Buffalo"


# Give each dataset a number
santa_ana_index = 2
santa_ana_path = "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv"
santa_ana_pretty_name = "Santa Ana"




all_datasets = list("buffalo", "santa_ana")
pretty_names = list(buffalo=buffalo_pretty_name, santa_ana=santa_ana_pretty_name)
all_data_paths = list(buffalo=buffalo_path, santa_ana=santa_ana_path)

all_indices = list(buffalo=buffalo_index, santa_ana=santa_ana_index)

get_index_by_name = function(name) {
  return(all_indices[[name]])
}

model_pretty_names = c("speed random walk + noise", "DLM", "speed RWN, direction PDLM + regression", "speed TVAR", "speed RWN, PDLM indep", "basis functions")
model_names = c("speed_rw", "DLM", "speed_pdlm_regression", "speed_tvar", "independ", "basis")

model_alphanumeric_identifiers = c("1A", "two_dlms", "2A", "1B", "3A", "4A")

model_indices = list("1A" = 1, "two_dlms"=2, "2A"=3, "1B"=4, "3A"=5, "4A"=6)


num_models = length(model_alphanumeric_identifiers)












valid_datasets = function(datasets, print_output=TRUE) {
  
  datasets = unique(datasets)
  is_subset <- function(x, y) { all(x %in% y) }
  
  removals = setdiff(datasets, all_datasets)
  
  if (print_output) {
    for (r in removals) {
      cat(paste0(r, " is not the name of dataset."))
    }
  }
  
  keepers = setdiff(datasets, removals)
  
  return(keepers)
}

