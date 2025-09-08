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

