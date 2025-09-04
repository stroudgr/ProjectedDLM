library(imputeTS)

interpolation = TRUE

dataset_name = "santa_ana"

path = "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv"
path = "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv"
#path = 

speed_dir_data = read.csv(path)


if (path == "WindSpeed/datasets/buffalo_airport/buffalo_wind_data_small_set.csv") {
  
  degrees = speed_dir_data[["wind_direction"]]
  x = speed_dir_data[["wind_speed"]]
  
  TT = length(x)
  
  
  if (TT > 128) {
    
    period = 38 + 1:128
    x = x[period]
    degrees = degrees[period]
    TT = length(x)
  }
  
  logx = log(x+1)
  a = degrees2radians(degrees)
  U = radians2unitcircle(a)
  
  
  if (interpolation) {
    x = na_interpolation(x)
    logx=log(x+1)
    a = na_interpolation(a)
    U = radians2unitcircle(a)
  }
  
}



if (path == "WindSpeed/datasets/santa_ana_airport/santa_ana_rdata.csv") {
  degrees = speed_dir_data[["drct"]]
  x = speed_dir_data[["sknt"]]
  TT = length(x)
  a = degrees2radians(degrees)
  
  if (interpolation) {
    x = na_interpolation(x)
    logx=log(x+1)
    a = na_interpolation(a)
    U = radians2unitcircle(a)
  }
  
}
