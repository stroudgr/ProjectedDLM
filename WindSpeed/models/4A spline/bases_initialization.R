generate_design_matrix <- function(x, knot_vector, degree){
  return(cbind(outer(x,1:degree,"^"),outer(x,knot_vector,">")*outer(x,knot_vector,"-")^degree))
}


# ------------------------------------------------------------------------------
# Basis 1: Just three basis functions.
# ------------------------------------------------------------------------------ 
bases_i = list(
  function(x) {x}, 
  function(x) {log(x+1)}, 
  function(x){ (x>10)*1 }
)

get_design_matrix1 = function(x) {
  return(sapply(bases_i, function(f) sapply(x, f)))
}


# ------------------------------------------------------------------------------
# Basis 1: Cubic splines at prespecified knots that seem to work well for 
#          Buffalo and Santa Ana datasets.
# ------------------------------------------------------------------------------
knot_vector_ii = c(4,5,6,7, 10,20, 30)

get_design_matrix2 = function(x){
  return(generate_design_matrix(x,knot_vector = knot_vector_ii, degree=3  ))
}

#design_matrix <- generate_design_matrix(degree = 3, knot_vector = c(4,5,6,7, 10,20, 30), x = x)
#basis_name = "cubic splines"
#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)



# ------------------------------------------------------------------------------
# Basis 3: All unique points are knots, but we will penalize roughness.
# ------------------------------------------------------------------------------
get_design_matrix3 = function(x) {
  return(generate_design_matrix(degree = 3, knot_vector = sort(unique(x)), x = x))
}



# ------------------------------------------------------------------------------
# Information about all bases choices.
# ------------------------------------------------------------------------------

bases_list = list(1, 2, 3)
num_bases = length(bases_list)
get_num_bases = function(){return(num_bases)}

bases_names = list("Three basis", "Hand-picked knots", "All knots + penalize roughness")

get_num_basis_functions = function(i, x=NULL){
  if (i==1){
    return(length(bases_i))
  } else if (i == 2){
    return(length(knot_vector_ii) + 3)
  } else if (i == 3){
    if (is.null(x)){
      stop("This basis needs x-data, as knots are at every point.\n")
    }
    return(length(sort(unique(x))) + 3)
    
  } else{
    stop("Bases: there are only ", i, " bases options to choose from. Pick a number between 1 to ", i, " please.\n")
  }
}

get_design_matrix = function(i, x){
  
  if (i==1){
    return(get_design_matrix1(x))
  } else if (i==2) { 
    return(get_design_matrix2(x))
  } else if (i==3) {
    return(get_design_matrix3(x))
    
  } else {
    stop("Bases: there are only ", i, " bases options to choose from. Pick a number between 1 to ", i, " please.\n")
  }
    
  
}



get_roughness_matrix = function(i, x){
  
  return(NULL)
  if (i==1){
    stop("Bases 1: roughness not implemented.\n")
  } else if (i==2) { 
    stop("Bases 2: roughness not implemented.\n")
  } else if (i==3) {
    #TODO
    stop("Bases 3: roughness not implemented (YET) .\n")
    
  } else {
    stop("Bases: there are only ", i, " bases options to choose from. Pick a number between 1 to ", i, " please.\n")
  }
  
}










# ------------------------------------------------------------------------------
# Old stuff.
# ------------------------------------------------------------------------------
#gdm2 = function(x){
#  return(cbind(x, (x>10)*1))
#}

#gdm3 = function(x){
#  return(cbind(x, log(x+1), (x>10)*1))
#}


# TODO, these should depend on dataset I think.
#design_matrix <- generate_design_matrix(degree = 1, knot_vector = c(10,20, 30), x = x)
#basis_name = "Linear splines"
#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)


#design_matrix = gdm2(x)
#basis_name = "Indicator"
#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)

#design_matrix = gdm3(x)
#basis_name = "Indicator and log"
#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)


#design_matrix <- generate_design_matrix(degree = 3, knot_vector = sort(unique(x)), x = x)
#basis_name = "cubic splines_all_knots"

#library(fda)

#b = create.bspline.basis(rangeval = c(0, max(x)+1),
#                         breaks = sort(x), # knot locations
#                         norder = 4) # cubic spline (order = degree + 1)

#design_matrix = eval.basis(x, b)

#Omega = eval.penalty(b, Lfdobj = 2)
#lambda = 0.01  

#theta = solve(crossprod(Bmat) + lambda*Omega)%*%crossprod(Bmat, y)
#f = Bmat%*%theta  # fitted curve

#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)

















