library(splines)


# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------
generate_design_matrix <- function(x, knot_vector, degree){
  return(cbind(outer(x,1:degree,"^"),outer(x,knot_vector,">")*outer(x,knot_vector,"-")^degree)) 
}

get_angle_predictions = function(basis, x, a, U){
  
  if (basis == 1 | basis == 2) {
    Bmat = get_design_matrix(basis, x)
    
    #mod_spline <- lm(a~Bmat)
   
    mod_spline <- lm(U~Bmat)
    
    prediction = predict(mod_spline)
    
    # Use U or a, doesn't seem to make a difference.
    #if (dim(prediction)[2] == 2) {
    a_pred = unitcircle2radians(prediction)
    #} else {
    #a_pred = prediction
    #}
  } else if (basis == 3) {
    # TODO move to bases_initialization file.
    roughness_rate = 0.1
    
    fit <- smooth.spline(x, a, spar = 0.5)  # spar controls smoothness
    a_pred  <- predict(fit, x)$y
    
    #a_pred = Bmat %*% solve(crossprod(Bmat) + roughness_rate*roughness)%*%crossprod(Bmat, a)
    #u1_pred = Bmat %*% solve(crossprod(Bmat) + roughness_rate*roughness)%*%crossprod(Bmat, U[,1])
    #u2_pred = Bmat %*% solve(crossprod(Bmat) + roughness_rate*roughness)%*%crossprod(Bmat, U[,2])
    
    #U_pred = cbind(u1_pred, u2_pred)
    #a_pred = unitcircle2radians(U_pred)
    
  } else if (basis == 4) {
    
    a_pred = ols4(x,a)
    
  }
  
  return(a_pred)
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
# Basis 2: Cubic splines at prespecified knots that seem to work well for 
#          Buffalo and Santa Ana datasets.
# ------------------------------------------------------------------------------
knot_vector_ii = c(-1, 1,4,5,6,7, 10,20, 30)

get_design_matrix2 = function(x){
  return(generate_design_matrix(x,knot_vector = knot_vector_ii, degree=3  ))
}

#design_matrix <- generate_design_matrix(degree = 3, knot_vector = c(4,5,6,7, 10,20, 30), x = x)
#basis_name = "cubic splines"
#create_OLS_regression_plot(dataset, root_path, a, x, basis_name, design_matrix)



# ------------------------------------------------------------------------------
# Basis type #3
# ------------------------------------------------------------------------------
get_design_matrix3 = function(x){
  fit <- smooth.spline(x, x, spar = 0.5)
  
  # The design matrix evaluated at x
  B <- predict(fit, x, type = "lpmatrix")$lp
  return(B)
}


# ------------------------------------------------------------------------------
# Basis type #4. Using 
# ------------------------------------------------------------------------------
get_design_matrix4 = function(x){
  B <- ns(x, df = 10)
  return(B)
}

# JUST OLS for this basis.
ols4 = function(x, y, lambda=0.5) {
  B <- ns(x, df = 10, intercept=TRUE)
  p <- ncol(B)
  # Penalty matrix (ridge on coefficients for simplicity)
  Omega <- diag(p)
  
  # Precompute cross-products
  BtB <- t(B) %*% B
  BtY <- t(B) %*% y
  
  # Initialize
  c_current <- solve(BtB + lambda * Omega, BtY)
  
  return(B %*% c_current)
}



# Creates design matrix AND does OLS AND ...
smoothing_spline_ns <- function(x, y, df = NULL, knots = NULL, lambda = 0){
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  
  # Build natural cubic spline basis
  # df = number of basis functions (columns)
  # knots = internal knots (optional)
  B <- ns(x, df = df, knots = knots, intercept=TRUE)
  p <- ncol(B)
  
  # --- Roughness penalty ---
  # Compute Omega = \int B''(x)^T B''(x) dx numerically
  # B'' can be approximated using finite differences
  B_dd <- apply(B, 2, function(f) diff(diff(f)))
  Omega <- matrix(0, p, p)
  if(lambda > 0){
    # integrate roughly: sum of squared second differences
    Omega <- t(B_dd) %*% B_dd
  }
  
  # --- Penalized OLS ---
  A <- crossprod(B) + lambda * Omega
  c_hat <- solve(A, crossprod(B, y))
  
  # Fitted values
  y_hat <- B %*% c_hat
  
  # Optional: effective degrees of freedom
  M <- solve(A, t(B))
  df_eff <- sum(colSums(B * M))
  
  list(
    coefficients = as.numeric(c_hat),
    fitted = as.numeric(y_hat),
    df = df_eff,
    B = B,
    Omega = Omega
  )
}













# ------------------------------------------------------------------------------
# Basis 3: All unique points are knots, but we will penalize roughness.
# ------------------------------------------------------------------------------
#get_design_matrix3 = function(x) {
  #return(generate_design_matrix(degree = 3, knot_vector = sort(unique(x)), x = x))
#  return(generate_design_matrix(degree = 3, knot_vector = x, x=x))
#}



# ------------------------------------------------------------------------------
# Information about all bases choices.
# ------------------------------------------------------------------------------

bases_list = list(1, 2, 3, 4)
num_bases = length(bases_list)
get_num_bases = function(){return(num_bases)}

bases_names = list("Three basis", "Hand-picked knots", "All knots + penalize roughness", "ns")

get_num_basis_functions = function(i, x=NULL){
  if (i==1){
    return(length(bases_i))
  } else if (i == 2){
    return(length(knot_vector_ii) + 3)
  } else if (i == 3){
    stop("Err\n")
    if (is.null(x)){
      stop("This basis needs x-data, as knots are at every point.\n")
    }
    return(length(sort(unique(x))) + 3)
  } else if (i == 4){
    return(10)
  } else{
    stop("Bases ,", i, " : there are only ", num_bases, " bases options to choose from. Pick a number between 1 to ", num_bases, " please.\n")
  }
}

get_design_matrix = function(i, x){
  
  if (i==1){
    return(get_design_matrix1(x))
  } else if (i==2) { 
    return(get_design_matrix2(x))
  } else if (i==3) {
    return(get_design_matrix3(x))
  }
  else if (i==4) {
    return(get_design_matrix4(x))
  } else {
    stop("Bases: there are only ", num_bases, " bases options to choose from. Pick a number between 1 to ", num_bases, " please.\n")
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

















