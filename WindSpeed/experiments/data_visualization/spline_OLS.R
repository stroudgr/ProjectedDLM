





generate_design_matrix <- function(x, knot_vector, degree){
  return(cbind(outer(x,1:degree,"^"),outer(x,knot_vector,">")*outer(x,knot_vector,"-")^degree))
}

gdm2 = function(x){
  return(cbind(x, (x>10)*1))
}

gdm3 = function(x){
  return(cbind(x, log(x+1), (x>10)*1))
}

design_matrix <- generate_design_matrix(degree = 1, knot_vector = c(10,20, 30), x = x)
design_matrix = gdm2(x)
design_matrix = gdm3(x)
design_matrix <- generate_design_matrix(degree = 3, knot_vector = c(10,20, 30), x = x)


mod_spline <- lm(a~design_matrix)
#mod_spline <- lm(U~design_matrix)

prediction = predict(mod_spline)
prediction

#if (dim(prediction)[2] == 2) {
#  a_pred = unitcircle2radians(prediction)
#} else {
  a_pred = prediction
#}


ggplot() +
  geom_point(aes(x = x, y = a), color = "black", alpha = .5) +
  geom_line(aes(x = x, y = a_pred), color = "red", linewidth=1.5)+
  labs(title="OLS for direction on cubic basis")

            