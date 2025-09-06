


a_0_5 = a[x <= 5]

a_5_10 = a[x> 5 & x <= 10]

a_10_20 = a[x> 10 & x <= 20]

a_20 = a[x> 20]


angle_breaks = c(c(0:(4*pi))/2, 6.3) 

hist(a_0_5, breaks = angle_breaks  )
hist(a_5_10, breaks = angle_breaks )
hist(a_10_20, breaks = angle_breaks)
hist(a_20, breaks = angle_breaks)


#png(paste0("WindSpeed/data_visualization/2. santa_ana/hist_0_5.png"), width = 800, height = 600, res = 100)
# plot 
#dev.off()



n = length(a_0_5)
mu = c(1,1)

U_0_5 = radians2unitcircle(a_0_5)
U_small = U_0_5[1:4,]

for (t in (1:5)) {
  M = U_small%*%mu
  
  
}


mu = c(1,0)
sigma = matrix(c(1,0,0,1), nrow=2)


for (t in (1:1000)) {
  M = U_small%*%mu
  l = dnorm(-M) / 1-pnorm(-M)
  
  A = 

  
  mu = sweep(U_small, MARGIN=1, STATS = M+l, FUN="*")
  mu = colMeans(mu)
  
  #S1 = n * mu
    
}

#install.packages("circular")
library(circular)

sigma = matrix(c(1,0,0,1), nrow=2)

discrete_angles <- seq(0, 2 * pi, length.out = 100)

density_values = dpnorm(discrete_angles, mu=mu, sigma=sigma)
plot(discrete_angles, density_values)
