### Estimation of the rate parameter of an exponential family

set.seed(1); n = 50; theta = 7

# Generate a sample with the exponential distribution
X <- rexp(n, rate = 1/theta)

# Bootstrap parameter 
B = 5000
B_est <- c() # bootstrap estimations of theta

for (i in 1:B){
  X_star <- sample(X, size =n, replace = TRUE)
  est_theta <- mean(X_star)
  B_est <- c(B_est, est_theta)
}

# Bootstrap distribution of the estimator of the param theta
B_dens = density(B_est, n = 500)

# True sampling distribution of the estimator of theta 
est_dens = dgamma(x=B_dens$x, shape = n, scale = mean(X) / n )

# Plot the true density and bootstrap density of the estimator 
plot(B_dens$x, B_dens$y, xlab = "theta", ylab = "Density", 
     col = "red", type ='l', lwd = 2)

lines(B_dens$x, est_dens, col = "blue", lwd = 2)

legend("topright",                  
       legend = c("Bootstrap density", "True density"),  
       col = c("red", "blue"),      
       lwd = 2,                     
       lty = 1)