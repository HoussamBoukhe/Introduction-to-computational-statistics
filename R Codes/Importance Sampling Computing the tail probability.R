#### Computation of the tail probability of the standard normal 

t <- 2.5 # Compute the tail starting from t 

# Number of iteration of each Monte Carlo experiment 
nbr_itm <- seq(100,15000, 50)

### 1.Estimation with direct Monte Carlo experiment 
est_t <- c()

set.seed(12)
for (n in nbr_itm){
    # Generate a standard normal sample of size n
    X <- rnorm(n)
    
    # Estimate the quantity of interest 
    est_mu <- sum(X>t)/n 
    
    # Append the quantity of interest in est_t
    est_t <- c(est_t, est_mu)
} 


# True value 
pt <- 1 - pnorm(t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


# Plot 
plot(nbr_itm,est_t, type ='l',title = "", xlab="Sample size", ylab="Estimated value",
     ylim = c(0.004,0.010))

# Add the true value line
abline(h = pt, col = 'red', lwd = 2)

# Add margins above and below the true value
abline(h = pt + 0.001, col = "blue", lty = 2)
abline(h = pt - 0.001, col = "blue", lty = 2)

# Add a legend
legend("topright", legend = c("True Value", "Margin ±0.001"), 
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)


### 2. Estimation using importance sampling 

est_t_IP <- c() 
# Parameters of the instrumental density: truncated exponential 
rate <- 1

set.seed(12)
for (n in nbr_itm){
    # Generate a sample of size n from the truncated exponential
    X <- rexp(n,rate = rate) 
    
    # Estimate the quantity of interest 
    Y <- sapply(X, function(x) if (x>t) {dnorm(x) / dexp(x)} else {0})
    est_mu <- mean(Y)
    
    # Append the quantity of interest in est_t
    est_t_IP <- c(est_t_IP, est_mu)
} 

# Plot 
plot(nbr_itm,est_t_IP, type ='l',title = "", xlab="Sample size", ylab="Estimated value",
     ylim = c(0.004,0.010) )

# Add the true value line
abline(h = pt, col = 'red', lwd = 2)

# Add margins above and below the true value
abline(h = pt + 0.001, col = "blue", lty = 2)
abline(h = pt - 0.001, col = "blue", lty = 2)

# Add a legend
legend("topright", legend = c("True Value", "Margin ±0.001"), 
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)
