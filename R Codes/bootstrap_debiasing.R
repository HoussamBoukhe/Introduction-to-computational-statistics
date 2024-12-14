theta <- 3.5 # Unknown parameter
n <- 30 # Population size

set.seed(12)
S <- runif(n,0,theta)

M <- 20000 

theta_hat <- c()

for (i in 1:M){
  # Create a bootstrap sample
  S_star = sample(S, size = length(S), replace = TRUE) 
  # Apply the estimator T to S_star 
  theta_hat[i] <- max(S_star)
}

print(max(S) - mean(theta_hat))

bias_1 <- mean(theta_hat) - max(S)

# debiasing the estimator T
print(max(S) - bias_1)

# notice that the found value is close to the real value of theta 