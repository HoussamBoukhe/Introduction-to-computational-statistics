# TODO: Write the likelihood, prior and the posterior in latex 
# Exercice 2.29 Robert Casela Monte Carlo method

set.seed(12)

# Observed sample 
theta <- 2
X <- rnorm(10, theta, 1)

# Number of points to be generated (or alternatively number of iterations)
n <- 200

# This vector will store wether we accepted the value or not 
accRej <- c()
postSample <- c()

iter <- 0 
while (iter < n){
  # Generate a number with the candidate density 
  y <- rcauchy(1,0,1)
  # Accept or reject the generated value M = pi 
  u <- runif(1, min = 0, max = (pi*dcauchy(y)) )
  
  if (u < (1/(1+y^2))*exp(-0.5*(mean(X)-y)^2) ){
    accRej <- c(accRej,1)
    postSample <- c(postSample, y)
    iter <- iter + 1
  } else {accRej <- c(accRej, 0)} 
  
}

# Acceptance probability 
pAcc <- mean(accRej)
print(pAcc)