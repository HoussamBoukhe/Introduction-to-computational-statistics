theta <- 10

n <- 25 # 25, 50, 100

# Asymptotic (1-alpha)% confidence interval for theta 
theta.aci <- function(data, alpha = 0.05){
  z_alpha <- qnorm(1 - alpha / 2)
  n <- length(data)
  # MLE
  theta.hat <- 1/mean(data)
  return( c(theta.hat - z_alpha*theta.hat/sqrt(n), 
            theta.hat + z_alpha*theta.hat/sqrt(n)) )
}

# Basic Bootstrap 95% confidence interval for theta 
theta.bci <- function(data, B = 9999, alpha = 0.05, seed = NULL){
  if (!is.null(seed)){
    set.seed(seed)
  }
  n <- length(data)
  
  theta.hat <- 1/mean(data)
  
  obs.theta <- c()
  for (i in 1:B){
    # Take a bootstrap sample
    S.b <- sample(data, size = n, replace = TRUE)
    theta.b <- 1/mean(S.b)
    obs.theta[i] <- theta.b
  }
  
  # Bootstrap confidence interval
  bci <- quantile(obs.theta, probs = c(alpha/2, 1-alpha/2))
  return(bci)
}

# Monte carlo experiment to compute the coverage probability
M <- 50

# Number of covering of theta using asymptotic CI
cov.aci <- 0 

# Number of covering of theta using bootstrap CI
cov.bci <- 0

set.seed(12)
for (i in 1:M){
  S <- rexp(n, theta)
  aci <- theta.aci(S)
  bci <- theta.bci(S)
  
  if ((theta >= aci[1]) & (theta <= aci[2])){
    cov.aci <- cov.aci + 1
  }
  
  if ((theta >= bci[1]) & (theta <= bci[2])){
    cov.bci <- cov.bci + 1
  }
}

print(paste('The covering probability of asymptotic confidence interval is', round(cov.aci/M,2)))
print(paste('The covering probability of bootstrap confidence interval is', round(cov.bci/M,2)))
