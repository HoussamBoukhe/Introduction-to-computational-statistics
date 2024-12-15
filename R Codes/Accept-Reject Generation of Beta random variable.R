### Generating Beta sample with Accept-Reject method 

# Density function of the beta distribution
target_dist <- function(x, alpha = 2, beta = 4) {
  return(dbeta(x, alpha, beta))
}

# Maximum value of the density function
M <- 11/5

# Initialize vectors to store results
gen_pair <- list()  # List to store (x, u) pairs
acc <- numeric(0)   # Empty numeric vector for accepted elements

set.seed(12)
for (i in 1:1000) {
  x <- runif(1, min = 0, max = 1)  # Generate x from unif (0,1)
  u <- runif(1, min = 0, max = M) # Generate u from unif (0,M)
  
  gen_pair[[i]] <- c(x, u)  # Store the pair (x, u) in a list
  
  # Acceptance condition
  if (target_dist(x) >= u) {
    acc <- c(acc, x)  # Append accepted x to acc
  }
}


# Plot 
x <- sapply(gen_pair, function(coord) coord[1])
y <- sapply(gen_pair, function(coord) coord[2])

# Plot the points
y_beta <- target_dist(x)
colors <- ifelse(y <= y_beta, "blue", "red")

plot(x, y, 
     main = "", 
     xlab = "x", 
     ylab = "Density", 
     cex = 0.5,      # Pint size
     pch = 19,       # Point shape
     col = colors)   # Point color

# Plot the Beta density 
x_seq <- seq(0, 1, length.out = 100)  
y_beta <- target_dist(x_seq)          # Beta density values
lines(x_seq, y_beta, col = "black", lwd = 2) 

legend("topright",                      # Position of the legend
       legend = c("Accepted",   
                  "Rejected"),  
       col = c("blue", "red"),         # Colors in the legend
       pch = 19,                        # Point shape in the legend
       cex = 0.8) 
