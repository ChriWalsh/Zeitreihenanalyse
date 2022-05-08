# Create 100 (i.i.d.) data-points x_t~N(1,4)
x <- rnorm(100, 1, 2)

# Create empirical mean function
emp.mean <- function(x){
  T <- length(x)
  x_bar <- solve(T)* sum(x)
  return(x_bar)
}
# Get result: same as built in
emp.mean(x)
mean(x)

# Create empirical variance function
emp.var <- function(x){
  T <- length(x)
  x_bar <- emp.mean(x)
  v_hat <- solve(T) * sum((x-rep(x_bar, T))**2)
  return(as.numeric(v_hat))
}
# Check result:
emp.var(x)
var(x)
# Result almost same as built in...
# Probable reason: we don't use unbiased estimator

