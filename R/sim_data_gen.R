# Generate simulated data
get_sim_data <- function(seed = 102888){
  set.seed(seed)
  x <- cbind(rep(1, 1000),
             runif(1000),
             sample(0:1, 1000, replace = T),
             runif(1000) * 100
  )

  y <- x %*% c(15, 55, 25, 3) +
    rnorm(1000, 0, 30)
  list(x = x, y = y)
}

