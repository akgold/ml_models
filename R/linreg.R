# Code to run OLS on a matrix

#' Ordinary Least Squares
#'
#' @param x matrix X
#' @param y vector y
#'
#' @return betas from OLS regression
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' # Actual coefficients are 15, 55, 25, 3
#' lm(d$y ~ d$x + 0)
#' ols(d$y, d$x)
ols <- function(y, x) {
  l <- list(b = solve(t(x) %*% x) %*% t(x) %*% y,
            x = x,
            y = y)
  class(l) <- "ols"
  l
}

#' Predict from OLS
#'
#' @param b
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' lm_p <- lm(d$y ~ d$x + 0) %>% predict()
#' ols_p <- ols(d$y, d$x) %>% predict(d$x)
#' # Equal to machine rounding error
#' all(round(lm_p, 8) == round(ols_p, 8))
predict.ols <- function(ols, x = ols$x) {
  x %*% ols$b
}

#' Title
#'
#' @param model
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' f_step_reg(ols, d$x, d$y)
f_step_reg <- function(model, x, y) {
  all_ones <- which(apply(x, 2, function(x_i) all(x_i == 1)))
  if (length(all_ones) > 0) {
    x <- x[,-all_ones]
  }

  # Get initial residuals from just intercept
  ones <- rep(1, length(y))
  trained <- model(y, ones)

  do_f_step(ones, x, y, predict(trained), model)
}

do_f_step <- function(x_in, x_not_in, y, y_hat, model) {
  if (ncol(x_not_in) == 1) {
    return(model(y, cbind(x_in, x_not_in)))
  }

  # get col of x most correlated with residuals
  cors <- apply(x_not_in, 2, function(x_i) cor(x_i, y - y_hat)) %>% abs()
  col <- which(cors == max(cors))

  # retrain model
  x <- cbind(x_in, x_not_in[,col])
  mod <- model(y, x)
  list(mod,
       do_f_step(x, x_not_in[,-col, drop = FALSE], y, predict(mod), model))
}


