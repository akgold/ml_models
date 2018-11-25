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
            y = y,
            model = ols)
  class(l) <- c("ols", "ml_model")
  l
}

print.ols <- function(x) {
  print(x$b)
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
f_step_reg <- function(model, x, y, ...) {
  x <- add_intercept(x)

  do_f_step(y, x[,seq(2, ncol(x))], model(y, x[,1]), ...)
}

do_f_step <- function(y, x_not_in, model, ...) {
  if (ncol(x_not_in) == 0) {
    return(list(model))
  }

  # get col of x most correlated with residuals
  cors <- apply(x_not_in, 2, function(x_i) cor(x_i, y - predict(model))) %>%
    abs()
  col <- which(cors == max(cors))

  # retrain model
  c(list(model),
       do_f_step(x_not_in = x_not_in[,-col, drop = FALSE],
                 y = y,
                 model = model$model(y, cbind(model$x, x_not_in[,col]), ...)))
}

add_intercept <- function(x) {
  all_ones <- which(apply(x, 2, function(x_i) all(x_i == 1)))
  if (all_ones != 1) {
    x <- x[,-all_ones]
    x <- cbind(rep(1, length(y)), x)
  }
  x
}



