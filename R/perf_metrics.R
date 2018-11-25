# Compute performance metrics

#' RMSE
#'
#' @param actual actual
#' @param predicted predicted
#'
#' @return RMSE
#' @export
#'
#' @examples
#' a <- runif(100)
#' b <- runif(100)
#' rmse(a, b)
#' testthat::expect_lt(abs(rmse(a, b) - ModelMetrics::rmse(a, b)), 0.001)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


#' Get Performance Metrics on a sequence of models
#'
#' @param mods
#' @param perf_fn
#'
#' @return
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' a <- f_step_reg(ols, d$x, d$y)
#' p <- get_perf(a)
get_perf <- function(mods, perf_fn = rmse, lower_better = TRUE) {
  l <- list(vals = map_dbl(mods, ~ get_metric(., perf_fn)),
            fn = formals()$perf_fn)
  l$best <- which(l$vals == min(l$vals * ifelse(lower_better, 1, -1)))
  class(l) <- "perf_seq"
  l
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' a <- f_step_reg(ols, d$x, d$y)
#' get_perf(a)
print.perf_seq <- function(x) {
  cat(glue::glue("Metric: {x$fn} \n\tValues: {paste(x$vals, collapse = ', ')}"))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' d <- get_sim_data()
#' a <- f_step_reg(ols, d$x, d$y)
#' p <- get_perf(a)
#' plot(p)
plot.perf_seq <- function(x) {
  pd <- tibble::tibble(mod = seq(length(x$vals)),
                       val = x$vals)
  pd$Best <- pd$mod == x$best

  pd %>%
    ggplot(aes(x = mod, y = val, group = 1)) +
    geom_line() +
    geom_point(aes(color = Best), size = 3) +
    xlab("Model Number") +
    ylab(x$fn) +
    scale_color_manual(values = c(`TRUE` = "forest green", `FALSE` = "black"),
                       guide = FALSE)
}

get_metric <- function(mod, perf_fn = rmse) {
  perf_fn(mod$y, predict(mod))
}
