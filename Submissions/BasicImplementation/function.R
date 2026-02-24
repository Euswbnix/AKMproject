#' Generate regression data
#'
#' @param n Sample size
#' @param sigma Noise level
#' @param f True function
#' @return data.frame with x and y
#' @export
generate_data <- function(n, sigma, f) {
  x <- runif(n, -2, 2)
  y <- f(x) + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}