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

#' Fit polynomial regression
#'
#' @description Fits y ~ poly(x, degree, raw = TRUE) using least squares.
#' 
#' @param data Training dataset
#' @param degree Polynomial degree
#' @return A fitted lm object.
#' 
#' @examples
#' set.seed(1)
#' f <- true_function("wiggly")
#' train <- generate_data(80, f, sigma = 1)
#' mod <- fit_polynomial(train, degree = 5)
#' summary(mod)
#' @importFrom stats lm poly
#' @export
fit_polynomial <- function(data, degree) {
  lm(y ~ poly(x, degree, raw = TRUE), data = data)
}

compute_mse <- function(model, data) {
  preds <- predict(model, newdata = data)
  mean((data$y - preds)^2)
}


monte_carlo_bias_variance <- function(B, n, sigma, degree, f) {
  
  x_grid <- seq(-2, 2, length.out = 200)
  predictions <- matrix(NA, nrow = B, ncol = length(x_grid))
  
  for (b in 1:B) {
    train <- generate_data(n, sigma, f)
    model <- fit_polynomial(train, degree)
    predictions[b, ] <- predict(model, 
                                newdata = data.frame(x = x_grid))
  }
  
  f_true <- f(x_grid)
  
  mean_pred <- colMeans(predictions)
  
  bias2 <- mean((mean_pred - f_true)^2)
  variance <- mean(apply(predictions, 2, var))
  
  list(bias2 = bias2, variance = variance)
}