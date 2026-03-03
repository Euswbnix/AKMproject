#' Define the true underlying function
#'
#' @description Generates the true regression function for the simulation.
#' @param type A character string specifying the function type: "smooth" or "wiggly".
#' @return A function that takes a numeric vector x and returns a numeric vector.
#' @export
true_function <- function(type = c("smooth", "wiggly")) {
  if (match.arg(type) == "smooth") {
    return(function(x) sin(x))
  } else {
    return(function(x) sin(x) + 0.5 * cos(3 * x))
  }
}


#' Generate regression data
#'
#' @param n Sample size
#' @param sigma Noise level
#' @param f True function
#' @return data.frame with x and y
#' @importFrom stats runif rnorm
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
#' train <- generate_data(n = 80, sigma = 1, f = f)
#' mod <- fit_polynomial(train, degree = 5)
#' summary(mod)
#' @importFrom stats lm poly
#' @export
fit_polynomial <- function(data, degree) {
  lm(y ~ poly(x, degree, raw = TRUE), data = data)
}

#' Compute Mean Squared Error (MSE)
#'
#' @description Calculates the mean squared error between the model's predictions and the actual values.
#'
#' @param model A fitted model object (e.g., an lm object).
#' @param data A data.frame containing the predictor 'x' and response 'y'.
#' @return A numeric value representing the mean squared error.
#' @importFrom stats predict
#' @export
compute_mse <- function(model, data) {
  preds <- predict(model, newdata = data)
  mean((data$y - preds)^2)
}


#' Monte Carlo simulation for Bias-Variance Tradeoff
#'
#' @description Estimates bias squared and variance for a given polynomial degree using Monte Carlo simulation.
#'
#' @param B Number of Monte Carlo replications.
#' @param n Sample size for each training dataset.
#' @param sigma Noise level (standard deviation of the Gaussian noise).
#' @param degree Polynomial degree to fit.
#' @param f The true data-generating function.
#' @return A list containing the estimated `bias2` and `variance`.
#' @importFrom stats predict var
#' @export
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

