## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(AKMProject)
# Load our core functions

## ----generate_data------------------------------------------------------------
set.seed(42)
# Define the true function (smooth version: sin(x))
f_true <- true_function("smooth")

# Generate 100 observations with noise level sigma = 0.5
train_data <- generate_data(n = 100, sigma = 0.5, f = f_true)

# Plot the generated data against the true function
x_seq <- seq(-2, 2, length.out = 200)
y_true <- f_true(x_seq)

plot(train_data$x, train_data$y, pch = 16, col = "darkgray",
     xlab = "x", ylab = "y", main = "Simulated Data vs. True Function")
lines(x_seq, y_true, col = "blue", lwd = 2)
legend("topright", legend = c("True f(x)", "Observed Data"), 
       col = c("blue", "darkgray"), lwd = c(2, NA), pch = c(NA, 16))

## ----fit_model----------------------------------------------------------------
# Fit a degree 3 polynomial
model_deg3 <- fit_polynomial(data = train_data, degree = 3)

# Predict over the grid to visualize the fit
y_pred_deg3 <- predict(model_deg3, newdata = data.frame(x = x_seq))

plot(train_data$x, train_data$y, pch = 16, col = "darkgray",
     xlab = "x", ylab = "y", main = "Polynomial Fit (Degree 3)")
lines(x_seq, y_true, col = "blue", lwd = 2, lty = 2)
lines(x_seq, y_pred_deg3, col = "red", lwd = 2)
legend("topright", legend = c("True f(x)", "Degree 3 Fit"), 
       col = c("blue", "red"), lty = c(2, 1), lwd = 2)

## ----compute_mse--------------------------------------------------------------
train_mse <- compute_mse(model = model_deg3, data = train_data)
cat("Training MSE for Degree 3:", round(train_mse, 4), "\n")

## ----mc_simulation------------------------------------------------------------
set.seed(42)
mc_results <- monte_carlo_bias_variance(B = 50, n = 100, sigma = 0.5, 
                                        degree = 3, f = f_true)

cat("Estimated Bias Squared:", round(mc_results$bias2, 4), "\n")
cat("Estimated Variance:", round(mc_results$variance, 4), "\n")

