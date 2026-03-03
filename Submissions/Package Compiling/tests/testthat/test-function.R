test_f <- function(x) sin(x)

#########################################
# Tests for generate_data               #
#########################################
test_that("generate_data returns data.frame with correct columns and size", {
  set.seed(114)
  dat <- generate_data(n = 50, sigma = 0.5, f = test_f)
  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), 50)
  expect_true(all(c("x", "y") %in% names(dat)))
  expect_true(is.numeric(dat$x))
  expect_true(is.numeric(dat$y))
})


#########################################
# Tests for fit_polynomial              #
#########################################
test_that("fit_polynomial returns an lm object with correct degrees", {
  set.seed(114)
  dat <- generate_data(n = 50, sigma = 0.5, f = test_f)
  mod <- fit_polynomial(data = dat, degree = 3)
  expect_s3_class(mod, "lm")
  expect_equal(length(coef(mod)), 4)
})


#########################################
# Tests for compute_mse                 #
#########################################
test_that("compute_mse calculates mean squared error correctly", {
  dummy_data <- data.frame(x = 1:4, y = c(2, 4, 6, 8))
  perfect_mod <- lm(y ~ x, data = dummy_data)
  mse <- compute_mse(perfect_mod, dummy_data)
  expect_true(mse < 1e-10)
})


#########################################
# Tests for monte_carlo_bias_variance   #
#########################################
test_that("monte_carlo_bias_variance returns expected list structure and numeric values", {
  set.seed(114)
  res <- monte_carlo_bias_variance(B = 5, n = 20, sigma = 0.5, degree = 2, f = test_f)
  expect_type(res, "list")
  expect_true(all(c("bias2", "variance") %in% names(res)))
  expect_true(is.numeric(res$bias2))
  expect_true(is.numeric(res$variance))
})
