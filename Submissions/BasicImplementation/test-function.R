#########################################
# Independent generate data Example     #
#########################################
test_that("generate_data returns data.frame with correct columns and size", {
  set.seed(114)
  f <- true_function("smooth")
  dat <- generate_data(n = 50, f = f, sigma = 0.5)
  
  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), 50)
  expect_true(all(c("x", "y") %in% names(dat)))
  expect_true(is.numeric(dat$x))
  expect_true(is.numeric(dat$y))
})