# mcmc_example.R
# Monte Carlo simulation engine for the Bias-Variance Tradeoff study.
#
# Public API (called by server-plots.R):
#   run_mc_simulation(n, sigma, seed, model_type, complexity, B)
#     -> single-complexity results (cards, prediction spread, MSE table)
#
#   run_mc_sweep(n, sigma, seed, model_type, B)
#     -> results across ALL complexity levels (bias-variance curve tab)
#
# Notation follows the project proposal and ESL Ch. 7:
#   y = f(x) + ε,   ε ~ N(0, σ²)
#   Bias²(x₀) = ( E[f̂(x₀)] − f(x₀) )²
#   Var(x₀) = E[ (f̂(x₀) − E[f̂(x₀)])² ]
#   Test MSE ≈ Bias² + Variance + σ² (irreducible noise)

# ═══════════════════════════════════════════════════════════════════════════
# 1.  TRUE REGRESSION FUNCTION
# ═══════════════════════════════════════════════════════════════════════════

# Domain: x ∈ [0, 1]
# f(x) = sin(2πx) + 0.5·sin(4πx)
# Chosen because it is clearly nonlinear, has multiple local extrema
# (making it a good stress-test for model complexity), and is bounded.

TRUE_F <- function(x) {
  sin(2 * pi * x) + 0.5 * sin(4 * pi * x)
}

# Fixed evaluation grid shared across all MC repetitions (G = 100 points).
# Using a fixed grid is essential: bias and variance are computed pointwise
# then averaged, so x₀ values must be identical across reps.
EVAL_GRID <- seq(0.01, 0.99, length.out = 100)   # avoid exact boundary edge cases

# ═══════════════════════════════════════════════════════════════════════════
# 2.  DATA GENERATION
# ═══════════════════════════════════════════════════════════════════════════

# Generate one training dataset of size n with noise level sigma.
# Each call uses a different internal seed (derived from the outer seed + rep
# index) so that datasets are independently drawn across MC repetitions.
generate_dataset <- function(n, sigma, rep_seed) {
  set.seed(rep_seed)
  x <- sort(runif(n, 0, 1))          # sorted for cleaner plots later
  y <- TRUE_F(x) + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}

# ═══════════════════════════════════════════════════════════════════════════
# 3.  MODEL FITTING  (polynomial regression & kNN regression)
# ═══════════════════════════════════════════════════════════════════════════

# --- 3a. Polynomial regression -------------------------------------------

# Fits lm(y ~ poly(x, degree)).
# Uses orthogonal polynomials (raw = FALSE) for numerical stability at
# high degrees — avoids multicollinearity issues that raw powers cause.
fit_poly <- function(x_train, y_train, degree) {
  df <- data.frame(x = x_train, y = y_train)
  lm(y ~ poly(x, degree, raw = FALSE), data = df)
}

# Predict from a fitted polynomial model at new x values.
predict_poly <- function(model, x_new) {
  as.numeric(predict(model, newdata = data.frame(x = x_new)))
}

# Training MSE for a polynomial model.
train_mse_poly <- function(model, x_train, y_train) {
  y_hat <- predict_poly(model, x_train)
  mean((y_train - y_hat)^2)
}

# --- 3b. kNN regression --------------------------------------------------

# 1-D kNN regression: for each test point x₀, take the mean of the k
# nearest training responses.  O(n·G) — fast enough for n ≤ 500, G = 100.
# No external package required.
knn_predict_1d <- function(x_train, y_train, x_new, k) {
  # Guard: k must not exceed training set size
  k <- min(k, length(x_train))

  sapply(x_new, function(x0) {
    distances <- abs(x_train - x0)
    neighbour_idx <- order(distances)[seq_len(k)]
    mean(y_train[neighbour_idx])
  })
}

# Training MSE for kNN (predict back on training data).
train_mse_knn <- function(x_train, y_train, k) {
  y_hat <- knn_predict_1d(x_train, y_train, x_train, k)
  mean((y_train - y_hat)^2)
}

# ═══════════════════════════════════════════════════════════════════════════
# 4.  UNIFIED FIT / PREDICT INTERFACE
# ═══════════════════════════════════════════════════════════════════════════

# Dispatches to poly or kNN depending on model_type.
# Returns a lightweight object used by predict_fitted() and train_mse_fitted().
fit_model <- function(x_train, y_train, model_type, complexity) {
  if (model_type == "poly") {
    model <- fit_poly(x_train, y_train, degree = complexity)
    list(type = "poly",
         model = model,
         x_train = x_train,
         y_train = y_train,
         complexity = complexity)
  } else {                                  # knn
    list(type = "knn",
         k = complexity,
         x_train = x_train,
         y_train = y_train,
         complexity = complexity)
  }
}

predict_fitted <- function(fit_obj, x_new) {
  if (fit_obj$type == "poly") {
    predict_poly(fit_obj$model, x_new)
  } else {
    knn_predict_1d(fit_obj$x_train, fit_obj$y_train, x_new, fit_obj$k)
  }
}

train_mse_fitted <- function(fit_obj) {
  if (fit_obj$type == "poly") {
    train_mse_poly(fit_obj$model, fit_obj$x_train, fit_obj$y_train)
  } else {
    train_mse_knn(fit_obj$x_train, fit_obj$y_train, fit_obj$k)
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# 5.  BIAS & VARIANCE ESTIMATORS
# ═══════════════════════════════════════════════════════════════════════════

# Given a B × G prediction matrix (one row per MC rep, one column per grid
# point) and the true function values at the grid, compute pointwise then
# averaged bias², variance, and test MSE.
#
# Estimators (ESL §7.3):
#   Bias²(x₀)  = ( mean_b[ f̂_b(x₀) ] − f(x₀) )²
#   Var(x₀)    = mean_b[ ( f̂_b(x₀) − mean_b[f̂_b(x₀)] )² ]
#   TestMSE(x₀)= Bias²(x₀) + Var(x₀) + σ²

compute_bv_stats <- function(pred_matrix, true_y, sigma) {
  # pred_matrix: B × G,  true_y: length G
  G <- length(true_y)
  B <- nrow(pred_matrix)

  mean_pred <- colMeans(pred_matrix)                        # E[f̂(x₀)] over reps

  # Pointwise quantities (length-G vectors)
  pw_bias2 <- (mean_pred - true_y)^2
  pw_variance <- colMeans(sweep(pred_matrix, 2, mean_pred)^2)  # E[(f̂-E[f̂])²]
  pw_test_mse <- pw_bias2 + pw_variance + sigma^2

  # Scalar summaries (averaged over grid)
  list(
    bias2 = mean(pw_bias2),
    variance = mean(pw_variance),
    test_mse = mean(pw_test_mse),
    irreducible = sigma^2,

    # Pointwise vectors for detailed plots
    pointwise = data.frame(
      x = EVAL_GRID,
      true_y  = true_y,
      mean_pred = mean_pred,
      bias2 = pw_bias2,
      variance = pw_variance,
      test_mse = pw_test_mse
    )
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 6.  SINGLE-COMPLEXITY MC SIMULATION  (main public function)
# ═══════════════════════════════════════════════════════════════════════════
#
# Returns a named list consumed by server-plots.R for:
#   - Summary cards  (bias2, variance, train_mse, test_mse)
#   - Prediction spread plot (pred_matrix + x_grid + true_y)
#   - MSE decomposition table (summary_df)
#   - Download CSV (export_df)

run_mc_simulation <- function(n, sigma, seed, model_type, complexity, B = 200) {

  true_y <- TRUE_F(EVAL_GRID)           # fixed reference values

  # Pre-allocate B × G prediction matrix
  pred_matrix <- matrix(NA_real_, nrow = B, ncol = length(EVAL_GRID))
  train_mse_vec <- numeric(B)

  # ── Monte Carlo loop ───────────────────────────────────────────────────
  for (b in seq_len(B)) {
    rep_seed <- seed * 1000L + b        # deterministic per-rep seed

    # 1. Draw an independent training set
    dat <- generate_dataset(n, sigma, rep_seed)

    # 2. Fit the model
    fit <- fit_model(dat$x, dat$y, model_type, complexity)

    # 3. Predict on the fixed evaluation grid
    pred_matrix[b, ] <- predict_fitted(fit, EVAL_GRID)

    # 4. Record training MSE
    train_mse_vec[b] <- train_mse_fitted(fit)
  }

  # ── Bias-variance decomposition ────────────────────────────────────────
  bv <- compute_bv_stats(pred_matrix, true_y, sigma)

  train_mse_avg <- mean(train_mse_vec)

  # ── Export data frame (for Download button) ────────────────────────────
  summary_df <- data.frame(
    Metric = c("Train MSE", "Test MSE", "Bias²",
               "Variance",  "Irreducible Noise"),
    Value  = round(c(train_mse_avg, bv$test_mse, bv$bias2,
                     bv$variance,   bv$irreducible), 6)
  )

  # ── Return ─────────────────────────────────────────────────────────────
  list(
    # Scalar summaries for UI cards
    train_mse = train_mse_avg,
    test_mse = bv$test_mse,
    bias2 = bv$bias2,
    variance = bv$variance,
    irreducible = bv$irreducible,

    # Matrix + grid for prediction spread plot
    pred_matrix  = pred_matrix,
    x_grid = EVAL_GRID,
    true_y = true_y,

    # Pointwise breakdown for potential detailed plot
    pointwise = bv$pointwise,

    # Table & CSV export
    summary_df = summary_df,

    # Echo parameters back (useful for display / logging)
    params = list(
      n = n,
      sigma = sigma,
      seed = seed,
      model_type = model_type,
      complexity = complexity,
      B = B
    )
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 7.  SWEEP ACROSS ALL COMPLEXITY LEVELS  (for BV curve tab)
# ═══════════════════════════════════════════════════════════════════════════
#
# Runs run_mc_simulation() at every complexity level and stacks the scalar
# summaries into a data frame, which server-plots.R can pass directly to
# ggplot for the bias-variance tradeoff curve.
#
# Complexity grids:
#   poly  → degrees 1:15
#   knn   → k values 1..50 (note: k=1 is most complex; k=50 is smoothest)
#            x-axis will be labelled "1/k" or reversed for intuitive direction.

POLY_DEGREES <- 1:15
KNN_K_VALUES <- c(1, 2, 3, 5, 7, 10, 15, 20, 30, 40, 50)

run_mc_sweep <- function(n, sigma, seed, model_type, B = 200) {

  complexity_grid <- if (model_type == "poly") POLY_DEGREES else KNN_K_VALUES

  results_list <- lapply(complexity_grid, function(cx) {
    sim <- run_mc_simulation(
      n = n,
      sigma = sigma,
      seed = seed,
      model_type = model_type,
      complexity = cx,
      B = B
    )
    data.frame(
      complexity = cx,
      train_mse = sim$train_mse,
      test_mse = sim$test_mse,
      bias2 = sim$bias2,
      variance = sim$variance,
      irreducible = sim$irreducible
    )
  })

  sweep_df <- do.call(rbind, results_list)

  # For kNN, add an "effective complexity" column (1/k) so the x-axis
  # runs low→high complexity left→right, matching the polynomial axis.
  if (model_type == "knn") {
    sweep_df$eff_complexity <- 1 / sweep_df$complexity  # 1/k ↑ = more complex
  } else {
    sweep_df$eff_complexity <- sweep_df$complexity  # degree ↑ = more complex
  }

  list(
    sweep_df = sweep_df,         # the plottable data frame
    complexity_grid = complexity_grid,
    model_type = model_type,
    params = list(n = n, sigma = sigma, seed = seed, B = B)
  )
  
}