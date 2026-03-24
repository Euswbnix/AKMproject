# server-plots.R
# Server logic for the STA380 Bias-Variance Shiny app.
# Calls run_mc_simulation() and run_mc_sweep() from R/mcmc_example.R.

server <- function(input, output, session) {

  # ── Helper: active complexity parameter ─────────────────────────────────
  active_complexity <- reactive({
    if (input$model_type == "poly") input$poly_degree else input$knn_k
  })

  # ── Reactive: single-complexity simulation (cards / spread / table) ─────
  sim_single <- eventReactive(input$run_sim, {
    withProgress(message = "Running Monte Carlo simulation…", value = 0, {

      setProgress(0.2)

      result <- run_mc_simulation(
        n = input$n,
        sigma = input$sigma,
        seed = input$seed,
        model_type = input$model_type,
        complexity = active_complexity(),
        B = input$mc_reps
      )

      setProgress(1)
      result
    })
  })

  # ── Reactive: full sweep simulation (BV curve tab) ───────────────────────
  sim_sweep <- eventReactive(input$run_sim, {
    withProgress(message = "Running complexity sweep…", value = 0, {

      setProgress(0.2)

      result <- run_mc_sweep(
        n = input$n,
        sigma = input$sigma,
        seed  = input$seed,
        model_type = input$model_type,
        B = input$mc_reps
      )

      setProgress(1)
      result
    })
  })

  # ── Summary metric cards ─────────────────────────────────────────────────
  fmt <- function(x) ifelse(is.null(x), "—", sprintf("%.4f", x))

  output$card_train_mse <- renderText({
    req(sim_single())
    fmt(sim_single()$train_mse)
  })
  output$card_test_mse <- renderText({
    req(sim_single())
    fmt(sim_single()$test_mse)
  })
  output$card_bias2 <- renderText({
    req(sim_single())
    fmt(sim_single()$bias2)
  })
  output$card_variance <- renderText({
    req(sim_single())
    fmt(sim_single()$variance)
  })

  # ── Plot: Bias-Variance curves (sweep) ───────────────────────────────────
  output$plot_bv_curve <- renderPlot({
    req(sim_sweep())
    df  <- sim_sweep()$sweep_df
    mdl <- sim_sweep()$model_type

    x_label <- if (mdl == "poly") "Polynomial Degree" else "Effective Complexity (1/k)"
    x_var <- df$eff_complexity
    xlim <- range(x_var)
    ylim <- c(0, max(df$test_mse, df$train_mse) * 1.05)

    par(mar = c(4.5, 4.5, 2.5, 1.5), bg = "white")
    plot(NULL, xlim = xlim, ylim = ylim,
         xlab = x_label, ylab = "MSE",
         main = paste("Bias\u00b2 \u2013 Variance Tradeoff:", input$n,
                      "obs, \u03c3 =", input$sigma))

    # Irreducible noise floor
    abline(h = df$irreducible[1], lty = 2, col = "grey50", lwd = 1.2)
    text(xlim[1], df$irreducible[1] * 1.04,
         labels = paste0("\u03c3\u00b2 = ", round(df$irreducible[1], 3)),
         adj = 0, cex = 0.75, col = "grey40")

    lines(x_var, df$bias2, col = PALETTE$bias2, lwd = 2.2, type = "b", pch = 16, cex = 0.7)
    lines(x_var, df$variance, col = PALETTE$variance, lwd = 2.2, type = "b", pch = 16, cex = 0.7)
    lines(x_var, df$test_mse, col = PALETTE$test, lwd = 2.5, type = "b", pch = 16, cex = 0.7)
    lines(x_var, df$train_mse,col = PALETTE$train, lwd = 2.5, type = "b", pch = 16, cex = 0.7)

    # Mark selected complexity with a vertical line
    cx  <- active_complexity()
    eff <- if (mdl == "knn") 1 / cx else cx
    abline(v = eff, lty = 3, col = "black", lwd = 1.2)
    text(eff, ylim[2] * 0.97,
         labels = if (mdl == "poly") paste("degree =", cx) else paste("k =", cx),
         adj = -0.1, cex = 0.78)

    legend("topright",
           legend = c("Train MSE", "Test MSE", "Bias\u00b2", "Variance",
                      "Irreducible (\u03c3\u00b2)"),
           col = c(PALETTE$train, PALETTE$test, PALETTE$bias2,
                      PALETTE$variance, "grey50"),
           lwd = c(2.5, 2.5, 2.2, 2.2, 1.2),
           lty = c(1, 1, 1, 1, 2),
           pch = c(16, 16, 16, 16, NA),
           bty = "n", cex = 0.82)
  })

  # ── Plot: Prediction spread ──────────────────────────────────────────────
  output$plot_pred_spread <- renderPlot({

    req(sim_single())
    res <- sim_single()

    par(mar = c(4.5, 4.5, 2.5, 1.5), bg = "white")
    plot(NULL,
         xlim = c(0, 1), ylim = range(res$pred_matrix) * 1.05,
         xlab = "x", ylab = "y",
         main = paste("MC Prediction Spread \u2014 B =", res$params$B, "repetitions"))

    # Up to 100 MC curves in translucent grey
    B_plot <- min(nrow(res$pred_matrix), 100)
    for (b in seq_len(B_plot)) {
      lines(res$x_grid, res$pred_matrix[b, ], col = "#00000018", lwd = 0.8)
    }

    lines(res$x_grid, res$pointwise$mean_pred, col = PALETTE$test, lwd = 2.2, lty = 2)
    lines(res$x_grid, res$true_y,              col = "black",       lwd = 2.5)

    legend("topright",
           legend = c("True f(x)", "Mean prediction", "MC fitted curves"),
           col = c("black", PALETTE$test, "#555555"),
           lwd = c(2.5, 2.2, 0.8),
           lty = c(1, 2, 1),
           bty = "n", cex = 0.82)
  })

  # ── Table: MSE decomposition ─────────────────────────────────────────────
  output$table_mse <- renderTable({
    req(sim_single())
    sim_single()$summary_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")

  # ── Download handler ─────────────────────────────────────────────────────
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("bv_results_n", input$n, "_sigma", input$sigma,
             "_", input$model_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(sim_single())
      write.csv(sim_single()$pointwise, file, row.names = FALSE)
    }
  )
}