# server-plots.R
# Server logic for the STA380 Bias-Variance Shiny app.
# Calls run_mc_simulation() and run_mc_sweep() from R/mcmc_example.R.
library(ggplot2)
library(plotly)
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
  output$card_best_model <- renderText({
  req(sim_sweep())
  df <- sim_sweep()$sweep_df
  best_idx <- which.min(df$test_mse)

  paste0("Degree ", df$complexity[best_idx])  })

  # ── Plot: Bias-Variance curves (sweep) [Refactored for Plotly] ───────────
  output$plot_bv_curve <- renderPlotly({
    req(sim_sweep())
    mdl <- input$model_type
    df  <- sim_sweep()$sweep_df
    best_idx <- which.min(df$test_mse)
    best_cx  <- df$complexity[best_idx]
    
    x_label <- if (mdl == "poly") "Polynomial Degree" else "Effective Complexity (1/k)"
    
    # Calculate complexities for vertical reference lines
    cx  <- active_complexity()
    eff <- if (mdl == "knn") 1 / cx else cx
    best_eff <- if (mdl == "knn") 1 / best_cx else best_cx
    
    p <- ggplot(df, aes(x = eff_complexity)) +
      # Irreducible noise line
      geom_hline(yintercept = df$irreducible[1], linetype = "dashed", color = "grey50") +
      
      # Bias2 Line & Points
      geom_line(aes(y = bias2, color = "Bias\u00b2")) +
      geom_point(aes(y = bias2, color = "Bias\u00b2", text = paste("Bias\u00b2:", round(bias2, 4)))) +
      
      # Variance Line & Points
      geom_line(aes(y = variance, color = "Variance")) +
      geom_point(aes(y = variance, color = "Variance", text = paste("Variance:", round(variance, 4)))) +
      
      # Test MSE Line & Points
      geom_line(aes(y = test_mse, color = "Test MSE"), linewidth = 1) +
      geom_point(aes(y = test_mse, color = "Test MSE", text = paste("Test MSE:", round(test_mse, 4)))) +
      
      # Train MSE Line & Points
      geom_line(aes(y = train_mse, color = "Train MSE"), linewidth = 1) +
      geom_point(aes(y = train_mse, color = "Train MSE", text = paste("Train MSE:", round(train_mse, 4)))) +
      
      # Vertical reference lines
      geom_vline(xintercept = eff, linetype = "dotted", color = "black") +
      geom_vline(xintercept = best_eff, linetype = "dashed", color = "purple") +
      
      labs(
        title = paste("Bias\u00b2 \u2013 Variance Tradeoff:", input$n, "obs, \u03c3 =", input$sigma),
        x = x_label,
        y = "MSE",
        color = ""
      ) +
      scale_color_manual(values = c(
        "Train MSE" = "#3182bd",
        "Test MSE"  = "#de2d26",
        "Bias\u00b2"  = "#31a354",
        "Variance"  = "#e6550d"
      )) +
      theme_minimal()
    
    # Convert ggplot to interactive Plotly object
    ggplotly(p, tooltip = c("x", "text")) %>%
      layout(legend = list(orientation = "h", y = -0.15))
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

    lines(res$x_grid, res$pointwise$mean_pred, col = "#de2d26", lwd = 2.2, lty = 2)
    lines(res$x_grid, res$true_y,              col = "black",       lwd = 2.5)

    legend("topright",
           legend = c("True f(x)", "Mean prediction", "MC fitted curves"),
           col = c("black", "#de2d26", "#555555"),
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
