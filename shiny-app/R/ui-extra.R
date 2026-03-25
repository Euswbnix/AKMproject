# ui-extra.R
# Supplementary UI components:
#   - about_tab(): project info / references tab
#   - results_header(): descriptive text above the plot area
#   - summary_card(): a styled info card for a single metric

# ── About Tab ──────────────────────────────────────────────────────────────
about_tab <- function() {
  tabPanel(
    title = "About",
    icon  = icon("info-circle"),
    fluidRow(
      column(
        width = 8, offset = 2,
        br(),
        h3("Monte Carlo Study of the Bias–Variance Tradeoff"),
        p(
          "This application accompanies the STA380 project by",
          strong("Jizheng Huang, Victor Jiang, Tianchen Xu,"), "and",
          strong("Kai Rui Zhu"), "(University of Toronto, Mississauga)."
        ),
	p(
          "The goal of this project is to illustrate the bias-variance tradeoff in regression through repeated Monte Carlo simulation. ",
          "Users can explore how model flexibility, sample size, and noise level affect training MSE, test MSE, bias^2, and variance."
        ),
	hr(),
	h4("Simulation Framework"),
        p(
          "Data are generated according to the model",
          "\\( y = f(x) + \\varepsilon \\)",
          "where \\( f(x) \\) is a known nonlinear function and",
          "\\( \\varepsilon \\sim \\mathcal{N}(0, \\sigma^2) \\).",
          "Monte Carlo simulation is used to estimate bias\\(^2\\),",
          "variance, and MSE across model complexities."
        ),
	p(
          "For each Monte Carlo repetition, the app generates a new training sample, fits the selected regression model, and evaluates predictive performance. ",
          "Repeated fits are then used to estimate bias², variance, and mean squared error across model complexities."
        ),
        hr(),
	h4("How to Use the App"),
	tags$ol(
          tags$li("Choose the simulation settings in the sidebar, including the random seed, sample size, noise level, model type, and model complexity."),
          tags$li("Adjust the number of Monte Carlo repetitions to control the stability of the estimates."),
          tags$li("Click ", strong("Run Simulation"), " to generate updated results."),
          tags$li("Use the plot tabs to examine the bias-variance curves, prediction spread, and MSE decomposition table."),
          tags$li("Download the summary results if desired.")
        ),
	hr(),
	h4("Interpretation"),
        p(
          "As model complexity increases, bias² typically decreases while variance increases. ",
          "This tradeoff often leads to a U-shaped test MSE curve, where overly simple models underfit and overly flexible models overfit."
        ),
        p(
          "The prediction spread plot shows how fitted models vary across Monte Carlo repetitions, while the decomposition table summarizes the average training MSE, test MSE, bias², and variance."
        ),
        hr(),
        h4("References"),
        tags$ol(
          tags$li(
            "Hastie, T., Tibshirani, R., & Friedman, J. (2009).",
            em("The Elements of Statistical Learning."), "Springer."
          ),
          tags$li(
            "R Core Team (2024).",
            em("R: A Language and Environment for Statistical Computing.")
          ),
          tags$li(
            "R Core Team (2024).",
            em("shiny: Web Application Framework for R.")
          ),
          tags$li(
            "Voss, J. (2013).",
            em("An Introduction to Statistical Computing."), "Wiley."
          )
        )
      )
    )
  )
}

# ── Results Header ──────────────────────────────────────────────────────────
results_header <- function() {
  div(
    class = "results-header",
    style = "margin-bottom: 16px;",
    p(
      class = "text-muted",
      "Adjust the simulation parameters in the sidebar and click",
      strong("Run Simulation"), "to generate results.",
      "Training MSE, test MSE, bias\\(^2\\), and variance are computed",
      "from independent Monte Carlo draws."
    )
  )
}

# ── Summary Metric Card ─────────────────────────────────────────────────────
# value_id: outputId of a textOutput() that the server will populate
summary_card <- function(title, value_id, color = "#0275d8") {
  div(
    class = "summary-card",
    style = paste0(
      "border: 1px solid #dee2e6; border-top: 4px solid ", color, ";",
      "border-radius: 4px; padding: 14px 18px; margin-bottom: 12px;",
      "background: #fff;"
    ),
    p(style = "margin: 0; font-size: 0.85em; color: #6c757d; font-weight: 600;
               text-transform: uppercase; letter-spacing: 0.04em;",
      title),
    h4(style = "margin: 4px 0 0; font-size: 1.6em;",
       textOutput(value_id, inline = TRUE))
  )
}

# ── Metric Cards Row ────────────────────────────────────────────────────────
metrics_row <- function() {
  fluidRow(
    column(2, summary_card("Train MSE",  "card_train_mse", "#0275d8")),
    column(2, summary_card("Test MSE",   "card_test_mse",  "#d9534f")),
    column(2, summary_card("Bias²",      "card_bias2",     "#5cb85c")),
    column(2, summary_card("Variance",   "card_variance",  "#f0ad4e")),
    column(2, summary_card("Best Complexity",  "card_best_model","#6f42c1"))
  )
}
