# app.R
# Entry point for the STA380 Shiny application.
# Sources all modular UI and server files, then calls shinyApp().

library(shiny)
library(plotly)
library(shinycssloaders)

# ── Source modular files ────────────────────────────────────────────────────
source("R/site-helper.R")
source("R/ui-latex.R")
source("R/ui-extra.R")
source("R/ui-sim-input.R")
source("R/mcmc.R")    # simulation engine (TRUE_F, run_mc_simulation, run_mc_sweep)
source("server-plots.R")      # defines server()

# ── UI ──────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = APP_TITLE,
  id    = "navbar",

  # Inject MathJax once for the whole app
  header = latex_header(),

  # ── Tab 1: Simulation ────────────────────────────────────────────────────
  tabPanel(
    title = "Simulation",
    icon  = icon("chart-line"),

    sidebarLayout(

      # Sidebar: all Section 4 inputs
      sidebarPanel(
        width = 3,

        labelled_hr("Simulation Settings"),
        sim_input_ui(),

        br(),

        # Optional: Monte Carlo repetitions
        labelled_hr("Advanced"),
        sliderInput(
          inputId = "mc_reps",
          label = paste0("MC Repetitions (B)"),
          min = MC_REPS_MIN,
          max  = MC_REPS_MAX,
          value = MC_REPS_DEFAULT,
          step = 50
        ),
        helpText(
          style = "font-size: 0.8em;",
          "Higher B gives smoother bias/variance estimates but takes longer."
        )
      ),

      # Main panel: metric summary + tabbed plot outputs
      mainPanel(
        width = 9,

        results_header(),
        bv_decomp_display(),

        br(),

        # Summary cards (Train MSE / Test MSE / Bias² / Variance)
        metrics_row(),

        br(),

        # Plot tabs
        tabsetPanel(
          id = "plot_tabs",

          tabPanel(
            title = "Bias–Variance Curves",
            icon  = icon("wave-square"),
            br(),
            plotOutput("plot_bv_curve", height = "420px"),
            br(),
	    div(
    	      style = "margin-top: 8px; margin-bottom: 12px; padding: 12px 16px; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;",
    	      h4(style = "margin-top: 0; margin-bottom: 8px;", "Model Recommendation"),
    	      textOutput("model_recommendation"),
    	      tags$div(style = "height: 6px;"),
    	      textOutput("fit_feedback")
  	    ),
  	    hr(),
            helpText(
              "Test MSE (red) = Bias² (green) + Variance (amber) + irreducible noise.",
              "Training MSE (blue) decreases monotonically with model complexity."
            )
          ),

          tabPanel(
            title = "Prediction Spread",
            icon = icon("crosshairs"),
            br(),
            plotOutput("plot_pred_spread", height = "420px"),
            hr(),
            helpText(
              "Each grey curve is a fitted model from one MC repetition.",
              "The true function f(x) is shown in black."
            )
          ),

          tabPanel(
            title = "MSE Decomposition",
            icon = icon("table"),
            br(),
            tableOutput("table_mse"),
            br(),
            helpText("Values shown are averaged over all Monte Carlo repetitions.")
	  
            )
        )
       )
      )
    ),

  # ── Tab 2: About ─────────────────────────────────────────────────────────
  about_tab()
)

# ── Launch ──────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
