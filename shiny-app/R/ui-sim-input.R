# ui-sim_input.R
# Defines all user-facing simulation inputs (Section 4 of project proposal).
# Returns a tagList() intended to be embedded inside a sidebarPanel().

sim_input_ui <- function() {
  tagList(

    # ── 1. Sample Size ──────────────────────────────────────────────────────
    sliderInput(
      inputId  = "n",
      label    = "Sample Size (n)",
      min      = 20,
      max      = 500,
      value    = 100,
      step     = 10
    ),

    # ── 2. Noise Level ──────────────────────────────────────────────────────
    sliderInput(
      inputId  = "sigma",
      label    = HTML("Noise Level (&sigma;)"),
      min      = 0.1,
      max      = 5.0,
      value    = 1.0,
      step     = 0.1
    ),

    # ── 3. Random Seed ──────────────────────────────────────────────────────
    numericInput(
      inputId = "seed",
      label   = "Random Seed",
      value   = 42,
      min     = 1,
      max     = 99999,
      step    = 1
    ),

    hr(),

    # ── 4. Model Choice ─────────────────────────────────────────────────────
    radioButtons(
      inputId  = "model_type",
      label    = "Regression Model",
      choices  = c(
        "Polynomial Regression" = "poly",
        "k-Nearest Neighbours"  = "knn"
      ),
      selected = "poly"
    ),

    # ── 5. Model Complexity (conditional on model choice) ───────────────────

    # Shown when polynomial is selected
    conditionalPanel(
      condition = "input.model_type == 'poly'",
      sliderInput(
        inputId = "poly_degree",
        label   = "Polynomial Degree",
        min     = 1,
        max     = 15,
        value   = 3,
        step    = 1
      )
    ),

    # Shown when kNN is selected
    conditionalPanel(
      condition = "input.model_type == 'knn'",
      sliderInput(
        inputId = "knn_k",
        label   = "Number of Neighbours (k)",
        min     = 1,
        max     = 50,
        value   = 5,
        step    = 1
      )
    ),

    hr(),

    # ── 6. Run Simulation ───────────────────────────────────────────────────
    actionButton(
      inputId = "run_sim",
      label   = "Run Simulation",
      class   = "btn btn-primary btn-block",
      icon    = icon("play")
    ),

    br(),

    # ── 6. Download Results ─────────────────────────────────────────────────
    downloadButton(
      outputId = "download_results",
      label    = "Download Results (.csv)",
      class    = "btn btn-default btn-block"
    )
  )
}