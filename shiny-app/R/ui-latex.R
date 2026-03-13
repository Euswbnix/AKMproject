# ui-latex.R
# UI helpers for rendering mathematical notation via MathJax.
# Call latex_header() once inside your ui to load MathJax,
# then use latex_block() wherever you need a rendered equation.

# Inject MathJax into the page (call once, at the top level of ui)
latex_header <- function() {
  withMathJax()
}

# Render an inline display of the bias-variance decomposition
bv_decomp_display <- function() {
  div(
    class = "bv-formula",
    style = "margin: 12px 0; padding: 10px; background: #f8f9fa;
             border-left: 4px solid #0275d8; border-radius: 2px;",

    p(strong("Bias–Variance Decomposition:")),

    # E[MSE] = Bias² + Variance + σ²
    helpText(
      "$$\\mathbb{E}\\left[(y - \\hat{f}(x))^2\\right]
        = \\underbrace{\\left[\\text{Bias}(\\hat{f}(x))\\right]^2}_{\\text{Bias}^2}
        + \\underbrace{\\text{Var}(\\hat{f}(x))}_{\\text{Variance}}
        + \\underbrace{\\sigma^2}_{\\text{Irreducible}}$$"
    ),

    # Monte Carlo estimators
    p(strong("MC Estimates (across B simulations):")),
    helpText(
      "$$\\widehat{\\text{Bias}^2}(x_0)
        = \\left(\\frac{1}{B}\\sum_{b=1}^{B} \\hat{f}_b(x_0) - f(x_0)\\right)^2$$"
    ),
    helpText(
      "$$\\widehat{\\text{Var}}(x_0)
        = \\frac{1}{B}\\sum_{b=1}^{B}
          \\left(\\hat{f}_b(x_0) - \\bar{\\hat{f}}(x_0)\\right)^2$$"
    )
  )
}

# Compact equation label used next to a plot/table output
latex_note <- function(text_before = NULL, eq = "", text_after = NULL) {
  tagList(
    if (!is.null(text_before)) p(text_before),
    helpText(eq),
    if (!is.null(text_after)) p(text_after)
  )
}