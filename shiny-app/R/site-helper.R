# site-helper.R
# Shared constants and small utility functions used across UI files.

# ── App Metadata ────────────────────────────────────────────────────────────
APP_TITLE  <- "Bias–Variance Tradeoff Explorer"
APP_AUTHOR <- "Huang · Jiang · Xu · Zhu — UTM STA380 Winter 2026"

# ── Number of Monte Carlo repetitions (shared between UI hint and server) ──
MC_REPS_DEFAULT <- 200
MC_REPS_MIN     <- 50
MC_REPS_MAX     <- 1000

# ── Colour palette (shared between UI cards and server ggplot themes) ───────
PALETTE <- list(
  train    = "#0275d8",   # blue  — training MSE
  test     = "#d9534f",   # red   — test MSE
  bias2    = "#5cb85c",   # green — bias squared
  variance = "#f0ad4e",   # amber — variance
  noise    = "#9b59b6"    # purple — irreducible noise
)

# ── Helpers ─────────────────────────────────────────────────────────────────

# Wrap any UI element in a styled section box
section_box <- function(..., title = NULL) {
  div(
    class = "section-box",
    style = "border: 1px solid #dee2e6; border-radius: 4px;
             padding: 16px; margin-bottom: 20px; background: #fff;",
    if (!is.null(title)) h5(style = "margin-top: 0;", title),
    ...
  )
}

# Horizontal rule with optional label
labelled_hr <- function(label = NULL) {
  if (is.null(label)) return(hr())
  div(
    style = "display: flex; align-items: center; margin: 12px 0;",
    div(style = "flex: 1; height: 1px; background: #dee2e6;"),
    span(style = "padding: 0 10px; color: #6c757d; font-size: 0.8em;
                  font-weight: 600; text-transform: uppercase;", label),
    div(style = "flex: 1; height: 1px; background: #dee2e6;")
  )
}

# Return a spinner placeholder (used before server output is ready)
loading_placeholder <- function(height = "300px") {
  div(
    style = paste0("height: ", height, "; display: flex; align-items: center;
                    justify-content: center; color: #6c757d; border: 1px dashed #dee2e6;
                    border-radius: 4px;"),
    icon("spinner", class = "fa-spin"),
    span(style = "margin-left: 8px;", "Waiting for simulation...")
  )
}