## AKMProject v0.1.1 — Patch Release

Patch release that removes internal `grade/` and `Submissions/` directories that were accidentally included in v0.1.0.

Everything else is unchanged from v0.1.0.

🔗 **[Live Demo on Posit Connect Cloud](https://019cea29-16b6-3560-2cce-56feb47d4a51.share.connect.posit.cloud/)**

### Installation

    # install.packages("devtools")
    devtools::install_github("Euswbnix/AKMproject@v0.1.1")

### Features

**Core simulation toolkit** (`R/function.R`)

- `true_function()` — smooth or wiggly ground-truth regression functions
- `generate_data()` — synthetic regression data with Gaussian noise
- `fit_polynomial()` — least-squares polynomial regression of arbitrary degree
- `compute_mse()` — mean squared error against held-out data
- `monte_carlo_bias_variance()` — estimates squared bias and variance via Monte Carlo replication

**Interactive Shiny app** (`shiny-app/`)

- Side-by-side comparison of **Polynomial Regression** and **k-Nearest Neighbors**
- Dynamic `plotly` charts with hoverable MSE / Bias² / Variance decomposition
- Automatic best-complexity detection with text-based recommendations
- Prediction spread visualization across 100+ Monte Carlo fits
- CSV export of raw pointwise simulation data
- Loading spinners via `shinycssloaders` for long-running simulations

### Package status

- `devtools::check()` passes with **0 errors / 0 warnings** on Windows 11, macOS, and Ubuntu 24.04 LTS
- Full reproducible environment via `manifest.json` and `DESCRIPTION`
- Generative AI usage statement included at `vignettes/ai_usage_statement.Rmd`

### Contributors

Alex Huang, Jizheng Huang, Kerry Zhu, Morrison Xu, Tianchen Ni, Anna Huynh, Victor Jiang.

---
*Created for the University of Toronto STA380 Final Project.*
