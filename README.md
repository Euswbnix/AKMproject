# Bias-Variance Tradeoff Explorer (AKMproject)

Current project progress: **Final Implementation Fully Deployed!** 

🔗 **[Live Demo: View our Shiny App on Posit Connect Cloud](https://019cea29-16b6-3560-2cce-56feb47d4a51.share.connect.posit.cloud/)**

---

## Project Overview
This R package and interactive Shiny application provide a comprehensive toolkit for exploring the **Bias-Variance Tradeoff** through Monte Carlo simulations. It allows users to visualize how model complexity (Polynomial Degree or k-NN) affects error decomposition in real-time.

## ✨ Key Features (Final Milestone)

### 1. Interactive Error Decomposition
* **Dynamic Hover Charts:** Built with `plotly`, allowing users to see precise MSE, Bias², and Variance values by hovering over the curves.
* **Model Selection:** Compare **Polynomial Regression** and **k-Nearest Neighbors (k-NN)** models side-by-side.

### 2. Intelligent Model Recommendations
* **Best Complexity Detection:** The app automatically identifies the optimal complexity (e.g., "Degree 5") that minimizes Test MSE.
* **Automated Feedback:** Provides clear, text-based guidance on the recommended model and minimum test error achieved.

### 3. Advanced Monte Carlo Visualizations
* **Prediction Spread:** Visualizes model stability by plotting 100+ individual MC fitted curves against the true function.
* **Smooth UX:** Integrated `shinycssloaders` to provide visual feedback (spinners) during heavy simulation computations.

### 4. Data Transparency
* **Exportable Results:** Users can download the raw pointwise simulation data as a `.csv` for further independent analysis.

---

## 🛠️ Package Status & Compatibility
* **Cross-Platform Tested:** `devtools::check()` passed with **0 Errors / 0 Warnings** on Windows 11, macOS, and Ubuntu 24.04 LTS.
* **Dependency Management:** Full environment reproducibility via `manifest.json` and a robust `DESCRIPTION` file.

##
In accordance with the course requirements, a detailed **Generative AI Usage Statement** is included in the package vignettes.
* **Vignette Path:** `vignettes/ai_usage_statement.Rmd`
* **Final Update:** Includes documentation on refactoring visualizations to `plotly` and implementing asynchronous UI feedback.

---
*Created for the University of Toronto - STA380 Final Project.*