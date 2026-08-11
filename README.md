<p align="center">
  <img src="https://img.shields.io/badge/R-%3E%3D%204.0-blue?logo=r&logoColor=white" alt="R">
  <img src="https://img.shields.io/badge/Shiny-interactive-orange?logo=r&logoColor=white" alt="Shiny">
  <img src="https://img.shields.io/badge/Stan-Bayesian-red?logo=data:image/svg+xml;base64,..." alt="Stan">
  <img src="https://img.shields.io/badge/license-MIT-green" alt="License">
  <img src="https://img.shields.io/github/v/release/albertocarm/bystools?include_prereleases&label=version" alt="Version">
</p>

# bystools

**From Kaplan-Meier Plots to Bayesian Cure Models**

> Interactive R/Shiny toolkit that digitizes published Kaplan-Meier figures, reconstructs individual patient data, evaluates data stability, and fits Bayesian mixture cure models — all in a single session.

---

## Overview

**bystools** provides a reproducible workflow for oncology researchers who need to:

- **Digitize** published Kaplan-Meier curves and reconstruct individual patient data (IPD)
- **Evaluate** the stability and maturity of reconstructed survival data through forensic metrics
- **Fit** Bayesian Weibull mixture cure models with informative or historical priors via Stan
- **Diagnose** model convergence and inspect posterior distributions interactively

All steps run inside a single Shiny application with point-and-click interaction — no command-line expertise required.

---

## Pipeline

```
┌─────────────────────────┐     ┌─────────────────────────┐     ┌─────────────────────────┐
│  1. DATA EXTRACTION     │     │  2. STABILITY METRICS   │     │  3. BAYESIAN MODEL      │
│                         │     │                         │     │                         │
│  KM plot upload         │     │  Instability checks     │     │  Weibull mixture cure   │
│  Image preprocessing    │────▶│  Maturity index         │────▶│  Historical priors      │
│  Point-and-click dig.   │     │  Pearson correlation    │     │  Posterior densities    │
│  Risk table editing     │     │  AIC (shared vs free)   │     │  Correlated densities   │
│  IPD reconstruction     │     │  Calibration plot       │     │  MCMC diagnostics       │
│  Cox validation         │     │  Decision scenarios     │     │  Reproducible R code    │
└─────────────────────────┘     └─────────────────────────┘     └─────────────────────────┘
```

---

## Installation

bystools has two parts: the **R package**, and — only for the **one-click automatic digitizer** — a small **Python + Tesseract OCR** engine. Everything else (point-and-click digitization, LLM preprocessing, manual entry, all the modelling) works without the engine.

### Step 1 — Install the R package

```r
# install.packages("devtools")
devtools::install_github("albertocarm/bystools", force = TRUE, upgrade = "never")
```

The R dependencies are resolved automatically, including two packages from GitHub:

| Package | Source | Purpose |
|---------|--------|---------|
| `bayescores` | [albertocarm/BayeScores](https://github.com/albertocarm/BayeScores) | Bayesian cure model fitting |
| `SurvdigitizeR` | [Pechli-Lab/SurvdigitizeR](https://github.com/Pechli-Lab/SurvdigitizeR) | KM curve digitization engine |

All CRAN dependencies (`rstan`, `survival`, `flexsurvcure`, `shiny`, `bslib`, `magick`, `reticulate`, ...) are listed in `DESCRIPTION` and installed automatically.

### Step 2 — Install the automatic-digitizer engine (optional)

Only needed for the one-click *Auto-digitize* feature. The easiest path is a single command, then **restart R**:

```r
bystools::km2_install_all()   # R deps + Python environment + Tesseract OCR
```

`km2_install_all()` is fault-tolerant: each piece is tried with fallbacks and it prints a summary of what succeeded. If you'd rather install the pieces one by one, there are two granular helpers:

| Function | What it installs | When to use |
|----------|------------------|-------------|
| `km2_install_all()` | **Everything**: R dependencies, the Python environment, and the Tesseract OCR engine | **Recommended** — run once, then restart R |
| `km2_setup_python()` | Only the isolated Python environment `r-bystools` (`numpy`, `opencv`, `pytesseract`, `scikit-learn`) | If only the Python side is missing |
| `km2_install_tesseract()` | Only the Tesseract OCR engine (on Windows it downloads and launches the official installer) | If only the OCR engine is missing |

> You don't have to memorise these: if the automatic digitizer can't find the engine, the app shows in-app instructions with exactly the command(s) you need, listing only what is missing.

### If the installer fails — manual installation

`km2_install_all()` covers the usual setups, but corporate machines, restricted networks and unusual Python installations can defeat it. The engine is just **Python 3 with four libraries** plus **Tesseract OCR**, so it can always be installed by hand.

**1. Python libraries.** From R, into an isolated environment:

```r
reticulate::virtualenv_create("r-bystools")
reticulate::virtualenv_install(
  "r-bystools",
  packages = c("numpy", "opencv-python-headless", "pytesseract", "scikit-learn")
)
```

If `virtualenv_create()` fails because no base Python is found, install one with `reticulate::install_python()` and repeat. If virtualenvs are unavailable altogether, conda works too:

```r
reticulate::conda_create("r-bystools")
reticulate::conda_install(
  "r-bystools",
  packages = c("numpy", "opencv-python-headless", "pytesseract", "scikit-learn"),
  pip = TRUE
)
```

You can equally use a Python installation you already have — install the same four packages with `pip` in a terminal, and bystools will pick it up:

```
pip install numpy opencv-python-headless pytesseract scikit-learn
```

**2. Tesseract OCR** (a normal desktop application, not an R package):

| Platform | Command / action |
|----------|------------------|
| Windows | Download `tesseract-ocr-w64-setup-5.x.x.exe` from the [University of Mannheim builds](https://digi.bib.uni-mannheim.de/tesseract/) and run it with default settings. Keep the default folder — bystools finds it automatically, so you never need to edit `PATH`. |
| macOS | `brew install tesseract` |
| Linux (Debian/Ubuntu) | `sudo apt-get install tesseract-ocr` |

**3. Restart R** and open the app. To confirm the engine is visible:

```r
reticulate::use_virtualenv("r-bystools", required = FALSE)
reticulate::py_module_available("cv2")   # TRUE when the Python side is ready
Sys.which("tesseract")                   # non-empty when the OCR engine is on PATH
```

Even with no engine at all, the app remains fully usable: point-and-click digitization, LLM preprocessing, manual data entry and every modelling module work without Python or Tesseract.

---

## Reporting figures that fail to digitize

The automatic digitizer is tuned on real published figures, and each new plotting style teaches it something. If a Kaplan-Meier figure is digitized poorly — a missing arm, a trace that drifts off the curve, a numbers-at-risk table read incorrectly — please **send us the figure**.

Open an [issue](https://github.com/albertocarm/bystools/issues) with the image attached, or email it to [carmonab@um.es](mailto:carmonab@um.es). Please include the source (journal, article, figure number) where possible.

Reported figures go into the **curve archive** in [`curves/`](curves/), the regression suite for the digitizer. Each figure gets a record holding its source, its checksum and the result the engine is expected to produce, so a fix made for one figure is checked against every figure that already worked:

```
python tests/curve_regression.py     # or, from R:  source("tests/curve_regression.R")
```

A figure the engine cannot yet handle is recorded as a `known-failure`: it is replayed on every run and the suite reports the day it starts working, so nothing that was reported is quietly lost. The records are versioned; the published figures themselves are held locally rather than redistributed, and a record whose image is absent is skipped. See [`curves/README.md`](curves/README.md) for how the archive is organised and how to add to it.

---

## Quick Start

```r
library(bystools)
km2bayesPro()   # auto-digitize -> review numbers at risk -> Confirm & Analyze
```

`km2bayesPro()` is the recommended app: a guided two-arm workflow that auto-digitizes the figure, asks you to confirm (or edit) the numbers at risk, then runs the full analysis. `km2bayes()` is the classic step-by-step variant. Both open in your default browser.

---

## Modules in Detail

### 1. Data Extraction (IPD Reconstruction)

Upload, paste (Ctrl+V / drag-and-drop), or load an existing dataset, then extract patient-level data:

- **Automatic digitization** — one click extracts the two survival curves and the numbers-at-risk table; a confirmation banner lets you review and edit the grid before analysis
- **LLM preprocessing** — optional prompt to clean a figure with any vision LLM (fallback)
- **Point-and-click digitization** — manual curve extraction via `SurvdigitizeR` (fallback)
- **Risk table editing** — interactive number-at-risk tables with `rhandsontable`
- **IPD reconstruction** — recover individual time-to-event data from digitized curves
- **Validation** — overlay reconstructed Kaplan-Meier curves and Cox model summaries; download a side-by-side comparison PNG (original vs reconstructed, with Cox HR/CI/p)
- **Export** — download reconstructed IPD as Excel or R data

### 2. Stability Metrics (Forensic Analysis)

Assess the reliability and maturity of the reconstructed data before modeling:

- **Instability checks** — sample sizes, events, censoring counts
- **Pearson correlation** — parametric stability of the survival function
- **Maturity index** — ratio of follow-up to survival time
- **MAE & MAE ratio** — mean absolute error per arm and between arms
- **AIC comparison** — shared-shape vs free-shape Weibull mixture cure models (`flexsurvcure`)
- **Calibration plot** — visual comparison of Kaplan-Meier curve vs mixture cure fit
- **Decision scenarios** — automated interpretation suggestions based on stability thresholds

### 3. Bayesian Model

Fit Bayesian Weibull mixture cure models to the reconstructed IPD:

- **Configurable MCMC** — iterations, chains, warmup, shared/free shape
- **Tail assumptions** — neutral, immature skeptical, biologically null, supportive, optimistic
- **Historical priors** — optional informative priors with user-defined mean and SD
- **Posterior densities** — visualize marginal parameter distributions
- **Correlated densities** — inspect pairwise parameter correlations
- **Model fit plot** — Bayesian survival prediction overlaid on Kaplan-Meier data
- **Diagnostics** — convergence table (Rhat, effective sample size) and diagnostic plots
- **Reproducible R code** — auto-generated script to replicate every analysis step
- **Save/load models** — export and import fitted models as `.rds` files
- **MCMC draws export** — download posterior samples as CSV

---

## Tech Stack

| Layer | Technologies |
|-------|-------------|
| **Interface** | Shiny, bslib, DT, rhandsontable |
| **Bayesian engine** | rstan (Stan/C++) |
| **Survival analysis** | survival, flexsurv, flexsurvcure |
| **Image processing** | magick, SurvdigitizeR |
| **Visualization** | ggplot2, survminer |
| **Data wrangling** | dplyr, purrr, tibble, stringr, readxl, writexl |

---

## References

This tool builds on the following methods and packages:

> Zhang J, Cunningham R, Hagmann C, et al. **SurvdigitizeR: an algorithm for automated survival curve digitization.** *BMC Medical Research Methodology* 24.1 (2024): 147.

> Liu N, Zhou Y, Lee JJ. **IPDfromKM: reconstruct individual patient data from published Kaplan-Meier survival curves.** *BMC Medical Research Methodology* 21.1 (2021): 111.

> Guyot P, Ades AE, Ouwens MJNM, Welton NJ. **Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves.** *BMC Medical Research Methodology* 12.1 (2012): 9.

---

## Citation

If you use **bystools** in your research, please cite:

```
Carmona-Bayonas A (2025). bystools: Interactive Shiny Application for
Bayesian Survival Analysis Pipeline. R package version 0.2.0.
https://github.com/albertocarm/bystools
```

---

## License

MIT License — see [LICENSE](LICENSE) for details.

---

## Author

**Alberto Carmona Bayonas**
ORCID [0000-0002-1930-9660](https://orcid.org/0000-0002-1930-9660) · [carmonab@um.es](mailto:carmonab@um.es)
