
# bystools: Full Bayesian Pipeline for Survival & Toxicity

**bystools** is an interactive R/Shiny package designed to streamline the end-to-end execution of the Bayescores methodology. It provides a user-friendly interface for reconstructing individual patient data (IPD) from published Kaplan-Meier plots, running Bayesian cure models via Stan, and synthesizing efficacy with toxicity/QoL data.

## Key Features

The application is structured into four sequential modules:

1. **Data Reconstruction (IPD):**
   * Automated digitization of Kaplan-Meier plots and Number at Risk tables using OCR (tesseract) and image processing (magick).
   * Reconstruction of patient-level data from digitized coordinates.
   * Instant validation with Cox models and survival plots.

2. **Bayesian Modeling:**
   * Fits Bayesian Cure Models (Weibull mixture cure models) using rstan.
   * Supports historical priors and expert elicitation for cure fractions.
   * Provides full MCMC diagnostics (traceplots, posterior densities) and model summaries.

3. **Safety & Quality of Life (QoL):**
   * Structured entry for Adverse Events (AEs) mapped to System Organ Classes (SOC).
   * Interactive definition of Quality of Life scenarios and utility weights.

4. **Final Synthesis:**
   * Calculates the final Bayescore by integrating efficacy (cure/treatment probabilities) with safety penalties.
   * Generates calibrated utility density plots and relative contribution visualizations.
   * Produces reproducible R code for every step of the pipeline.

## Installation

You can install the development version of bystools directly from GitHub.

devtools::install_github("albertocarm/bystools")

## Prerequisites: Since this package uses rstan (for Bayesian modeling) and tesseract (for OCR), ensure you have the necessary system tools installed:

Windows: Install Rtools (https://cran.r-project.org/bin/windows/Rtools/).

macOS: Install Xcode command line tools.

## Usage
To launch the interactive application, simply load the library and run the main function:

library(bystools)

# Launch the dashboard
km2bayes()
