#' One-shot, fault-tolerant installation of every dependency
#'
#' @description
#' Installs **everything** `bystools` needs in a single run: the R package
#' dependencies, the Python environment for the automatic digitizer, and the
#' Tesseract OCR engine. Each step is isolated and tried with several fallback
#' methods, so a failure in one component does not abort the rest. A summary is
#' printed at the end listing what succeeded and what (if anything) still needs
#' attention.
#'
#' R packages are installed with this cascade: skip if already present ->
#' `install.packages()` (CRAN) -> `remotes::install_github()` (for the two GitHub
#' packages) -> `pak::pkg_install()` as a last resort.
#'
#' Python is set up via [km2_setup_python()] (virtualenv -> standalone Python ->
#' conda fallback) and Tesseract via [km2_install_tesseract()].
#'
#' @param r_packages Logical; install the R dependencies. Default `TRUE`.
#' @param python Logical; set up the Python environment. Default `TRUE`.
#' @param tesseract Logical; install the Tesseract OCR engine. Default `TRUE`.
#' @param tesseract_silent Logical; unattended Tesseract install (no GUI). Default `TRUE`.
#' @param envname Python environment name. Default `"r-bystools"`.
#'
#' @return (Invisibly) a named character vector: the status of each step.
#'
#' @examples
#' \dontrun{
#'   bystools::km2_install_all()   # run once, then restart R
#' }
#'
#' @seealso [km2_setup_python()], [km2_install_tesseract()], [km2bayes()]
#' @export
km2_install_all <- function(r_packages = TRUE,
                            python = TRUE,
                            tesseract = TRUE,
                            tesseract_silent = TRUE,
                            envname = "r-bystools") {

  status <- character(0)
  record <- function(name, value) { status[[name]] <<- value; invisible() }

  # ----- robust single-package installer (CRAN -> GitHub -> pak) -----
  ensure_pkg <- function(pkg, github = NULL) {
    if (requireNamespace(pkg, quietly = TRUE)) return("already installed")
    # 1. CRAN
    try(utils::install.packages(pkg), silent = TRUE)
    if (requireNamespace(pkg, quietly = TRUE)) return("CRAN")
    # 2. GitHub via remotes
    if (!is.null(github)) {
      if (!requireNamespace("remotes", quietly = TRUE)) try(utils::install.packages("remotes"), silent = TRUE)
      if (requireNamespace("remotes", quietly = TRUE)) {
        try(remotes::install_github(github, upgrade = "never"), silent = TRUE)
        if (requireNamespace(pkg, quietly = TRUE)) return("GitHub (remotes)")
      }
    }
    # 3. pak last resort
    if (!requireNamespace("pak", quietly = TRUE)) try(utils::install.packages("pak"), silent = TRUE)
    if (requireNamespace("pak", quietly = TRUE)) {
      target <- if (!is.null(github)) github else pkg
      try(pak::pkg_install(target, ask = FALSE), silent = TRUE)
      if (requireNamespace(pkg, quietly = TRUE)) return("pak")
    }
    "FAILED"
  }

  # ----- 1. R dependencies -----
  if (r_packages) {
    message("=== Installing R dependencies ===")
    cran_pkgs <- c("bslib", "dplyr", "DT", "flexsurv", "flexsurvcure", "future",
                   "ggplot2", "here", "magick", "purrr", "readxl", "rhandsontable",
                   "rstan", "shiny", "stringr", "survival", "survminer", "tibble",
                   "writexl", "reticulate")
    gh_pkgs <- list(SurvdigitizeR = "Pechli-Lab/SurvdigitizeR",
                    bayescores    = "albertocarm/BayeScores")

    for (p in cran_pkgs) {
      res <- tryCatch(ensure_pkg(p), error = function(e) paste("FAILED:", conditionMessage(e)))
      message(sprintf("  %-14s %s", p, res)); record(p, res)
    }
    for (p in names(gh_pkgs)) {
      res <- tryCatch(ensure_pkg(p, github = gh_pkgs[[p]]), error = function(e) paste("FAILED:", conditionMessage(e)))
      message(sprintf("  %-14s %s", p, res)); record(p, res)
    }
  }

  # ----- 2. Python environment -----
  if (python) {
    message("\n=== Setting up Python environment '", envname, "' ===")
    res <- tryCatch({ km2_setup_python(envname = envname); "OK" },
                    error = function(e) paste("FAILED:", conditionMessage(e)))
    message("  python-env     ", res); record("python_env", res)
  }

  # ----- 3. Tesseract OCR engine -----
  if (tesseract) {
    message("\n=== Installing Tesseract OCR engine ===")
    res <- tryCatch({ km2_install_tesseract(silent = tesseract_silent); "OK (launched/installed)" },
                    error = function(e) paste("FAILED:", conditionMessage(e)))
    message("  tesseract      ", res); record("tesseract", res)
  }

  # ----- Summary -----
  message("\n========== SETUP SUMMARY ==========")
  failed <- names(status)[grepl("FAILED", status)]
  for (nm in names(status)) message(sprintf("  %-14s %s", nm, status[[nm]]))
  if (length(failed)) {
    message("\nSome steps need attention: ", paste(failed, collapse = ", "),
            ". Re-run km2_install_all() or install those manually (see ?km2_install_all).")
  } else {
    message("\nAll set. Restart R and run  km2bayes()  .")
  }
  invisible(status)
}
