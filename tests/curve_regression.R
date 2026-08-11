# Launcher for the curve-archive regression suite (see curves/README.md).
#
# Runs tests/curve_regression.py with the Python environment bystools already
# uses, so the suite can be started from R without locating the interpreter.
#
# Usage:  source("tests/curve_regression.R")            check every figure
#         source("tests/curve_regression.R"); km2_check_curves("titan")
#         Rscript tests/curve_regression.R [extra python arguments]

source("R/km2_setup_python.R")

km2_check_curves <- function(filter = NULL, args = character()) {
  py <- Sys.getenv("KM2_PY", "")
  if (!nzchar(py) || !file.exists(py)) py <- .km2_env_python("r-bystools")
  if (is.null(py) || !file.exists(py)) {
    stop("No Python environment found. Run bystools::km2_setup_python(), or set ",
         "KM2_PY to an interpreter that has numpy, cv2, pytesseract and sklearn.",
         call. = FALSE)
  }
  script <- file.path("tests", "curve_regression.py")
  if (!file.exists(script)) {
    stop("Run this from the repository root: ", script, " not found.", call. = FALSE)
  }
  if (!is.null(filter)) args <- c(args, "-k", filter)
  status <- system2(py, shQuote(c(script, args)))
  invisible(status)
}

if (!interactive()) {
  extra <- commandArgs(trailingOnly = TRUE)
  quit(status = km2_check_curves(args = extra))
}
