#' Set up the Python environment used by the automatic KM digitizer
#'
#' @description
#' Sets up the Python libraries required by the automatic image pre-processing of
#' [km2bayes()] (the bundled `kmdig.py` engine). It creates a dedicated, isolated
#' environment (default `"r-bystools"`) and installs `numpy`, `opencv-python-headless`,
#' `pytesseract` and `scikit-learn`.
#'
#' Installation is attempted in this order:
#' \enumerate{
#'   \item a virtualenv built on an existing base Python;
#'   \item if no base Python is found, a standalone Python is installed
#'     ([reticulate::install_python()]) and the virtualenv is retried;
#'   \item if the virtualenv route fails, a conda environment is used
#'     (Miniconda is installed first if needed).
#' }
#'
#' The external Tesseract OCR engine is installed separately with [km2_install_tesseract()].
#'
#' @param envname Name of the environment to create/use. Default `"r-bystools"`.
#' @param ... Additional arguments forwarded to the installer.
#'
#' @return (Invisibly) `TRUE` on success. Restart R afterwards, then run [km2bayes()].
#'
#' @examples
#' \dontrun{
#'   bystools::km2_setup_python()   # run once; then restart R
#' }
#'
#' @seealso [km2_install_tesseract()], [km2_install_all()], [km2bayes()]
#' @export
km2_setup_python <- function(envname = "r-bystools", ...) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    message("Installing reticulate ...")
    try(utils::install.packages("reticulate"), silent = TRUE)
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("Could not install reticulate. Run install.packages('reticulate') manually.", call. = FALSE)
    }
  }
  # headless build: provides cv2 without GUI libraries (imports on headless Linux).
  pkgs <- c("numpy", "opencv-python-headless", "pytesseract", "scikit-learn")

  # virtualenv (installing a standalone Python if none is found)
  ok <- tryCatch({
    if (!reticulate::virtualenv_exists(envname)) {
      base <- tryCatch(reticulate::virtualenv_starter(), error = function(e) NULL)
      if (is.null(base) || !length(base)) {
        message("No base Python found - installing a standalone Python (one-time, ~25 MB) ...")
        reticulate::install_python()
      }
      message("Creating virtual environment '", envname, "' ...")
      reticulate::virtualenv_create(envname)
    }
    message("Installing Python libraries into '", envname, "' (virtualenv) ...")
    reticulate::virtualenv_install(envname, packages = pkgs, ignore_installed = TRUE, ...)
    TRUE
  }, error = function(e) {
    message("Virtualenv route failed: ", conditionMessage(e))
    FALSE
  })

  # conda fallback
  if (!ok) {
    message("Falling back to a conda environment ...")
    ok <- tryCatch({
      has_conda <- !is.null(tryCatch(reticulate::conda_binary(), error = function(e) NULL))
      if (!has_conda) {
        message("No conda found - installing Miniconda (one-time) ...")
        reticulate::install_miniconda()
      }
      if (!(envname %in% tryCatch(reticulate::conda_list()$name, error = function(e) character()))) {
        reticulate::conda_create(envname)
      }
      reticulate::conda_install(envname, packages = pkgs, pip = TRUE)
      TRUE
    }, error = function(e) {
      message("Conda route failed: ", conditionMessage(e))
      FALSE
    })
  }

  # Verify by running the environment's own python.
  if (!.km2_env_ready(envname)) {
    if (ok) {
      warning("Packages were installed into '", envname,
              "' but verification via its python could not confirm all modules. ",
              "Restart R and try again; the install itself reported success.",
              call. = FALSE)
    } else {
      stop("Python setup did not complete. See messages above; you can also install ",
           "manually with reticulate::py_install(c('numpy','opencv-python','pytesseract','scikit-learn'), ",
           "pip = TRUE, ignore_installed = TRUE).", call. = FALSE)
    }
  }

  message("\nPython environment '", envname, "' is ready (numpy, cv2, pytesseract, sklearn).")
  message("Next: install the Tesseract OCR engine (km2_install_tesseract()), restart R, run km2bayes().")
  invisible(TRUE)
}

# --------------------------------------------------------------------------- #
# Internal helpers: inspect an environment by running its own python binary,
# independent of the Python the R session is bound to.
# --------------------------------------------------------------------------- #

#' @keywords internal
#' @noRd
.km2_env_python <- function(envname = "r-bystools") {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(NULL)
  py <- tryCatch(reticulate::virtualenv_python(envname), error = function(e) NULL)
  if (!is.null(py) && file.exists(py)) return(py)
  py <- tryCatch(reticulate::conda_python(envname), error = function(e) NULL)
  if (!is.null(py) && file.exists(py)) return(py)
  NULL
}

#' @keywords internal
#' @noRd
.km2_env_ready <- function(envname = "r-bystools") {
  py <- .km2_env_python(envname)
  if (is.null(py)) return(FALSE)
  code <- tryCatch(
    system2(py, c("-c", shQuote("import numpy, cv2, pytesseract, sklearn")),
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  )
  identical(as.integer(code), 0L)
}

# --------------------------------------------------------------------------- #
# Axis grid inference: turn noisy OCR tick values into a regular grid.
# --------------------------------------------------------------------------- #

#' @keywords internal
#' @noRd
.km2_nice_step <- function(raw) {
  if (!is.finite(raw) || raw <= 0) return(raw)
  mag  <- 10^floor(log10(raw))
  cand <- c(1, 2, 2.5, 5, 10) * mag
  cand[which.min(abs(cand - raw))]
}

# Clean tiny OCR jitter off an otherwise-real step: if the spacing is within ~8%
# of an integer, take that integer (so 5.97 -> 6); otherwise keep it as measured
# (so fractional axes such as 0.5-year steps survive).
.km2_snap_step <- function(x) {
  if (!is.finite(x) || x <= 0) return(x)
  r <- round(x)
  if (r > 0 && abs(x - r) <= 0.08 * x) r else x
}

#' @keywords internal
#' @noRd
.km2_infer_axis <- function(ticks, zero_start = FALSE, n = NULL) {
  ticks <- sort(unique(ticks[is.finite(ticks)]))
  if (length(ticks) < 2) return(NULL)
  d <- diff(ticks); d <- d[d > 0]
  if (!length(d)) return(NULL)
  md <- stats::median(d)
  # Trust the detected tick spacing when the ticks are evenly spaced (the normal
  # case): this preserves real but non-"nice" steps such as 3 or 6 months, which a
  # 1/2/2.5/5/10 snap would corrupt (e.g. 6 -> 5). Only fall back to a "nice" step
  # when the spacing is irregular, i.e. OCR likely dropped or misread a tick.
  even <- length(d) == 1 || stats::sd(d) <= 0.15 * md
  step <- if (even) .km2_snap_step(md) else .km2_nice_step(md)
  lo <- min(ticks); hi <- max(ticks)
  if (zero_start && lo <= step * 1.5) lo <- 0
  lo <- round(lo / step) * step
  hi <- round(hi / step) * step
  if (!is.null(n) && n >= 2) hi <- lo + step * (n - 1)  # column count drives the end
  if (hi <= lo) return(NULL)
  list(start = lo, end = hi, inc = step, seq = seq(lo, hi, by = step))
}

#' Build the editable numbers-at-risk table from a digitizer meta object.
#'
#' Pure (no Shiny) so it can be unit-tested outside the app. Selects the two real
#' arm rows, infers the time grid from their own calibrated x-positions, and pads
#' to a common length. Returns a list with `table` (data.frame Time/N_Risk_G1/
#' N_Risk_G2), `ax` (the inferred axis), and `n_rows`, or `NULL`.
#'
#' @keywords internal
#' @noRd
.km2_build_risk_table <- function(meta, ax_x = NULL) {
  ar <- meta$at_risk
  if (is.null(ar) || length(ar) < 1) return(NULL)
  rows <- lapply(ar, function(r) list(
    values = suppressWarnings(as.numeric(unlist(r$values))),
    times  = tryCatch(suppressWarnings(as.numeric(unlist(r$times))), error = function(e) NULL),
    y      = tryCatch(as.numeric(unlist(r$y))[1], error = function(e) NA_real_)
  ))
  valid <- function(v) {
    v <- v[!is.na(v)]
    length(v) >= 4 && v[1] > 0 && all(diff(v) <= 0) && (max(v) - min(v) > 0)
  }
  rows <- Filter(function(r) valid(r$values), rows)
  if (length(rows) == 0) return(NULL)
  # Keep the two real arms; drop stray rows (e.g. c(11,0,0,...) read from the plot
  # area) by ranking on column count first, then cohort size.
  if (length(rows) > 2) {
    ord <- order(
      -vapply(rows, function(r) sum(!is.na(r$values)), integer(1)),
      -vapply(rows, function(r) suppressWarnings(max(r$values, na.rm = TRUE)), numeric(1))
    )
    rows <- rows[ord][1:2]
  }
  # Drop a clearly-truncated stray (e.g. c(55,1,1,1)) when the other arm is much
  # longer: better to leave that arm empty (red cells to fill) than to seat junk.
  if (length(rows) == 2) {
    lens <- vapply(rows, function(r) sum(!is.na(r$values)), integer(1))
    if (min(lens) < 0.4 * max(lens)) rows <- rows[which.max(lens)]
  }
  rows <- rows[order(vapply(rows, function(r) r$y, numeric(1)))]
  n_rows <- length(rows)
  g1 <- rows[[1]]$values
  g2 <- if (n_rows >= 2) rows[[2]]$values else NA_real_
  n  <- max(length(g1), length(g2))

  # Time grid: the at-risk columns are a regular, integer-stepped sequence. Take the
  # cadence (start + step) from the OCR'd axis-tick VALUES (ax_x), which are correct
  # even when the pixel-calibration slope is off -- relying on the slope-derived
  # times would otherwise give decimal junk like 8.1 by 2.7. Use the column count for
  # the length. Fall back to the at-risk numbers' own positions only if no tick grid.
  longest <- rows[[ which.max(vapply(rows, function(r) length(r$values), integer(1))) ]]
  ax_grid <- ax_x
  if (is.null(ax_grid) && !is.null(longest$times) && sum(is.finite(longest$times)) >= 2)
    ax_grid <- .km2_infer_axis(longest$times, zero_start = TRUE, n = n)
  tt <- if (!is.null(ax_grid)) seq(ax_grid$start, by = ax_grid$inc, length.out = n) else rep(NA_real_, n)

  pad  <- function(x, k) { x <- as.numeric(x); c(x, rep(NA_real_, max(0, k - length(x)))) }
  ipad <- function(x, k) as.integer(round(pad(x, k)))
  list(
    table = data.frame(Time = pad(tt, n), N_Risk_G1 = ipad(g1, n), N_Risk_G2 = ipad(g2, n)),
    ax = ax_grid, n_rows = n_rows
  )
}

#' Robust SurvdigitizeR call: retry with tweaked image parameters until it works.
#'
#' SurvdigitizeR can fail on a particular rendering (e.g. "Step 4: Clustering
#' colors" or "Step 8: Summarizing data"). We retry the same digitization with a
#' few background-threshold / contrast / brightness variants -- empirically this
#' recovers figures that fail on the default settings -- and return the first
#' successful result, or raise the last error.
#'
#' @keywords internal
#' @noRd
.km2_survdigitize_robust <- function(img_path, x_start, x_end, x_increment,
                                     y_start, y_end, y_increment, num_curves = 2,
                                     censoring = FALSE, bg_lightness = 0.3,
                                     enhance = FALSE, y_text_vertical = FALSE) {
  attempts <- list(
    list(bg = bg_lightness, enh = enhance, ct = 1, br = 100),   # the requested settings
    list(bg = 0.20, enh = FALSE, ct = 1, br = 100),             # lower background threshold
    list(bg = 0.30, enh = FALSE, ct = 2, br = 110),             # boost contrast + brightness
    list(bg = 0.50, enh = FALSE, ct = 1, br = 100),
    list(bg = 0.30, enh = TRUE,  ct = 1, br = 100),
    list(bg = 0.20, enh = FALSE, ct = 2, br = 110),
    list(bg = 0.40, enh = FALSE, ct = 3, br = 110),
    list(bg = 0.25, enh = FALSE, ct = 2, br = 90)
  )
  prep <- function(ct, br) {                                   # contrast/brightness via magick
    if (ct == 1 && br == 100) return(img_path)
    im <- magick::image_read(img_path)
    if (br != 100) im <- magick::image_modulate(im, brightness = br)
    if (ct > 1) for (i in seq_len(ct - 1)) im <- magick::image_contrast(im)
    p <- tempfile(fileext = ".png"); magick::image_write(im, p); p
  }
  last_err <- NULL
  for (a in attempts) {
    res <- tryCatch(
      SurvdigitizeR::survival_digitize(
        img_path = prep(a$ct, a$br), bg_lightness = a$bg, attempt_OCR = FALSE,
        num_curves = num_curves, censoring = censoring,
        x_start = x_start, x_end = x_end, x_increment = x_increment,
        y_start = y_start, y_end = y_end, y_increment = y_increment,
        y_text_vertical = y_text_vertical, enhance = a$enh),
      error = function(e) { last_err <<- conditionMessage(e); NULL })
    if (!is.null(res) && nrow(res) > 0) return(res)
  }
  stop(if (is.null(last_err)) "SurvdigitizeR returned no data" else last_err, call. = FALSE)
}
