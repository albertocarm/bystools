# Full end-to-end harness for the WHOLE auto pipeline on many figures, outside
# Shiny: digitizer -> axis inference -> numbers-at-risk grid -> SurvdigitizeR
# reconstruction (the step that raises "Step 7" in the app). Prints a per-figure
# report and a summary, so the complete process can be checked without the app.
#
# Usage: Rscript tests/harness_full.R [img1 img2 ...]   (default: _real/p*.png)

suppressMessages({library(jsonlite); library(SurvdigitizeR)})
source("R/km2_setup_python.R")
PY <- Sys.getenv("KM2_PY", "C:/Users/Usuario/Documents/.virtualenvs/r-bystools/Scripts/python.exe")
ENGINE <- "inst/python/kmdig3.py"

fmtn <- function(x) paste(ifelse(is.na(x), ".", as.character(x)), collapse = " ")

test_one <- function(img) {
  nm <- basename(img)
  pre <- tempfile()
  system2(PY, shQuote(c(ENGINE, img, pre, "2")), stdout = FALSE, stderr = FALSE)
  mf <- paste0(pre, "_meta.json"); clean <- paste0(pre, "_clean.png")
  if (!file.exists(mf)) return(data.frame(fig = nm, status = "engine-failed"))
  meta <- fromJSON(mf, simplifyVector = FALSE)

  yt <- tryCatch(sort(unique(as.numeric(vapply(meta$ycal$ticks, function(t) t[[1]], numeric(1))))), error = function(e) NULL)
  xt <- tryCatch(sort(unique(as.numeric(vapply(meta$xcal$ticks, function(t) t[[1]], numeric(1))))), error = function(e) NULL)
  ax_y <- .km2_infer_axis(yt, zero_start = TRUE)
  ax_x <- .km2_infer_axis(xt, zero_start = TRUE)
  rt <- .km2_build_risk_table(meta, ax_x)

  ncurves <- tryCatch(as.integer(meta$n_curves), error = function(e) NA)
  cat(sprintf("\n=== %s ===  curves=%s  arms_detected=%s\n", nm, ncurves,
              tryCatch(meta$arms_detected, error = function(e) NA)))

  if (is.null(ax_y) || is.null(rt) || is.null(rt$ax)) {
    cat("  (no usable axis / risk table)\n")
    return(data.frame(fig = nm, status = "no-axis-or-grid"))
  }
  x_start <- rt$ax$start; x_inc <- rt$ax$inc
  x_end   <- rt$ax$start + rt$ax$inc * (nrow(rt$table) - 1)
  y_start <- ax_y$start; y_end <- ax_y$end; y_inc <- ax_y$inc
  cat(sprintf("  X: %g..%g by %g   Y: %g..%g by %g\n", x_start, x_end, x_inc, y_start, y_end, y_inc))
  cat("  Time:", fmtn(round(rt$table$Time, 1)), "\n")
  cat("  G1  :", fmtn(rt$table$N_Risk_G1), "\n")
  cat("  G2  :", fmtn(rt$table$N_Risk_G2), "\n")

  # scalar sanity (the cause of "'to' must be of length 1")
  scalars_ok <- all(vapply(list(x_start, x_end, x_inc, y_start, y_end, y_inc), length, integer(1)) == 1)

  rec <- tryCatch(
    .km2_survdigitize_robust(
      img_path = clean, x_start = x_start, x_end = x_end, x_increment = x_inc,
      y_start = y_start, y_end = y_end, y_increment = y_inc, num_curves = 2,
      censoring = FALSE, bg_lightness = 0.3, enhance = FALSE),
    error = function(e) structure(conditionMessage(e), class = "err"))
  if (inherits(rec, "err")) {
    cat("  RECONSTRUCT: ERROR ->", as.character(rec), "\n")
    return(data.frame(fig = nm, status = "reconstruct-error", scalars_ok = scalars_ok))
  }
  cat(sprintf("  RECONSTRUCT: OK  rows=%d  time=%s-%s  curves=%s\n",
              nrow(rec), round(min(rec$time),1), round(max(rec$time),1),
              paste(sort(unique(rec$curve)), collapse = ",")))
  data.frame(fig = nm, status = "ok", scalars_ok = scalars_ok)
}

imgs <- commandArgs(trailingOnly = TRUE)
if (!length(imgs)) imgs <- Sys.glob("_real/p*.png")
res <- do.call(rbind, lapply(imgs, function(i) tryCatch(test_one(i),
              error = function(e) data.frame(fig = basename(i), status = paste("harness-error:", conditionMessage(e))))))
cat("\n\n========== SUMMARY ==========\n")
print(res, row.names = FALSE)
cat(sprintf("\nreconstruct OK: %d / %d\n", sum(res$status == "ok"), nrow(res)))
