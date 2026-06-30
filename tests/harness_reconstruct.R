# Full reconstruction harness: reproduces what the app does on Auto-clean + Confirm,
# outside Shiny. Runs the digitizer, computes the axis inputs exactly as the app,
# then calls SurvdigitizeR::survival_digitize() on the cleaned image -- the step that
# raises "Step 7: Detecting ranges" inside the app.
#
# Usage: Rscript tests/harness_reconstruct.R image.png

suppressMessages({library(jsonlite)})
source("R/km2_setup_python.R")
PY <- Sys.getenv("KM2_PY", "C:/Users/Usuario/Documents/.virtualenvs/r-bystools/Scripts/python.exe")
ENGINE <- "inst/python/kmdig3.py"

img <- commandArgs(trailingOnly = TRUE)[1]
pre <- tempfile()
system2(PY, shQuote(c(ENGINE, img, pre, "2")), stdout = FALSE, stderr = FALSE)
meta <- fromJSON(paste0(pre, "_meta.json"), simplifyVector = FALSE)
clean <- paste0(pre, "_clean.png")

# --- axis inputs exactly as run_auto_preprocess sets them ---
yt <- tryCatch(sort(unique(as.numeric(vapply(meta$ycal$ticks, function(t) t[[1]], numeric(1))))), error=function(e) NULL)
ax_y <- .km2_infer_axis(yt, zero_start = TRUE)
xt <- tryCatch(sort(unique(as.numeric(vapply(meta$xcal$ticks, function(t) t[[1]], numeric(1))))), error=function(e) NULL)
ax_x <- .km2_infer_axis(xt, zero_start = TRUE)
rt <- .km2_build_risk_table(meta, ax_x)

man_y_start <- ax_y$start; man_y_end <- ax_y$end; man_y_inc <- ax_y$inc
if (!is.null(rt) && !is.null(rt$ax)) {
  man_x_start <- rt$ax$start; man_x_inc <- rt$ax$inc
  man_x_end   <- rt$ax$start + rt$ax$inc * (nrow(rt$table) - 1)
} else { man_x_start <- ax_x$start; man_x_inc <- ax_x$inc; man_x_end <- ax_x$end }

cat(sprintf("axis X: start=%s end=%s inc=%s  (lengths %d/%d/%d)\n",
            man_x_start, man_x_end, man_x_inc, length(man_x_start), length(man_x_end), length(man_x_inc)))
cat(sprintf("axis Y: start=%s end=%s inc=%s\n", man_y_start, man_y_end, man_y_inc))

cat("\n--- calling SurvdigitizeR::survival_digitize() ---\n")
res <- tryCatch(
  SurvdigitizeR::survival_digitize(
    img_path = clean, bg_lightness = 0.3, attempt_OCR = FALSE, num_curves = 2,
    censoring = FALSE, x_start = man_x_start, x_end = man_x_end, x_increment = man_x_inc,
    y_start = man_y_start, y_end = man_y_end, y_increment = man_y_inc,
    y_text_vertical = FALSE, enhance = FALSE),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(res)) {
  cat("OK. rows:", nrow(res), " range time:", paste(round(range(res$time),1), collapse="-"),
      " curves:", paste(sort(unique(res$curve)), collapse=","), "\n")
}
