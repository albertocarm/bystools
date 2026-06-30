# Outside-Shiny test harness: runs the bundled digitizer on real figures and the
# real .km2_build_risk_table() logic, then prints the resulting grid. Lets us check
# the numbers-at-risk pipeline end to end without launching the app.
#
# Usage:  Rscript tests/harness_risk_table.R [image1.png image2.png ...]
# Default: every _real/p*.png if present.

suppressMessages(library(jsonlite))
source("R/km2_setup_python.R")

PY <- Sys.getenv("KM2_PY", "C:/Users/Usuario/Documents/.virtualenvs/r-bystools/Scripts/python.exe")
ENGINE <- "inst/python/kmdig3.py"

run_one <- function(img) {
  pre <- tempfile()
  out <- suppressWarnings(system2(PY, shQuote(c(ENGINE, img, pre, "2")), stdout = TRUE, stderr = TRUE))
  mf <- paste0(pre, "_meta.json")
  if (!file.exists(mf)) { cat(sprintf("[%s] ENGINE FAILED\n", basename(img))); return(invisible()) }
  meta <- fromJSON(mf, simplifyVector = FALSE)
  xt <- tryCatch(sort(unique(as.numeric(vapply(meta$xcal$ticks, function(t) t[[1]], numeric(1))))),
                 error = function(e) NULL)
  ax_x <- .km2_infer_axis(xt, zero_start = TRUE)
  rt <- .km2_build_risk_table(meta, ax_x)
  cat(sprintf("\n=== %s ===\n", basename(img)))
  if (is.null(rt)) { cat("  no risk table\n"); return(invisible()) }
  tb <- rt$table
  fmt <- function(x) paste(ifelse(is.na(x), ".", as.character(x)), collapse = " ")
  cat("  Time:", fmt(round(tb$Time, 2)), "\n")
  cat("  G1  :", fmt(tb$N_Risk_G1), "\n")
  cat("  G2  :", fmt(tb$N_Risk_G2), "\n")
  invisible(tb)
}

args <- commandArgs(trailingOnly = TRUE)
imgs <- if (length(args)) args else Sys.glob("_real/p*.png")
for (im in imgs) run_one(im)
