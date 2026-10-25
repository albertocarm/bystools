# --------------------------------------------------------------------------- #
# Kaplan-Meier digitizer implemented in R.
#
# Support engine for the automatic digitization in km2bayesPro(). The primary
# engine is the bundled Python one (inst/python/kmdig3.py), which needs a Python
# environment and the Tesseract program; this one needs neither, so the feature
# still works on a plain install of the package.
#
# It writes the same four files as the Python engine for a given prefix:
#
#   <prefix>.csv         points (curve, x_px, y_px, x_val, y_val)
#   <prefix>_meta.json   box, calibration, numbers at risk, per-curve quality
#   <prefix>_clean.png   the curves redrawn on a blank canvas
#   <prefix>_overlay.png the original with the digitization drawn over it
#
# Pixel coordinates are written zero-based, as the Python engine writes them,
# so everything downstream reads either engine without knowing which ran.
#
# Colour work is done in CIE Lab on the same 8-bit scale OpenCV uses (L scaled
# to 0-255, a and b offset by 128), so the thresholds carry over unchanged.
# --------------------------------------------------------------------------- #


#' @keywords internal
#' @noRd
.km2_lab <- function(R, G, B) {
  lin <- function(c) ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  r <- lin(R); g <- lin(G); b <- lin(B)
  X <- 0.412453 * r + 0.357580 * g + 0.180423 * b
  Y <- 0.212671 * r + 0.715160 * g + 0.072169 * b
  Z <- 0.019334 * r + 0.119193 * g + 0.950227 * b
  f <- function(t) ifelse(t > 0.008856, t^(1 / 3), 7.787 * t + 16 / 116)
  fx <- f(X / 0.950456); fy <- f(Y); fz <- f(Z / 1.088754)
  list(L = ifelse(Y > 0.008856, 116 * fy - 16, 903.3 * Y) * 255 / 100,
       a = 500 * (fx - fy) + 128,
       b = 200 * (fy - fz) + 128)
}


#' @keywords internal
#' @noRd
.km2_read_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  px <- if (ext %in% c("jpg", "jpeg")) {
    jpeg::readJPEG(path)
  } else if (ext == "png") {
    png::readPNG(path)
  } else {
    img <- magick::image_read(path)
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    magick::image_write(img, tmp, format = "png")
    png::readPNG(tmp)
  }
  if (length(dim(px)) == 2L) px <- array(px, dim = c(dim(px), 3L))      # grayscale
  if (dim(px)[3] >= 4L) {                                              # flatten alpha onto white
    al <- px[, , 4]
    for (k in 1:3) px[, , k] <- px[, , k] * al + (1 - al)
  }
  rgb <- px[, , 1:3, drop = FALSE]
  lab <- .km2_lab(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  list(rgb = rgb * 255,
       lab = lab,
       gray = 0.299 * rgb[, , 1] * 255 + 0.587 * rgb[, , 2] * 255 + 0.114 * rgb[, , 3] * 255,
       h = dim(rgb)[1], w = dim(rgb)[2])
}


#' @keywords internal
#' @noRd
.km2_colorful <- function(rgb, thr = 25) {
  hi <- pmax(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  lo <- pmin(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  (hi - lo) > thr
}


# Longest run of TRUE in a logical vector: length, first and last index.
#' @keywords internal
#' @noRd
.km2_longest_run <- function(v) {
  if (!any(v)) return(c(0L, 0L, 0L))
  r <- rle(v)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$values)
  i <- keep[which.max(r$lengths[keep])]
  c(r$lengths[i], starts[i], ends[i])
}


# Connected components of a logical matrix. Returns a label matrix (0 is
# background) and, per label, area and bounding box in matrix coordinates.
#' @keywords internal
#' @noRd
.km2_components <- function(mask) {
  if (!any(mask)) return(list(labels = NULL, stats = NULL))
  im <- imager::as.cimg(t(mask) * 1)                      # cimg is (x, y)
  lb <- imager::label(im, high_connectivity = TRUE)
  labels <- t(matrix(as.numeric(lb), nrow = ncol(mask), ncol = nrow(mask)))
  # label() numbers from zero and numbers the background too, so a genuine
  # component can carry the same id as background. Shift before clearing it,
  # or that component's id selects every background pixel as well.
  labels <- labels + 1
  labels[!mask] <- 0
  id <- labels[mask]
  if (!length(id)) return(list(labels = labels, stats = NULL))
  idx <- which(mask, arr.ind = TRUE)
  st <- data.frame(id = id, y = idx[, 1], x = idx[, 2])
  ag <- stats::aggregate(cbind(y, x) ~ id, data = st,
                         FUN = function(z) c(min(z), max(z)))
  tb <- table(st$id)
  out <- data.frame(
    id    = ag$id,
    y0    = ag$y[, 1], y1 = ag$y[, 2],
    x0    = ag$x[, 1], x1 = ag$x[, 2],
    area  = as.integer(tb)[match(ag$id, as.integer(names(tb)))]
  )
  out$w <- out$x1 - out$x0 + 1L
  out$h <- out$y1 - out$y0 + 1L
  list(labels = labels, stats = out)
}


# Plot frame: the longest vertical dark run on the left, and the lowest long
# horizontal dark run, with a fallback to the span of the coloured pixels when
# the figure has no drawn axes.
#' @keywords internal
#' @noRd
.km2_detect_box <- function(gray, colorful, dark_thr = 175) {
  H <- nrow(gray); W <- ncol(gray)
  dark <- gray < dark_thr
  bd <- max(2L, as.integer(0.01 * min(H, W)))

  bestv <- NULL
  for (x in bd:max(bd + 1L, as.integer(0.45 * W))) {
    r <- .km2_longest_run(dark[, x])
    if (is.null(bestv) || r[1] > bestv[2]) bestv <- c(x, r)
  }
  if (!is.null(bestv) && bestv[2] >= 0.30 * H) {
    L_axis <- bestv[1]; ytop <- bestv[3]; ybot <- bestv[4]
  } else {
    idx <- which(colorful, arr.ind = TRUE)
    if (nrow(idx) > 50) {
      L_axis <- as.integer(stats::quantile(idx[, 2], 0.02))
      ytop   <- as.integer(stats::quantile(idx[, 1], 0.02))
      ybot   <- as.integer(stats::quantile(idx[, 1], 0.98))
    } else {
      L_axis <- as.integer(0.12 * W); ytop <- as.integer(0.08 * H); ybot <- as.integer(0.88 * H)
    }
    colcount <- colSums(colorful)
    for (x in 1:min(W, as.integer(0.5 * W))) {
      k <- pmin(W, x + 0:2)
      if (all(colcount[k] >= 3)) { L_axis <- max(3L, min(L_axis, x)); break }
    }
  }

  x_from <- max(1L, L_axis - 2L)
  rows <- seq.int(as.integer(H * 0.30), H - bd)
  runs <- vapply(rows, function(y) {
    r <- .km2_longest_run(dark[y, x_from:(W - bd)])
    c(y, r[1], r[2] + x_from - 1L, r[3] + x_from - 1L)
  }, numeric(4))
  rmax <- max(runs[2, ], na.rm = TRUE)
  cand <- which(runs[2, ] >= 0.8 * rmax & runs[2, ] >= 0.30 * W)
  if (length(cand)) {
    pick <- cand[which.max(runs[1, cand])]
    B <- runs[1, pick]; bx1 <- runs[4, pick]
  } else {
    B <- ybot; bx1 <- W - bd
  }

  if ((B - ytop) < 0.25 * H || ytop > 0.60 * H) {
    idx <- which(colorful, arr.ind = TRUE)
    if (nrow(idx) > 50) {
      ytop <- as.integer(stats::quantile(idx[, 1], 0.01))
      B    <- as.integer(stats::quantile(idx[, 1], 0.99))
      L_axis <- min(L_axis, as.integer(stats::quantile(idx[, 2], 0.01)))
      bx1  <- max(bx1, as.integer(stats::quantile(idx[, 2], 0.99)))
    } else {
      ytop <- as.integer(0.08 * H); B <- as.integer(0.88 * H)
    }
  }
  list(L = as.integer(L_axis), R = as.integer(bx1),
       T = as.integer(ytop), B = as.integer(B), L_axis = as.integer(L_axis))
}


# Up to k curve colours as (L, a, b) centroids. Over-clusters the coloured
# pixels by hue, keeps the clusters that are chromatic by magnitude and hold
# real support, then merges those that are the same colour in weighted (L,a,b)
# so an arm's anti-aliased edge does not become a second colour.
#' @keywords internal
#' @noRd
.km2_find_colors <- function(lab, colorful, interior, k = NULL, kmax = 4L) {
  sel <- colorful & interior
  n <- sum(sel)
  if (n < 20) return(list())
  La <- lab$L[sel]; aa <- lab$a[sel]; bb <- lab$b[sel]
  ab <- cbind(aa, bb)
  want <- max(1L, if (is.null(k)) 2L else k)
  uniq <- nrow(unique(ab))
  kk <- min(kmax, max(want, 2L), max(1L, uniq))
  set.seed(0)
  km <- stats::kmeans(ab, centers = kk, nstart = 5L, iter.max = 50L)
  cand <- list()
  for (i in seq_len(kk)) {
    m <- km$cluster == i
    cnt <- sum(m)
    cen <- km$centers[i, ]
    if (sqrt((cen[1] - 128)^2 + (cen[2] - 128)^2) > 6 && cnt >= 100) {
      cand[[length(cand) + 1L]] <- list(n = cnt, L = mean(La[m]), a = cen[1], b = cen[2])
    }
  }
  if (!length(cand)) return(list())
  cand <- cand[order(vapply(cand, function(z) z$n, numeric(1)), decreasing = TRUE)]
  merged <- list()
  for (cd in cand) {
    far <- all(vapply(merged, function(m)
      sqrt((0.4 * (cd$L - m[1]))^2 + (cd$a - m[2])^2 + (cd$b - m[3])^2) >= 12, logical(1)))
    if (far) merged[[length(merged) + 1L]] <- c(cd$L, cd$a, cd$b)
  }
  utils::head(merged, want)
}


# Otsu split of a set of intensities.
#' @keywords internal
#' @noRd
.km2_otsu <- function(v) {
  v <- as.integer(pmax(0, pmin(255, round(v))))
  h <- tabulate(v + 1L, nbins = 256L)
  tot <- sum(h)
  if (tot == 0) return(128)
  lev <- 0:255
  wB <- cumsum(h); wF <- tot - wB
  sumAll <- sum(lev * h); sumB <- cumsum(lev * h)
  ok <- wB > 0 & wF > 0
  mB <- ifelse(ok, sumB / pmax(wB, 1), 0)
  mF <- ifelse(ok, (sumAll - sumB) / pmax(wF, 1), 0)
  between <- ifelse(ok, wB * wF * (mB - mF)^2, -1)
  (which.max(between) - 1L)
}


# A mask thick enough to carry a translucent confidence band is split by
# lightness: the band is light and the line drawn over it is dark. Thin masks
# pass through untouched.
#' @keywords internal
#' @noRd
.km2_peel_ci_band <- function(light, hue) {
  if (sum(hue) < 10) return(hue)
  idx <- which(hue, arr.ind = TRUE)
  xspan <- max(1L, max(idx[, 2]) - min(idx[, 2]))
  if (sum(hue) / xspan <= 8) return(hue)
  thr <- .km2_otsu(light[hue])
  line <- hue & (light <= thr)
  if (sum(line) >= 0.04 * sum(hue)) line else hue
}


# Pixels of one curve colour: within tol of this centroid in hue, and this
# centroid the nearest of all of them in weighted (L, a, b), which separates
# two arms that share a hue but differ in lightness.
#' @keywords internal
#' @noRd
.km2_line_mask <- function(lab, centroid, others, colorful, interior, tol = 20) {
  d <- sqrt((lab$a - centroid[2])^2 + (lab$b - centroid[3])^2)
  hue <- (d < tol) & colorful & interior
  if (length(others)) {
    dlab <- function(cen) (0.5 * (lab$L - cen[1]))^2 + (lab$a - cen[2])^2 + (lab$b - cen[3])^2
    dme <- dlab(centroid)
    win <- matrix(TRUE, nrow(hue), ncol(hue))
    for (o in others) win <- win & (dme <= dlab(o))
    hue <- hue & win
  }
  .km2_peel_ci_band(lab$L, hue)
}


# Keep only elongated (line-like) components.
#' @keywords internal
#' @noRd
.km2_clean_components <- function(m, span, high) {
  cc <- .km2_components(m)
  if (is.null(cc$stats)) return(m & FALSE)
  keep <- cc$stats$id[cc$stats$area >= 3 &
                        (cc$stats$w >= 0.04 * span | cc$stats$h >= 0.04 * high)]
  matrix(cc$labels %in% keep, nrow(m), ncol(m)) & m
}


# Area a component's outer contour would enclose: the component plus the holes
# inside it, found as the parts of its complement that do not touch the
# bounding box border.
#' @keywords internal
#' @noRd
.km2_filled_area <- function(sub) {
  inv <- !sub
  if (!any(inv)) return(sum(sub))
  cc <- .km2_components(inv)
  if (is.null(cc$stats)) return(sum(sub))
  nr <- nrow(sub); nc <- ncol(sub)
  border <- unique(c(cc$labels[1, ], cc$labels[nr, ], cc$labels[, 1], cc$labels[, nc]))
  holes <- setdiff(cc$stats$id, border)
  sum(sub) + sum(cc$stats$area[cc$stats$id %in% holes])
}


# Hollow rectangular components: bordered legend or callout frames. A curve is
# an open path, so filling its contour adds almost nothing.
#' @keywords internal
#' @noRd
.km2_drop_boxes <- function(mask, span, min_area = 200, hole_frac = 0.5) {
  cc <- .km2_components(mask)
  if (is.null(cc$stats)) return(mask)
  out <- mask
  for (i in seq_len(nrow(cc$stats))) {
    s <- cc$stats[i, ]
    if (s$area < min_area || s$w >= 0.6 * span) next
    sub <- matrix(cc$labels[s$y0:s$y1, s$x0:s$x1] == s$id, s$h, s$w)
    fa <- .km2_filled_area(sub)
    if (fa > 0 && (fa - s$area) / fa > hole_frac) out[cc$labels == s$id] <- FALSE
  }
  out
}


# Solid-filled rectangular panels drawn in an arm's own colour. Dropped only
# when every test holds at once, which no trace of a curve satisfies.
#' @keywords internal
#' @noRd
.km2_drop_filled_panels <- function(mask, span, high, min_area = 200,
                                    rect_frac = 0.85, min_holes = 0.02, max_holes = 0.5) {
  cc <- .km2_components(mask)
  if (is.null(cc$stats)) return(mask)
  out <- mask
  w_min <- max(12, as.integer(0.05 * span))
  h_min <- max(12, as.integer(0.04 * high))
  for (i in seq_len(nrow(cc$stats))) {
    s <- cc$stats[i, ]
    if (s$area < min_area || s$w >= 0.6 * span || s$w < w_min || s$h < h_min) next
    sub <- matrix(cc$labels[s$y0:s$y1, s$x0:s$x1] == s$id, s$h, s$w)
    fa <- .km2_filled_area(sub)
    if (fa == 0 || fa < rect_frac * s$w * s$h) next
    holes <- (fa - s$area) / fa
    if (holes >= min_holes && holes <= max_holes) {
      out[s$y0:s$y1, s$x0:s$x1][sub | .km2_hole_fill(sub)] <- FALSE
    }
  }
  out
}


#' @keywords internal
#' @noRd
.km2_hole_fill <- function(sub) {
  inv <- !sub
  cc <- .km2_components(inv)
  if (is.null(cc$stats)) return(sub & FALSE)
  nr <- nrow(sub); nc <- ncol(sub)
  border <- unique(c(cc$labels[1, ], cc$labels[nr, ], cc$labels[, 1], cc$labels[, nc]))
  holes <- setdiff(cc$stats$id, border)
  matrix(cc$labels %in% holes, nr, nc)
}


# Per-column trace: in each column take the tallest run of mask pixels and, if
# it is thin enough to be a line rather than a blob, its median row.
#' @keywords internal
#' @noRd
.km2_column_trace <- function(mask, L, R, thin_max) {
  cols <- integer(0); ys <- numeric(0)
  for (x in L:R) {
    yy <- which(mask[, x])
    if (!length(yy)) next
    if (length(yy) > 1) {
      g <- diff(yy)
      if (max(g) > 3) {
        grp <- cumsum(c(1L, as.integer(g > 3)))
        yy <- yy[grp == as.integer(names(which.max(table(grp))))]
      }
    }
    if (max(yy) - min(yy) <= thin_max) { cols <- c(cols, x); ys <- c(ys, stats::median(yy)) }
  }
  list(x = cols, y = ys)
}


#' @keywords internal
#' @noRd
.km2_reconstruct <- function(mask, L, R, thin_max = 8) {
  tr <- .km2_column_trace(mask, L, R, thin_max)
  if (length(tr$x) < 5) {
    cols <- integer(0); yv <- numeric(0)
    for (x in L:R) {
      yy <- which(mask[, x])
      if (length(yy)) { cols <- c(cols, x); yv <- c(yv, stats::median(yy)) }
    }
    tr <- list(x = cols, y = yv)
  }
  if (length(tr$x) < 3) return(NULL)
  xs <- tr$x; ys <- tr$y
  if (min(xs) > L + 1) { xs <- c(L, xs); ys <- c(ys[1], ys) }
  o <- order(xs); xs <- xs[o]; ys <- ys[o]
  keep <- !duplicated(xs); xs <- xs[keep]; ys <- ys[keep]
  list(x = as.numeric(xs), y = stats::isoreg(xs, ys)$yf)
}


# Share of mask columns whose step-held trace sits further than tol from every
# curve pixel in that column. A faithful trace scores near zero.
#' @keywords internal
#' @noRd
.km2_offmask <- function(mask, xs, ys, tol) {
  L <- min(xs); R <- max(xs)
  off <- 0L; tot <- 0L
  j <- 1L; cur <- ys[1]
  for (x in L:R) {
    while (j < length(xs) && xs[j + 1L] <= x) { j <- j + 1L; cur <- ys[j] }
    yy <- which(mask[, x])
    if (!length(yy)) next
    tot <- tot + 1L
    if (min(abs(yy - cur)) > tol) off <- off + 1L
  }
  if (tot) off / tot else 1
}


#' @keywords internal
#' @noRd
.km2_reconstruct_best <- function(mask, L, R, tol, thin_options = c(8, 20, 40)) {
  best <- NULL
  for (tm in thin_options) {
    rec <- .km2_reconstruct(mask, L, R, thin_max = tm)
    if (is.null(rec)) next
    s <- .km2_offmask(mask, rec$x, rec$y, tol)
    if (is.null(best) || s < best$q) best <- list(x = rec$x, y = rec$y, q = s)
  }
  best
}


# --------------------------------------------------------------------------- #
# Text: axis calibration and the numbers-at-risk table
# --------------------------------------------------------------------------- #

.km2_engines <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.km2_engine <- function(psm, whitelist = NULL) {
  key <- paste0(psm, "|", if (is.null(whitelist)) "" else whitelist)
  if (!is.null(.km2_engines[[key]])) return(.km2_engines[[key]])
  opts <- list(tessedit_pageseg_mode = as.character(psm))
  if (!is.null(whitelist)) opts$tessedit_char_whitelist <- whitelist
  eng <- tesseract::tesseract("eng", options = opts)
  assign(key, eng, envir = .km2_engines)
  eng
}


#' @keywords internal
#' @noRd
.km2_as_image <- function(gray) {
  m <- matrix(pmax(0, pmin(255, as.numeric(gray))) / 255, nrow(gray), ncol(gray))
  magick::image_read(grDevices::as.raster(m))
}


# Words in a grayscale crop, upscaled before reading, with positions brought
# back to the crop's own scale.
#' @keywords internal
#' @noRd
.km2_ocr_tokens <- function(gray, scale, psm, whitelist = NULL) {
  if (nrow(gray) < 3 || ncol(gray) < 3) return(NULL)
  img <- .km2_as_image(gray)
  scale <- min(scale, 4000 / max(1, ncol(gray)))          # bound the work on wide crops
  if (scale > 1.01) {
    img <- magick::image_resize(img, paste0(round(ncol(gray) * scale), "x",
                                            round(nrow(gray) * scale)), filter = "Cubic")
  } else {
    scale <- 1
  }
  d <- try(tesseract::ocr_data(img, engine = .km2_engine(psm, whitelist)), silent = TRUE)
  if (inherits(d, "try-error") || !nrow(d)) return(NULL)
  bb <- do.call(rbind, lapply(strsplit(as.character(d$bbox), ","), as.numeric))
  if (is.null(bb)) return(NULL)
  data.frame(text = trimws(as.character(d$word)),
             conf = as.numeric(d$confidence),
             x = bb[, 1] / scale, y = bb[, 2] / scale,
             w = (bb[, 3] - bb[, 1]) / scale, h = (bb[, 4] - bb[, 2]) / scale,
             stringsAsFactors = FALSE)
}


#' @keywords internal
#' @noRd
.km2_clean_num <- function(s) {
  for (p in list(c("O", "0"), c("o", "0"), c("l", "1"), c("I", "1"), c("|", "1"),
                 c("S", "5"), c("B", "8"), c(",", "."))) {
    s <- gsub(p[1], p[2], s, fixed = TRUE)
  }
  s <- gsub("[^0-9.]", "", s)
  while (lengths(regmatches(s, gregexpr(".", s, fixed = TRUE))) > 1) {
    s <- sub(".", "", s, fixed = TRUE)
  }
  s
}


#' @keywords internal
#' @noRd
.km2_ocr_numeric_pos <- function(gray, x_off, y_off, allow_dot,
                                 scales = c(2, 3, 4), psms = c(11, 6, 7),
                                 enough = NULL) {
  # No character whitelist. With the LSTM reader, restricting the alphabet
  # degrades recognition badly and can suppress a tick row outright, so the
  # text is read freely and the numbers are selected afterwards. A token has to
  # carry at least one real digit, which stops the repair of look-alike
  # characters from turning a word into a number.
  pat <- if (allow_dot) "^[0-9]+(\\.[0-9]+)?$" else "^[0-9]+$"
  out <- NULL
  for (sc in scales) for (ps in psms) {
    tk <- .km2_ocr_tokens(gray, sc, ps)
    if (is.null(tk)) next
    cs <- vapply(tk$text, .km2_clean_num, character(1), USE.NAMES = FALSE)
    keep <- grepl(pat, cs) & grepl("[0-9]", tk$text)
    if (!any(keep)) next
    out <- rbind(out, data.frame(
      val = as.numeric(cs[keep]),
      dot = grepl(".", cs[keep], fixed = TRUE),
      xc = tk$x[keep] + tk$w[keep] / 2 + x_off,
      yc = tk$y[keep] + tk$h[keep] / 2 + y_off,
      w = tk$w[keep], h = tk$h[keep], conf = tk$conf[keep],
      stringsAsFactors = FALSE))
    if (!is.null(enough) && enough(out)) return(out)   # no need to read it again
  }
  out
}


# Line through the points that the most of them agree with, so a few misread
# labels cannot tilt the axis.
#' @keywords internal
#' @noRd
.km2_robust_line <- function(px, val, tol_frac = 0.02, valid = NULL) {
  n <- length(px)
  if (n < 2) return(NULL)
  tol <- max(tol_frac * max(max(val) - min(val), 1e-6), 1e-6)
  search <- function(check) {
    best <- NULL
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      if (px[j] == px[i]) next
      m <- (val[j] - val[i]) / (px[j] - px[i])
      c0 <- val[i] - m * px[i]
      if (!is.null(check) && !check(m, c0)) next
      res <- abs(m * px + c0 - val)
      inl <- res <= tol
      sc <- sum(inl)
      if (is.null(best) || sc > best$n || (sc == best$n && sum(res[inl]) < best$r)) {
        best <- list(n = sc, r = sum(res[inl]), inl = inl)
      }
    }
    best
  }
  # Candidate lines that cannot be the axis are not allowed to collect votes.
  # Misread labels are numerous and nearly constant, so without this they form
  # a flat line with more inliers than the axis itself.
  best <- search(valid)
  if (is.null(best)) best <- search(NULL)
  if (is.null(best)) return(NULL)
  inl <- best$inl
  if (sum(inl) < 2) inl <- rep(TRUE, n)
  cf <- stats::coef(stats::lm(val[inl] ~ px[inl]))
  list(m = unname(cf[2]), c = unname(cf[1]), inl = inl)
}


#' @keywords internal
#' @noRd
.km2_dedup <- function(tk, key) {
  if (is.null(tk) || !nrow(tk)) return(NULL)
  b <- round(tk[[key]] / 4) * 4
  o <- order(b, -tk$conf)
  tk <- tk[o, ]; b <- b[o]
  tk <- tk[!duplicated(b), ]
  data.frame(pos = tk[[key]], val = tk$val, conf = tk$conf)
}


#' @keywords internal
#' @noRd
.km2_fit_axis <- function(pts, valid = NULL) {
  if (is.null(pts) || nrow(pts) < 2) return(NULL)
  fit <- .km2_robust_line(pts$pos, pts$val, valid = valid)
  if (is.null(fit)) return(NULL)
  keep <- pts[fit$inl, ]
  ag <- stats::aggregate(pos ~ val, data = keep, FUN = stats::median)
  ag <- ag[order(ag$pos), ]
  list(m = fit$m, c = fit$c, ticks = data.frame(val = ag$val, px = ag$pos))
}


# Y axis. A survival axis runs downward and spans most of the figure, and a
# candidate line that does not is never the axis however many misread labels
# agree with it. Reading stops as soon as a pass yields such a line, so an
# ordinary figure costs one pass and only a hard one costs all of them.
#' @keywords internal
#' @noRd
.km2_calibrate_y <- function(gray, L_band, T_rough, yband) {
  H <- nrow(gray); W <- ncol(gray)
  x0 <- max(1L, L_band - yband); x1 <- min(W, L_band + 4L)
  y0 <- max(1L, T_rough - as.integer(0.04 * H))
  if (x1 <= x0 || y0 >= H) return(list(cal = NULL, fraction = FALSE))
  crop <- gray[y0:H, x0:x1, drop = FALSE]

  is_fraction <- function(tk) {
    length(unique(round(tk$val[tk$dot & tk$val >= 0 & tk$val <= 1], 2))) >= 3
  }
  axis_for <- function(tk, frac) {
    topv <- if (frac) 1 else 100
    valid <- function(m, c0) {
      if (is.na(m) || m >= 0) return(FALSE)
      t2 <- (topv - c0) / m; b2 <- -c0 / m
      is.finite(t2) && is.finite(b2) && t2 < b2 &&
        t2 >= -0.2 * H && b2 <= 1.2 * H && (b2 - t2) >= 0.2 * H
    }
    sel <- if (frac) tk[tk$val >= 0 & tk$val <= 1.5, ] else tk[!tk$dot, ]
    if (!nrow(sel)) return(NULL)
    # Confident labels first: garbage read below the axis can out-vote the real
    # ticks. Confidence alone does not decide, though, since the reader reports
    # none for some page segmentation modes and those readings are often good.
    strong <- sel[sel$conf >= 30, ]
    cand <- if (nrow(strong) >= 2) .km2_fit_axis(.km2_dedup(strong, "yc"), valid) else NULL
    if (is.null(cand) || !valid(cand$m, cand$c)) {
      alt <- .km2_fit_axis(.km2_dedup(sel, "yc"), valid)
      if (is.null(cand) || (!is.null(alt) && valid(alt$m, alt$c))) cand <- alt
    }
    cand
  }
  # Reading stops when another pass would not move the answer, not merely when
  # enough ticks have been seen: an early pass can settle on four ticks one of
  # which is misread, and a later pass corrects it.
  prev <- NA_real_
  settled <- function(tk) {
    if (is.null(tk) || nrow(tk) < 3) return(FALSE)
    f <- axis_for(tk, is_fraction(tk))
    if (is.null(f) || nrow(f$ticks) < 4) { prev <<- NA_real_; return(FALSE) }
    b <- (0 - f$c) / f$m
    stable <- is.finite(prev) && is.finite(b) && abs(b - prev) <= 1
    prev <<- b
    stable
  }

  tk <- .km2_ocr_numeric_pos(crop, x0 - 1L, y0 - 1L, allow_dot = TRUE, enough = settled)
  if (is.null(tk) || !nrow(tk)) return(list(cal = NULL, fraction = FALSE))
  fraction <- is_fraction(tk)
  fit <- axis_for(tk, fraction)
  list(cal = fit, fraction = fraction)
}


# X axis. Scans the whole band below the plot, where the tick labels often read
# more cleanly than in a thin strip, and keeps the row whose fit increases left
# to right, which rejects the numbers-at-risk rows.
#' @keywords internal
#' @noRd
.km2_calibrate_x <- function(gray, L, R, B) {
  H <- nrow(gray); W <- ncol(gray)
  y0 <- min(B + 1L, H)
  if (R <= L || y0 >= H) return(NULL)
  crop <- gray[y0:H, , drop = FALSE]
  pick_row <- function(tk) {
    tk <- tk[tk$xc >= L - 6 & tk$xc <= R + 6 & tk$val >= 0 & tk$val <= 10000, ]
    if (!nrow(tk)) return(NULL)
    tk <- tk[order(tk$yc), ]
    grp <- cumsum(c(1L, as.integer(diff(tk$yc) > 12)))
    best <- NULL
    for (g in unique(grp)) {
      row <- tk[grp == g, ]
      pts <- .km2_dedup(row, "xc")
      if (is.null(pts) || nrow(pts) < 3) next
      fit <- .km2_fit_axis(pts, function(m, c0) m > 0)
      if (is.null(fit) || fit$m <= 0) next
      score <- c(nrow(fit$ticks), -mean(row$yc))
      if (is.null(best) || score[1] > best$s[1] ||
          (score[1] == best$s[1] && score[2] > best$s[2])) best <- list(s = score, fit = fit)
    }
    best
  }
  tk <- .km2_ocr_numeric_pos(crop, 0L, y0 - 1L, allow_dot = FALSE,
                             scales = c(2, 3), psms = c(6, 11, 7),
                             enough = local({
                               prev <- NA_real_
                               function(t) {
                                 b <- pick_row(t)
                                 if (is.null(b) || b$s[1] < 4) { prev <<- NA_real_; return(FALSE) }
                                 v <- b$fit$m * R + b$fit$c
                                 stable <- is.finite(prev) && abs(v - prev) <= 0.01 * abs(v)
                                 prev <<- v
                                 stable
                               }
                             }))
  if (is.null(tk)) return(NULL)
  best <- pick_row(tk)
  if (is.null(best)) NULL else best$fit
}


# Text mask used to erase labels. A coloured pixel is protected only when it
# belongs to a wide component, which is the data curve itself, so coloured
# titles and in-plot tables are erased while the curves are not.
#' @keywords internal
#' @noRd
.km2_ocr_text_mask <- function(gray, rgb, ocr_conf = 25) {
  H <- nrow(gray); W <- ncol(gray)
  txt <- matrix(FALSE, H, W)
  hi <- pmax(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  lo <- pmin(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  strong <- (hi - lo) > 30
  protect <- matrix(FALSE, H, W)
  cc <- .km2_components(strong)
  if (!is.null(cc$stats)) {
    wide <- cc$stats$id[cc$stats$w >= 0.30 * W]
    if (length(wide)) protect <- matrix(cc$labels %in% wide, H, W)
  }
  tk <- .km2_ocr_tokens(gray, 2, 11)
  if (!is.null(tk)) {
    for (i in seq_len(nrow(tk))) {
      if (!nzchar(tk$text[i]) || is.na(tk$conf[i]) || tk$conf[i] < ocr_conf) next
      h <- tk$h[i]; w <- tk$w[i]
      if (h >= 60 || w >= 0.8 * W) next
      y <- max(1L, as.integer(tk$y[i]) - 2L); x <- max(1L, as.integer(tk$x[i]) - 2L)
      y2 <- min(H, as.integer(tk$y[i] + h) + 2L); x2 <- min(W, as.integer(tk$x[i] + w) + 2L)
      txt[y:y2, x:x2] <- TRUE
    }
  }
  txt & !protect
}


# --------------------------------------------------------------------------- #
# Arms
# --------------------------------------------------------------------------- #

# Two centroids belong to one arm's colour family (its line plus a translucent
# band or halo) rather than to two arms. The test is hue angle, since a band
# keeps its line's hue at lower saturation; (a, b) proximity decides only when
# a centroid is so close to grey that its hue angle means nothing.
#' @keywords internal
#' @noRd
.km2_same_family <- function(c1, c2, hue_tol = 0.70, gray_chroma = 10, gray_sep = 20) {
  ch1 <- sqrt((c1[2] - 128)^2 + (c1[3] - 128)^2)
  ch2 <- sqrt((c2[2] - 128)^2 + (c2[3] - 128)^2)
  if (min(ch1, ch2) < gray_chroma) {
    return(sqrt((c1[2] - c2[2])^2 + (c1[3] - c2[3])^2) < gray_sep)
  }
  h1 <- atan2(c1[3] - 128, c1[2] - 128)
  h2 <- atan2(c2[3] - 128, c2[2] - 128)
  abs(((h1 - h2 + pi) %% (2 * pi)) - pi) < hue_tol
}


# How many distinct coloured arms the plot really has, independent of the
# requested curve count, so a three-arm figure can be refused.
#' @keywords internal
#' @noRd
.km2_count_arms <- function(lab, colorful, interior, min_support = 0.15,
                            sep = 35, hue_tol = 0.30, kmax = 4L) {
  sel <- colorful & interior
  n <- sum(sel)
  if (n < 50) return(0L)
  pts <- cbind(lab$a[sel], lab$b[sel])
  hi <- min(kmax, nrow(unique(pts)))
  best <- 0L
  for (kk in seq_len(hi)) {
    set.seed(0)
    km <- try(stats::kmeans(pts, centers = kk, nstart = 5L, iter.max = 50L), silent = TRUE)
    if (inherits(km, "try-error")) next
    keep <- list()
    for (i in seq_len(kk)) {
      cen <- km$centers[i, ]
      if ((abs(cen[1] - 128) > 8 || abs(cen[2] - 128) > 8) &&
          sum(km$cluster == i) / n >= min_support) keep[[length(keep) + 1L]] <- cen
    }
    m <- length(keep)
    if (!m) next
    parent <- seq_len(m)
    find <- function(x) { while (parent[x] != x) x <- parent[x]; x }
    for (a in seq_len(m)) for (b in seq_len(m)) {
      if (b <= a) next
      eu <- sqrt((keep[[a]][1] - keep[[b]][1])^2 + (keep[[a]][2] - keep[[b]][2])^2)
      ha <- atan2(keep[[a]][2] - 128, keep[[a]][1] - 128)
      hb <- atan2(keep[[b]][2] - 128, keep[[b]][1] - 128)
      ang <- abs(((ha - hb + pi) %% (2 * pi)) - pi)
      if (eu < sep || ang < hue_tol) { ra <- find(a); rb <- find(b); if (ra != rb) parent[ra] <- rb }
    }
    best <- max(best, length(unique(vapply(seq_len(m), find, integer(1)))))
  }
  best
}


# Masks for near-neutral arms, which carry no chroma and so are invisible to
# the colour clustering. Black and grey arms are split by lightness.
#' @keywords internal
#' @noRd
.km2_neutral_masks <- function(rgb, gray, colorful, interior, txt, box, n_needed) {
  if (n_needed < 1) return(list())
  L <- box$L; R <- box$R; Tt <- box$T; B <- box$B
  hi <- pmax(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  lo <- pmin(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  sat <- hi - lo
  neutral <- (sat < 28) & (gray < 240) & interior & !colorful & !txt
  neutral[max(Tt, B - 3L):nrow(neutral), ] <- FALSE
  neutral[, 1:min(R, L + 4L)] <- FALSE
  span <- max(1L, R - L); high <- max(1L, B - Tt)

  kw <- max(15L, as.integer(0.7 * span))                 # strip full-width gridlines
  keep <- neutral
  for (y in seq_len(nrow(neutral))) {
    v <- neutral[y, ]
    if (!any(v)) next
    r <- rle(v)
    if (any(r$values & r$lengths >= kw)) {
      ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
      for (i in which(r$values & r$lengths >= kw)) keep[y, starts[i]:ends[i]] <- FALSE
    }
  }
  neutral <- keep
  neutral <- .km2_drop_boxes(neutral, span)
  if (sum(neutral) < 50) return(list())

  masks <- list(neutral)
  if (n_needed >= 2) {
    gv <- gray[neutral]
    if (length(unique(gv)) > 1) {
      set.seed(0)
      km <- try(stats::kmeans(matrix(gv, ncol = 1), centers = 2L, nstart = 5L), silent = TRUE)
      if (!inherits(km, "try-error")) {
        cs <- sort(as.numeric(km$centers))
        if (cs[2] - cs[1] >= 50) {
          thr <- mean(cs)
          masks <- list(neutral & (gray <= thr), neutral & (gray > thr))
        }
      }
    }
  } else if (sum(masks[[1]])) {
    peeled <- .km2_peel_ci_band(gray, masks[[1]])
    if (sum(peeled) < sum(masks[[1]])) {
      kp <- .km2_clean_components(peeled, span, high)
      cols_before <- sum(apply(masks[[1]], 2, any))
      if (any(kp) && sum(apply(kp, 2, any)) >= 0.7 * max(1, cols_before)) masks <- list(peeled)
    }
  }
  out <- list()
  for (m in utils::head(masks, n_needed)) {
    cm <- .km2_clean_components(m, span, high)
    if (any(cm)) out[[length(out) + 1L]] <- cm
  }
  out
}


# --------------------------------------------------------------------------- #
# Numbers at risk
# --------------------------------------------------------------------------- #

# Split a cell the OCR ran together (e.g. "141110") into k non-increasing
# integers of balanced length, which for a numbers-at-risk row is almost
# always a single possibility.
#' @keywords internal
#' @noRd
.km2_split_merged <- function(digits, k, hi) {
  n <- nchar(digits)
  if (k <= 1 || n < k) return(NULL)
  best <- NULL
  cuts <- utils::combn(seq_len(n - 1), k - 1, simplify = FALSE)
  for (cc in cuts) {
    idx <- c(0, cc, n)
    parts <- substring(digits, idx[-length(idx)] + 1, idx[-1])
    if (any(nchar(parts) > 1 & substr(parts, 1, 1) == "0")) next
    vals <- as.integer(parts)
    if (any(diff(vals) > 0)) next
    if (!is.null(hi) && vals[1] > hi) next
    score <- c(max(nchar(parts)) - min(nchar(parts)), -min(vals))
    if (is.null(best) || score[1] < best$s[1] ||
        (score[1] == best$s[1] && score[2] < best$s[2])) best <- list(s = score, v = vals)
  }
  if (is.null(best)) NULL else best$v
}


# Indices of the longest non-increasing subsequence of v.
#' @keywords internal
#' @noRd
.km2_longest_noninc <- function(v) {
  n <- length(v)
  if (n == 0) return(integer(0))
  best <- rep(1L, n); prev <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i - 1L)) {
      if (v[j] >= v[i] && best[j] + 1L > best[i]) { best[i] <- best[j] + 1L; prev[i] <- j }
    }
  }
  i <- which.max(best)
  out <- integer(0)
  while (!is.na(i)) { out <- c(i, out); i <- prev[i] }
  out
}


#' @keywords internal
#' @noRd
.km2_extract_at_risk <- function(gray, L, R, B, xcal) {
  H <- nrow(gray)
  y0 <- B + 1L
  if (y0 >= H) return(list())
  region <- gray[y0:H, , drop = FALSE]
  tk <- NULL
  for (sc in c(2, 3)) for (ps in c(6, 11)) {
    t1 <- .km2_ocr_tokens(region, sc, ps)
    if (!is.null(t1)) {
      tk <- rbind(tk, data.frame(text = t1$text, xc = t1$x + t1$w / 2, w = t1$w,
                                 yc = t1$y + t1$h / 2 + y0 - 1L, conf = t1$conf,
                                 stringsAsFactors = FALSE))
    }
  }
  if (is.null(tk) || !nrow(tk)) return(list())

  ys <- sort(unique(round(tk$yc)))
  grp <- cumsum(c(1L, as.integer(diff(ys) > 12)))
  rows <- lapply(unique(grp), function(g) range(ys[grp == g]))
  axis_vals <- if (is.null(xcal)) numeric(0) else round(xcal$ticks$val)

  out <- list()
  for (rg in rows) {
    rt <- tk[tk$yc >= rg[1] - 6 & tk$yc <= rg[2] + 6, ]
    if (!nrow(rt)) next
    cs <- vapply(rt$text, .km2_clean_num, character(1), USE.NAMES = FALSE)
    isnum <- grepl("^[0-9]+$", cs) & rt$xc >= L - 5
    nums <- rt[isnum, ]; numv <- as.integer(cs[isnum])
    labels <- rt$text[!isnum & !grepl("^[0-9.]+$", rt$text)]
    if (!nrow(nums)) next
    o <- order(nums$xc); nums <- nums[o, ]; numv <- numv[o]

    pitch <- if (nrow(nums) >= 2) stats::median(diff(nums$xc)) else 0
    xs <- numeric(0); vs <- integer(0); hi <- NULL
    for (i in seq_len(nrow(nums))) {
      k <- if (pitch > 0) round(nums$w[i] / pitch) else 1
      digits <- cs[isnum][o][i]
      parts <- if (k >= 2 && nchar(digits) >= 4)
        .km2_split_merged(digits, min(k, nchar(digits)), hi) else NULL
      if (!is.null(parts)) {
        left <- nums$xc[i] - nums$w[i] / 2
        for (j in seq_along(parts)) {
          xs <- c(xs, left + (j - 0.5) * nums$w[i] / length(parts)); vs <- c(vs, parts[j])
        }
        hi <- parts[length(parts)]
      } else { xs <- c(xs, nums$xc[i]); vs <- c(vs, numv[i]); hi <- numv[i] }
    }
    keep <- c(TRUE, diff(xs) >= 8)
    xs <- xs[keep]; vs <- vs[keep]
    if (length(vs) < 4) next

    times <- if (is.null(xcal)) NULL else round(xcal$m * xs + xcal$c, 2)
    if (!is.null(times)) {
      rng <- max(max(times) - min(times), 1)
      if (stats::median(abs(vs - times)) <= 0.06 * rng) next     # the axis tick row
    }
    if (length(axis_vals) && all(round(vs) %in% axis_vals)) next

    # Numbers at risk only decrease, so the row is the longest non-increasing
    # run of readings. Taking the longest *subsequence* rather than walking
    # forward from the maximum matters: one misread cell (a 541 read as 54)
    # would otherwise reject every larger value after it and truncate the row.
    keep_idx <- .km2_longest_noninc(vs)
    if (length(keep_idx) >= 4) {
      xs <- xs[keep_idx]; vs <- vs[keep_idx]
      times <- if (is.null(xcal)) NULL else round(xcal$m * xs + xcal$c, 2)
    }
    # A real row runs the width of the axis; a handful of readings huddled in
    # one place is stray text, not a table.
    if (length(vs) < 4 || (max(xs) - min(xs)) < 0.5 * (R - L)) next
    lab <- if (length(labels)) paste(utils::head(unique(labels), 3), collapse = " ")
           else paste0("row@", as.integer(rg[1]))
    out[[length(out) + 1L]] <- list(label = lab, values = vs, times = times,
                                    y = as.integer(mean(rg)))
  }
  out
}


# --------------------------------------------------------------------------- #
# Output
# --------------------------------------------------------------------------- #

# Scale an 8-bit image array to 0-1 for writing. pmax and pmin drop the
# dimensions of their argument, so they are applied to the values alone.
#' @keywords internal
#' @noRd
.km2_clamp01 <- function(a) {
  out <- a
  out[] <- pmax(0, pmin(255, as.numeric(a))) / 255
  out
}


#' @keywords internal
#' @noRd
.km2_step_points <- function(xs, ys) {
  n <- length(xs)
  if (n < 2) return(cbind(xs, ys))
  px <- c(xs[1], rep(xs[-1], each = 2))
  py <- c(ys[1], as.numeric(rbind(ys[-n], ys[-1])))
  cbind(round(px), round(py))
}


#' @keywords internal
#' @noRd
.km2_draw <- function(canvas, pts, col, lwd = 2L) {
  H <- dim(canvas)[1]; W <- dim(canvas)[2]
  for (i in seq_len(nrow(pts) - 1L)) {
    x1 <- pts[i, 1]; y1 <- pts[i, 2]; x2 <- pts[i + 1, 1]; y2 <- pts[i + 1, 2]
    xs <- if (x1 == x2) x1 else seq(min(x1, x2), max(x1, x2))
    ys <- if (y1 == y2) y1 else seq(min(y1, y2), max(y1, y2))
    for (dx in 0:(lwd - 1L)) for (dy in 0:(lwd - 1L)) {
      gx <- pmin(W, pmax(1L, xs + dx)); gy <- pmin(H, pmax(1L, ys + dy))
      g <- expand.grid(y = gy, x = gx)
      for (k in 1:3) canvas[cbind(g$y, g$x, k)] <- col[k]
    }
  }
  canvas
}


# --------------------------------------------------------------------------- #
# Entry point
# --------------------------------------------------------------------------- #

#' Digitize a Kaplan-Meier figure without the Python engine
#'
#' @description
#' Support engine for the automatic digitization in [km2bayesPro()]. It needs
#' only R packages, so the feature still works where the bundled Python engine
#' cannot run, and it writes the same files that engine writes: `<prefix>.csv`,
#' `<prefix>_meta.json`, `<prefix>_clean.png` and `<prefix>_overlay.png`.
#'
#' @param path Path to the figure.
#' @param prefix Path prefix for the four output files.
#' @param n_colors Number of arms to look for. Default 2.
#' @param color_tol Hue radius, in Lab units, admitted around a curve colour.
#'
#' @return (Invisibly) the metadata list that is also written as JSON.
#' @keywords internal
#' @noRd
.km2_digitize_r <- function(path, prefix, n_colors = 2L, color_tol = 20) {
  im <- .km2_read_image(path)
  H <- im$h; W <- im$w
  colorful <- .km2_colorful(im$rgb)
  bx <- .km2_detect_box(im$gray, colorful)

  yb <- max(30L, as.integer(0.13 * W))
  yc <- .km2_calibrate_y(im$gray, bx$L_axis, bx$T, yb)
  ycal <- yc$cal; fraction <- yc$fraction
  if (!is.null(ycal) && (is.na(ycal$m) || ycal$m >= 0)) ycal <- NULL
  Tt <- bx$T; B <- bx$B
  if (!is.null(ycal)) {
    topval <- max(if (fraction) 1 else 100, max(ycal$ticks$val))
    t2 <- round((topval - ycal$c) / ycal$m)
    b2 <- round((0 - ycal$c) / ycal$m)
    if (!(t2 >= -0.2 * H && t2 < b2 && b2 <= 1.2 * H) || (b2 - t2) < 0.2 * H) {
      ycal <- NULL
    } else {
      Tt <- max(1L, min(as.integer(t2), H - 2L))
      B  <- max(Tt + 10L, min(as.integer(b2), H))
    }
  }
  box <- list(L = bx$L, R = bx$R, T = Tt, B = B)
  interior <- matrix(FALSE, H, W)
  interior[Tt:B, bx$L:bx$R] <- TRUE
  span <- max(1L, bx$R - bx$L); high <- max(1L, B - Tt)

  xcal <- .km2_calibrate_x(im$gray, bx$L, bx$R, B)
  txt <- .km2_ocr_text_mask(im$gray, im$rgb)

  arms_mask <- .km2_drop_boxes(colorful & interior, span)
  arms_mask <- .km2_drop_filled_panels(arms_mask, span, high)
  n_arms <- .km2_count_arms(im$lab, arms_mask, interior)

  tol_fit <- 0.04 * high
  centroids <- .km2_find_colors(im$lab, arms_mask, interior, k = n_colors)

  # One colour family (a line plus its band or halo) must not fill both curve
  # slots when a genuine neutral arm is present, which the colour clustering
  # cannot see. The family then collapses to one band-peeled mask.
  pairs <- NULL
  if (length(centroids) == 2 && .km2_same_family(centroids[[1]], centroids[[2]])) {
    for (gm in .km2_neutral_masks(im$rgb, im$gray, colorful, interior, txt, box, 1L)) {
      gcols <- sum(apply(gm, 2, any))
      if (sum(gm) < 30 || gcols < 0.45 * span) next
      rec <- .km2_reconstruct_best(gm, bx$L, bx$R, tol_fit)
      if (is.null(rec) || rec$q > 0.30 || (max(rec$y) - min(rec$y)) < 0.08 * high) next
      hue <- matrix(FALSE, H, W)
      for (cen in centroids) {
        hue <- hue | (sqrt((im$lab$a - cen[2])^2 + (im$lab$b - cen[3])^2) < color_tol)
      }
      hue <- hue & arms_mask & interior
      union <- .km2_peel_ci_band(im$lab$L, hue)
      if (sum(union) >= 10) {
        ord <- which.min(vapply(centroids, function(z) z[1], numeric(1)))
        pairs <- list(list(cen = centroids[[ord]], mask = union))
      }
      break
    }
  }
  if (is.null(pairs)) {
    pairs <- lapply(seq_along(centroids), function(i) {
      list(cen = centroids[[i]],
           mask = .km2_line_mask(im$lab, centroids[[i]], centroids[-i],
                                 arms_mask, interior, tol = color_tol))
    })
  }

  curves <- list(); quals <- numeric(0)
  for (p in pairs) {
    m <- p$mask & !txt
    cc <- .km2_components(m)
    if (!is.null(cc$stats)) {
      keep <- cc$stats$id[cc$stats$area >= 3]
      m <- matrix(cc$labels %in% keep, H, W) & m
    }
    rec <- .km2_reconstruct_best(m, bx$L, bx$R, tol_fit)
    if (is.null(rec)) next
    col <- c(stats::median(im$rgb[, , 1][m]), stats::median(im$rgb[, , 2][m]),
             stats::median(im$rgb[, , 3][m]))
    curves[[length(curves) + 1L]] <- list(x = rec$x, y = rec$y, col = col)
    quals <- c(quals, round(rec$q, 3))
  }

  want <- if (is.null(n_colors)) 2L else n_colors
  for (gm in .km2_neutral_masks(im$rgb, im$gray, colorful, interior, txt, box,
                                want - length(curves))) {
    gcols <- sum(apply(gm, 2, any))
    if (sum(gm) < 30 || gcols < 0.45 * span) next
    rec <- .km2_reconstruct_best(gm, bx$L, bx$R, tol_fit)
    if (is.null(rec)) next
    if ((max(rec$y) - min(rec$y)) >= 0.02 * high) {
      col <- c(stats::median(im$rgb[, , 1][gm]), stats::median(im$rgb[, , 2][gm]),
               stats::median(im$rgb[, , 3][gm]))
      curves[[length(curves) + 1L]] <- list(x = rec$x, y = rec$y, col = col)
      quals <- c(quals, round(rec$q, 3))
    }
  }

  at_risk <- .km2_extract_at_risk(im$gray, bx$L, bx$R, B, xcal)

  # overlay and clean canvas
  overlay <- im$rgb
  clean <- array(255, dim = c(H, W, 3))
  palette <- list(c(200, 30, 30), c(30, 30, 200), c(20, 140, 40), c(190, 120, 10))
  for (i in seq_along(curves)) {
    pts <- .km2_step_points(curves[[i]]$x, curves[[i]]$y)
    overlay <- .km2_draw(overlay, pts, c(255, 0, 0))
    clean <- .km2_draw(clean, pts, palette[[(i - 1L) %% 4L + 1L]])
  }
  for (y in c(Tt, B)) clean[max(1, y):min(H, y + 1), bx$L:bx$R, ] <- 0
  clean[Tt:B, max(1, bx$L):min(W, bx$L + 1), ] <- 0
  png::writePNG(.km2_clamp01(overlay), paste0(prefix, "_overlay.png"))
  png::writePNG(.km2_clamp01(clean), paste0(prefix, "_clean.png"))

  con <- file(paste0(prefix, ".csv"), "w")
  writeLines("curve,x_px,y_px,x_val,y_val", con)
  for (i in seq_along(curves)) {
    cv <- curves[[i]]
    xv <- if (is.null(xcal)) rep(NA_real_, length(cv$x)) else xcal$m * cv$x + xcal$c
    yv <- if (is.null(ycal)) rep(NA_real_, length(cv$y)) else ycal$m * cv$y + ycal$c
    writeLines(sprintf("curve%d,%d,%d,%.3f,%.4f", i,
                       as.integer(cv$x) - 1L, as.integer(round(cv$y)) - 1L, xv, yv), con)
  }
  close(con)

  tick_list <- function(cal) {
    if (is.null(cal)) return(NULL)
    lapply(seq_len(nrow(cal$ticks)), function(i) list(cal$ticks$val[i], cal$ticks$px[i] - 1))
  }
  meta <- list(
    box = c(bx$L, bx$R, Tt, B) - 1L,
    fraction = fraction,
    xcal = if (is.null(xcal)) NULL else list(m = xcal$m, c = xcal$c - xcal$m, ticks = tick_list(xcal)),
    ycal = if (is.null(ycal)) NULL else list(m = ycal$m, c = ycal$c - ycal$m, ticks = tick_list(ycal)),
    n_curves = length(curves),
    arms_detected = as.integer(n_arms),
    too_many_arms = n_arms >= 3L,
    at_risk = at_risk,
    curve_offmask = quals,
    fit_ok = length(quals) > 0 && max(quals) <= 0.30,
    engine = "r"
  )
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null", digits = 10),
             paste0(prefix, "_meta.json"))
  invisible(meta)
}


# --------------------------------------------------------------------------- #
# Engine selection
# --------------------------------------------------------------------------- #

#' Digitize with the Python engine, falling back to the R one
#'
#' @description
#' Runs the bundled Python digitizer when it is available and trust its result
#' when its own self-check passes. The R engine is used when Python cannot run
#' at all (no environment, no Tesseract), when it produces nothing, or when it
#' reports an unreliable fit and the R engine reports a reliable one. Either way
#' the four output files end up under `prefix`, so the caller does not need to
#' know which engine produced them.
#'
#' @param path Path to the figure.
#' @param prefix Path prefix for the output files.
#' @param n_colors Number of arms to look for.
#' @param python Path to a Python interpreter with the engine's modules, or NULL.
#' @param script Path to `kmdig3.py`, or NULL.
#'
#' @return A list with `meta` (the metadata of the chosen run, NULL if both
#'   failed), `engine` ("python" or "r"), and `log` (the Python output).
#' @keywords internal
#' @noRd
.km2_digitize_auto <- function(path, prefix, n_colors = 2L, python = NULL, script = NULL) {
  read_meta <- function(pre) {
    f <- paste0(pre, "_meta.json")
    if (!file.exists(f)) return(NULL)
    tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  }
  py_meta <- NULL; log <- character(0)
  if (!is.null(python) && !is.null(script) && nzchar(python) && nzchar(script) &&
      file.exists(python) && file.exists(script)) {
    log <- tryCatch(
      suppressWarnings(system2(python,
        args = shQuote(c(script, path, prefix, as.character(n_colors))),
        stdout = TRUE, stderr = TRUE)),
      error = function(e) conditionMessage(e))
    py_meta <- read_meta(prefix)
  }
  if (isTRUE(py_meta$fit_ok)) return(list(meta = py_meta, engine = "python", log = log))

  # Python is missing, produced nothing, or does not trust its own trace.
  r_pre <- paste0(prefix, "__r")
  r_meta <- tryCatch({
    .km2_digitize_r(path, r_pre, n_colors = n_colors)
    read_meta(r_pre)
  }, error = function(e) NULL)

  take_r <- !is.null(r_meta) && (is.null(py_meta) || isTRUE(r_meta$fit_ok))
  if (!take_r) {
    unlink(paste0(r_pre, c(".csv", "_meta.json", "_clean.png", "_overlay.png")))
    return(list(meta = py_meta, engine = "python", log = log))
  }
  for (sfx in c(".csv", "_meta.json", "_clean.png", "_overlay.png")) {
    src <- paste0(r_pre, sfx)
    if (file.exists(src)) file.rename(src, paste0(prefix, sfx))
  }
  list(meta = read_meta(prefix), engine = "r", log = log)
}
