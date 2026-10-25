# --------------------------------------------------------------------------- #
# Agreement between a digitized curve and its numbers-at-risk row.
#
# The two carry overlapping information and can contradict each other. Over an
# interval between two ticks, every patient who leaves the risk set is either an
# event or a censoring, so the steepest fall the curve can take is the one where
# all of them are events:
#
#     S(t1) / S(t0)  >=  n(t1) / n(t0)
#
# A curve that falls faster than that cannot be produced by the stated numbers.
# The reconstruction does not refuse such an interval: it honours the table and
# trims events, which flattens that arm and moves the treatment effect. Checking
# first turns a silent bias into a question the user can answer.
# --------------------------------------------------------------------------- #


# Step function value at t: the last digitized point at or before it.
#' @keywords internal
#' @noRd
.km2_surv_at <- function(time, surv, t) {
  k <- which(time <= t)
  if (!length(k)) return(if (length(surv)) surv[which.min(time)] else NA_real_)
  surv[k[which.max(time[k])]]
}


#' Check one arm's numbers at risk against its digitized curve
#'
#' @param time,surv The digitized curve, survival on any consistent scale.
#' @param ticks,nrisk The numbers-at-risk row and the times it is given at.
#' @param tol_abs,tol_rel How far the numbers may fall short before the interval
#'   counts as contradicted, as a number of patients and as a share of the risk
#'   set. A curve read one pixel high or low moves the implied events by well
#'   under one patient, so a shortfall of one or two is reading noise rather
#'   than a contradiction, and flagging it would only teach the user to ignore
#'   the warning.
#'
#' @return A data frame with one row per interval: the numbers at each end, how
#'   many leave, the survival at each end, the events that fall implies, the
#'   largest number at risk that would be consistent, how far the stated number
#'   exceeds it, and whether the interval stands.
#' @keywords internal
#' @noRd
.km2_atrisk_consistency <- function(time, surv, ticks, nrisk,
                                    tol_abs = 2, tol_rel = 0.02) {
  o <- order(ticks); ticks <- ticks[o]; nrisk <- nrisk[o]
  n <- length(ticks)
  if (n < 2 || !length(time)) return(NULL)
  out <- do.call(rbind, lapply(seq_len(n - 1), function(i) {
    n0 <- nrisk[i]; n1 <- nrisk[i + 1]
    s0 <- .km2_surv_at(time, surv, ticks[i])
    s1 <- .km2_surv_at(time, surv, ticks[i + 1])
    if (is.na(s0) || is.na(s1) || s0 <= 0 || n0 <= 0) return(NULL)
    ratio_s <- s1 / s0
    n1_max <- floor(n0 * ratio_s + 1e-9)
    short <- n1 - n1_max
    data.frame(t0 = ticks[i], t1 = ticks[i + 1], n0 = n0, n1 = n1,
               exits = n0 - n1, s0 = s0, s1 = s1,
               events_implied = n0 * (1 - ratio_s),
               n1_max = n1_max, shortfall = short,
               feasible = short <= max(tol_abs, tol_rel * n0))
  }))
  out
}


#' Report where a numbers-at-risk table contradicts its curve
#'
#' @param curves A list of data frames with `time` and `surv`, one per arm.
#' @param ticks The times the table is given at.
#' @param rows A list of numbers-at-risk rows, in the same order as `curves`.
#' @param labels Arm names used in the messages.
#'
#' @return A list with `detail` (the per-interval table for every arm), `ok`
#'   (TRUE when nothing contradicts) and `messages`, one sentence per offending
#'   interval naming the value that would resolve it.
#' @keywords internal
#' @noRd
.km2_atrisk_report <- function(curves, ticks, rows, labels = NULL) {
  if (is.null(labels)) labels <- paste("Arm", seq_along(curves))
  detail <- list(); msg <- character(0)
  for (i in seq_along(curves)) {
    d <- .km2_atrisk_consistency(curves[[i]]$time, curves[[i]]$surv, ticks, rows[[i]])
    if (is.null(d)) next
    d$arm <- labels[i]
    detail[[length(detail) + 1L]] <- d
    for (k in which(!d$feasible)) {
      msg <- c(msg, sprintf(
        paste("%s, %g to %g: the curve falls from %.1f to %.1f, which takes about %d events,",
              "but only %d patients leave the risk set. The table is short by %d; the number",
              "at risk at %g would have to be %d or fewer to match the curve."),
        labels[i], d$t0[k], d$t1[k], d$s0[k], d$s1[k], round(d$events_implied[k]),
        d$exits[k], d$shortfall[k], d$t1[k], d$n1_max[k]))
    }
  }
  detail <- if (length(detail)) do.call(rbind, detail) else NULL
  list(detail = detail, ok = !length(msg), messages = msg)
}


#' Does swapping the table rows between arms fit the curves better?
#'
#' A numbers-at-risk row read from a figure is not labelled by arm, so it can be
#' paired with the wrong curve. A wrong pairing usually contradicts the curves in
#' many intervals while the right one contradicts few, which makes the count of
#' offending intervals a usable test.
#'
#' @inheritParams .km2_atrisk_report
#' @return A list with `swap` (TRUE when the other pairing fits better) and the
#'   number of offending intervals each way.
#' @keywords internal
#' @noRd
.km2_atrisk_pairing <- function(curves, ticks, rows) {
  if (length(curves) != 2 || length(rows) != 2) return(list(swap = FALSE))
  bad <- function(rs) {
    sum(vapply(seq_along(curves), function(i) {
      d <- .km2_atrisk_consistency(curves[[i]]$time, curves[[i]]$surv, ticks, rs[[i]])
      if (is.null(d)) 0L else sum(!d$feasible)
    }, integer(1)))
  }
  a <- bad(rows); b <- bad(rows[c(2, 1)])
  list(swap = b < a, bad_as_is = a, bad_swapped = b)
}
