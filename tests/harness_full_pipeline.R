# Headless replica of km2bayesPro run_core_analysis for one image: kmdig3 -> clean
# + at-risk, SurvdigitizeR digitize, curve<->risk mapping, IPD reconstruction,
# survfit. Prints diagnostics and saves the reconstructed KM to <out>.png so the
# whole chain can be inspected and iterated on outside Shiny.
#
# Usage: Rscript tests/harness_full_pipeline.R image.png out_prefix

suppressWarnings(suppressMessages({
  library(jsonlite); library(survival); library(dplyr); library(survminer)
}))
source("R/km2_setup_python.R")
PY <- Sys.getenv("KM2_PY", "C:/Users/Usuario/Documents/.virtualenvs/r-bystools/Scripts/python.exe")
ENGINE <- "inst/python/kmdig3.py"
args <- commandArgs(trailingOnly = TRUE)
img <- args[1]; outp <- if (length(args) >= 2) args[2] else tempfile()

# 1. kmdig3 -> clean + meta -------------------------------------------------------
pre <- tempfile()
system2(PY, shQuote(c(ENGINE, img, pre, "2")), stdout = FALSE, stderr = FALSE)
meta  <- fromJSON(paste0(pre, "_meta.json"), simplifyVector = FALSE)
clean <- paste0(pre, "_clean.png")
cat("clean:", file.exists(clean), " meta n_curves:", meta$n_curves,
    " offmask:", paste(unlist(meta$curve_offmask), collapse=","), "\n")

yt <- tryCatch(sort(unique(as.numeric(vapply(meta$ycal$ticks, function(t) t[[1]], numeric(1))))), error=function(e) NULL)
ax_y <- .km2_infer_axis(yt, zero_start = TRUE)
xt <- tryCatch(sort(unique(as.numeric(vapply(meta$xcal$ticks, function(t) t[[1]], numeric(1))))), error=function(e) NULL)
ax_x <- .km2_infer_axis(xt, zero_start = TRUE)
rt <- tryCatch(.km2_build_risk_table(meta, ax_x)$table, error=function(e) NULL)
xs <- ax_x$start; xe <- ax_x$end; xi <- ax_x$inc
# kmdig3 OCR of the at-risk table is unreliable on this figure; inject the real
# TOPAZ-1 numbers (times 0..33 by 3) so we can debug curve<->risk mapping cleanly.
# G1 column = Durvalumab (slower decline), G2 column = Placebo (faster).
if (is.null(rt) || nrow(rt) < 2 || Sys.getenv("KM2_FORCE_RT") == "1") {
  rt <- data.frame(
    Time = seq(0, 33, by = 3),
    N_Risk_G1 = c(341,324,294,268,240,208,169,134, 96, 74, 52, 33),  # Durvalumab
    N_Risk_G2 = c(344,316,282,241,198,175,138,104, 76, 53, 37, 21)   # Placebo
  )
  if (Sys.getenv("KM2_SHORT_RT") == "1") rt <- rt[rt$Time <= 21, ]   # simulate OCR that stops early
  cat("(using injected TOPAZ at-risk, last time =", max(rt$Time), ")\n")
}
cat("risk table rows:", nrow(rt), "\n"); print(utils::head(rt, 4))

# 2. SurvdigitizeR on the clean image --------------------------------------------
raw <- .km2_survdigitize_robust(img_path=clean, x_start=xs, x_end=xe, x_increment=xi,
        y_start=ax_y$start, y_end=ax_y$end, y_increment=ax_y$inc, num_curves=2,
        censoring=FALSE, bg_lightness=0.3, enhance=FALSE, y_text_vertical=FALSE)
raw$survival <- if (max(raw$St, na.rm=TRUE) > 1.5) raw$St/100 else raw$St

# Alternative: use kmdig3's own separated curves (CSV) instead of re-digitizing the
# clean image with SurvdigitizeR (which re-merges same-hue arms).
if (Sys.getenv("KM2_USE_CSV") == "1") {
  csv <- read.csv(paste0(pre, ".csv"))
  raw <- data.frame(
    time = csv$x_val,
    St   = csv$y_val,
    survival = csv$y_val,
    curve = as.integer(gsub("curve", "", csv$curve))
  )
  raw <- raw[raw$time >= 0, ]
  cat("(using kmdig3 CSV curves)\n")
}
cat("\ndigitized curves:", paste(sort(unique(raw$curve)), collapse=","), " rows:", nrow(raw), "\n")
for (c in sort(unique(raw$curve))) {
  s <- raw[raw$curve==c,]
  cat(sprintf("  curve %s: mean surv=%.2f  surv range %.2f-%.2f  t range %.0f-%.0f\n",
              c, mean(s$survival,na.rm=T), min(s$survival,na.rm=T), max(s$survival,na.rm=T),
              min(s$time,na.rm=T), max(s$time,na.rm=T)))
}

# 3. mapping curves <-> risk rows (app logic) ------------------------------------
map_curves <- function(raw_data, risk_table) {
  curves <- unique(raw_data$curve)
  if (length(curves) < 2) return(list(curve_to_G1=curves[1], curve_to_G2=curves[1]))
  surv_avg <- sapply(curves, function(c) mean(raw_data$survival[raw_data$curve==c], na.rm=TRUE))
  cb <- curves[which.max(surv_avg)]; cw <- curves[which.min(surv_avg)]
  lp <- function(x) as.numeric(gsub("[^0-9.]","",as.character(x)))
  s1 <- sum(lp(risk_table$N_Risk_G1),na.rm=T); s2 <- sum(lp(risk_table$N_Risk_G2),na.rm=T)
  cat(sprintf("map: best-surv curve=%s worst=%s  sum_G1=%.0f sum_G2=%.0f\n", cb, cw, s1, s2))
  if (s1 >= s2) list(curve_to_G1=cb, curve_to_G2=cw) else list(curve_to_G1=cw, curve_to_G2=cb)
}
fix_mono <- function(x){x<-as.numeric(x); if(length(x)<2) return(x); for(i in 2:length(x)) if(!is.na(x[i])&&!is.na(x[i-1])&&x[i]>x[i-1]) x[i]<-x[i-1]; x}
rt$N_Risk_G1 <- fix_mono(rt$N_Risk_G1); rt$N_Risk_G2 <- fix_mono(rt$N_Risk_G2)
cm <- map_curves(raw, rt)

reconstruct_ipd_safe <- function(km_df, nr_df) {
  km_df <- km_df[,c("time","St")]; nr_df <- nr_df[,c("time_tick","nrisk")]
  tl<-suppressWarnings(max(nr_df$time_tick,na.rm=T)); tc<-suppressWarnings(max(km_df$time,na.rm=T))
  if (is.finite(tc)&&is.finite(tl)&&tc>tl+1e-6) { nr_df<-rbind(nr_df,data.frame(time_tick=tc,nrisk=nr_df$nrisk[which.max(nr_df$time_tick)])); nr_df<-nr_df[order(nr_df$time_tick),] }
  eps<-1e-6; ticks<-sort(nr_df$time_tick); extra<-list()
  for (i in seq_len(length(ticks)-1)) {
    interior <- km_df$time[km_df$time>ticks[i]+eps & km_df$time<ticks[i+1]-eps]
    if (!length(interior)) { tm<-(ticks[i]+ticks[i+1])/2
      extra[[length(extra)+1]] <- data.frame(time=tm, St=approx(km_df$time,km_df$St,xout=tm,method="constant",rule=2)$y) }
  }
  if (length(extra)) { km_df<-rbind(km_df,do.call(rbind,extra)); km_df<-km_df[order(km_df$time),] }
  bayescores::reconstruct_ipd(km_df, nr_df, verbose=FALSE)
}

lp <- function(x) as.numeric(gsub("[^0-9.]","",as.character(x)))
df1 <- rt %>% transmute(time_tick=as.numeric(Time), nrisk=lp(N_Risk_G1), curve=cm$curve_to_G1) %>% filter(!is.na(time_tick),!is.na(nrisk))
df2 <- rt %>% transmute(time_tick=as.numeric(Time), nrisk=lp(N_Risk_G2), curve=cm$curve_to_G2) %>% filter(!is.na(time_tick),!is.na(nrisk))
nr_all <- bind_rows(df1, df2)

ipd_list <- list()
for (cid in unique(nr_all$curve)) {
  km <- subset(raw, curve==cid); nr <- subset(nr_all, curve==cid)
  km <- km[km$survival>0,]; nr <- nr[nr$nrisk>0,]; nr <- distinct(nr, time_tick, .keep_all=TRUE)
  if (!"St" %in% names(km)) km$St <- km$survival
  if (nrow(nr)>=2 && nrow(km)>0) {
    rec <- reconstruct_ipd_safe(km[,c("time","St")], nr)$ipd
    rec$arm <- if (cid==cm$curve_to_G1) "Group 1" else "Group 2"
    ipd_list[[length(ipd_list)+1]] <- rec
  }
}
final <- bind_rows(ipd_list); final$arm <- factor(final$arm, levels=c("Group 1","Group 2"))
cox <- coxph(Surv(time,status)~arm, final); hr <- exp(coef(cox)[1])
cat(sprintf("\ninitial HR(G2 vs G1)=%.3f\n", hr))
if (!is.na(hr) && hr > 1) { final$arm <- factor(ifelse(final$arm=="Group 1","Group 2","Group 1"), levels=c("Group 1","Group 2")); cat("  -> arms swapped (HR>1)\n") }

fit <- survfit(Surv(time,status)~arm, data=final)
med <- summary(fit)$table[,"median"]
cat("\n=== RESULT ===\n"); print(med)
cat(sprintf("max IPD time per arm: %s   (curves reach %.1f; risk table last time %.0f)\n",
            paste(round(tapply(final$time, final$arm, max),1), collapse=" / "),
            max(raw$time, na.rm=TRUE), max(rt$Time, na.rm=TRUE)))
cat("real TOPAZ: Durvalumab median 12.9, Placebo 11.3, HR 0.76\n")
cat("n per arm:", paste(table(final$arm),collapse=" / "), " events:", paste(tapply(final$status,final$arm,sum),collapse=" / "),"\n")
# at-risk assignment check: which Group got which risk row
cat(sprintf("at-risk sums: G1 row sum=%.0f (curve %s), G2 row sum=%.0f (curve %s)\n",
            sum(lp(rt$N_Risk_G1)), cm$curve_to_G1, sum(lp(rt$N_Risk_G2)), cm$curve_to_G2))

png(paste0(outp,".png"), width=1000, height=560)
print(ggsurvplot(fit, data=final, risk.table=TRUE, pval=TRUE, palette=c("#F8766D","#00BFC4")))
dev.off()
cat("saved plot:", paste0(outp,".png"), "\n")
