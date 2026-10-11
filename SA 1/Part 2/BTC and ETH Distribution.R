# Section II: Distribution fitting (BTC/USD, 1-min) and Shapiro-Wilk test (ETH, 5-min)
#
# Usage (terminal):
#   Rscript crypto_returns_analysis.R btc_1min.csv eth_5min.csv results
# Or in RStudio: set the three variables below and Source the file.
#
# Needs: data.table  (install.packages("data.table"))
# CSV needs a timestamp column (Unix sec/ms or date-time text) and a Close/Price column.
# Kaggle format "Timestamp,Open,High,Low,Close,Volume" works as is.

suppressWarnings(suppressMessages(library(data.table)))
set.seed(42)

############ >>> SET YOUR FILE NAMES HERE <<< ############
# Use the EXACT file names (including .csv). The files must be in the R working
# directory, or give the full path (e.g. "C:/Users/USER/Downloads/btc_1min.csv",
# using forward slashes /). eth_path may also be a folder of csv files; Part 2 is
# skipped if the ETH file is not found.
btc_path <- "btc_1min.csv"
eth_path <- "eth_5min.csv"
outdir   <- "results"
###########################################################
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------ helpers
# Reads ONE csv -> data.table(ts, p). Handles:
#  - files with a header (Kaggle: Timestamp, Open, ..., Close, Volume)
#  - headerless files (Kraken OHLCVT: time,open,high,low,close,volume,trades;
#    Binance klines: open time,open,high,low,close,...) -> col 1 = time, col 5 = close
#  - timestamps in seconds, milliseconds or microseconds (detected per file)
read_one <- function(f) {
  dt <- fread(f, integer64 = "double")   # big integer timestamps (e.g. microseconds) -> double
  nms <- tolower(names(dt))
  if (all(grepl("^v[0-9]+$", nms))) {
    tcol <- names(dt)[1]; pcol <- names(dt)[5]
  } else {
    tcol <- names(dt)[match(TRUE, nms %in% c("timestamp", "date", "time", "datetime", "open time"))]
    pcol <- names(dt)[match(TRUE, nms %in% c("close", "price", "close price"))]
    if (is.na(tcol) || is.na(pcol)) stop("Could not find timestamp / close columns in ", f)
  }
  t <- dt[[tcol]]
  if (is.numeric(t)) {
    m <- median(t, na.rm = TRUE)
    div <- if (m > 1e14) 1e6 else if (m > 1e11) 1e3 else 1
    ts <- as.POSIXct(t / div, origin = "1970-01-01", tz = "UTC")
  } else {
    ts <- as.POSIXct(t, tz = "UTC")
  }
  data.table(ts = ts, p = as.numeric(dt[[pcol]]))
}

# `path` may be one csv file OR a folder (all .csv files inside are combined,
# e.g. Binance monthly files). Put only ONE interval/pair per folder.
load_returns <- function(path, start, end) {
  files <- if (dir.exists(path)) list.files(path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) else path
  if (length(files) == 0) stop("No csv files found at: ", path)
  d <- rbindlist(lapply(files, read_one))
  d <- d[!is.na(ts) & !is.na(p) & p > 0]
  setorder(d, ts)
  d <- unique(d, by = "ts")
  cat(sprintf("  %s: data available %s to %s (%d rows)\n", basename(path),
              format(min(d$ts), "%Y-%m-%d"), format(max(d$ts), "%Y-%m-%d"), nrow(d)))
  d <- d[ts >= as.POSIXct(start, tz = "UTC") & ts <= as.POSIXct(paste(end, "23:59:59"), tz = "UTC")]
  cat(sprintf("  used in analysis (requested window %s to %s): %s to %s, %d rows\n", start, end,
              format(min(d$ts), "%Y-%m-%d"), format(max(d$ts), "%Y-%m-%d"), nrow(d)))
  r <- diff(log(d$p))
  r[is.finite(r)]
}

skew <- function(x) { m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
exkurt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 - 3 }

describe <- function(r) {
  data.frame(stat = c("n", "mean", "sd", "skewness", "excess_kurtosis", "min", "max"),
             value = c(length(r), mean(r), sd(r), skew(r), exkurt(r), min(r), max(r)))
}

# ------------------------------------------------------------- distributions
# each: logpdf(x, par), cdf(q, par), start(x), and par transform handled inside
# numerically stable log(cosh(a)); avoids exp() overflow on extreme returns
logcosh <- function(a) abs(a) + log1p(exp(-2 * abs(a))) - log(2)

# robust starting scale: many 1-min returns are exactly 0, so mad() can be 0
rscale <- function(x) {
  s <- mad(x)
  if (!is.finite(s) || s <= 0) s <- IQR(x) / 1.349
  if (!is.finite(s) || s <= 0) s <- sd(x)
  s
}

dists <- list(
  "Normal" = list(
    npar = 2,
    fit = function(x) c(mean(x), sqrt(mean((x - mean(x))^2))),
    logpdf = function(x, p) dnorm(x, p[1], p[2], log = TRUE),
    cdf = function(q, p) pnorm(q, p[1], p[2])),
  "Laplace" = list(
    npar = 2,
    fit = function(x) { m <- median(x); c(m, mean(abs(x - m))) },
    logpdf = function(x, p) -log(2 * p[2]) - abs(x - p[1]) / p[2],
    cdf = function(q, p) { z <- (q - p[1]) / p[2]; ifelse(z < 0, 0.5 * exp(z), 1 - 0.5 * exp(-z)) }),
  "Student-t" = list(
    npar = 3,
    start = function(x) c(median(x), log(rscale(x)), log(4)),
    nll = function(th, x) { s <- exp(th[2]); -sum(dt((x - th[1]) / s, exp(th[3]), log = TRUE) - log(s)) },
    transform = function(th) c(th[1], exp(th[2]), exp(th[3])),
    logpdf = function(x, p) dt((x - p[1]) / p[2], p[3], log = TRUE) - log(p[2]),
    cdf = function(q, p) pt((q - p[1]) / p[2], p[3])),
  "Logistic" = list(
    npar = 2,
    start = function(x) c(median(x), log(rscale(x))),
    nll = function(th, x) -sum(dlogis(x, th[1], exp(th[2]), log = TRUE)),
    transform = function(th) c(th[1], exp(th[2])),
    logpdf = function(x, p) dlogis(x, p[1], p[2], log = TRUE),
    cdf = function(q, p) plogis(q, p[1], p[2])),
  "Cauchy" = list(
    npar = 2,
    start = function(x) c(median(x), log(rscale(x))),
    nll = function(th, x) -sum(dcauchy(x, th[1], exp(th[2]), log = TRUE)),
    transform = function(th) c(th[1], exp(th[2])),
    logpdf = function(x, p) dcauchy(x, p[1], p[2], log = TRUE),
    cdf = function(q, p) pcauchy(q, p[1], p[2])),
  "Hyperbolic secant" = list(
    npar = 2,
    start = function(x) c(median(x), log(rscale(x))),
    nll = function(th, x) { s <- exp(th[2]); z <- (x - th[1]) / s
    -sum(-log(2 * s) - logcosh(pi / 2 * z)) },
    transform = function(th) c(th[1], exp(th[2])),
    logpdf = function(x, p) { z <- (x - p[1]) / p[2]
    -log(2 * p[2]) - logcosh(pi / 2 * z) },
    cdf = function(q, p) 2 / pi * atan(exp(pi / 2 * (q - p[1]) / p[2]))),
  "Gen. normal" = list(
    npar = 3,
    start = function(x) c(median(x), log(rscale(x)), log(1.2)),
    nll = function(th, x) { s <- exp(th[2]); b <- exp(th[3])
    -sum(log(b) - log(2 * s) - lgamma(1 / b) - (abs(x - th[1]) / s)^b) },
    transform = function(th) c(th[1], exp(th[2]), exp(th[3])),
    logpdf = function(x, p) log(p[3]) - log(2 * p[2]) - lgamma(1 / p[3]) - (abs(x - p[1]) / p[2])^p[3],
    cdf = function(q, p) { z <- q - p[1]
    0.5 + sign(z) * 0.5 * pgamma((abs(z) / p[2])^p[3], shape = 1 / p[3]) }),
  "Johnson SU" = list(
    npar = 4,   # xi, lambda, gamma, delta
    start = function(x) c(median(x), log(rscale(x)), 0, log(1)),
    nll = function(th, x) { lam <- exp(th[2]); del <- exp(th[4]); u <- (x - th[1]) / lam
    z <- th[3] + del * asinh(u)
    -sum(log(del) - log(lam) - 0.5 * log(2 * pi) - 0.5 * log1p(u^2) - 0.5 * z^2) },
    transform = function(th) c(th[1], exp(th[2]), th[3], exp(th[4])),
    logpdf = function(x, p) { u <- (x - p[1]) / p[2]; z <- p[3] + p[4] * asinh(u)
    log(p[4]) - log(p[2]) - 0.5 * log(2 * pi) - 0.5 * log1p(u^2) - 0.5 * z^2 },
    cdf = function(q, p) pnorm(p[3] + p[4] * asinh((q - p[1]) / p[2])))
)

fit_one <- function(d, x) {
  if (!is.null(d$fit)) return(d$fit(x))
  # non-finite likelihood values are replaced by a large penalty so optim() can recover
  safe_nll <- function(th, x) {
    p <- d$transform(th)
    sc <- if (length(p) == 4) p[2] / p[4] else p[2]      # effective scale
    if (!is.finite(sc) || sc < 1e-7) return(1e15)         # block degenerate spikes (scale -> 0)
    if (length(p) == 3 && p[3] < 0.1) return(1e15)       # block df / shape -> 0
    v <- tryCatch(d$nll(th, x), error = function(e) NA_real_)
    if (!is.finite(v)) 1e15 else v
  }
  best <- NULL
  # try 3 starting scales and keep the best result
  for (shift in log(c(1, 0.1, 10))) {
    st <- d$start(x); st[2] <- st[2] + shift
    for (meth in c("Nelder-Mead", "BFGS")) {
      o <- tryCatch(optim(st, safe_nll, x = x, method = meth,
                          control = list(maxit = 5000, reltol = 1e-12)), error = function(e) NULL)
      if (!is.null(o) && is.finite(o$value) && o$value < 1e14) {
        st <- o$par
        if (is.null(best) || o$value < best$value) best <- o
      }
    }
  }
  if (is.null(best)) stop("optimisation failed")
  d$transform(best$par)
}

# ------------------------------------------------------------------- Part 1
fit_distributions <- function(r, outdir, max_n = 150000) {
  # Many 1-min returns are exactly 0 (price unchanged). That point mass makes the
  # likelihood of any continuous density unbounded (the fit collapses onto the spike),
  # so the distributions are fitted to the NON-ZERO returns; the zero share is reported.
  zero_share <- mean(r == 0)
  cat(sprintf("Fitting on the %.1f%% non-zero returns (%.1f%% of returns are exactly 0).\n",
              100 * (1 - zero_share), 100 * zero_share))
  write.csv(data.frame(zero_share = zero_share, n_nonzero = sum(r != 0)),
            file.path(outdir, "btc_zero_share.csv"), row.names = FALSE)
  r <- r[r != 0]
  xs <- if (length(r) > max_n) sample(r, max_n) else r
  rows <- list(); fitted <- list()
  for (nm in names(dists)) {
    d <- dists[[nm]]
    res <- tryCatch({
      p <- fit_one(d, xs)
      ll <- sum(d$logpdf(xs, p))
      if (!is.finite(ll)) stop("non-finite loglik")
      ks <- suppressWarnings(ks.test(xs, function(q) d$cdf(q, p)))
      k <- d$npar
      list(p = p, row = data.frame(Distribution = nm, k = k, LogLik = ll,
                                   AIC = 2 * k - 2 * ll, BIC = k * log(length(xs)) - 2 * ll,
                                   KS_stat = unname(ks$statistic), KS_p = ks$p.value))
    }, error = function(e) { message("fit failed for ", nm, ": ", conditionMessage(e)); NULL })
    if (!is.null(res)) { fitted[[nm]] <- res$p; rows[[nm]] <- res$row }
  }
  tbl <- do.call(rbind, rows)
  tbl <- tbl[order(tbl$AIC), ]
  tbl$dAIC <- tbl$AIC - min(tbl$AIC)
  rownames(tbl) <- NULL
  write.csv(tbl, file.path(outdir, "btc_fit_table.csv"), row.names = FALSE)
  best <- tbl$Distribution[1]
  
  # figure: top-4 densities (log y) + Q-Q of best vs Normal
  png(file.path(outdir, "btc_distribution_fit.png"), width = 1800, height = 750, res = 150)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  rng <- quantile(xs, c(0.0005, 0.9995))
  h <- hist(xs[xs >= rng[1] & xs <= rng[2]], breaks = 300, plot = FALSE)
  plot(h$mids, h$density, log = "y", type = "h", col = "grey70", ylim = c(1e-2, max(h$density) * 3),
       xlab = "1-min log return", ylab = "Density (log scale)",
       main = "BTC/USD 1-min returns: top-4 fits")
  g <- seq(rng[1], rng[2], length.out = 800)
  cols <- c("red", "blue", "darkgreen", "orange")
  top <- head(tbl$Distribution, 4)
  for (i in seq_along(top)) lines(g, exp(dists[[top[i]]]$logpdf(g, fitted[[top[i]]])), col = cols[i], lwd = 2)
  legend("topright", c("Empirical", top), col = c("grey70", cols[seq_along(top)]), lwd = 2, bty = "n")
  # Q-Q: empirical quantiles vs theoretical quantiles (numeric inversion of the cdf)
  qs <- seq(0.001, 0.999, length.out = 400)
  emp <- quantile(xs, qs)
  inv <- function(nm, qv) {
    d <- dists[[nm]]; p <- fitted[[nm]]
    sapply(qv, function(pp) uniroot(function(z) d$cdf(z, p) - pp,
                                    interval = c(-1, 1), extendInt = "yes", tol = 1e-12)$root)
  }
  tb <- inv(best, qs); tn <- inv("Normal", qs)
  plot(tb, emp, pch = 16, cex = .5, col = "blue", xlab = "Theoretical quantile", ylab = "Empirical quantile",
       main = "Q-Q plot", xlim = range(c(tb, tn)), ylim = range(emp))
  points(tn, emp, pch = 16, cex = .5, col = "red")
  abline(0, 1, lty = 2)
  legend("topleft", c(best, "Normal"), col = c("blue", "red"), pch = 16, bty = "n")
  dev.off()
  list(tbl = tbl, best = best, par = fitted[[best]])
}

# ------------------------------------------------------------------- Part 2
shapiro_eth <- function(r, outdir, n_sub = 5000, n_rep = 200) {
  # shapiro.test() in R only accepts 3 <= n <= 5000, so test random 5000-obs samples
  n <- min(n_sub, length(r))
  s <- sample(r, n)
  sw <- shapiro.test(s)
  reps <- replicate(n_rep, { w <- shapiro.test(sample(r, n)); c(w$statistic, w$p.value) })
  res <- data.frame(N_full = length(r), N_sample = n, W_single = unname(sw$statistic),
                    p_single = sw$p.value, W_mean_over_reps = mean(reps[1, ]),
                    median_p_over_reps = median(reps[2, ]),
                    share_reps_reject_5pct = mean(reps[2, ] < 0.05), reps = n_rep)
  write.csv(res, file.path(outdir, "eth_shapiro.csv"), row.names = FALSE)
  
  N <- length(r); S <- skew(r); K <- exkurt(r) + 3
  jb <- N / 6 * (S^2 + (K - 3)^2 / 4)
  extra <- data.frame(JB_stat = jb, JB_p = pchisq(jb, 2, lower.tail = FALSE), skew = S, excess_kurt = K - 3)
  write.csv(extra, file.path(outdir, "eth_extra_normality.csv"), row.names = FALSE)
  
  png(file.path(outdir, "eth_normality.png"), width = 1800, height = 750, res = 150)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  rng <- quantile(r, c(0.001, 0.999))
  h <- hist(r[r >= rng[1] & r <= rng[2]], breaks = 250, plot = FALSE)
  plot(h$mids, h$density, log = "y", type = "h", col = "grey70", ylim = c(1e-2, max(h$density) * 3),
       xlab = "5-min log return", ylab = "Density (log scale)", main = "ETH/USD 5-min returns vs Normal")
  g <- seq(rng[1], rng[2], length.out = 600)
  lines(g, dnorm(g, mean(r), sd(r)), col = "red", lwd = 2)
  legend("topright", c("Empirical", "Normal"), col = c("grey70", "red"), lwd = 2, bty = "n")
  qqnorm(s, main = "Normal Q-Q (5000-obs sample)", pch = 16, cex = .4); qqline(s, col = "red")
  dev.off()
  list(res = res, extra = extra)
}

# --------------------------------------------------------------------- main
cat("=== Part 1: BTC/USD 1-minute returns ===\n")
rb <- load_returns(btc_path, "2012-01-01", "2026-09-15")
db <- describe(rb); print(db); write.csv(db, file.path(outdir, "btc_descriptives.csv"), row.names = FALSE)
cat(sprintf("Share of exactly-zero 1-min returns: %.1f%%\n", 100 * mean(rb == 0)))
f <- fit_distributions(rb, outdir)
print(f$tbl, digits = 6)
cat("\nBest fit by AIC:", f$best, "| params:", signif(f$par, 5), "\n")

cat("\n=== Part 2: ETH/USD 5-minute returns ===\n")
if (file.exists(eth_path) || dir.exists(eth_path)) {
  re <- load_returns(eth_path, "2015-08-07", "2026-09-15")
  de <- describe(re); print(de); write.csv(de, file.path(outdir, "eth_descriptives.csv"), row.names = FALSE)
  cat(sprintf("Share of exactly-zero 5-min returns: %.1f%%\n", 100 * mean(re == 0)))
  s <- shapiro_eth(re, outdir)
  print(t(s$res)); print(t(s$extra))
} else {
  cat("ETH file not found (", eth_path, ") -> Part 2 skipped. Run again once you have it.\n")
}
cat("\nAll tables and figures saved in:", normalizePath(outdir), "\n")