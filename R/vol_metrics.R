# Advanced realized-volatility metrics: spot/vol correlation and vol-of-vol.
# Promoted from Tuser/vol/logic/vol_metrics.R into Tdata so the vol UI,
# /analyze, and scan/wheel share one implementation.
# These describe the volatility *surface/quirks*, not the underlying-move
# projection (see atr_move.R / IV / HV for that).

#' Compute spot/vol correlation
#'
#' Historical correlation between underlying price returns and changes in
#' realized volatility (the leverage effect for equities).
#'
#' @param sym string - ticker symbol
#' @param lookback_days numeric - trading days to analyze (default 90)
#' @param vol_window numeric - rolling window for realized vol (default 10)
#' @return list with correlation value and interpretation, or NULL on error
#' @export
compute_spot_vol_correlation <- function(sym, lookback_days = 90, vol_window = 10) {
  tryCatch({
    # Fetch historical prices with buffer for weekends/holidays (trading days ~ 5/7 of calendar)
    fetch_days <- ceiling((lookback_days + vol_window) * 7 / 5) + 10
    from_date <- Sys.Date() - fetch_days
    to_date <- Sys.Date()

    prices <- getSymIntervalDate(sym, from_date, to_date)
    if (is.null(prices) || nrow(prices) < (lookback_days + vol_window)) {
      logger::log_warn("Insufficient price data for spot/vol correlation on {sym}", namespace = "Tdata")
      return(NULL)
    }

    adj_prices <- as.numeric(prices$Adjusted)
    log_returns <- diff(log(adj_prices))

    # Rolling realized volatility (annualized)
    n <- length(log_returns)
    rolling_rv <- rep(NA_real_, n)
    for (i in vol_window:n) {
      window_returns <- log_returns[(i - vol_window + 1):i]
      rolling_rv[i] <- stats::sd(window_returns) * sqrt(252)
    }

    rv_changes <- diff(rolling_rv)

    # Align returns[vol_window:n] with rv_changes[(vol_window-1):(n-1)], last lookback_days
    valid_start <- max(vol_window + 1, n - lookback_days + 1)
    aligned_returns <- log_returns[valid_start:n]
    aligned_rv_changes <- rv_changes[(valid_start - 1):(n - 1)]

    valid <- !is.na(aligned_returns) & !is.na(aligned_rv_changes)
    if (sum(valid) < 20) {
      logger::log_warn("Too few valid observations for spot/vol correlation on {sym}", namespace = "Tdata")
      return(NULL)
    }

    correlation <- stats::cor(aligned_returns[valid], aligned_rv_changes[valid])

    interpretation <- if (correlation < -0.5) {
      "Strong negative - typical equity behavior (leverage effect)"
    } else if (correlation < -0.2) {
      "Moderate negative - vol rises when price falls"
    } else if (correlation < 0.2) {
      "Near zero - vol independent of price direction"
    } else if (correlation < 0.5) {
      "Moderate positive - unusual, watch for regime change"
    } else {
      "Strong positive - atypical, possibly commodity-like behavior"
    }

    list(
      correlation = round(correlation, 3),
      interpretation = interpretation,
      observations = sum(valid),
      lookback_days = lookback_days,
      vol_window = vol_window
    )

  }, error = function(e) {
    logger::log_error("Spot/vol correlation failed for {sym}: {e$message}", namespace = "Tdata")
    NULL
  })
}


#' Core vol-of-vol numerics (no percentile lookup)
#'
#' Shared by [compute_vol_of_vol()] and [refresh_vov_breakpoints()] so that the
#' basket refresh does not re-read the breakpoint table once per symbol.
#' @noRd
.vov_core <- function(sym, lookback_days = 504, vol_window = 10) {
  fetch_days <- lookback_days + vol_window + 30
  prices <- getSymIntervalDate(sym, Sys.Date() - fetch_days, Sys.Date())
  if (is.null(prices) || nrow(prices) < (lookback_days / 2 + vol_window)) {
    logger::log_warn("Insufficient price data for vol-of-vol on {sym}", namespace = "Tdata")
    return(NULL)
  }

  log_returns <- diff(log(as.numeric(prices$Adjusted)))

  n <- length(log_returns)
  rolling_rv <- rep(NA_real_, n)
  for (i in vol_window:n) {
    rolling_rv[i] <- stats::sd(log_returns[(i - vol_window + 1):i]) * sqrt(252)
  }

  valid_rv <- rolling_rv[!is.na(rolling_rv)]
  if (length(valid_rv) < 20) {
    logger::log_warn("Too few valid vol observations for vol-of-vol on {sym}", namespace = "Tdata")
    return(NULL)
  }

  ### Close-to-close is deliberate. Yang-Zhang and Rogers-Satchell were scored
  ### against it on the full universe: out-of-sample skill (older block -> recent
  ### block, target = non-overlapping 21-day RV) is +0.386 C2C, +0.391 YZ,
  ### +0.252 RS. YZ's edge is +0.005, 95% CI [-0.094, +0.101] - a coin flip - and
  ### it re-ranks 30% of names because its var(o) term makes its noise floor rise
  ### with overnight-gap share (0.68 -> 1.21 across terciles) where C2C's is flat.
  ### C2C is uniformly inefficient, which compresses the ranking without tilting
  ### it, and already sits at the +0.371 ceiling set by the target's reliability.

  ### Annualized standard deviation of the RV log-changes.
  rv_log_changes <- diff(log(valid_rv[valid_rv > 0]))
  vov <- stats::sd(rv_log_changes) * sqrt(252)

  current_rv <- valid_rv[length(valid_rv)]
  recent_n <- min(30, length(rv_log_changes))
  recent_rv_changes <- rv_log_changes[(length(rv_log_changes) - recent_n + 1):length(rv_log_changes)]

  list(
    vol_of_vol        = round(vov, 3),
    recent_vol_of_vol = round(stats::sd(recent_rv_changes) * sqrt(252), 3),
    current_rv        = round(current_rv * 100, 1),
    rv_percentile     = round(100 * mean(valid_rv <= current_rv), 0),
    observations      = length(valid_rv)
  )
}


#' Stored vol-of-vol percentile breakpoints
#'
#' Reads the cross-sectional quantiles written by [refresh_vov_breakpoints()].
#' Returns NULL when the table is missing so callers degrade to "percentile
#' unavailable" instead of failing.
#'
#' @return data.frame ordered by Quantile, or NULL
#' @export
get_vov_breakpoints <- function() {
  tryCatch({
    conn <- safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    if (!DBI::dbExistsTable(conn, "VolOfVolBreakpoints")) return(NULL)
    bp <- DBI::dbReadTable(conn, "VolOfVolBreakpoints")
    if (nrow(bp) < 2) return(NULL)
    bp[order(bp$Quantile), ]
  }, error = function(e) {
    logger::log_warn("Could not read VolOfVolBreakpoints: {e$message}", namespace = "Tdata")
    NULL
  })
}


#' Place a vol-of-vol reading in the stored cross-sectional distribution
#'
#' The absolute level of vol-of-vol is not interpretable. A 10-day rolling
#' estimator returns ~1.94 even when true vol-of-vol is exactly zero, because
#' adjacent windows share 9 of their 10 returns and that sampling noise is then
#' annualized by sqrt(252). What does carry information is the cross-sectional
#' ranking: it holds at Spearman +0.45 across disjoint 2-year blocks (205 names)
#' once lookback_days is 504. Hence percentile, not level. An earlier +0.76 read
#' on 24 names does not reproduce - at that n the Spearman standard error is
#' ~0.21, and 24-name subsamples of the full universe reach +0.76 only 1.4% of
#' the time.
#'
#' @param vov numeric vol-of-vol reading
#' @param breakpoints data.frame from [get_vov_breakpoints()]
#' @return integer percentile in 0-100, or NA when breakpoints are unavailable
#' @export
vov_to_percentile <- function(vov, breakpoints = get_vov_breakpoints()) {
  if (is.null(breakpoints) || is.null(vov) || length(vov) != 1 || !is.finite(vov)) {
    return(NA_integer_)
  }
  pct <- stats::approx(x = breakpoints$Value, y = breakpoints$Quantile,
                       xout = vov, rule = 2, ties = "ordered")$y
  as.integer(round(min(100, max(0, pct))))
}


#' Compute volatility of volatility (vol-of-vol)
#'
#' How much realized volatility itself fluctuates over time, reported as a
#' percentile of a stored reference basket rather than as an absolute level -
#' see [vov_to_percentile()] for why the level alone says nothing.
#'
#' @param sym string - ticker symbol
#' @param lookback_days numeric - trading days to analyze (default 504). Halving
#'   it to 252 drops cross-sectional rank persistence from +0.45 to +0.18 between
#'   disjoint blocks, which makes the percentile far weaker; do not lower this
#'   without re-checking that.
#' @param vol_window numeric - rolling window for realized vol (default 10).
#'   Widening is not the hazard once believed: 63d persists at +0.45, the same as
#'   10d. It measures slower vol variation and would invalidate stored
#'   breakpoints, so 10 stays the default for continuity, not for accuracy.
#' @return list with vol-of-vol value, its percentile, and context, or NULL on error
#' @export
compute_vol_of_vol <- function(sym, lookback_days = 504, vol_window = 10) {
  tryCatch({
    core <- .vov_core(sym, lookback_days, vol_window)
    if (is.null(core)) return(NULL)

    bp  <- get_vov_breakpoints()
    pct <- vov_to_percentile(core$vol_of_vol, bp)

    interpretation <- if (is.na(pct)) {
      "percentile unavailable - run refresh_vov_breakpoints()"
    } else {
      med <- bp$Value[which.min(abs(bp$Quantile - 50))]
      sprintf(paste("percentile %d of the %d-name reference basket (median %.2f)",
                    "- the ranking is informative, the absolute level is not"),
              pct, bp$NObs[1], med)
    }

    c(core, list(vov_percentile = pct, interpretation = interpretation))

  }, error = function(e) {
    logger::log_error("Vol-of-vol calculation failed for {sym}: {e$message}", namespace = "Tdata")
    NULL
  })
}


#' Recompute and store the vol-of-vol cross-sectional breakpoints
#'
#' Computes vol-of-vol across a reference basket and stores its quantiles in the
#' `VolOfVolBreakpoints` table, so a single-ticker report can place its reading
#' in the distribution without re-fetching the whole basket.
#'
#' Between refreshes the stored cut-points stay valid because the ranking is
#' persistent (Spearman +0.45 across disjoint 2-year blocks, 205 names); a
#' quarterly refresh is ample.
#'
#' @param symbols character vector; defaults to the active scanner universe
#' @param lookback_days numeric passed through to the estimator
#' @param vol_window numeric passed through to the estimator
#' @return the stored breakpoint data.frame, invisibly
#' @export
refresh_vov_breakpoints <- function(symbols = NULL, lookback_days = 504,
                                    vol_window = 10) {
  if (is.null(symbols)) {
    su <- getScannerUniverse(role = "scanner")
    symbols <- unique(su$Symbol)
  }
  logger::log_info("Refreshing vol-of-vol breakpoints over {length(symbols)} symbols",
                   namespace = "Tdata")

  vals <- vapply(symbols, function(s) {
    r <- tryCatch(.vov_core(s, lookback_days, vol_window), error = function(e) NULL)
    if (is.null(r)) NA_real_ else r$vol_of_vol
  }, numeric(1))

  vals <- vals[is.finite(vals)]
  if (length(vals) < 20) {
    logger::log_error("Only {length(vals)} usable symbols - breakpoints not written",
                      namespace = "Tdata")
    return(invisible(NULL))
  }

  ### Tails included so that vov_to_percentile()'s rule=2 clamp bites only at
  ### the true extremes rather than collapsing everything past 5/95.
  probs <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
  bp <- data.frame(
    Quantile     = probs,
    Value        = round(as.numeric(stats::quantile(vals, probs / 100)), 4),
    NObs         = length(vals),
    LookbackDays = lookback_days,
    VolWindow    = vol_window,
    LastUpdate   = format(Sys.time(), "%Y%m%d %H:%M"),
    stringsAsFactors = FALSE
  )

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbWriteTable(conn, "VolOfVolBreakpoints", bp, overwrite = TRUE)
  logger::log_info("Stored vol-of-vol breakpoints from {length(vals)} symbols",
                   namespace = "Tdata")
  invisible(bp)
}


#' Format advanced vol metrics for display
#'
#' @param spot_vol_result list from compute_spot_vol_correlation
#' @param vov_result list from compute_vol_of_vol
#' @return data.frame formatted for table display, or NULL if both inputs NULL
#' @export
format_vol_metrics <- function(spot_vol_result = NULL, vov_result = NULL) {
  rows <- list()

  if (!is.null(spot_vol_result)) {
    rows <- c(rows, list(
      data.frame(
        Metric = "Spot/Vol Correlation",
        Value = sprintf("%.3f", spot_vol_result$correlation),
        Detail = spot_vol_result$interpretation,
        stringsAsFactors = FALSE
      )
    ))
  }

  if (!is.null(vov_result)) {
    rows <- c(rows, list(
      data.frame(
        Metric = c("Vol-of-Vol (percentile)", "Vol-of-Vol (raw, uncalibrated)",
                    "Recent Vol-of-Vol (30d)", "Current RV", "RV Percentile"),
        Value = c(if (is.na(vov_result$vov_percentile)) "n/a"
                  else sprintf("%d%%", vov_result$vov_percentile),
                  sprintf("%.3f", vov_result$vol_of_vol),
                  sprintf("%.3f", vov_result$recent_vol_of_vol),
                  sprintf("%.1f%%", vov_result$current_rv),
                  sprintf("%d%%", vov_result$rv_percentile)),
        Detail = c(vov_result$interpretation, "level is not comparable across tickers",
                   "noise-dominated, indicative only", "", ""),
        stringsAsFactors = FALSE
      )
    ))
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
