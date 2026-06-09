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


#' Compute volatility of volatility (vol-of-vol)
#'
#' How much realized volatility itself fluctuates over time.
#'
#' @param sym string - ticker symbol
#' @param lookback_days numeric - trading days to analyze (default 252)
#' @param vol_window numeric - rolling window for realized vol (default 10)
#' @return list with vol-of-vol value, percentile, and interpretation, or NULL on error
#' @export
compute_vol_of_vol <- function(sym, lookback_days = 252, vol_window = 10) {
  tryCatch({
    fetch_days <- lookback_days + vol_window + 30
    from_date <- Sys.Date() - fetch_days
    to_date <- Sys.Date()

    prices <- getSymIntervalDate(sym, from_date, to_date)
    if (is.null(prices) || nrow(prices) < (lookback_days / 2 + vol_window)) {
      logger::log_warn("Insufficient price data for vol-of-vol on {sym}", namespace = "Tdata")
      return(NULL)
    }

    adj_prices <- as.numeric(prices$Adjusted)
    log_returns <- diff(log(adj_prices))

    n <- length(log_returns)
    rolling_rv <- rep(NA_real_, n)
    for (i in vol_window:n) {
      window_returns <- log_returns[(i - vol_window + 1):i]
      rolling_rv[i] <- stats::sd(window_returns) * sqrt(252)
    }

    valid_rv <- rolling_rv[!is.na(rolling_rv)]
    if (length(valid_rv) < 20) {
      logger::log_warn("Too few valid vol observations for vol-of-vol on {sym}", namespace = "Tdata")
      return(NULL)
    }

    # Vol-of-vol: annualized standard deviation of RV log-changes
    rv_log_changes <- diff(log(valid_rv[valid_rv > 0]))
    vov <- stats::sd(rv_log_changes) * sqrt(252)

    current_rv <- valid_rv[length(valid_rv)]
    rv_percentile <- round(100 * mean(valid_rv <= current_rv), 0)

    recent_n <- min(30, length(rv_log_changes))
    recent_rv_changes <- rv_log_changes[(length(rv_log_changes) - recent_n + 1):length(rv_log_changes)]
    recent_vov <- stats::sd(recent_rv_changes) * sqrt(252)

    interpretation <- if (vov > 2.0) {
      "Very high - extreme vol instability, option prices will swing significantly"
    } else if (vov > 1.0) {
      "High - elevated vol instability, wider option price swings expected"
    } else if (vov > 0.5) {
      "Moderate - normal vol fluctuation range"
    } else {
      "Low - stable vol environment, predictable option pricing"
    }

    list(
      vol_of_vol = round(vov, 3),
      recent_vol_of_vol = round(recent_vov, 3),
      current_rv = round(current_rv * 100, 1),
      rv_percentile = rv_percentile,
      interpretation = interpretation,
      observations = length(valid_rv)
    )

  }, error = function(e) {
    logger::log_error("Vol-of-vol calculation failed for {sym}: {e$message}", namespace = "Tdata")
    NULL
  })
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
        Metric = c("Vol-of-Vol (annualized)", "Recent Vol-of-Vol (30d)",
                    "Current RV", "RV Percentile"),
        Value = c(sprintf("%.3f", vov_result$vol_of_vol),
                  sprintf("%.3f", vov_result$recent_vol_of_vol),
                  sprintf("%.1f%%", vov_result$current_rv),
                  sprintf("%d%%", vov_result$rv_percentile)),
        Detail = c(vov_result$interpretation, "", "", ""),
        stringsAsFactors = FALSE
      )
    ))
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
