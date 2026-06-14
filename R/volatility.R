
# Generic walker function that applies a function to numeric elements
#'@noRd
walk_apply <- function(x, fun, ...) {
  # For lists, process each element individually
  if (is.list(x)) {
    result <- lapply(x, function(element) {
      # Skip NA/NULL elements
      if (is.null(element) || is.na(element)) {
        return(element)
      }

      # Process numeric elements
      if (is.numeric(element)) {
        return(fun(element, ...))
      }

      # Try to convert to numeric if possible
      num_value <- suppressWarnings(as.numeric(element))
      if (!is.na(num_value)) {
        return(fun(num_value, ...))
      }

      # Return unchanged if not convertible to numeric
      return(element)
    })
    return(result)
  } else {
    # For vectors, use vectorized approach
    result <- x
    # Find non-NA numeric elements or convertible elements
    idx <- !is.na(x) & !is.nan(x) & is.numeric(x)

    # Handle character vectors with numeric values
    if (!is.numeric(x)) {
      numeric_idx <- !is.na(suppressWarnings(as.numeric(x)))
      if (any(numeric_idx)) {
        result[numeric_idx] <- suppressWarnings(as.numeric(x[numeric_idx]))
        idx <- numeric_idx
      }
    }

    # Apply function to appropriate elements
    if (any(idx)) {
      result[idx] <- fun(as.numeric(result[idx]), ...)
    }

    return(result)
  }
}

# Now create signif_na as a wrapper around walk_apply
#'@noRd
signif_na <- function(x, digits = 3) {
  walk_apply(x, signif, digits = digits)
}

####################  helper function for checking missing IVs
#'@noRd
check_na_ivs <- function(option_prices, option_type, sym, expiry) {
  # Determine which IV column to check based on option type
  iv_col <- paste0(option_type, "_iv")

  # Check for NA values
  idx <- is.na(option_prices$matched_pairs[[iv_col]])

  if (any(idx)) {
    strikes <- option_prices$matched_pairs$strike[idx]
    for (strike in strikes) {
      Tbasics::display_message(
        paste0("IBKR returned NA for ", sym, ": ", option_type, " option IV ",
               " at ", expiry, " expiry with strike: ", strike)
      )
    }
    return(TRUE)  # Return TRUE if NAs found
  }

  return(FALSE)  # Return FALSE if no NAs found
}


# Helper function to calculate forward index level using put-call parity
#'@noRd
calculate_forward_index <- function(options, interest_rate, time_to_expiry, dividend_yield) {
  # Find ATM options by minimizing the difference between call and put prices
  price_diffs <- abs(options$call_mid - options$put_mid)
  atm_strike_index <- which.min(price_diffs)

  # Calculate forward using put-call parity
  F <- options$strike[atm_strike_index] +
    exp((interest_rate - dividend_yield) * time_to_expiry) *
    (options$call_mid[atm_strike_index] - options$put_mid[atm_strike_index])

  return(F)
}

# Helper function to fit a parabola to volatility data
#'@noRd
fit_volatility_parabola <- function(strikes, ivs) {
  # Guard: need at least 3 non-NA IV values to fit a quadratic
  valid <- !is.na(ivs) & !is.na(strikes)
  if (sum(valid) < 3) {
    return(rep(NA_real_, 3))
  }

  # Fit quadratic model: IV = a*strike^2 + b*strike + c
  model <- stats::lm(ivs ~ poly(strikes, 2, raw = TRUE))

  # Return coefficients as unnamed numeric values
  return(as.numeric(stats::coef(model)))
}

# Helper function to predict volatility from parabola at a given strike
#'@noRd
predict_from_parabola <- function(parabola_coefs, strike) {
  # Extract coefficients
  c <- parabola_coefs[1]  # Intercept
  b <- parabola_coefs[2]  # Linear term
  a <- parabola_coefs[3]  # Quadratic term

  # Calculate implied volatility using the parabola equation
  iv <- a * strike^2 + b * strike + c

  # Ensure we return a plain numeric value, not a named one
  return(as.numeric(iv))
}

### Target volatility function
#'@noRd
calculate_target_vol <- function(near_options, next_options,
                                 dividend_yield, near_interest_rate, next_interest_rate,
                                 target_days = 30) {

  # Calculate days to expiry for both months
  near_time <- near_options$days_to_expiry[1] / 365
  next_time <- next_options$days_to_expiry[1] / 365
  target_time <- target_days/365 # Convert from days to years

  # Calculate forward index levels for both expiration months
  # For near-term and next term options
  near_forward <- calculate_forward_index(near_options, near_interest_rate, near_time, dividend_yield)
  next_forward <- calculate_forward_index(next_options, next_interest_rate, next_time, dividend_yield)

  # Fit parabolas to implied volatilities as function of strike price for each expiry
  # For near-term options (using both call and put IVs)
  # Combine strikes and IVs for parabola fitting
  near_parabola <- fit_volatility_parabola(
    strikes = rep(near_options$strike, 2),  # Repeat strikes for calls and puts
    ivs = c(near_options$call_iv, near_options$put_iv)  # Combine call and put IVs
  )

  # Repeat for next-term options
  next_parabola <- fit_volatility_parabola(
    strikes = rep(next_options$strike, 2),  # Repeat strikes for calls and puts
    ivs = c(next_options$call_iv, next_options$put_iv)  # Combine call and put IVs
  )

  # Get at-market implied volatilities by evaluating parabolas at forward prices
  near_atm_vol <- predict_from_parabola(near_parabola, near_forward)
  next_atm_vol <- predict_from_parabola(next_parabola, next_forward)

  # Convert to variances
  near_variance <- near_atm_vol^2
  next_variance <- next_atm_vol^2

  # Linear interpolation/extrapolation of variance to target_time.
  # The formula w2 = (target - near) / (next - near); w1 = 1 - w2 is correct
  # for all three cases (target inside, before, or after the [near, next]
  # bracket): for target outside the bracket one weight goes negative and the
  # other above 1, which is exactly the linear-extrapolation behaviour.
  w2 <- (target_time - near_time) / (next_time - near_time)
  w1 <- 1 - w2

  # Calculate interpolated x-days variance
  variance_day <- w1 * near_variance + w2 * next_variance

  # Final V calculation - square root of the interpolated variance
  v <- sqrt(variance_day)

  # Return a list with intermediate results for validation
  # Ensure all returned values are simple numerics without names
  return(list(
    near_forward = as.numeric(near_forward),
    next_forward = as.numeric(next_forward),
    near_atm_vol = as.numeric(near_atm_vol),
    next_atm_vol = as.numeric(next_atm_vol),
    near_variance = as.numeric(near_variance),
    next_variance = as.numeric(next_variance),
    weights = as.numeric(c(w1, w2)),
    variance_day = as.numeric(variance_day),
    v = as.numeric(v)
  ))
}

# Example usage:
# v30_result <- calculate_v30(near_options, next_options,
#                           dividend_yield, near_interest_rate, next_interest_rate)
# print(v30_result$v30)



#' Calculate Forward Price Using Black-Scholes Model
#'
#' Computes the forward price of an asset under the Black-Scholes framework,
#' taking into account continuous dividend yield and risk-free interest rate.
#'
#' @param spot_price Numeric. Current price of the underlying asset.
#' @param risk_free_rate Numeric. Annual risk-free interest rate (as decimal, not percentage).
#' @param dividend_yield Numeric. Annual continuous dividend yield (as decimal, not percentage).
#' @param DTE Numeric. Days to expiration
#' @return Numeric. The calculated forward price.
#' @examples
#' getForwardPrice(100, 0.05, 0.02, 100)
#' getForwardPrice(50, 0.03, 0, 50)
#' @export
getForwardPrice <- function(spot_price, risk_free_rate, dividend_yield, DTE) {
  # Cost of carry is the difference between interest rate and dividend yield
  cost_of_carry <- risk_free_rate - dividend_yield

  # Forward price formula: F = S * e^((r-q)*T) - T in years
  forward_price <- spot_price * exp(cost_of_carry * DTE/365)

  return(round(forward_price, 2))
}


#' get30dIV
#'
#' Get 30 days IV for tickers.
#'
#' Computes 30 days IV using option IVs for 30 days target expiration
#'
#' @param tickers data frame with each row a ticker
#' @param LastIBKRPrice data frame with each row a ticker, datetime, sym and price
#' @return a tibble with Name and IV30 as columns. IV30 is rounded to 3rd decimal
#' @examples
#' \dontrun{
#' get30dIV(getTicker("AI"), getStockPrice("AI"))
#' get30dIV(getTickers(c("AI", "SPX")), getStockPrice(c("AI", "SPX")))
#' }
#' @export
get30dIV <- function(tickers, LastIBKRPrice) {

  if (!is.data.frame(tickers)) return(NA)

  res <- dplyr::tibble(Name=character(), iv30=numeric())

  for (i in seq_len(nrow(tickers))) {
    name = tickers[i,1]
    last_ibkr_price = LastIBKRPrice[i,"price"]
    iv30 = getIV_DTE(tickers[i, "Name"], tickers[i, "Currency"], last_ibkr_price, 30)
    if (is.list(iv30)) iv30 = signif_na(iv30$v)
    else iv30 = NA
    res <- res |> dplyr::add_row(Name=name, iv30=iv30)
  }
  return(res)
}

#' get180dIV
#'
#' Get 180 days IV for tickers
#'
#' Computes 180 days IV using option IVs for 180 days target days expiration
#'
#' @param tickers data frame with each row a ticker
#' @param LastIBKRPrice data frame with each row a ticker, datetime, sym and price
#' @return a tibble with Name and IV180 as columns. IV180 is rounded to 3rd decimal
#' @examples
#' \dontrun{
#' get180dIV(getTicker("AI"), getStockPrice("AI"))
#' get180dIV(getTickers(c("AI", "SPX")), getStockPrice(c("AI", "SPX")))
#' }
#' @export
get180dIV <- function(tickers, LastIBKRPrice) {

  if (!is.data.frame(tickers)) return(NA)

  res <- dplyr::tibble(Name=character(), iv180=numeric())

  ## seq_len will make this loop work even if nrow(df) = 0
  for (i in seq_len(nrow(tickers))) {
    name = tickers[i,1]
    last_ibkr_price = LastIBKRPrice[i,"price"]
    iv180 = getIV_DTE(tickers[i, "Name"], tickers[i, "Currency"], last_ibkr_price, 180)
    if (is.list(iv180)) iv180 = signif_na(iv180$v)
    else iv180 = NA
    res <- res |> dplyr::add_row(Name=name, iv180=iv180)
  }
  return(res)
}


#' Get volatility data from IBKR
#'
#' It will request historical data from IBKR TWS API for a number of days, and then store vol metrics in Prices DB.
#'
#' If sym is not present in Ticker DB, it will make assumptions like currency=USD, exchange=SMART, etc...
#' @param sym_list IBKR symbol or vector of symbols
#' @param force_refresh logical - bypass quote caches and pull fresh from TWS (default FALSE)
#' @param capture_surface logical - if TRUE, also capture the ~30 DTE IV surface to
#'   OptionSurface via \code{captureOptionSurface} (once-per-day guarded). Default FALSE
#'   keeps the scanner hot path unchanged; the daily collector sets it TRUE (TODO #50).
#' @return a data frame with for each row the following fields :
#' \itemize{
#' \item{\code{symbol} element of sym_list argument}
#' \item{\code{days_covered} returns the number of calendar days actually spanned over}
#' \item{\code{requested_days} equals lookback_days}
#' \item{\code{data_points} the number of bars actually taken into account for computations}
#' \item{\code{bar_size} will be 2 hours if lookback_days is smaller or equal to 252, 4 hours if greater.}
#' \item{\code{current_iv} latest close IV recorded}
#' \item{\code{iv_percentile} will look at all data points - so it is a lookback_days percentile}
#' \item{\code{iv_min} minimal IV value for all data points}
#' \item{\code{iv_max} maximal IV value for all data points}
#' \item{\code{iv_mean} average IV mean value}
#' \item{\code{iv_30d_back} IV 30 days ago mean value}
#' \item{\code{iv_180d_back} IV 180 days ago mean value}
#' \item{\code{current_hv} latest close HV recorded}
#' \item{\code{hv_percentile} will look at all data points - so it is a lookback_days percentile}
#' \item{\code{hv_min} minimal HV value for all data points}
#' \item{\code{hv_max} maximal HV value for all data points}
#' \item{\code{hv_mean} average HV mean value}
#' \item{\code{hv_30d_back} HV 30 days ago mean value}
#' \item{\code{hv_180d_back} HV 180 days ago mean value}
#' }
#'
#' @examples
#' \dontrun{
#' Requires IBKR TWS connection and Python environment
#' getVolMetrics(c("ABT", "AMD"))
#' getVolMetrics("SBSW")
#' }
#' @export
getVolMetrics <- function(sym_list, force_refresh = FALSE, capture_surface = FALSE) {

  if (!all(is.character(sym_list))) {
    logger::log_info("getVolMetrics: argument is not all character: {sym_list}", namespace="Tdata")
    return(NA)
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  n_total <- length(sym_list)
  purrr::imap_dfr(sym_list, \(sym, idx) {

    if (n_total > 1) message(sprintf("  Retrieving vol data for %s (%d/%d)...", sym, idx, n_total))
    logger::log_debug("Retrieve vol and price data from IBKR for {sym}", namespace="Tdata")

    ibkr_data <- as.data.frame(tdata_py$get_volatility_metrics(sym=sym, lookback_days=252, hist=TRUE, price = TRUE))
    ibkr_data = dplyr::mutate(ibkr_data, dplyr::across(c(current_iv, current_hv), \(x) {round(x, 4)}))
    ibkr_data = dplyr::mutate(ibkr_data, dplyr::across(c(iv_percentile, hv_percentile, current_price), \(x) {round(x, 2)}))
    ibkr_data = dplyr::mutate(ibkr_data, datetime = format(Sys.time(), format="%Y%m%d %H:%M"))

    ### Subset data
    metrics <- dplyr::select(ibkr_data, datetime, sym=symbol, iv30=current_iv, ivp=iv_percentile, rv30=current_hv, rvp=hv_percentile,
                               price=current_price)


    ### Retrieve currency from Ticker DB or use USD by default
    ticker = getTicker(sym)
    if (nrow(ticker) == 0) {
      Tbasics::display_message(paste0("Ticker ", sym, " not found in DB. Using default values (USD, SMART exchange)."))
      currency = "USD"
    } else {
      currency = ticker$Currency
    }

    ### Fallback: iv30 from option chains when IBKR aggregate IV unavailable
    if (is.na(metrics$iv30)) {
      ### Option-strike selection needs a finite spot; otherwise NaN strikes get
      ### shipped to IBKR (Error 320, connection drop). See TODO #49.
      if (!is.finite(metrics$price)) {
        logger::log_warn("iv30 unavailable for {sym} and spot price is NaN — skipping option-chain fallback", namespace = "Tdata")
      } else {
        logger::log_info("IBKR aggregate IV unavailable for {sym}, computing iv30 from option chains", namespace = "Tdata")
        iv30_data <- tryCatch(getIV_DTE(sym, currency, metrics$price, 30, force_refresh = force_refresh), error = function(e) NA)
        if (is.list(iv30_data) && !is.na(iv30_data$v)) {
          metrics$iv30 <- round(iv30_data$v, 4)
          logger::log_info("iv30 fallback for {sym}: {metrics$iv30}", namespace = "Tdata")
        }
      }
    }

    ### Fallback: rv30 from Yahoo historical prices when IBKR aggregate HV unavailable
    if (is.na(metrics$rv30)) {
      logger::log_info("IBKR aggregate HV unavailable for {sym}, computing rv30 from Yahoo prices", namespace = "Tdata")
      rv30 <- tryCatch({
        prices <- get_har_price_data(sym, lookback_days = 60, source = "yahoo")
        if (!is.null(prices) && nrow(prices) >= 22) {
          close_col <- grep("\\.Close$", colnames(prices), value = TRUE)
          closes <- as.numeric(prices[, close_col])
          n_days <- min(22, length(closes) - 1)
          recent_closes <- utils::tail(closes, n_days + 1)
          log_returns <- diff(log(recent_closes))
          sd(log_returns) * sqrt(252)
        } else NA_real_
      }, error = function(e) {
        logger::log_warn("rv30 Yahoo fallback failed for {sym}: {e$message}", namespace = "Tdata")
        NA_real_
      })
      if (!is.na(rv30) && is.finite(rv30)) {
        metrics$rv30 <- round(rv30, 4)
        logger::log_info("rv30 fallback for {sym}: {metrics$rv30}", namespace = "Tdata")
      }
    }

    ### Compute iv15, iv90, iv180 from option chains — skip if spot is non-finite (see TODO #49).
    ### iv15 is event-sensitive and noisy near expiry; interpret as indicative, not decision-grade.
    compute_iv_dte <- function(target_dte) {
      if (!is.finite(metrics$price)) {
        logger::log_warn("iv{target_dte} skipped for {sym} — spot price is NaN", namespace = "Tdata")
        return(NA_real_)
      }
      res <- tryCatch(getIV_DTE(sym, currency, metrics$price, target_dte, force_refresh = force_refresh), error = function(e) NA)
      if (is.list(res) && !is.null(res[["v"]]) && !is.na(res[["v"]])) round(res[["v"]], 4) else NA_real_
    }

    metrics$iv15  <- compute_iv_dte(15)
    metrics$iv90  <- compute_iv_dte(90)
    metrics$iv180 <- compute_iv_dte(180)

    ### VRP (Volatility Risk Premium) = log(iv30/rv30) * 100
    ### Positive = options priced richer than realized (premium-sell edge)
    ### Negative = options cheap vs realized (favor long gamma / debit structures)
    metrics$vrp <- if (!is.na(metrics$iv30) && !is.na(metrics$rv30) &&
                      metrics$iv30 > 0 && metrics$rv30 > 0) {
      round(log(metrics$iv30 / metrics$rv30) * 100, 2)
    } else NA_real_

    ### IVR (IV Rank) over last 1y = 100 * (current - min) / (max - min)
    ### IVP_2y (IV Percentile) over last 2y = % of historical iv30 at-or-below current
    ### Both computed from Prices table (excluding current row since not yet written)
    iv30_history_1y <- DBI::dbGetQuery(conn,
      "SELECT iv30 FROM Prices WHERE sym = ? AND iv30 IS NOT NULL AND datetime >= ?",
      params = list(sym, format(Sys.Date() - 365, "%Y%m%d"))
    )$iv30
    iv30_history_2y <- DBI::dbGetQuery(conn,
      "SELECT iv30 FROM Prices WHERE sym = ? AND iv30 IS NOT NULL AND datetime >= ?",
      params = list(sym, format(Sys.Date() - 730, "%Y%m%d"))
    )$iv30

    ### Sample-size thresholds kept low because Prices history is sparse at feature-launch.
    ### Metrics will become statistically meaningful once the scanner writes to Prices regularly.
    ### Until then, IVR/IVP_2y are approximate — IBKR's aggregate ivp remains the robust baseline.
    metrics$ivr <- if (!is.na(metrics$iv30) && length(iv30_history_1y) >= 5) {
      iv_min <- min(iv30_history_1y, na.rm = TRUE)
      iv_max <- max(iv30_history_1y, na.rm = TRUE)
      if (iv_max - iv_min > 1e-6) round(100 * (metrics$iv30 - iv_min) / (iv_max - iv_min), 2) else NA_real_
    } else NA_real_

    metrics$ivp_2y <- if (!is.na(metrics$iv30) && length(iv30_history_2y) >= 10) {
      round(100 * sum(iv30_history_2y <= metrics$iv30, na.rm = TRUE) / length(iv30_history_2y), 2)
    } else NA_real_

    if (length(iv30_history_1y) > 0 && length(iv30_history_1y) < 20) {
      logger::log_info("ivr for {sym} uses only {length(iv30_history_1y)} historical rows — warm-up phase",
                       namespace = "Tdata")
    }

    ### Append to DB
    safe_db_append(conn, "Prices", metrics)

    ### Forward IV-surface capture for skew percentiles (TODO #50, Phase 2a).
    ### Opt-in (default FALSE) so the scanner's hot path is unaffected; the daily
    ### collector passes capture_surface=TRUE. Best-effort, once-per-day guarded,
    ### never throws — reuses the spot + iv30 already computed above.
    if (isTRUE(capture_surface)) {
      captureOptionSurface(sym, currency, spot = metrics$price, iv30 = metrics$iv30,
                           force_refresh = force_refresh)
    }

    metrics
  })
}

### ---- 30-day IV skew percentiles (TODO #50, Phase 2a) -------------------------
### Read off the OptionSurface table (populated by the forward surface-capture hook).
### Put and call skew are tracked INDEPENDENTLY -- their comparison is the signal:
### put skew >> call skew = downside richly priced; call skew >> put skew = upside
### richly priced. A combined metric would destroy that information.

#' Implied vol at a target delta within a single capture/right, by nearest delta.
#'
#' Nearest-delta (vs. linear interpolation between bracketing strikes) is robust to
#' sparse chains and matches how getVolMetrics selects ATM strikes. Revisit if the
#' +/-1.5 sigma capture turns out dense enough to interpolate cleanly.
#'@noRd
.iv_at_delta <- function(df_right, target_delta) {
  ok <- !is.na(df_right$delta) & !is.na(df_right$iv)
  if (!any(ok)) return(NA_real_)
  df_right <- df_right[ok, , drop = FALSE]
  df_right$iv[which.min(abs(df_right$delta - target_delta))]
}

#' Per-capture put/call skew for one symbol's OptionSurface slice.
#'
#' skew_put  = iv(25d put)  - iv(50d put)   (put deltas negative: -0.25 vs -0.50)
#' skew_call = iv(25d call) - iv(50d call)  (call deltas positive: +0.25 vs +0.50)
#'@noRd
.compute_capture_skews <- function(surface) {
  if (is.null(surface) || nrow(surface) == 0) return(data.frame())
  splits <- split(surface, surface$datetime)
  do.call(rbind, lapply(names(splits), function(dt) {
    cap   <- splits[[dt]]
    puts  <- cap[cap$right == "P", , drop = FALSE]
    calls <- cap[cap$right == "C", , drop = FALSE]
    data.frame(
      datetime  = dt,
      skew_put  = .iv_at_delta(puts,  -0.25) - .iv_at_delta(puts,  -0.50),
      skew_call = .iv_at_delta(calls,  0.25) - .iv_at_delta(calls,  0.50),
      stringsAsFactors = FALSE
    )
  }))
}

#' getSkewPercentiles
#'
#' Current 30-day IV skew (put and call, tracked separately) and their percentile
#' ranks over the trailing OptionSurface history. Skew = iv(25-delta) - iv(50-delta),
#' read off the forward-collected OptionSurface table (see TODO #50, Phase 2a).
#'
#' One capture row-set per symbol per day; the latest capture is the "current" skew
#' and is ranked against all captures in the lookback window (percentile = fraction of
#' historical observations at-or-below current, same convention as ivp_2y).
#'
#' @param sym character symbol
#' @param lookback_days integer calendar-day window for the percentile history (default 365)
#' @return one-row data frame: \code{sym, datetime, skew_put, skew_call,
#'   skew_put_pct, skew_call_pct, n_obs}. Metric columns are NA when the surface is
#'   empty or history is too short (< 5 captures) for a meaningful percentile.
#' @examples
#' \dontrun{
#'   getSkewPercentiles("AAPL")
#' }
#' @export
getSkewPercentiles <- function(sym, lookback_days = 365L) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  na_row <- data.frame(sym = sym, datetime = NA_character_,
                       skew_put = NA_real_, skew_call = NA_real_,
                       skew_put_pct = NA_real_, skew_call_pct = NA_real_,
                       n_obs = 0L, stringsAsFactors = FALSE)

  if (!"OptionSurface" %in% DBI::dbListTables(conn)) {
    logger::log_info("getSkewPercentiles: OptionSurface table missing — run scripts/migrate_option_surface.R", namespace = "Tdata")
    return(na_row)
  }

  ### datetime is 'YYYYMMDD HH:MM'; lexicographic >= on the 'YYYYMMDD' cutoff is correct.
  surface <- DBI::dbGetQuery(conn,
    "SELECT datetime, right, strike, iv, delta FROM OptionSurface
     WHERE sym = ? AND datetime >= ?",
    params = list(sym, format(Sys.Date() - lookback_days, "%Y%m%d")))

  if (nrow(surface) == 0) return(na_row)

  skews <- .compute_capture_skews(surface)
  if (nrow(skews) == 0) return(na_row)
  skews  <- skews[order(skews$datetime), , drop = FALSE]
  latest <- skews[nrow(skews), ]

  pct <- function(series, current) {
    series <- series[!is.na(series)]
    if (length(series) < 5 || is.na(current)) return(NA_real_)
    round(100 * sum(series <= current) / length(series), 2)
  }

  data.frame(
    sym           = sym,
    datetime      = latest$datetime,
    skew_put      = round(latest$skew_put, 4),
    skew_call     = round(latest$skew_call, 4),
    skew_put_pct  = pct(skews$skew_put,  latest$skew_put),
    skew_call_pct = pct(skews$skew_call, latest$skew_call),
    n_obs         = nrow(skews),
    stringsAsFactors = FALSE
  )
}

#' captureOptionSurface
#'
#' Capture the ~30 DTE implied-vol surface slice -- per-strike implied vol + delta
#' for calls and puts spanning +/-1.5 sigma around spot -- and append it to the
#' OptionSurface table (TODO #50, Phase 2a). One call = one capture timestamp.
#' Storing the full sliced surface (not just derived skews) is future-proof: any
#' delta-bucket analytic (25d/50d skew, wings, term skew) is computable post-hoc.
#' Read back by \code{getSkewPercentiles}.
#'
#' Best-effort and side-effecting: it never throws and returns the number of rows
#' written (0 on any failure), so getVolMetrics can call it without risk to the
#' vol-metrics path. Quote fetch goes through the cache-backed getOptValue, so the
#' marginal IBKR cost is ~one batch of strike quotes per symbol per day.
#'
#' @param sym character symbol
#' @param currency character currency code (EUR/USD/...) — reserved for future
#'   forward/rate-aware band sizing; band currently uses spot and iv30 directly
#' @param spot numeric current underlying price (must be finite)
#' @param iv30 numeric 30-day IV as a fraction (e.g. 0.30); sizes the +/-1.5 sigma band
#' @param target_dte integer target days-to-expiry for the captured expiry (default 30)
#' @param force_refresh logical pass-through to getOptValue (default FALSE)
#' @param force logical bypass the once-per-day guard to force a re-capture (default FALSE)
#' @return integer count of rows appended (invisibly); 0 if already captured today
#' @examples
#' \dontrun{
#'   captureOptionSurface("AAPL", "USD", spot = 205.3, iv30 = 0.28)
#' }
#' @export
captureOptionSurface <- function(sym, currency, spot, iv30, target_dte = 30L,
                                 force_refresh = FALSE, force = FALSE) {
  if (!is.finite(spot) || is.na(iv30) || iv30 <= 0) return(invisible(0L))

  ### Surface any stale-cache deletions from the chain/strike fetches below.
  on.exit(surface_cache_warnings(), add = TRUE)

  ### Inner worker so `return()` short-circuits cleanly inside tryCatch.
  do_capture <- function() {
    conn <- safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    ### Once-per-day guard: the percentile model assumes one capture row-set per
    ### symbol per day (getSkewPercentiles splits by datetime). Repeated scanner
    ### calls must not double-write. force=TRUE bypasses for manual re-capture.
    if (!force && "OptionSurface" %in% DBI::dbListTables(conn)) {
      today_str <- format(Sys.Date(), "%Y%m%d")
      already <- DBI::dbGetQuery(conn,
        "SELECT COUNT(*) AS n FROM OptionSurface WHERE sym = ? AND substr(datetime,1,8) = ?",
        params = list(sym, today_str))$n
      if (already > 0) {
        logger::log_debug("OptionSurface: {sym} already captured today — skipping", namespace = "Tdata")
        return(0L)
      }
    }

    min_expiry_date <- as.integer(format(Sys.Date() + 7, "%Y%m%d"))
    expdates <- tdata_py$getExpirationDates(sym, min_date = min_expiry_date)
    if (is.null(expdates) || length(expdates) == 0) return(0L)

    ### Nearest available expiry to the 30 DTE target.
    target_date <- as.integer(format(Sys.Date() + target_dte, "%Y%m%d"))
    expiry <- expdates[which.min(abs(as.integer(expdates) - target_date))]
    dte <- as.numeric(Tbasics::getDTE(Sys.time(), as.Date(expiry, "%Y%m%d")))

    ### +/-1.5 sigma price band at this horizon (lognormal).
    band  <- 1.5 * iv30 * sqrt(dte / 365)
    lower <- spot * exp(-band)
    upper <- spot * exp(band)

    strikes <- tdata_py$getStrikesfromExpDate(sym = sym, expdate = expiry)
    if (is.null(strikes) || length(strikes) == 0) return(0L)
    strikes <- strikes[strikes >= lower & strikes <= upper]
    strikes <- subsample_strikes(strikes, spot, max_strikes = 20)
    if (length(strikes) == 0) return(0L)

    capture_dt <- format(Sys.time(), "%Y%m%d %H:%M")
    fetch_side <- function(right) {
      df <- tryCatch(as.data.frame(
        tdata_py$getOptValue(sym = sym, expiration = expiry, strikes = strikes,
                             right = right, force_refresh = force_refresh)),
        error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[!is.na(df$impliedvol) & !is.na(df$delta), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      data.frame(
        sym = sym, datetime = capture_dt, expiry = as.character(expiry),
        dte = as.integer(round(dte)), right = right,
        strike = as.numeric(df$strike),
        iv = round(as.numeric(df$impliedvol), 4),
        delta = round(as.numeric(df$delta), 4),
        spot = round(spot, 4), stringsAsFactors = FALSE
      )
    }

    rows <- rbind(fetch_side("C"), fetch_side("P"))
    if (is.null(rows) || nrow(rows) == 0) return(0L)

    safe_db_append(conn, "OptionSurface", rows)
    logger::log_info("OptionSurface: captured {nrow(rows)} strikes for {sym} @ {expiry}",
                     namespace = "Tdata")
    nrow(rows)
  }

  res <- tryCatch(do_capture(), error = function(e) {
    logger::log_warn("captureOptionSurface failed for {sym}: {e$message}", namespace = "Tdata")
    0L
  })
  invisible(res)
}

# Adaptive ATM strike selection. Widen the range only until enough strikes
# qualify: dense $1-strike chains stop at 1%, sparse/low-priced names widen to
# 3%+ to find 2 on each side. Uses getStrikesInRange (qualifies only near-money
# strikes, per-expiry, cached) instead of listing/qualifying the whole chain.
#'@noRd
.atm_strikes_in_range <- function(sym, expiry, center, n_below = 2, n_above = 2,
                                  ranges = c(0.01, 0.03, 0.06, 0.12)) {
  need <- n_below + n_above
  strikes <- NULL
  for (rng in ranges) {
    strikes <- tryCatch(
      tdata_py$getStrikesInRange(sym, expiration = expiry,
                                 center_strike = center, range_pct = rng),
      error = function(e) NULL)
    if (!is.null(strikes) && length(strikes) >= need) break
  }
  if (is.null(strikes) || length(strikes) == 0) return(numeric(0))
  Tbasics::get_nearest_values(strikes, center, n_below = n_below, n_above = n_above)
}

#' getIV_DTE
#' Get IV for a symbol
#'
#' Computes IV using option IVs for a defined target days.
#' It will look for nearest expiration dates compared with DTE for the ticker and then retrieve option prices and IV from IBKR.
#'
#'
#' @param sym string
#' @param currency string - either EUR, USD, CHF,...
#' @param spot_price last price known for ticker - or a vector of prices, usually taken from IBKR API
#' @param DTE numeric - should not be smaller than 10 days as model would not work correctly for target days smaller than 8 days,
#' should be less than 2 years (730 days)
#' @return a list containing the following: near_forward, next_forward, near_atm_vol, next_atm_vol
#' near_variance, next_variance, weights, variance_day, v - v is the implied volatility
#' @examples
#' \dontrun{
#'   getIV_DTE("AI", "EUR", 175.5, 30)
#' }
#' @export
getIV_DTE <- function(sym, currency, spot_price, DTE=30, force_refresh=FALSE){

  #### Validate input arguments
  ### Should not be used for DTE smaller than 10 days or greater than 730 days
  if ((DTE <= 10)| (DTE >= 730)) return(NA)

  ### Only one symbol
  if (length(sym) > 1 || length(currency) > 1 || length(spot_price) > 1) {
    Tbasics::display_error_message(paste0("No proper format for ", sym,"\n"))
    return(NA)
  }

  ### Surface any stale-cache deletions from the chain/strike fetches below.
  on.exit(surface_cache_warnings(), add = TRUE)

  ### Retrieve the expiration dates for sym
  min_expiry_date <- as.integer(format(Sys.Date() + 7, "%Y%m%d"))
  expdates_list <- tdata_py$getExpirationDates(sym, min_date = min_expiry_date)

  ### It is assumed that expdates_list is sorted
  if(is.unsorted(as.integer(expdates_list))) {
    Tbasics::display_error_message("Unsorted chain expiration date!!")
  }

  ### Retrieve expiration dates that are just before and after DTE days
  ### In case of IBKR near_expiry that are less than 8 days are not taken into account
  target_date <- as.integer(format(Sys.Date() + DTE, "%Y%m%d"))

  ### Look at case where target date is smaller than the first
  if (target_date < as.integer(expdates_list[1])) {
    Tbasics::display_message(paste0("getIV_DTE: target date smaller than first date for ", sym))
    near_expiry <- expdates_list[1]
    next_expiry <- expdates_list[2]
  }

  ## Look at case where target date is greater than last date
  else if (target_date > as.integer(expdates_list[length(expdates_list)])) {
    Tbasics::display_message(paste0("getIV_DTE: target date greater than last date for ", sym))
    near_expiry <- expdates_list[length(expdates_list)-1]
    next_expiry <- expdates_list[length(expdates_list)]
  }

  ### Standard case where target date is within the expdates_list
  else {
    near_expiry <- expdates_list[max(which(as.integer(expdates_list) < target_date))]
    next_expiry <- expdates_list[min(which(as.integer(expdates_list) > target_date))]
  }

  ### Compute near_DTE and next_DTE
  near_DTE <- Tbasics::getDTE(Sys.time(), as.Date(near_expiry, "%Y%m%d"))
  next_DTE <- Tbasics::getDTE(Sys.time(), as.Date(next_expiry, "%Y%m%d"))

  ### Get interest rates and dividend yield
  dividend_yield = getLastDivYield(sym)
  near_interest_rate = getLastRate(currency, near_DTE)
  next_interest_rate = getLastRate(currency, next_DTE)

  ### Get theoretical forward prices
  near_forward_price <- getForwardPrice(spot_price, near_interest_rate, dividend_yield, near_DTE)
  next_forward_price <- getForwardPrice(spot_price, next_interest_rate, dividend_yield, next_DTE)

  #### Take 2 strikes above/below the forward. Source them via getStrikesInRange
  #### (qualifies only strikes near the money, per-expiry) instead of listing/
  #### qualifying the whole chain — see .atm_strikes_in_range. Refinement:
  #### the strike where put is nearest to call is found in calculate_target_vol.
  near_strikes <- .atm_strikes_in_range(sym, near_expiry, near_forward_price)
  next_strikes <- .atm_strikes_in_range(sym, next_expiry, next_forward_price)

  logger::log_debug("Retrieve option prices for {near_expiry}...", namespace="Tdata")
  near_option_prices <- getOptionPrices(sym, near_strikes, near_expiry, force_refresh = force_refresh)
  logger::log_debug("Retrieve option prices for {next_expiry}...", namespace="Tdata")
  next_option_prices <- getOptionPrices(sym, next_strikes, next_expiry, force_refresh = force_refresh)

  if( (nrow(near_option_prices$unmatched_calls) != 0) | (nrow(near_option_prices$unmatched_puts) != 0)) {
    Tbasics::display_message(paste0("Issue with chain ", sym,"  for ", near_expiry," with strikes: ", near_strikes))
  }

  if( (nrow(next_option_prices$unmatched_calls) != 0) | (nrow(next_option_prices$unmatched_puts) != 0)) {
    Tbasics::display_message(paste0("Issue with chain ", sym,"  for ", next_expiry," with strikes: ", next_strikes))
  }

  # Check all IVs four scenarios
  if (check_na_ivs(near_option_prices, "call", sym, near_expiry) |
      check_na_ivs(near_option_prices, "put", sym, near_expiry) |
      check_na_ivs(next_option_prices, "put", sym, next_expiry) |
      check_na_ivs(next_option_prices, "call", sym, next_expiry)) {
    Tbasics::display_message(paste0("Could not retrieve IVs for ", sym,"  from IBKR! NA value will be used for IV."))
  }


  ### Prepare call to calculate_target_vol
  near_term_options <- near_option_prices$matched_pairs
  next_term_options <- next_option_prices$matched_pairs
  near_term_options$days_to_expiry <- near_DTE
  next_term_options$days_to_expiry <- next_DTE

  return(calculate_target_vol(near_term_options, next_term_options,
                              dividend_yield, near_interest_rate, next_interest_rate, DTE))
  }


  # iv_computations <- calculate_vix_optimized(near_term_options, next_term_options,
  #                                                    risk_free_rate_near = near_interest_rate,
  #                                                    risk_free_rate_next = next_interest_rate,
  #                                                    current_time = Sys.time(),
  #                                                    as.Date(near_expiry, "%Y%m%d"), as.Date(next_expiry, "%Y%m%d"),
  #                                                    target_days = DTE, # Target duration in days (default 30 for standard VIX)
  #                                                    sigma_limit = 3, # Limit strikes to n-sigma range
  #                                                    round_digits = 3, # Number of significant digits
  #                                                    separate_components = TRUE)
  #
  # return(list(implied_vol= iv_computations$VIX, iv_call_component = iv_computations$VIX_call_component,
  #             iv_put_component = iv_computations$VIX_put_component))



###
#'getOptionPrices
#'
#'Retrieves a dataframe of option price from IBKR. This function is not vectorized.
#'
#'For a given contract and expiration, this function returns a set of prices from IBKR
#'If IBKR service is not available, or option price not available, or contract does not exist, it will return an error code -1.
#'@param sym string - IBKR style of ticker, if unknown then function returns -1
#'@param expiration number, date or string - expiration date, format is Y/M/D
#'@param strikes double vector - strikes to get value from
#'@returns a dataframe with with columns strikes, call, put, empty if no price are found
#'@export
getOptionPrices <- function(sym, strikes, expiration, force_refresh = FALSE) {

  test_price <- function(x) {
    ### x should be different from NA
    ### x should be greater or equal to 0.01
    !is.na(x) & (floor(x*100) != 0)
  }

  ### Case where expiration is a number
  if (is.numeric(expiration)) expiration = as.character(expiration)

  ### Case where expiration has a date class - convert it into a string with IBKR format for expiration date
  else if (inherits(expiration,"Date")) expiration = format(expiration,"%Y%m%d")
  else if (!is.character(expiration)) stop("expiration date must be either a date, a number or a character string!")

  logger::log_debug("Retrieve put data for {sym} at {expiration} for", strikes, namespace="Tdata")
  df_put = tdata_py$getOptValue(sym = sym, expiration = expiration, strikes = strikes, right="P",
                                force_refresh = force_refresh) |>
    dplyr::rename(put_value = value, put_bid = bid, put_ask = ask, put_iv = impliedvol, put_delta = delta) |>
    dplyr::mutate(put_iv = signif_na(put_iv))
  df_put <- df_put |> dplyr::mutate(put_mid = (put_bid + put_ask)/2)

  logger::log_debug("Retrieve call data for {sym} at {expiration} for", strikes, namespace="Tdata")
  df_call = tdata_py$getOptValue(sym = sym, expiration = expiration, strikes = strikes, right = "C",
                                 force_refresh = force_refresh) |>
    dplyr::rename(call_value = value, call_bid = bid, call_ask = ask, call_iv = impliedvol, call_delta = delta) |>
    dplyr::mutate(call_iv = signif_na(call_iv))
  df_call <- df_call |> dplyr::mutate(call_mid = (call_bid + call_ask)/2)

  option_prices <- dplyr::full_join(df_call, df_put, by= "strike") |> ### Join on strike column
    dplyr::mutate(
      has_call = test_price(call_value) | test_price(call_bid)  | test_price(call_ask) ,
      has_put = test_price(put_value) | test_price(put_bid)  | test_price(put_ask))

  # 1. Matched pairs (inner join equivalent)
  matched_pairs <- option_prices |>
    dplyr::filter(has_call == TRUE, has_put == TRUE) |>
    dplyr::select(strike, call_bid, call_ask, put_bid, put_ask, call_mid, put_mid, call_iv, put_iv, call_value, put_value)

  # 2. Calls without matching puts
  unmatched_calls <- option_prices |>
    dplyr::filter(has_call == TRUE, has_put == FALSE)

  # 3. Puts without matching calls
  unmatched_puts <- option_prices |>
    dplyr::filter(has_call == FALSE, has_put == TRUE)

  return(list(matched_pairs=matched_pairs,
              unmatched_calls=unmatched_calls,
              unmatched_puts=unmatched_puts))

  ### Data.Frame with columns strikes, call, put
}


#' Get Historical Daily Price Data
#'
#' Retrieves daily OHLCV history from either Yahoo Finance or IBKR TWS API.
#' Generic price fetcher used by getVolMetrics and trend.R (name retained for
#' caller/test-mock stability; no longer HAR-specific).
#'
#' @param sym Symbol to retrieve data for (IBKR-style ticker)
#' @param lookback_days Number of calendar days to look back (default 400)
#' @param source Data source: "yahoo" or "ibkr" (default "yahoo")
#' @return An xts object with OHLCV data, or NULL if retrieval fails
#' @noRd
get_har_price_data <- function(sym, lookback_days = 400, source = c("yahoo", "ibkr")) {
  source <- match.arg(source)

  end_date <- Sys.Date()
  start_date <- end_date - lookback_days

  if (source == "yahoo") {
    # Use Tdata::getYahooData which handles IBKR->Yahoo ticker conversion
    tryCatch({
      yahoo_data <- getYahooData(sym, from_date = start_date, to_date = end_date)

      if (is.null(yahoo_data) || nrow(yahoo_data) == 0) {
        logger::log_warn("No Yahoo data returned for {sym}", namespace = "Tdata")
        return(NULL)
      }

      # Convert to xts format (OHLCV)
      prices <- xts::xts(
        yahoo_data[, c("Open", "High", "Low", "Close", "Volume")],
        order.by = as.Date(yahoo_data$date)
      )
      colnames(prices) <- paste0(sym, ".", c("Open", "High", "Low", "Close", "Volume"))

      return(prices)
    }, error = function(e) {
      logger::log_error("Failed to retrieve Yahoo data for {sym}: {e$message}", namespace = "Tdata")
      return(NULL)
    })

  } else if (source == "ibkr") {
    # Use IBKR TWS API via Python interface - 15 minute bars
    tryCatch({
      # Calculate duration string for IBKR (format: "X D" for days)
      duration_str <- paste0(lookback_days, " D")

      # Get 15-minute bars from IBKR
      ibkr_data <- tdata_py$get_historical_bars(
        sym = sym,
        duration = duration_str,
        bar_size = "15 mins"
      )

      if (is.null(ibkr_data) || nrow(ibkr_data) == 0) {
        logger::log_warn("No IBKR data returned for {sym}", namespace = "Tdata")
        return(NULL)
      }

      # Convert to xts format (OHLCV)
      # Aggregate 15-min bars to daily OHLCV
      ibkr_data$date <- as.Date(ibkr_data$datetime)

      daily_data <- ibkr_data |>
        dplyr::group_by(date) |>
        dplyr::summarise(
          Open = dplyr::first(open),
          High = max(high),
          Low = min(low),
          Close = dplyr::last(close),
          Volume = sum(volume),
          .groups = "drop"
        )

      # Convert to xts
      prices <- xts::xts(
        daily_data[, c("Open", "High", "Low", "Close", "Volume")],
        order.by = daily_data$date
      )
      colnames(prices) <- paste0(sym, ".", c("Open", "High", "Low", "Close", "Volume"))

      return(prices)
    }, error = function(e) {
      logger::log_error("Failed to retrieve IBKR data for {sym}: {e$message}", namespace = "Tdata")
      return(NULL)
    })
  }
}


#' Get IV Values at Historical Percentile Levels
#'
#' Fetches 252-day IV history from IBKR and returns IV values at specific
#' percentile breakpoints (10th, 25th, 50th, 75th, 90th). Requires IBKR TWS connection.
#'
#' @param sym IBKR symbol string
#' @return Named list with current_iv, p10, p25, p50, p75, p90, days_covered.
#'   Returns NULL if data unavailable.
#' @examples
#' \dontrun{
#' getIVPercentileLevels("SPY")
#' }
#' @export
getIVPercentileLevels <- function(sym) {
  stopifnot(is.character(sym), length(sym) == 1)

  result <- tryCatch(
    tdata_py$get_iv_percentile_levels(sym = sym),
    error = function(e) {
      logger::log_warn("getIVPercentileLevels failed for {sym}: {e$message}", namespace = "Tdata")
      NULL
    }
  )

  if (is.null(result)) return(NULL)

  list(
    current = as.numeric(result$current_iv),
    p10     = as.numeric(result$p10),
    p25     = as.numeric(result$p25),
    p50     = as.numeric(result$p50),
    p75     = as.numeric(result$p75),
    p90     = as.numeric(result$p90),
    days_covered = as.integer(result$days_covered)
  )
}

