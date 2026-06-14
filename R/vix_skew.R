# VIX-style variance decomposition with call/put skew analysis.
# Promoted from Tuser/vol/logic/vixf.R into Tdata so /analyze and scan can
# share it. Uses CBOE VIX methodology with separate put/call variance
# contributions. Within-package Tdata helpers (tdata_py, getOptionPrices,
# getForwardPrice, getLastRate, getLastDivYield) are called directly.

#' Retrieve option chain data and compute VIX-style skew analysis
#'
#' Main entry point: fetches near/next term option chains from IBKR,
#' then calculates VIX with call/put variance decomposition.
#'
#' @param sym string - IBKR ticker symbol
#' @param currency string - EUR, USD, CHF, etc.
#' @param spot_price numeric - current underlying price
#' @param DTE numeric - target days for VIX calculation (default 30)
#' @param sigma_limit numeric - strike range filter in standard deviations (default 2)
#' @return list with VIX results or NULL on error
#' @export
get_vix_skew <- function(sym, currency, spot_price, DTE = 30, sigma_limit = 2) {
  stopifnot(is.character(sym), length(sym) == 1)
  stopifnot(is.numeric(spot_price), length(spot_price) == 1)
  stopifnot(DTE >= 10, DTE <= 730)

  ### Surface any stale-cache deletions triggered by the chain/strike fetches
  ### below, on every exit path (including the tryCatch error handler).
  on.exit(surface_cache_warnings(), add = TRUE)

  tryCatch({
    # Step 1: Get expiration dates
    min_expiry_date <- as.integer(format(Sys.Date() + 7, "%Y%m%d"))
    expdates_list <- tdata_py$getExpirationDates(sym, min_date = min_expiry_date)

    if (is.null(expdates_list) || length(expdates_list) < 2) {
      logger::log_warn("Insufficient expiration dates for {sym}", namespace = "Tdata")
      return(NULL)
    }

    # Step 2: Identify near-term and next-term expirations (same logic as getIV_DTE)
    target_date <- as.integer(format(Sys.Date() + DTE, "%Y%m%d"))

    if (target_date < as.integer(expdates_list[1])) {
      near_expiry <- expdates_list[1]
      next_expiry <- expdates_list[2]
    } else if (target_date > as.integer(expdates_list[length(expdates_list)])) {
      near_expiry <- expdates_list[length(expdates_list) - 1]
      next_expiry <- expdates_list[length(expdates_list)]
    } else {
      near_expiry <- expdates_list[max(which(as.integer(expdates_list) < target_date))]
      next_expiry <- expdates_list[min(which(as.integer(expdates_list) > target_date))]
    }

    # Step 3: Compute DTE and rates
    near_DTE <- Tbasics::getDTE(Sys.time(), as.Date(near_expiry, "%Y%m%d"))
    next_DTE <- Tbasics::getDTE(Sys.time(), as.Date(next_expiry, "%Y%m%d"))

    dividend_yield <- getLastDivYield(sym)
    near_interest_rate <- getLastRate(currency, near_DTE)
    next_interest_rate <- getLastRate(currency, next_DTE)

    # Step 4: Get ALL available strikes for each expiration (not just 4 ATM)
    logger::log_info("VIX: Retrieving strikes for {sym} at {near_expiry}", namespace = "Tdata")
    near_strikes <- tdata_py$getStrikesfromExpDate(sym = sym, expdate = near_expiry)

    logger::log_info("VIX: Retrieving strikes for {sym} at {next_expiry}", namespace = "Tdata")
    next_strikes <- tdata_py$getStrikesfromExpDate(sym = sym, expdate = next_expiry)

    if (is.null(near_strikes) || length(near_strikes) < 8 ||
        is.null(next_strikes) || length(next_strikes) < 8) {
      logger::log_warn("Insufficient strikes for VIX calculation on {sym}", namespace = "Tdata")
      return(NULL)
    }

    # Pre-filter strikes to reasonable range around spot (avoid fetching far OTM)
    near_forward <- getForwardPrice(spot_price, near_interest_rate, dividend_yield, near_DTE)
    next_forward <- getForwardPrice(spot_price, next_interest_rate, dividend_yield, next_DTE)

    # Use approximate ATM vol for strike filtering
    approx_vol <- 0.30
    near_lower <- near_forward * exp(-approx_vol * sqrt(near_DTE / 365) * sigma_limit)
    near_upper <- near_forward * exp(approx_vol * sqrt(near_DTE / 365) * sigma_limit)
    next_lower <- next_forward * exp(-approx_vol * sqrt(next_DTE / 365) * sigma_limit)
    next_upper <- next_forward * exp(approx_vol * sqrt(next_DTE / 365) * sigma_limit)

    near_strikes <- near_strikes[near_strikes >= near_lower & near_strikes <= near_upper]
    next_strikes <- next_strikes[next_strikes >= next_lower & next_strikes <= next_upper]

    # Subsample dense strike chains to limit API calls (max ~40 strikes)
    near_strikes <- subsample_strikes(near_strikes, near_forward, max_strikes = 40)
    next_strikes <- subsample_strikes(next_strikes, next_forward, max_strikes = 40)

    # Step 5: Retrieve option prices for all strikes
    logger::log_info("VIX: Retrieving {length(near_strikes)} option prices for near-term {near_expiry}",
                     namespace = "Tdata")
    near_option_prices <- getOptionPrices(sym, near_strikes, near_expiry)

    logger::log_info("VIX: Retrieving {length(next_strikes)} option prices for next-term {next_expiry}",
                     namespace = "Tdata")
    next_option_prices <- getOptionPrices(sym, next_strikes, next_expiry)

    near_term_options <- near_option_prices$matched_pairs
    next_term_options <- next_option_prices$matched_pairs

    if (nrow(near_term_options) < 4 || nrow(next_term_options) < 4) {
      logger::log_warn("Insufficient matched option pairs for VIX on {sym}", namespace = "Tdata")
      return(NULL)
    }

    # Step 6: Calculate VIX with call/put decomposition
    result <- calculate_vix_optimized(
      near_term_options, next_term_options,
      risk_free_rate_near = near_interest_rate,
      risk_free_rate_next = next_interest_rate,
      current_time = Sys.time(),
      near_expiry = as.POSIXct(as.Date(near_expiry, "%Y%m%d")),
      next_expiry = as.POSIXct(as.Date(next_expiry, "%Y%m%d")),
      target_days = DTE,
      sigma_limit = sigma_limit,
      round_digits = 3,
      separate_components = TRUE
    )

    # Enrich result with context
    result$symbol <- sym
    result$near_expiry_date <- near_expiry
    result$next_expiry_date <- next_expiry
    result$near_DTE <- round(near_DTE, 1)
    result$next_DTE <- round(next_DTE, 1)
    result$near_strikes_count <- nrow(near_term_options)
    result$next_strikes_count <- nrow(next_term_options)
    result$skew_ratio <- if (result$VIX_call_component > 0) {
      round(result$VIX_put_component / result$VIX_call_component, 2)
    } else NA_real_

    return(result)

  }, error = function(e) {
    logger::log_error("VIX calculation failed for {sym}: {e$message}", namespace = "Tdata")
    return(NULL)
  })
}


#' Calculate VIX index with separate call/put variance components
#'
#' Pure calculation function (no IBKR dependency). Adapted from CBOE VIX methodology.
#'
#' @param near_term_options data.frame with columns: strike, call_mid, put_mid, call_bid, put_bid
#' @param next_term_options same format as near_term_options
#' @param risk_free_rate_near numeric - risk-free rate for near-term expiry
#' @param risk_free_rate_next numeric - risk-free rate for next-term expiry
#' @param current_time POSIXct - current timestamp
#' @param near_expiry POSIXct - near-term expiration
#' @param next_expiry POSIXct - next-term expiration
#' @param target_days numeric - target duration (default 30)
#' @param sigma_limit numeric - strike range filter in SDs (default 3)
#' @param round_digits numeric - output precision (default 3)
#' @param separate_components logical - return call/put components (default TRUE)
#' @return list with VIX, components, variances, weights
#' @export
calculate_vix_optimized <- function(near_term_options, next_term_options,
                                    risk_free_rate_near, risk_free_rate_next,
                                    current_time, near_expiry, next_expiry,
                                    target_days = 30,
                                    sigma_limit = 3,
                                    round_digits = 3,
                                    separate_components = TRUE) {

  # Time to expiration in years
  minutes_in_year <- 525600
  NT1 <- as.numeric(difftime(near_expiry, current_time, units = "mins")) / minutes_in_year
  NT2 <- as.numeric(difftime(next_expiry, current_time, units = "mins")) / minutes_in_year

  # Calculate variance for near-term and next-term
  var_result_near <- calc_variance_with_components(
    near_term_options, NT1, risk_free_rate_near, sigma_limit
  )
  var_result_next <- calc_variance_with_components(
    next_term_options, NT2, risk_free_rate_next, sigma_limit
  )

  # Interpolation weights
  target_minutes <- target_days * 1440
  t1 <- as.numeric(difftime(near_expiry, current_time, units = "mins"))
  t2 <- as.numeric(difftime(next_expiry, current_time, units = "mins"))

  w1 <- (t2 - target_minutes) / (t2 - t1)
  w2 <- (target_minutes - t1) / (t2 - t1)

  # Handle edge cases
  if (w1 < 0 || w2 < 0) {
    if (target_minutes < t1) {
      w1 <- 1; w2 <- 0
    } else if (target_minutes > t2) {
      w1 <- 0; w2 <- 1
    }
  }

  # Adjust variances for time scaling
  adjusted_near_var <- var_result_near$total_var * NT1 / (target_days / 365)
  adjusted_next_var <- var_result_next$total_var * NT2 / (target_days / 365)
  adjusted_near_call_var <- var_result_near$call_var * NT1 / (target_days / 365)
  adjusted_next_call_var <- var_result_next$call_var * NT2 / (target_days / 365)
  adjusted_near_put_var <- var_result_near$put_var * NT1 / (target_days / 365)
  adjusted_next_put_var <- var_result_next$put_var * NT2 / (target_days / 365)

  # Interpolated target variances
  sigma_squared_target <- w1 * adjusted_near_var + w2 * adjusted_next_var
  call_component_target <- w1 * adjusted_near_call_var + w2 * adjusted_next_call_var
  put_component_target <- w1 * adjusted_near_put_var + w2 * adjusted_next_put_var

  # VIX calculation
  VIX <- signif(100 * sqrt(max(sigma_squared_target, 0)), round_digits)
  VIX_call <- signif(100 * sqrt(max(call_component_target, 0)), round_digits)
  VIX_put <- signif(100 * sqrt(max(put_component_target, 0)), round_digits)

  list(
    VIX = VIX,
    VIX_call_component = VIX_call,
    VIX_put_component = VIX_put,
    target_days = target_days,
    near_term_variance = signif(var_result_near$total_var, round_digits),
    next_term_variance = signif(var_result_next$total_var, round_digits),
    near_term_call_variance = signif(var_result_near$call_var, round_digits),
    near_term_put_variance = signif(var_result_near$put_var, round_digits),
    next_term_call_variance = signif(var_result_next$call_var, round_digits),
    next_term_put_variance = signif(var_result_next$put_var, round_digits),
    time_to_near_expiry = NT1,
    time_to_next_expiry = NT2,
    weight_near = w1,
    weight_next = w2
  )
}


#' Format VIX results for display
#'
#' @param vix_result list returned by get_vix_skew
#' @return list with summary, term_structure, skew_interpretation
#' @export
format_vix_results <- function(vix_result) {
  if (is.null(vix_result)) return(NULL)

  summary_df <- data.frame(
    Metric = c("VIX (Combined)", "Call Component", "Put Component",
               "Put/Call Skew Ratio", "Target Days"),
    Value = c(
      sprintf("%.1f", vix_result$VIX),
      sprintf("%.1f", vix_result$VIX_call_component),
      sprintf("%.1f", vix_result$VIX_put_component),
      if (!is.na(vix_result$skew_ratio)) sprintf("%.2f", vix_result$skew_ratio) else "N/A",
      as.character(vix_result$target_days)
    ),
    stringsAsFactors = FALSE
  )

  term_structure_df <- data.frame(
    Term = c("Near", "Next"),
    Expiry = c(vix_result$near_expiry_date, vix_result$next_expiry_date),
    DTE = c(vix_result$near_DTE, vix_result$next_DTE),
    Variance = c(
      sprintf("%.4f", vix_result$near_term_variance),
      sprintf("%.4f", vix_result$next_term_variance)
    ),
    Call_Var = c(
      sprintf("%.4f", vix_result$near_term_call_variance),
      sprintf("%.4f", vix_result$next_term_call_variance)
    ),
    Put_Var = c(
      sprintf("%.4f", vix_result$near_term_put_variance),
      sprintf("%.4f", vix_result$next_term_put_variance)
    ),
    Weight = c(
      sprintf("%.2f", vix_result$weight_near),
      sprintf("%.2f", vix_result$weight_next)
    ),
    Strikes = c(vix_result$near_strikes_count, vix_result$next_strikes_count),
    stringsAsFactors = FALSE
  )

  # Skew interpretation
  skew_text <- if (is.na(vix_result$skew_ratio)) {
    "Insufficient data for skew analysis"
  } else if (vix_result$skew_ratio > 1.5) {
    "Significant put skew - elevated downside hedging demand"
  } else if (vix_result$skew_ratio >= 1.0) {
    "Moderate put skew - normal market conditions"
  } else {
    "Call skew - unusual, check for event-driven demand"
  }

  list(
    summary = summary_df,
    term_structure = term_structure_df,
    skew_interpretation = skew_text
  )
}


# --- Internal helper functions (not exported) ---

# Calculate variance with separate put and call components
#'@noRd
calc_variance_with_components <- function(options, time_to_expiry, rfr, sigma_limit = 3) {
  # Find forward index level (F) using only pairs with valid mid prices
  valid_pairs <- !is.na(options$call_mid) & !is.na(options$put_mid) &
                 options$call_mid > 0 & options$put_mid > 0

  if (sum(valid_pairs) < 4) {
    stop("Insufficient valid option pairs for variance calculation: ", sum(valid_pairs))
  }

  valid_options <- options[valid_pairs, ]
  price_diffs <- abs(valid_options$call_mid - valid_options$put_mid)
  atm_strike_index <- which.min(price_diffs)

  F <- valid_options$strike[atm_strike_index] +
    exp(rfr * time_to_expiry) *
    (valid_options$call_mid[atm_strike_index] - valid_options$put_mid[atm_strike_index])

  if (!is.finite(F)) {
    stop("Forward price is not finite (F=", F, ")")
  }

  strikes_below <- options$strike[options$strike <= F]
  if (length(strikes_below) == 0) {
    stop("No strikes at or below forward price F=", round(F, 2),
         "; strike range: [", min(options$strike), ", ", max(options$strike), "]")
  }
  K0 <- max(strikes_below)

  # Estimate ATM vol for strike filtering (use valid_options — atm_strike_index references it)
  if ("implied_vol" %in% names(valid_options) && !is.na(valid_options$implied_vol[atm_strike_index])) {
    approx_atm_vol <- valid_options$implied_vol[atm_strike_index]
  } else if ("call_iv" %in% names(valid_options) && !is.na(valid_options$call_iv[atm_strike_index])) {
    approx_atm_vol <- valid_options$call_iv[atm_strike_index]
  } else {
    atm_call <- valid_options$call_mid[atm_strike_index]
    atm_put <- valid_options$put_mid[atm_strike_index]
    approx_atm_vol <- sqrt(2 * pi / time_to_expiry) * (atm_call + atm_put) / F
    approx_atm_vol <- min(max(approx_atm_vol, 0.1), 0.6)
  }

  # Filter strikes to sigma_limit range
  lower_bound <- F * exp(-approx_atm_vol * sqrt(time_to_expiry) * sigma_limit)
  upper_bound <- F * exp(approx_atm_vol * sqrt(time_to_expiry) * sigma_limit)
  options_filtered <- options[options$strike >= lower_bound & options$strike <= upper_bound, ]

  # Ensure minimum strikes
  min_strikes_required <- 8
  expanded_sigma <- sigma_limit
  while (nrow(options_filtered) < min_strikes_required && expanded_sigma < 10) {
    expanded_sigma <- expanded_sigma + 0.5
    lower_bound <- F * exp(-approx_atm_vol * sqrt(time_to_expiry) * expanded_sigma)
    upper_bound <- F * exp(approx_atm_vol * sqrt(time_to_expiry) * expanded_sigma)
    options_filtered <- options[options$strike >= lower_bound & options$strike <= upper_bound, ]
  }

  # Apply CBOE criteria and calculate delta_k
  options_filtered <- filter_options_cboe_criteria(options_filtered, F)
  options_filtered <- calculate_delta_k(options_filtered)

  # Vectorized contribution calculation
  exp_rfr_t <- exp(rfr * time_to_expiry)
  put_contributions <- numeric(nrow(options_filtered))
  call_contributions <- numeric(nrow(options_filtered))

  put_indices <- which(options_filtered$strike < K0)
  call_indices <- which(options_filtered$strike > K0)
  k0_indices <- which(options_filtered$strike == K0)

  # Put contributions
  if (length(put_indices) > 0) {
    put_strikes <- options_filtered$strike[put_indices]
    put_delta_k <- options_filtered$delta_k[put_indices]
    put_prices <- options_filtered$put_mid[put_indices]
    valid_puts <- !is.na(put_prices) & put_prices > 0
    if (any(valid_puts)) {
      put_contributions[put_indices[valid_puts]] <-
        (put_delta_k[valid_puts] / put_strikes[valid_puts]^2) *
        exp_rfr_t * put_prices[valid_puts]
    }
  }

  # Call contributions
  if (length(call_indices) > 0) {
    call_strikes <- options_filtered$strike[call_indices]
    call_delta_k <- options_filtered$delta_k[call_indices]
    call_prices <- options_filtered$call_mid[call_indices]
    valid_calls <- !is.na(call_prices) & call_prices > 0
    if (any(valid_calls)) {
      call_contributions[call_indices[valid_calls]] <-
        (call_delta_k[valid_calls] / call_strikes[valid_calls]^2) *
        exp_rfr_t * call_prices[valid_calls]
    }
  }

  # K0 strike: split contribution between put and call
  if (length(k0_indices) > 0) {
    k0_strike <- options_filtered$strike[k0_indices]
    k0_delta_k <- options_filtered$delta_k[k0_indices]
    k0_put_price <- options_filtered$put_mid[k0_indices]
    k0_call_price <- options_filtered$call_mid[k0_indices]

    if (!is.na(k0_put_price) && !is.na(k0_call_price) &&
        k0_put_price > 0 && k0_call_price > 0) {
      k0_price <- (k0_put_price + k0_call_price) / 2
      k0_contribution <- (k0_delta_k / k0_strike^2) * exp_rfr_t * k0_price
      put_contributions[k0_indices] <- k0_contribution / 2
      call_contributions[k0_indices] <- k0_contribution / 2
    } else if (!is.na(k0_put_price) && k0_put_price > 0) {
      put_contributions[k0_indices] <-
        (k0_delta_k / k0_strike^2) * exp_rfr_t * k0_put_price
    } else if (!is.na(k0_call_price) && k0_call_price > 0) {
      call_contributions[k0_indices] <-
        (k0_delta_k / k0_strike^2) * exp_rfr_t * k0_call_price
    }
  }

  # Sum and apply multiplier
  put_variance <- 2 * sum(put_contributions) / time_to_expiry
  call_variance <- 2 * sum(call_contributions) / time_to_expiry

  # Squared term attributed to puts (downside)
  put_variance <- put_variance - (1 / time_to_expiry) * (F / K0 - 1)^2

  list(
    total_var = put_variance + call_variance,
    put_var = put_variance,
    call_var = call_variance,
    F = F,
    K0 = K0,
    sigma_used = approx_atm_vol,
    num_strikes_used = nrow(options_filtered)
  )
}


# Calculate delta K (strike spacing) for each option
#'@noRd
calculate_delta_k <- function(options) {
  options <- options[order(options$strike), ]
  options$delta_k <- NA_real_

  n <- nrow(options)
  for (i in seq_len(n)) {
    if (i == 1) {
      options$delta_k[i] <- options$strike[2] - options$strike[1]
    } else if (i == n) {
      options$delta_k[i] <- options$strike[n] - options$strike[n - 1]
    } else {
      options$delta_k[i] <- (options$strike[i + 1] - options$strike[i - 1]) / 2
    }
  }
  options
}


# Subsample strikes to limit API calls for dense chains (e.g. SPY $1 strikes)
# Keeps all strikes near ATM and thins out far OTM strikes
#'@noRd
subsample_strikes <- function(strikes, forward_price, max_strikes = 40) {
  if (length(strikes) <= max_strikes) return(strikes)

  strikes <- sort(strikes)
  # Always keep the 10 strikes closest to forward price
  distances <- abs(strikes - forward_price)
  atm_indices <- order(distances)[1:min(10, length(strikes))]

  # Evenly subsample the remaining strikes
  other_indices <- setdiff(seq_along(strikes), atm_indices)
  n_remaining <- max_strikes - length(atm_indices)
  if (n_remaining > 0 && length(other_indices) > 0) {
    sampled <- other_indices[round(seq(1, length(other_indices), length.out = n_remaining))]
    selected <- sort(unique(c(atm_indices, sampled)))
  } else {
    selected <- sort(atm_indices)
  }

  strikes[selected]
}


# Filter options according to CBOE criteria (two consecutive zero bids)
#'@noRd
filter_options_cboe_criteria <- function(options, forward_index) {
  strikes_below <- options$strike[options$strike <= forward_index]
  if (length(strikes_below) == 0) return(options[FALSE, ])  # empty data frame
  K0 <- max(strikes_below)
  options <- options[order(options$strike), ]
  include_option <- rep(TRUE, nrow(options))

  # Process puts (strikes <= K0): exclude after two consecutive zero bids
  zero_bid_count <- 0
  for (i in which(options$strike <= K0)) {
    if (is.na(options$put_bid[i]) || options$put_bid[i] <= 0) {
      zero_bid_count <- zero_bid_count + 1
    } else {
      zero_bid_count <- 0
    }
    if (zero_bid_count >= 2 || is.na(options$put_bid[i]) || options$put_bid[i] <= 0) {
      include_option[i] <- FALSE
    }
  }

  # Process calls (strikes > K0): exclude after two consecutive zero bids (from high to low)
  zero_bid_count <- 0
  for (i in rev(which(options$strike > K0))) {
    if (is.na(options$call_bid[i]) || options$call_bid[i] <= 0) {
      zero_bid_count <- zero_bid_count + 1
    } else {
      zero_bid_count <- 0
    }
    if (zero_bid_count >= 2 || is.na(options$call_bid[i]) || options$call_bid[i] <= 0) {
      include_option[i] <- FALSE
    }
  }

  options[include_option, ]
}
