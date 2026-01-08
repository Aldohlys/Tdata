
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

  # Linear interpolation/extrapolation of variance to get 30-day variance
  # Formula: w1*var1 + w2*var2 where w1 + w2 = 1
  if (near_time <= target_time && target_time <= next_time) {
    # Interpolation case
    w2 <- (target_time - near_time) / (next_time - near_time)
    w1 <- 1 - w2
  } else if (target_time < near_time) {
    # Extrapolation case (towards present)
    w1 <- next_time / (next_time - near_time)
    w2 <- -near_time / (next_time - near_time)
  } else {
    # Extrapolation case (towards future)
    w1 <- -next_time / (near_time - next_time)
    w2 <- near_time / (near_time - next_time)
  }

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
getVolMetrics <- function(sym_list) {

  if (!all(is.character(sym_list))) {
    logger::log_info("getVolMetrics: argument is not all character: {sym_list}", namespace="Tdata")
    return(NA)
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  purrr::map_df(sym_list, \(sym) {

    logger::log_info("Retrieve vol and price data from IBKR for {sym}", namespace="Tdata")

    ibkr_data <- as.data.frame(tdata_py$get_volatility_metrics(sym=sym, lookback_days=252, hist=TRUE, price = TRUE))
    ibkr_data = dplyr::mutate(ibkr_data, dplyr::across(c(current_iv, current_hv), \(x) {round(x, 4)}))
    ibkr_data = dplyr::mutate(ibkr_data, dplyr::across(c(iv_percentile, hv_percentile, current_price), \(x) {round(x, 2)}))
    ibkr_data = dplyr::mutate(ibkr_data, datetime = format(Sys.time(), format="%Y%m%d %H:%M"))

    ### Subset data
    metrics <- dplyr::select(ibkr_data, datetime, sym=symbol, iv30=current_iv, ivp=iv_percentile, rv30=current_hv, rvp=hv_percentile,
                               price=current_price)


    ### Retrieve currency from Ticker DB or use USD by default
    ticker = getTicker(sym)
    if (nrow(ticker) == 0) currency = "USD" else currency = ticker$Currency

    iv180_data <- getIV_DTE(sym, currency, metrics$price, 180)
    metrics$iv180 <- round(iv180_data$v, 4)
    ### Append to DB - with or without margin data retrieved from IBKR
    safe_db_append(conn, "Prices",metrics); metrics
  })
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
getIV_DTE <- function(sym, currency, spot_price, DTE=30){

  #### Validate input arguments
  ### Should not be used for DTE smaller than 10 days or greater than 730 days
  if ((DTE <= 10)| (DTE >= 730)) return(NA)

  ### Only one symbol
  if (length(sym) > 1 || length(currency) > 1 || length(spot_price) > 1) {
    Tbasics::display_error_message(paste0("No proper format for ", sym,"\n"))
    return(NA)
  }

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

  #### This will take 4 strikes above/below theoretical forward price - IBKR takes only 2 for this computation
  #### Refinement: finding from forward price the strike where put is nearest to call - this is done in calculate_target_vol
  near_strikes <- tdata_py$getStrikesfromExpDate(sym=sym, expdate=near_expiry)
  near_strikes <- Tbasics::get_nearest_values(near_strikes, near_forward_price, n_below = 2, n_above = 2)

  next_strikes <- tdata_py$getStrikesfromExpDate(sym=sym, expdate=next_expiry)
  next_strikes <- Tbasics::get_nearest_values(next_strikes, next_forward_price, n_below = 2, n_above = 2)

  logger::log_info("Retrieve option prices for {near_expiry}...", namespace="Tdata")
  near_option_prices <- getOptionPrices(sym, near_strikes, near_expiry)
  logger::log_info("Retrieve option prices for {next_expiry}...", namespace="Tdata")
  next_option_prices <- getOptionPrices(sym, next_strikes, next_expiry)

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
getOptionPrices <- function(sym, strikes, expiration) {

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

  logger::log_info("Retrieve put data for {sym} at {expiration} for", strikes, namespace="Tdata")
  df_put = tdata_py$getOptValue(sym = sym, expiration = expiration, strikes = strikes, right="P") |>
    dplyr::rename(put_value = value, put_bid = bid, put_ask = ask, put_iv = impliedvol, put_delta = delta) |>
    dplyr::mutate(put_iv = signif_na(put_iv))
  df_put <- df_put |> dplyr::mutate(put_mid = (put_bid + put_ask)/2)

  logger::log_info("Retrieve call data for {sym} at {expiration} for", strikes, namespace="Tdata")
  df_call = tdata_py$getOptValue(sym = sym, expiration = expiration, strikes = strikes, right = "C") |>
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


#' Get Historical Price Data for HAR Volatility
#'
#' Retrieves historical price data from either Yahoo Finance or IBKR TWS API.
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

      # Convert to xts format compatible with HAR functions
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

      # Convert to xts format compatible with HAR functions
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


#' Calculate Daily Realized Volatility
#'
#' Computes daily realized volatility using squared log returns.
#'
#' @param returns Numeric vector or xts of log returns
#' @return Realized volatility (squared returns)
#' @noRd
calculate_daily_rv <- function(returns) {
  rv <- returns^2
  return(rv)
}


#' Calculate Period Realized Volatility
#'
#' Computes rolling mean of daily RV over specified period.
#'
#' @param rv_daily Daily realized volatility series
#' @param period_length Number of days for rolling window (5 for weekly, 22 for monthly)
#' @return Rolling mean RV for the specified period
#' @noRd
calculate_period_rv <- function(rv_daily, period_length) {
  period_rv <- zoo::rollmean(rv_daily, k = period_length, align = "right", fill = NA)
  return(period_rv)
}


#' Prepare Data for HAR-RV Model
#'
#' Creates lagged variables needed for HAR-RV regression using TTR::volatility.
#'
#' @param prices xts object with OHLCV data
#' @param calc Volatility calculation method passed to TTR::volatility:
#'   "close" (default), "garman.klass", "parkinson", "rogers.satchell",
#'   "gk.yz", or "yang.zhang"
#' @return Data frame with rv_next, rv_daily, rv_weekly, rv_monthly columns,
#'   or NULL if calculation fails
#' @noRd
prepare_har_data <- function(prices, calc = "close") {

  # Check for minimum data requirements
  if (is.null(prices) || nrow(prices) < 30) {
    logger::log_warn("Insufficient price data for HAR model: {nrow(prices)} rows", namespace = "Tdata")
    return(NULL)
  }

  # Use TTR::volatility for daily RV calculation
  # TTR returns annualized volatility, we need to de-annualize for daily variance
  # Use n=5 for a 5-day rolling volatility window (minimum stable window)
  vol_daily <- tryCatch({
    TTR::volatility(prices, n = 5, calc = calc, N = 1)
  }, error = function(e) {
    logger::log_error("TTR::volatility failed: {e$message}", namespace = "Tdata")
    return(NULL)
  })

  if (is.null(vol_daily)) return(NULL)

  # Convert volatility to variance (squared)
  rv_daily <- vol_daily^2

  # Calculate weekly (5-day) and monthly (22-day) average RV
  # Use tryCatch to handle edge cases
 rv_weekly <- tryCatch({
    calculate_period_rv(rv_daily, 5)
  }, error = function(e) {
    logger::log_warn("Failed to calculate weekly RV: {e$message}", namespace = "Tdata")
    return(NULL)
  })

  rv_monthly <- tryCatch({
    calculate_period_rv(rv_daily, 22)
  }, error = function(e) {
    logger::log_warn("Failed to calculate monthly RV: {e$message}", namespace = "Tdata")
    return(NULL)
  })

  if (is.null(rv_weekly) || is.null(rv_monthly)) return(NULL)

  # Combine into data frame
  model_data <- data.frame(
    date = zoo::index(rv_daily),
    rv_next = as.numeric(dplyr::lead(rv_daily, 1)),  # Tomorrow's RV
    rv_daily = as.numeric(rv_daily),
    rv_weekly = as.numeric(rv_weekly),
    rv_monthly = as.numeric(rv_monthly)
  )

  # Remove NA values
  model_data <- stats::na.omit(model_data)

  if (nrow(model_data) == 0) {
    logger::log_warn("No valid data after HAR preparation", namespace = "Tdata")
    return(NULL)
  }

  return(model_data)
}


#' Fit HAR-RV Volatility Model
#'
#' Fits a Heterogeneous Autoregressive Realized Volatility (HAR-RV) model
#' using daily, weekly (5-day), and monthly (22-day) realized volatility components.
#'
#' @param sym Symbol to analyze (IBKR-style ticker)
#' @param lookback_days Number of calendar days for historical data (default 400)
#' @param source Data source: "yahoo" or "ibkr" (default "yahoo").
#'   IBKR source uses 15-minute bars aggregated to daily.
#' @param calc Volatility calculation method (via TTR::volatility):
#'   \itemize{
#'     \item "close" - Close-to-close volatility (default, standard method)
#'     \item "garman.klass" - Garman-Klass estimator using OHLC
#'     \item "parkinson" - Parkinson High-Low range estimator
#'     \item "rogers.satchell" - Rogers-Satchell estimator
#'     \item "gk.yz" - Garman-Klass Yang-Zhang extension
#'     \item "yang.zhang" - Yang-Zhang estimator (recommended for OHLC data)
#'   }
#' @param n_test Number of days to reserve for out-of-sample testing (default 30)
#' @return A list containing:
#' \itemize{
#'   \item{\code{model} The fitted lm object}
#'   \item{\code{summary} Model summary statistics}
#'   \item{\code{forecast} Next day annualized volatility forecast}
#'   \item{\code{rmse} Root mean squared error on test set}
#'   \item{\code{mae} Mean absolute error on test set}
#'   \item{\code{test_actual} Actual RV values for test period}
#'   \item{\code{test_predicted} Predicted RV values for test period}
#'   \item{\code{latest_data} Latest RV components used for forecast}
#' }
#' @examples
#' \dontrun{
#' # Using Yahoo Finance data with close-to-close volatility
#' har_spy <- fitHAR("SPY", source = "yahoo")
#' print(har_spy$forecast)
#'
#' # Using IBKR data with 15-minute bars and Yang-Zhang estimator
#' har_aapl <- fitHAR("AAPL", source = "ibkr", calc = "yang.zhang")
#' print(har_aapl$summary)
#'
#' # Compare different volatility estimators
#' har_close <- fitHAR("SPY", calc = "close")
#' har_yz <- fitHAR("SPY", calc = "yang.zhang")
#' }
#' @export
fitHAR <- function(sym, lookback_days = 400, source = c("yahoo", "ibkr"),
                   calc = c("close", "garman.klass", "parkinson",
                            "rogers.satchell", "gk.yz", "yang.zhang"),
                   n_test = 30) {
  source <- match.arg(source)
  calc <- match.arg(calc)

  logger::log_info("Fitting HAR-RV model for {sym} using {source} data, calc={calc}", namespace = "Tdata")

  # Get price data
  prices <- get_har_price_data(sym, lookback_days, source)
  if (is.null(prices)) {
    Tbasics::display_error_message(paste0("Failed to retrieve price data for ", sym))
    return(NULL)
  }

  # Prepare HAR model data
  model_data <- prepare_har_data(prices, calc = calc)

  if (is.null(model_data) || nrow(model_data) < n_test + 50) {
    n_obs <- if (is.null(model_data)) 0 else nrow(model_data)
    logger::log_warn("Insufficient data for HAR model: {n_obs} observations", namespace = "Tdata")
    return(NULL)
  }

  # Split into training and test sets
  train_data <- model_data[1:(nrow(model_data) - n_test), ]
  test_data <- model_data[(nrow(model_data) - n_test + 1):nrow(model_data), ]

  # Fit HAR-RV model
  har_model <- stats::lm(rv_next ~ rv_daily + rv_weekly + rv_monthly, data = train_data)

  # Make predictions for test set
  predictions <- stats::predict(har_model, newdata = test_data)

  # Calculate forecast accuracy metrics
  rmse <- sqrt(mean((test_data$rv_next - predictions)^2))
  mae <- mean(abs(test_data$rv_next - predictions))

  # Forecast for next day using latest data
  latest_data <- data.frame(
    rv_daily = utils::tail(model_data$rv_daily, 1),
    rv_weekly = utils::tail(model_data$rv_weekly, 1),
    rv_monthly = utils::tail(model_data$rv_monthly, 1)
  )

  next_day_rv <- stats::predict(har_model, newdata = latest_data)
  next_day_vol_annualized <- sqrt(next_day_rv * 252)

  logger::log_info("HAR forecast for {sym}: {round(next_day_vol_annualized, 4)} annualized vol", namespace = "Tdata")

  return(list(
    symbol = sym,
    source = source,
    calc = calc,
    model = har_model,
    summary = summary(har_model),
    forecast = as.numeric(next_day_vol_annualized),
    rmse = rmse,
    mae = mae,
    test_actual = test_data$rv_next,
    test_predicted = as.numeric(predictions),
    test_dates = test_data$date,
    latest_data = latest_data,
    n_observations = nrow(model_data)
  ))
}


#' Get HAR Volatility Forecast
#'
#' Convenience function to get the next-day annualized volatility forecast
#' using the HAR-RV model.
#'
#' @param sym Symbol to forecast (IBKR-style ticker)
#' @param source Data source: "yahoo" or "ibkr" (default "yahoo")
#' @param calc Volatility calculation method: "close" (default), "garman.klass",
#'   "parkinson", "rogers.satchell", "gk.yz", or "yang.zhang"
#' @return Numeric value of annualized volatility forecast, or NA if model fails
#' @examples
#' \dontrun{
#' getHARForecast("SPY")
#' getHARForecast("AAPL", source = "ibkr", calc = "yang.zhang")
#' }
#' @export
getHARForecast <- function(sym, source = c("yahoo", "ibkr"),
                           calc = c("close", "garman.klass", "parkinson",
                                    "rogers.satchell", "gk.yz", "yang.zhang")) {
  source <- match.arg(source)
  calc <- match.arg(calc)
  result <- fitHAR(sym, source = source, calc = calc)
  if (is.null(result)) return(NA_real_)
  return(result$forecast)
}


#' Plot HAR Model Results
#'
#' Creates a plot comparing actual vs predicted volatilities for the test period.
#'
#' @param har_result Result object from fitHAR()
#' @param main Plot title (optional, defaults to symbol name)
#' @return Invisibly returns the har_result object
#' @examples
#' \dontrun{
#' har_spy <- fitHAR("SPY")
#' plotHAR(har_spy)
#' }
#' @export
plotHAR <- function(har_result, main = NULL) {
  if (is.null(har_result)) {
    return(invisible(NULL))
  }

  if (is.null(main)) {
    main <- paste0(har_result$symbol, " - Actual vs Predicted Volatility (Annualized)")
  }

  actual_vol <- sqrt(har_result$test_actual * 252)
  predicted_vol <- sqrt(har_result$test_predicted * 252)

  plot(actual_vol, type = "l", col = "blue",
       main = main,
       ylab = "Volatility", xlab = "Day",
       ylim = range(c(actual_vol, predicted_vol), na.rm = TRUE))
  lines(predicted_vol, col = "red")
  legend("topright", c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)

  invisible(har_result)
}

