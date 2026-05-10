#' Calculate beta of a security against SPX using Yahoo Finance data with currency adjustment
#'
#'This function may be called even if ticker does not exist in DB
#'
#' @param sym Character string of the security ticker
#' @param benchmark Character string of the benchmark ticker (default: "^GSPC" for S&P 500)
#' @param from Start date for historical data (default: 1 year ago)
#' @param to End date for historical data (default: current date)
#' @param currency Character string of the security's currency (default: "USD")
#' @return A list containing ticker, benchmark, period, beta value, and number of data points
#' @export
calculate_beta_vs_spx <- function(sym,
                                  benchmark = "^GSPC",
                                  from = Sys.Date() - 365,
                                  to = Sys.Date(),
                                  currency = "USD") {

  # Download historical price data for the security and benchmark
  security_data <- quantmod::getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  benchmark_data <- quantmod::getSymbols(benchmark, src = "yahoo", from = from, to = to, auto.assign = FALSE)

  # If currency is not USD, get the exchange rate data
  if (currency != "USD") {
    # Currency pair format for Yahoo Finance (e.g., CHFUSD=X for CHF to USD)
    currency_pair <- paste0(currency, "USD=X")
    exchange_data <- try(
      quantmod::getSymbols(currency_pair, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      silent = TRUE
    )

    # If currency pair not found, try reverse pair
    if (inherits(exchange_data, "try-error")) {
      currency_pair <- paste0("USD", currency, "=X")
      exchange_data <- quantmod::getSymbols(currency_pair, src = "yahoo", from = from, to = to, auto.assign = FALSE)
      # Invert the exchange rate if using reverse pair
      exchange_rates <- 1 / quantmod::Ad(exchange_data)
    } else {
      exchange_rates <- quantmod::Ad(exchange_data)
    }

    # Handle missing values in exchange rates
    # Use na.locf (Last Observation Carried Forward) to fill gaps
    exchange_rates <- na.omit(zoo::na.locf(exchange_rates, fromLast = FALSE))

    # If there are still missing values at the beginning, use the earliest available value
    if (anyNA(exchange_rates)) {
      exchange_rates <- zoo::na.locf(exchange_rates, fromLast = TRUE)
    }
  }

  # Extract adjusted closing prices
  security_prices <- quantmod::Ad(security_data)
  benchmark_prices <- quantmod::Ad(benchmark_data)

  # Adjust security prices for currency if needed
  if (currency != "USD") {
    # Create a merged dataset with security prices and exchange rates
    # Ensure we have exchange rates for all trading days
    security_dates <- zoo::index(zoo::as.zoo(security_prices))

    # Create a full set of business days for the exchange rates
    exchange_rates_full <- zoo::na.approx(exchange_rates, xout = security_dates, na.rm = FALSE)

    # Handle any remaining missing values that might occur
    exchange_rates_full <- zoo::na.locf(exchange_rates_full, fromLast = FALSE)
    exchange_rates_full <- zoo::na.locf(exchange_rates_full, fromLast = TRUE)

    # Convert security prices to USD
    security_prices_usd <- security_prices * zoo::coredata(exchange_rates_full)
    colnames(security_prices_usd) <- colnames(security_prices)
    security_prices <- security_prices_usd

    # Log exchange rate adjustment information
    message("Adjusted ", sym, " prices from ", currency, " to USD using ",
            currency_pair, " exchange rates")
  }

  # Ensure the dates match
  merged_data <- merge(security_prices, benchmark_prices)
  merged_data <- na.omit(merged_data)

  # Calculate returns
  security_returns <- quantmod::dailyReturn(security_prices)
  benchmark_returns <- quantmod::dailyReturn(benchmark_prices)

  # Create a returns matrix and ensure dates match
  returns_matrix <- merge(security_returns, benchmark_returns)
  colnames(returns_matrix) <- c("security", "benchmark")
  returns_matrix <- na.omit(returns_matrix)

  # Calculate beta using CAPM
  beta_model <- PerformanceAnalytics::CAPM.beta(returns_matrix$security,
                                                returns_matrix$benchmark,
                                                Rf = 0)  # Assuming risk-free rate of 0 for simplicity

  # Return results
  return(list(
    ticker = sym,
    benchmark = benchmark,
    currency = currency,
    period = c(from, to),
    beta = as.numeric(beta_model),
    data_points = nrow(returns_matrix)
  ))
}

#' calculate_beta_vs_spx_periods
#'
#' Calculate beta for a security over multiple time periods with currency adjustment
#'
#' @param ticker Character string of the security ticker
#' @param benchmark Character string of the benchmark ticker (default: "^GSPC" for S&P 500)
#' @param currency Character string of the security's currency (default: "USD")
#'
#' @return A data frame containing beta values for 3-month, 6-month, 1-year, and 3-year periods
#' @export
calculate_beta_vs_spx_periods <- function(ticker, benchmark = "^GSPC", currency = "USD") {
  # Calculate beta for different time periods
  beta_3m <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 90, currency = currency)
  beta_6m <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 180, currency = currency)
  beta_1y <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 365, currency = currency)
  beta_3y <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 365*3, currency = currency)

  # Return all results in a data frame
  return(data.frame(
    ticker = ticker,
    benchmark = benchmark,
    currency = currency,
    beta_3m = beta_3m$beta,
    beta_6m = beta_6m$beta,
    beta_1y = beta_1y$beta,
    beta_3y = beta_3y$beta
  ))
}
