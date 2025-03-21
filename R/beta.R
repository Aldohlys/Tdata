#' Calculate beta of a security against SPX using Yahoo Finance data
#'
#' @param ticker Character string of the security ticker
#' @param benchmark Character string of the benchmark ticker (default: "^GSPC" for S&P 500)
#' @param from Start date for historical data (default: 1 year ago)
#' @param to End date for historical data (default: current date)
#'
#' @return A list containing ticker, benchmark, period, beta value, and number of data points
#' @export
calculate_beta_vs_spx <- function(ticker,
                                  benchmark = "^GSPC",
                                  from = Sys.Date() - 365,
                                  to = Sys.Date()) {

  # Download historical price data for the security and benchmark
  security_data <- quantmod::getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  benchmark_data <- quantmod::getSymbols(benchmark, src = "yahoo", from = from, to = to, auto.assign = FALSE)

  # Extract adjusted closing prices
  security_prices <- quantmod::Ad(security_data)
  benchmark_prices <- quantmod::Ad(benchmark_data)

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
    ticker = ticker,
    benchmark = benchmark,
    period = c(from, to),
    beta = as.numeric(beta_model),
    data_points = nrow(returns_matrix)
  ))
}

#' Calculate beta for a security over multiple time periods
#'
#' @param ticker Character string of the security ticker
#' @param benchmark Character string of the benchmark ticker (default: "^GSPC" for S&P 500)
#'
#' @return A data frame containing beta values for 3-month, 6-month, 1-year, and 3-year periods
#' @export
calculate_beta_vs_spx_periods <- function(ticker, benchmark = "^GSPC") {
  # Calculate beta for different time periods
  beta_3m <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 90)
  beta_6m <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 180)
  beta_1y <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 365)
  beta_3y <- calculate_beta_vs_spx(ticker, benchmark, from = Sys.Date() - 365*3)

  # Return all results in a data frame
  return(data.frame(
    ticker = ticker,
    benchmark = benchmark,
    beta_3m = beta_3m$beta,
    beta_6m = beta_6m$beta,
    beta_1y = beta_1y$beta,
    beta_3y = beta_3y$beta
  ))
}
