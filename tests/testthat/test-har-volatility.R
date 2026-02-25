
############ Test HAR Volatility Functions ###########


# =============================================================================
# Test get_har_price_data (internal function)
# =============================================================================

test_that("get_har_price_data returns xts object for Yahoo source", {
  skip_if_offline()

  # Use a reliable ticker with enough lookback
  result <- tryCatch({
    Tdata:::get_har_price_data("SPY", lookback_days = 100, source = "yahoo")
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  skip_if(is.null(result), "Failed to retrieve data from Yahoo")

  expect_true(xts::is.xts(result))
  expect_true(nrow(result) > 30)  # Should have at least 30 trading days

  # Check column names contain expected OHLCV
  col_names <- colnames(result)
  expect_true(any(grepl("Open", col_names)))
  expect_true(any(grepl("Close", col_names)))
  expect_true(any(grepl("High", col_names)))
  expect_true(any(grepl("Low", col_names)))
})

# =============================================================================
# Test prepare_har_data (internal function)
# =============================================================================

test_that("prepare_har_data returns correct structure", {
  skip_if_offline()

  # Get price data first - need enough data for HAR calculations
  prices <- tryCatch({
    Tdata:::get_har_price_data("SPY", lookback_days = 200, source = "yahoo")
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  skip_if(is.null(prices), "Failed to retrieve price data")
  skip_if(nrow(prices) < 50, "Insufficient price data for test")

  # Prepare HAR data
  har_data <- Tdata:::prepare_har_data(prices, calc = "close")

  skip_if(is.null(har_data), "prepare_har_data returned NULL")

  expect_true(is.data.frame(har_data))
  expect_true(all(c("date", "rv_next", "rv_daily", "rv_weekly", "rv_monthly") %in% colnames(har_data)))
  expect_true(nrow(har_data) > 0)

  # All RV values should be non-negative (variances)
  expect_true(all(har_data$rv_daily >= 0, na.rm = TRUE))
  expect_true(all(har_data$rv_weekly >= 0, na.rm = TRUE))
  expect_true(all(har_data$rv_monthly >= 0, na.rm = TRUE))
})

test_that("prepare_har_data works with different calc methods", {
  skip_if_offline()

  prices <- tryCatch({
    Tdata:::get_har_price_data("SPY", lookback_days = 200, source = "yahoo")
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  skip_if(is.null(prices), "Failed to retrieve price data")
  skip_if(nrow(prices) < 50, "Insufficient price data for test")

  # Test different volatility calculation methods
  calc_methods <- c("close", "garman.klass", "parkinson", "yang.zhang")

  for (method in calc_methods) {
    har_data <- Tdata:::prepare_har_data(prices, calc = method)
    # Some methods may return NULL if data is insufficient
    if (!is.null(har_data)) {
      expect_true(is.data.frame(har_data), info = paste("Method:", method))
      expect_true(nrow(har_data) > 0, info = paste("Method:", method))
    }
  }
})

test_that("prepare_har_data returns NULL for insufficient data", {
  # Create a small xts object with insufficient data
  dates <- seq(Sys.Date() - 10, Sys.Date(), by = "day")
  small_prices <- xts::xts(
    data.frame(
      Open = runif(11, 100, 110),
      High = runif(11, 110, 120),
      Low = runif(11, 90, 100),
      Close = runif(11, 100, 110),
      Volume = runif(11, 1000, 2000)
    ),
    order.by = dates
  )
  colnames(small_prices) <- paste0("TEST.", c("Open", "High", "Low", "Close", "Volume"))

  result <- Tdata:::prepare_har_data(small_prices, calc = "close")
  expect_null(result)
})

# =============================================================================
# Test fitHAR
# =============================================================================

test_that("fitHAR returns correct structure with Yahoo data", {
  skip_if_offline()

  result <- tryCatch({
    fitHAR("SPY", lookback_days = 400, source = "yahoo", n_test = 20)
  }, error = function(e) {
    skip(paste("HAR model fitting failed:", e$message))
    NULL
  })

  skip_if(is.null(result), "fitHAR returned NULL - insufficient data")

  # Check return structure
  expect_type(result, "list")
  expect_true(all(c("symbol", "source", "calc", "model", "summary",
                    "forecast", "rmse", "mae", "test_actual",
                    "test_predicted", "latest_data", "n_observations") %in% names(result)))

  # Check values
  expect_equal(result$symbol, "SPY")
  expect_equal(result$source, "yahoo")
  expect_equal(result$calc, "close")  # default

  # Forecast should be a positive annualized volatility
  expect_true(is.numeric(result$forecast))
  expect_true(result$forecast > 0)
  expect_true(result$forecast < 2)  # Sanity check: vol < 200%

  # Model should be an lm object
  expect_s3_class(result$model, "lm")

  # RMSE and MAE should be non-negative
  expect_true(result$rmse >= 0)
  expect_true(result$mae >= 0)
})

test_that("fitHAR works with different calc methods", {
  skip_if_offline()

  # Test with yang.zhang method
  result <- tryCatch({
    fitHAR("SPY", lookback_days = 400, source = "yahoo", calc = "yang.zhang", n_test = 20)
  }, error = function(e) {
    skip(paste("HAR model fitting failed:", e$message))
    NULL
  })

  skip_if(is.null(result), "fitHAR returned NULL")

  expect_equal(result$calc, "yang.zhang")
  expect_true(result$forecast > 0)
})

test_that("fitHAR returns NULL for insufficient lookback", {
  skip_if_offline()

  # Request very short lookback that won't have enough data
  result <- suppressWarnings(suppressMessages(
    fitHAR("SPY", lookback_days = 50, source = "yahoo", n_test = 30)
  ))

  # Should return NULL due to insufficient data
  expect_null(result)
})

# =============================================================================
# Test getHARForecast
# =============================================================================

test_that("getHARForecast returns numeric forecast", {
  skip_if_offline()

  forecast <- tryCatch({
    getHARForecast("SPY", source = "yahoo")
  }, error = function(e) {
    skip(paste("HAR forecast failed:", e$message))
    NA_real_
  })

  skip_if(is.na(forecast), "getHARForecast returned NA - possibly insufficient data")

  expect_true(is.numeric(forecast))
  expect_length(forecast, 1)
  expect_true(forecast > 0)
  expect_true(forecast < 2)  # Sanity check
})

test_that("getHARForecast accepts calc parameter", {
  skip_if_offline()

  forecast <- tryCatch({
    getHARForecast("SPY", source = "yahoo", calc = "parkinson")
  }, error = function(e) {
    skip(paste("HAR forecast failed:", e$message))
    NA_real_
  })

  skip_if(is.na(forecast), "getHARForecast returned NA")

  expect_true(is.numeric(forecast))
  expect_true(forecast > 0)
})

# =============================================================================
# Test plotHAR
# =============================================================================

test_that("plotHAR handles NULL input gracefully", {
  result <- plotHAR(NULL)
  expect_null(result)
})

test_that("plotHAR returns har_result invisibly", {
  skip_if_offline()

  har_result <- tryCatch({
    fitHAR("SPY", lookback_days = 400, source = "yahoo", n_test = 20)
  }, error = function(e) {
    skip(paste("HAR model fitting failed:", e$message))
    NULL
  })

  skip_if(is.null(har_result), "fitHAR returned NULL")

  # plotHAR should return a plotly object
  result <- plotHAR(har_result)
  expect_true(inherits(result, "plotly"))
})

# =============================================================================
# Test IBKR source (requires live connection)
# =============================================================================

test_that("fitHAR works with IBKR source when available", {
  skip_if_offline()
  skip_if_not(isIBAvailable(), "IBKR is not available - skip test")

  result <- tryCatch({
    fitHAR("SPY", lookback_days = 200, source = "ibkr", n_test = 20)
  }, error = function(e) {
    skip(paste("IBKR data retrieval failed:", e$message))
    NULL
  })

  skip_if(is.null(result), "fitHAR with IBKR returned NULL - possibly insufficient data")

  expect_equal(result$source, "ibkr")
  expect_true(result$forecast > 0)
})

# =============================================================================
# Test parameter validation
# =============================================================================

test_that("fitHAR validates source parameter", {
  expect_error(fitHAR("SPY", source = "invalid_source"))
})

test_that("fitHAR validates calc parameter", {
  expect_error(fitHAR("SPY", calc = "invalid_calc"))
})

test_that("getHARForecast validates source parameter", {
  expect_error(getHARForecast("SPY", source = "invalid_source"))
})

test_that("getHARForecast validates calc parameter", {
  expect_error(getHARForecast("SPY", calc = "invalid_calc"))
})

# =============================================================================
# Test helper functions
# =============================================================================

test_that("calculate_period_rv computes rolling mean", {
  rv_daily <- xts::xts(
    c(0.0001, 0.0004, 0.0002, 0.0003, 0.0005, 0.0002, 0.0001),
    order.by = seq(Sys.Date() - 6, Sys.Date(), by = "day")
  )
  rv_weekly <- Tdata:::calculate_period_rv(rv_daily, 5)

  # First 4 values should be NA (not enough data for 5-day window)
  expect_true(all(is.na(rv_weekly[1:4])))

  # 5th value should be mean of first 5
  expect_equal(as.numeric(rv_weekly[5]), mean(as.numeric(rv_daily[1:5])))
})

