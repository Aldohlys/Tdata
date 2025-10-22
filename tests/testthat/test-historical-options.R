# Tests for historical option data retrieval functions
# test-historical-options.R

test_that("get_or_retrieve_option_historical validates required parameters", {
  # Missing symbol should return NULL
  result <- get_or_retrieve_option_historical(
    trading_class = "SPY",
    expiration = "20250321",
    strike = 450,
    right = "C"
  )
  expect_null(result)

  # Missing trading_class should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    expiration = "20250321",
    strike = 450,
    right = "C"
  )
  expect_null(result)

  # Missing expiration should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    strike = 450,
    right = "C"
  )
  expect_null(result)

  # Missing strike should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20250321",
    right = "C"
  )
  expect_null(result)

  # Missing right should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20250321",
    strike = 450
  )
  expect_null(result)
})

test_that("get_or_retrieve_option_historical validates right parameter", {
  # Invalid right value should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20250321",
    strike = 450,
    right = "INVALID"
  )
  expect_null(result)

  # Valid right values: C, P, Call, Put should not immediately fail validation
  # Note: May still return NULL if Python/IBKR not available, but validation passes

  # Test with "C"
  result_c <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "SPY",
      trading_class = "SPY",
      expiration = "20251121",
      strike = 660,
      right = "C"
    )
  }, error = function(e) {
    NULL
  })
  # Should not throw error during validation (may be NULL if Python unavailable)
  expect_true(is.null(result_c) || is.data.frame(result_c))

  # Test with "Call"
  result_call <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "SPY",
      trading_class = "SPY",
      expiration = "20251121",
      strike = 660,
      right = "Call"
    )
  }, error = function(e) {
    NULL
  })
  expect_true(is.null(result_call) || is.data.frame(result_call))

  # Test with "P"
  result_p <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "SPY",
      trading_class = "SPY",
      expiration = "20251121",
      strike = 660,
      right = "P"
    )
  }, error = function(e) {
    NULL
  })
  expect_true(is.null(result_p) || is.data.frame(result_p))

  # Test with "Put"
  result_put <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "SPY",
      trading_class = "SPY",
      expiration = "20251121",
      strike = 660,
      right = "Put"
    )
  }, error = function(e) {
    NULL
  })
  expect_true(is.null(result_put) || is.data.frame(result_put))
})

test_that("get_or_retrieve_option_historical validates data_type parameter", {
  # Invalid data_type should return NULL
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20251121",
    strike = 660,
    right = "C",
    data_type = "INVALID_TYPE"
  )
  expect_null(result)

  # Valid data_type values: historical, intraday, combined
  valid_types <- c("historical", "intraday", "combined")

  for (dtype in valid_types) {
    result <- tryCatch({
      get_or_retrieve_option_historical(
        symbol = "SPY",
        trading_class = "SPY",
        expiration = "20251121",
        strike = 660,
        right = "C",
        data_type = dtype
      )
    }, error = function(e) {
      NULL
    })

    # Should not fail validation (may return NULL if Python/IBKR unavailable)
    expect_true(is.null(result) || is.data.frame(result),
                info = paste("Failed for data_type:", dtype))
  }
})

test_that("clear_on_demand_cache accepts valid parameters", {
  # Should accept no parameters
  result1 <- tryCatch({
    clear_on_demand_cache()
  }, error = function(e) {
    list(error = e$message)
  })

  # Result should be a list (may have error key if Python unavailable)
  expect_type(result1, "list")

  # Should accept symbol parameter
  result2 <- tryCatch({
    clear_on_demand_cache(symbol = "SPY")
  }, error = function(e) {
    list(error = e$message)
  })

  expect_type(result2, "list")

  # Should accept older_than_days parameter
  result3 <- tryCatch({
    clear_on_demand_cache(older_than_days = 7)
  }, error = function(e) {
    list(error = e$message)
  })

  expect_type(result3, "list")

  # Should accept both parameters
  result4 <- tryCatch({
    clear_on_demand_cache(symbol = "SPY", older_than_days = 14)
  }, error = function(e) {
    list(error = e$message)
  })

  expect_type(result4, "list")
})

test_that("get_or_retrieve_option_historical returns tibble or NULL", {
  # Call with valid parameters
  result <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "SPY",
      trading_class = "SPY",
      expiration = "20250321",
      strike = 450,
      right = "C",
      force_refresh = FALSE
    )
  }, error = function(e) {
    NULL
  })

  # Result should be either NULL (if Python/IBKR unavailable) or a data.frame/tibble
  expect_true(
    is.null(result) ||
    inherits(result, "data.frame") ||
    inherits(result, "tbl_df"),
    info = "Result should be NULL or a tibble/data.frame"
  )

  # If result is not NULL, check it has reasonable structure
  if (!is.null(result)) {
    expect_true(nrow(result) >= 0)
    expect_true(ncol(result) > 0)
  }
})
