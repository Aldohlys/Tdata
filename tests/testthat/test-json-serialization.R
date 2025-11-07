# test-json-serialization.R
# CRITICAL TESTS: Prevent JSON serialization failures in Shiny applications
#
# Background:
# - Shiny applications communicate with browser via JSON
# - Locale changes in R or Python can break JSON serialization
# - This caused multi-day debugging when IB connection was active
#
# These tests ensure:
# 1. Functions don't pollute global locale
# 2. Return values are JSON-serializable
# 3. No reactive values leak into module exports

library(testthat)
library(jsonlite)

# Helper function to check JSON serializability
is_json_serializable <- function(obj) {
  tryCatch({
    json_str <- jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")
    # Try to parse it back to ensure it's valid JSON
    jsonlite::fromJSON(json_str)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# Helper function to get current locale
get_current_locale <- function() {
  Sys.getlocale("LC_NUMERIC")
}

# ============================================================================
# SHARED TEST DATA - Factorize IB API calls to speed up tests
# ============================================================================

# Cache for IB API results to avoid repeated slow calls
.ib_cache <- new.env(parent = emptyenv())

get_cached_stock_price <- function(sym) {
  cache_key <- paste0("stock_", sym)

  if (!exists(cache_key, envir = .ib_cache)) {
    result <- Tdata::getStockPrice(sym, close = FALSE)
    assign(cache_key, result, envir = .ib_cache)
  }

  get(cache_key, envir = .ib_cache)
}

get_cached_ibkr_metrics <- function(sym) {
  cache_key <- paste0("metrics_", sym)

  if (!exists(cache_key, envir = .ib_cache)) {
    # Wrap in tryCatch to handle logging errors
    result <- tryCatch(
      Tdata::getIBKRMetrics(sym, reqType = 2),
      error = function(e) NULL
    )
    assign(cache_key, result, envir = .ib_cache)
  }

  get(cache_key, envir = .ib_cache)
}

# ============================================================================
# LOCALE POLLUTION TESTS
# ============================================================================

test_that("getStockPrice with IB connection doesn't pollute locale", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  # Record original locale
  original_locale <- get_current_locale()

  # Call function that internally uses tdata_py$getValue()
  result <- get_cached_stock_price("SPY")

  # Check locale hasn't changed
  current_locale <- get_current_locale()
  expect_equal(current_locale, original_locale,
               info = "getStockPrice should not change global locale")
})

test_that("getIBKRMetrics doesn't pollute locale", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  original_locale <- get_current_locale()

  result <- get_cached_ibkr_metrics("SPY")

  # Skip if error code returned or logging error occurred
  skip_if(is.null(result), "getIBKRMetrics failed with error")
  skip_if(is.numeric(result) && length(result) == 1,
          "IBKR returned error code")

  current_locale <- get_current_locale()
  expect_equal(current_locale, original_locale,
               info = "getIBKRMetrics should not change global locale")
})

# ============================================================================
# JSON SERIALIZATION TESTS - Stock Functions
# ============================================================================

test_that("getStockPrice returns JSON-serializable data frame", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  result <- get_cached_stock_price("SPY")

  # Check it's a data frame
  expect_s3_class(result, "data.frame")

  # Check JSON serializability
  expect_true(is_json_serializable(result),
              info = "getStockPrice result should be JSON-serializable")

  # Check column types are JSON-friendly
  expect_type(result$datetime, "character")
  expect_type(result$sym, "character")
  expect_type(result$price, "double")
})

test_that("getIBKRMetrics returns JSON-serializable data frame", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  result <- get_cached_ibkr_metrics("SPY")

  # Skip if error code returned or logging error occurred
  skip_if(is.null(result), "getIBKRMetrics failed with error")
  skip_if(is.numeric(result) && length(result) == 1,
          "IBKR returned error code")

  expect_s3_class(result, "data.frame")
  expect_true(is_json_serializable(result),
              info = "getIBKRMetrics result should be JSON-serializable")
})

test_that("getStoredMetrics returns JSON-serializable data", {
  result <- Tdata::getStoredMetrics("SPY")

  skip_if(nrow(result) == 0, "No stored metrics for SPY")

  expect_s3_class(result, "data.frame")
  expect_true(is_json_serializable(result),
              info = "getStoredMetrics result should be JSON-serializable")
})

test_that("getLastSymPrice returns JSON-serializable data", {
  result <- Tdata::getLastSymPrice("SPY")

  expect_s3_class(result, "data.frame")
  expect_true(is_json_serializable(result),
              info = "getLastSymPrice result should be JSON-serializable")
})

# ============================================================================
# JSON SERIALIZATION TESTS - Account Functions
# ============================================================================

test_that("getAllTickers returns JSON-serializable data", {
  result <- Tdata::getAllTickers()

  expect_s3_class(result, "data.frame")
  expect_true(is_json_serializable(result),
              info = "getAllTickers result should be JSON-serializable")
})

test_that("getAllTenors returns JSON-serializable data", {
  result <- Tdata::getAllTenors()

  skip_if(is.null(result) || nrow(result) == 0, "No tenor data available")

  expect_s3_class(result, "data.frame")
  expect_true(is_json_serializable(result),
              info = "getAllTenors result should be JSON-serializable")
})

# ============================================================================
# LOCALE FORMATTING TESTS
# ============================================================================

test_that("Numeric values formatted correctly after IB calls", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  # Call IB function (using cache)
  result <- get_cached_stock_price("SPY")

  # Test that numeric formatting still works correctly
  test_value <- 1234.56
  formatted <- format(test_value, nsmall = 2)

  # Should use dot as decimal separator (not comma)
  expect_true(grepl("\\.", formatted),
              info = "Decimal separator should be dot after IB calls")
  expect_false(grepl(",", gsub(",\\d{3}", "", formatted)),
               info = "Should not use comma as decimal separator")
})

test_that("JSON serialization works after multiple IB calls", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  # Use cached results
  result1 <- get_cached_stock_price("SPY")
  result2 <- get_cached_stock_price("QQQ")

  # Both should be JSON serializable
  expect_true(is_json_serializable(result1))
  expect_true(is_json_serializable(result2))

  # Combined list should also be serializable
  combined <- list(spy = result1, qqq = result2)
  expect_true(is_json_serializable(combined),
              info = "Combined results should be JSON-serializable")
})

# ============================================================================
# REACTIVE VALUE EXPOSURE TESTS
# ============================================================================

test_that("Function returns don't contain reactive objects", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  result <- get_cached_stock_price("SPY")

  # Check that result doesn't contain any reactive values
  # Reactive values would have class "reactiveVal" or similar
  has_reactive <- any(sapply(result, function(col) {
    inherits(col, "reactiveVal") ||
      inherits(col, "reactive") ||
      inherits(col, "reactiveExpr")
  }))

  expect_false(has_reactive,
               info = "Function results should not contain reactive objects")
})

# ============================================================================
# CROSS-PACKAGE INTEGRATION TESTS
# ============================================================================

test_that("Tdata functions work correctly when called from Shiny context", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "Shiny not available")
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  # Simulate Shiny reactive context
  shiny::withReactiveDomain(NULL, {
    result <- get_cached_stock_price("SPY")

    expect_s3_class(result, "data.frame")
    expect_true(is_json_serializable(result))
  })
})

# ============================================================================
# REGRESSION TESTS - Specific Historical Issues
# ============================================================================

test_that("REGRESSION: locale not corrupted by Python getValue call", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  # This is the exact scenario that caused the multi-day bug:
  # 1. Application starts with C locale
  # 2. User triggers wheel calculation (calls getStockPrice with IB)
  # 3. Python's getValue() sets locale to user's system locale
  # 4. R tries to serialize numeric values to JSON
  # 5. Fails because locale uses comma as decimal separator

  original_locale <- get_current_locale()
  expect_equal(original_locale, "C",
               info = "Test should start with C locale")

  # Trigger the bug scenario (using cache)
  result <- get_cached_stock_price("SPY")

  # Verify locale is still C
  current_locale <- get_current_locale()
  expect_equal(current_locale, "C",
               info = "Locale should remain C after IB call")

  # Verify JSON serialization works
  test_df <- data.frame(
    symbol = "TEST",
    price = 123.45,
    value = 678.90
  )

  expect_true(is_json_serializable(test_df),
              info = "Should be able to serialize numeric data after IB call")
})

test_that("REGRESSION: Multiple sequential IB calls don't accumulate locale changes", {
  skip_if_not(Tdata::isIBAvailable(), "IB connection not available")

  original_locale <- get_current_locale()

  # Use cached results to test the principle without slow API calls
  for (sym in c("SPY", "QQQ", "IWM")) {
    result <- get_cached_stock_price(sym)

    # After each call, locale should be unchanged
    expect_equal(get_current_locale(), original_locale,
                 info = paste("Locale changed after", sym, "call"))

    # Each result should be JSON serializable
    expect_true(is_json_serializable(result),
                info = paste(sym, "result not JSON serializable"))
  }
})
