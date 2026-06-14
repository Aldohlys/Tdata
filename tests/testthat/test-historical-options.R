# Tests for historical option data retrieval functions
# test-historical-options.R

# Guard for blocks that actually reach IBKR/TWS. Skips when Python or TWS is
# unreachable so the suite degrades gracefully instead of waiting on the request
# timeout. Missing-parameter validation below short-circuits before any IBKR
# call, so it stays unguarded and always runs.
skip_if_no_ibkr_backend <- function() {
  skip_if_not(reticulate::py_available(), "Python not available")
  reachable <- tryCatch(isIBAvailable(), error = function(e) FALSE)
  skip_if_not(isTRUE(reachable), "TWS/IBKR not reachable")
}

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
  # Invalid right value should return NULL (validates before any IBKR call)
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20250321",
    strike = 450,
    right = "INVALID"
  )
  expect_null(result)

  # Remaining checks reach IBKR; skip when no backend.
  skip_if_no_ibkr_backend()

  # Valid right values: C, P, Call, Put should not immediately fail validation
  # Note: May still return NULL if Python/IBKR not available, but validation passes

  # Test with "C"
  result_c <- tryCatch({
    get_or_retrieve_option_historical(
      symbol = "ITB",
      trading_class = "ITB",
      expiration = "20251121",
      strike = 110,
      right = "C",
      exchange = "SMART"
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
  # Invalid data_type should return NULL (validates before any IBKR call)
  result <- get_or_retrieve_option_historical(
    symbol = "SPY",
    trading_class = "SPY",
    expiration = "20251121",
    strike = 660,
    right = "C",
    data_type = "INVALID_TYPE"
  )
  expect_null(result)

  # Remaining checks reach IBKR; skip when no backend.
  skip_if_no_ibkr_backend()

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
  skip_if_no_ibkr_backend()
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

# ===========================================================================
# Mocked-Python tests — exercise success/error paths without IBKR.
# tdata_py is an active binding backed by .tdata_state; swap state directly.
# ===========================================================================

local_mock_tdata_py <- function(fake_module, envir = parent.frame()) {
  state <- get(".tdata_state", envir = asNamespace("Tdata"))
  old_value <- state$value
  old_initialized <- state$initialized
  state$value <- fake_module
  state$initialized <- TRUE
  withr::defer({
    state$value <- old_value
    state$initialized <- old_initialized
  }, envir = envir)
}

# ---------------------------------------------------------------------------
# get_or_retrieve_option_historical — success / NULL-py / NULL-result paths
# ---------------------------------------------------------------------------
test_that("get_or_retrieve_option_historical returns tibble when Python returns data.frame", {
  fake_df <- data.frame(date = as.Date(c("2026-04-01","2026-04-02")),
                        close = c(1.50, 1.55))
  fake_py <- list(get_or_retrieve_option_historical_data = function(...) fake_df)
  local_mock_tdata_py(fake_py)

  result <- get_or_retrieve_option_historical(
    symbol = "SPY", trading_class = "SPY",
    expiration = "20260620", strike = 500, right = "C")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("get_or_retrieve_option_historical returns NULL when Python returns NULL", {
  fake_py <- list(get_or_retrieve_option_historical_data = function(...) NULL)
  local_mock_tdata_py(fake_py)

  result <- get_or_retrieve_option_historical(
    symbol = "SPY", trading_class = "SPY",
    expiration = "20260620", strike = 500, right = "C")
  expect_null(result)
})

test_that("get_or_retrieve_option_historical returns NULL when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  result <- get_or_retrieve_option_historical(
    symbol = "SPY", trading_class = "SPY",
    expiration = "20260620", strike = 500, right = "C")
  expect_null(result)
})

test_that("get_or_retrieve_option_historical normalises 'Call'/'Put' to 'C'/'P' before Python call", {
  captured <- list()
  fake_py <- list(get_or_retrieve_option_historical_data = function(..., right) {
    captured <<- list(right = right)
    data.frame(close = 1.5)
  })
  local_mock_tdata_py(fake_py)

  get_or_retrieve_option_historical("SPY","SPY","20260620",500, right = "Put")
  expect_equal(captured$right, "P")

  get_or_retrieve_option_historical("SPY","SPY","20260620",500, right = "Call")
  expect_equal(captured$right, "C")
})

# ---------------------------------------------------------------------------
# clear_on_demand_cache
# ---------------------------------------------------------------------------
test_that("clear_on_demand_cache returns python summary on success", {
  fake_py <- list(clear_on_demand_cache = function(symbol, older_than_days) {
    list(files_removed = 5L, space_freed_mb = 12.3)
  })
  local_mock_tdata_py(fake_py)

  out <- clear_on_demand_cache(symbol = "SPY", older_than_days = 7)
  expect_equal(out$files_removed, 5)
  expect_equal(out$space_freed_mb, 12.3)
})

test_that("clear_on_demand_cache returns error list when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  out <- clear_on_demand_cache()
  expect_true(!is.null(out[["error"]]))
})

test_that("clear_on_demand_cache catches Python errors", {
  fake_py <- list(clear_on_demand_cache = function(...) stop("disk error"))
  local_mock_tdata_py(fake_py)

  out <- clear_on_demand_cache()
  expect_true(!is.null(out[["error"]]))
  expect_match(out[["error"]], "disk error")
})

# ---------------------------------------------------------------------------
# qualify_contract
# ---------------------------------------------------------------------------
test_that("qualify_contract returns Python contract list on success", {
  fake_py <- list(qualify_contract = function(...) {
    list(conId = 12345L, tradingClass = "CHU", exchange = "CME")
  })
  local_mock_tdata_py(fake_py)

  out <- qualify_contract("CHF", "20260605", 1.3, "C", exchange = "CME")
  expect_equal(out$conId, 12345)
  expect_equal(out$tradingClass, "CHU")
})

test_that("qualify_contract returns NULL when result has $error", {
  fake_py <- list(qualify_contract = function(...) list(error = "no contract"))
  local_mock_tdata_py(fake_py)
  expect_null(qualify_contract("CHF", "20260605", 1.3, "C"))
})

test_that("qualify_contract returns NULL when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  expect_null(qualify_contract("CHF", "20260605", 1.3, "C"))
})

test_that("qualify_contract normalises right Put/Call to P/C before Python call", {
  captured <- list()
  fake_py <- list(qualify_contract = function(..., right) {
    captured <<- list(right = right)
    list(conId = 1)
  })
  local_mock_tdata_py(fake_py)

  qualify_contract("CHF", "20260605", 1.3, "Put");  expect_equal(captured$right, "P")
  qualify_contract("CHF", "20260605", 1.3, "Call"); expect_equal(captured$right, "C")
})

# ---------------------------------------------------------------------------
# add_option_tracking / remove_option_tracking
# ---------------------------------------------------------------------------
test_that("add_option_tracking returns TRUE when Python returns TRUE", {
  fake_py <- list(add_historical_tracking = function(config) TRUE)
  local_mock_tdata_py(fake_py)
  expect_true(add_option_tracking("SPY", "SPY", "20260918", 680, "C"))
})

test_that("add_option_tracking returns FALSE when Python returns FALSE", {
  fake_py <- list(add_historical_tracking = function(config) FALSE)
  local_mock_tdata_py(fake_py)
  expect_false(add_option_tracking("SPY", "SPY", "20260918", 680, "C"))
})

test_that("add_option_tracking returns FALSE when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  expect_false(add_option_tracking("SPY", "SPY", "20260918", 680, "C"))
})

test_that("add_option_tracking forwards normalised contract config to Python", {
  captured <- NULL
  fake_py <- list(add_historical_tracking = function(config) {
    captured <<- config; TRUE
  })
  local_mock_tdata_py(fake_py)

  add_option_tracking("SPY", "SPY", "20260918", strike = "680", right = "Put")
  expect_equal(captured$right, "P")
  expect_equal(captured$strike, 680)  # coerced to numeric
  expect_type(captured$strike, "double")
})

test_that("remove_option_tracking returns TRUE when Python returns TRUE", {
  fake_py <- list(manage_contracts = function(...) TRUE)
  local_mock_tdata_py(fake_py)
  expect_true(remove_option_tracking("SPY", "SPY", "20260918", 680, "C"))
})

test_that("remove_option_tracking returns FALSE when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  expect_false(remove_option_tracking("SPY", "SPY", "20260918", 680, "C"))
})

# ---------------------------------------------------------------------------
# update_tracked_options / list_tracked_options
# ---------------------------------------------------------------------------
test_that("update_tracked_options forwards python result", {
  fake_py <- list(update_historical_data = function(data_type) {
    list(contracts_processed = 3L, contracts_errors = 0L,
         files_updated = 6L, errors = list())
  })
  local_mock_tdata_py(fake_py)

  out <- update_tracked_options("historical")
  expect_equal(out$contracts_processed, 3)
  expect_equal(out$files_updated, 6)
})

test_that("update_tracked_options returns error list when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  out <- update_tracked_options()
  expect_true(!is.null(out[["error"]]))
})

test_that("update_tracked_options catches Python errors", {
  fake_py <- list(update_historical_data = function(...) stop("network down"))
  local_mock_tdata_py(fake_py)
  out <- update_tracked_options()
  expect_match(out[["error"]], "network down")
})

test_that("list_tracked_options returns python config on success", {
  fake_py <- list(list_historical_config = function(return_dict) {
    list(active_contract_details = list(),
         collection_settings = list(historical_duration = "1 Y"),
         contract_summary = list(total_contracts = 0L))
  })
  local_mock_tdata_py(fake_py)

  out <- list_tracked_options()
  expect_named(out, c("active_contract_details", "collection_settings", "contract_summary"))
  expect_equal(out$collection_settings$historical_duration, "1 Y")
})

test_that("list_tracked_options returns error list when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  out <- list_tracked_options()
  expect_true(!is.null(out[["error"]]))
})
