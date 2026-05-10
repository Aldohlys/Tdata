# Tests for ibkr.R — input normalization, validation, and result handling.
# All Python calls are mocked via the .tdata_state shim used elsewhere.

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
# isIBAvailable
# ---------------------------------------------------------------------------
test_that("isIBAvailable returns FALSE when tdata_py module is NULL", {
  local_mock_tdata_py(NULL)
  expect_false(isIBAvailable())
})

test_that("isIBAvailable returns TRUE when Python reports connected", {
  local_mock_tdata_py(list(isIBAvailable = function() TRUE))
  expect_true(isIBAvailable())
})

test_that("isIBAvailable returns FALSE when Python reports not connected", {
  local_mock_tdata_py(list(isIBAvailable = function() FALSE))
  expect_false(isIBAvailable())
})

# ---------------------------------------------------------------------------
# getOptIBKRPrice — input normalization (right, expiration, tradingClass)
# ---------------------------------------------------------------------------
test_that("getOptIBKRPrice rejects tradingClass='Stock' via display_error_message", {
  # display_error_message calls stop(); return(NA) after it is dead code.
  expect_error(
    getOptIBKRPrice("SPY", tradingClass = "Stock", right = "P",
                    strike = 500, expiration = "20260620"),
    "valid Trading Class")
})

test_that("getOptIBKRPrice normalises 'Put'/'Call' to 'P'/'C' before Python call", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(right = right, expiration = expiration)
    list(value = 1.5)
  })
  local_mock_tdata_py(fake_py)

  getOptIBKRPrice("SPY", "SPY", "Put",  500, "20260620")
  expect_equal(captured$right, "P")
  getOptIBKRPrice("SPY", "SPY", "Call", 500, "20260620")
  expect_equal(captured$right, "C")
})

test_that("getOptIBKRPrice converts numeric expiration to character", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(expiration = expiration)
    list(value = 1.5)
  })
  local_mock_tdata_py(fake_py)

  getOptIBKRPrice("SPY", "SPY", "P", 500, expiration = 20260620)
  expect_type(captured$expiration, "character")
  expect_equal(captured$expiration, "20260620")
})

test_that("getOptIBKRPrice converts Date expiration to YYYYMMDD string", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(expiration = expiration)
    list(value = 1.5)
  })
  local_mock_tdata_py(fake_py)

  getOptIBKRPrice("SPY", "SPY", "P", 500, expiration = as.Date("2026-06-20"))
  expect_equal(captured$expiration, "20260620")
})

test_that("getOptIBKRPrice errors on non-character / non-Date / non-numeric expiration", {
  fake_py <- list(getOptValue = function(...) list(value = 1.5))
  local_mock_tdata_py(fake_py)

  expect_error(
    getOptIBKRPrice("SPY", "SPY", "P", 500, expiration = list(2026, 06, 20)),
    "expiration date must be either")
})

test_that("getOptIBKRPrice returns -1 when Python value is NULL", {
  fake_py <- list(getOptValue = function(...) list(value = NULL))
  local_mock_tdata_py(fake_py)
  expect_equal(getOptIBKRPrice("SPY", "SPY", "P", 500, "20260620"), -1)
})

test_that("getOptIBKRPrice returns -1 when Python value is NaN", {
  fake_py <- list(getOptValue = function(...) list(value = NaN))
  local_mock_tdata_py(fake_py)
  expect_equal(getOptIBKRPrice("SPY", "SPY", "P", 500, "20260620"), -1)
})

test_that("getOptIBKRPrice returns price when Python returns numeric value", {
  fake_py <- list(getOptValue = function(...) list(value = 2.75))
  local_mock_tdata_py(fake_py)
  expect_equal(getOptIBKRPrice("SPY", "SPY", "P", 500, "20260620"), 2.75)
})

test_that("getOptIBKRPrice coerces strike to numeric before Python call", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(strikes = strikes)
    list(value = 1)
  })
  local_mock_tdata_py(fake_py)

  getOptIBKRPrice("SPY", "SPY", "P", strike = "500", expiration = "20260620")
  expect_type(captured$strikes, "double")
  expect_equal(captured$strikes, 500)
})

# ---------------------------------------------------------------------------
# getOptMarketData — vector strikes, mid-price computation, NULL guard
# ---------------------------------------------------------------------------
test_that("getOptMarketData computes mid = (bid + ask) / 2", {
  fake_result <- list(
    strike      = c(500, 510),
    value       = c(1.5, 1.0),
    bid         = c(1.4, 0.9),
    ask         = c(1.6, 1.1),
    last        = c(1.5, 1.0),
    spread      = c(0.2, 0.2),
    impliedvol  = c(0.20, 0.21),
    delta       = c(-0.4, -0.3)
  )
  fake_py <- list(getOptValue = function(...) fake_result)
  local_mock_tdata_py(fake_py)

  out <- getOptMarketData("SPY", "P", c(500, 510), "20260620")
  expect_equal(out$mid, c(1.5, 1.0))
})

test_that("getOptMarketData returns NULL when Python returns NULL", {
  fake_py <- list(getOptValue = function(...) NULL)
  local_mock_tdata_py(fake_py)
  expect_null(getOptMarketData("SPY", "P", c(500, 510), "20260620"))
})

test_that("getOptMarketData normalises 'Call'/'Put' to 'C'/'P' before Python call", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(right = right)
    list(strike = 500, value = 1, bid = 0.9, ask = 1.1)
  })
  local_mock_tdata_py(fake_py)

  getOptMarketData("SPY", "Call", 500, "20260620"); expect_equal(captured$right, "C")
  getOptMarketData("SPY", "Put",  500, "20260620"); expect_equal(captured$right, "P")
})

test_that("getOptMarketData converts Date expiration to YYYYMMDD before Python call", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(expiration = expiration)
    list(strike = 500, value = 1, bid = 0.9, ask = 1.1)
  })
  local_mock_tdata_py(fake_py)

  getOptMarketData("SPY", "P", 500, expiration = as.Date("2026-06-20"))
  expect_equal(captured$expiration, "20260620")
})

test_that("getOptMarketData wraps strikes vector in as.list before Python call", {
  captured <- list()
  fake_py <- list(getOptValue = function(sym, expiration, strikes, right, force_refresh) {
    captured <<- list(strikes = strikes)
    list(strike = c(500, 510), value = c(1, 2), bid = c(0.9, 1.9), ask = c(1.1, 2.1))
  })
  local_mock_tdata_py(fake_py)

  getOptMarketData("SPY", "P", c(500, 510), "20260620")
  expect_type(captured$strikes, "list")
  expect_equal(unlist(captured$strikes), c(500, 510))
})

test_that("getOptMarketData errors on non-character / non-Date / non-numeric expiration", {
  fake_py <- list(getOptValue = function(...) list(value = 1, bid = 1, ask = 1, strike = 500))
  local_mock_tdata_py(fake_py)

  expect_error(
    getOptMarketData("SPY", "P", 500, expiration = list(2026, 06, 20)),
    "expiration date must be either")
})

# ---------------------------------------------------------------------------
# getSliceAllIBKRMetrics — index validation
# ---------------------------------------------------------------------------
test_that("getSliceAllIBKRMetrics errors when first == 0", {
  with_mocked_bindings(
    getAllTickers = function() data.frame(Name = c("SPY", "AAPL"),
                                          Exchange = c("SMART", "SMART")), {
      expect_error(getSliceAllIBKRMetrics(first = 0))
    })
})

test_that("getSliceAllIBKRMetrics errors when last exceeds nrow(tickers)", {
  with_mocked_bindings(
    getAllTickers = function() data.frame(Name = c("SPY", "AAPL"),
                                          Exchange = c("SMART", "SMART")), {
      expect_error(getSliceAllIBKRMetrics(first = 1, last = 99))
    })
})

test_that("getSliceAllIBKRMetrics filters tickers to SMART/EUREX/CBOE only", {
  fetched <- character()
  with_mocked_bindings(
    getAllTickers = function() data.frame(
      Name = c("SPY", "ABBN", "USO_LSE", "SHY"),
      Exchange = c("SMART", "EUREX", "LSEETF", "CBOE"),
      stringsAsFactors = FALSE),
    getIBKRMetrics = function(sym, ...) {
      fetched <<- c(fetched, sym)
      data.frame(sym = sym, price = 100)
    }, {
      result <- getSliceAllIBKRMetrics()
      expect_setequal(fetched, c("SPY", "ABBN", "SHY"))  # LSEETF filtered out
      expect_equal(nrow(result), 3)
    })
})
