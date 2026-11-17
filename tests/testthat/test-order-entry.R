# R wrappers of tdata_py/order_entry.py: the Python call is mocked, no TWS.

test_that("NA prices and currency go to Python as NULL, never NaN", {
  seen <- NULL
  with_mocked_bindings(
    orderEntryQuery = function(fn, args) { seen <<- list(fn = fn, args = args); list(ok = TRUE) }, {
      previewStockOrder("U1804173", "SPY", "buy", 21, "limit", limit = 738.85)
    })
  expect_equal(seen$fn, "preview_stock_order")
  expect_equal(seen$args$action, "BUY")
  expect_equal(seen$args$limit, 738.85)
  expect_null(seen$args$take_profit)
  expect_null(seen$args$stop_loss)
  expect_null(seen$args$currency)
  expect_false("take_profit" %in% names(Filter(function(x) is.numeric(x) && is.nan(x), seen$args)))
})

test_that("bracket passes its three prices and the account", {
  seen <- NULL
  with_mocked_bindings(
    orderEntryQuery = function(fn, args) { seen <<- args
      list(ok = TRUE, message = NULL, order_ids = list(11L, 12L, 13L), statuses = list("PreSubmitted", "", "")) }, {
      r <- stageStockOrder("DU5221795", "SPY", "BUY", 21, "bracket",
                           limit = 738.85, take_profit = 766.55, stop_loss = 725)
      expect_identical(r$order_ids, c(11L, 12L, 13L))
      expect_identical(r$statuses, c("PreSubmitted", "", ""))
    })
  expect_equal(seen$account, "DU5221795")
  expect_equal(c(seen$limit, seen$take_profit, seen$stop_loss), c(738.85, 766.55, 725))
})

test_that("Python unavailable: wrappers return ok = FALSE / NULL", {
  with_mocked_bindings(orderEntryQuery = function(fn, args) NULL, {
    expect_false(previewStockOrder("U1804173", "SPY", "BUY", 1, "market")$ok)
    r <- stageStockOrder("U1804173", "SPY", "BUY", 1, "market")
    expect_false(r$ok)
    expect_length(r$order_ids, 0)
    expect_null(quoteStock("SPY"))
  })
})

test_that("quote: missing values become NA", {
  with_mocked_bindings(
    orderEntryQuery = function(fn, args) list(bid = 738.8, ask = 738.9, last = NULL, close = 735,
                                              mid = 738.85, mktdata_type = 2L, conId = 756733L,
                                              currency = "USD"), {
      q <- quoteStock("SPY")
      expect_true(is.na(q$last))
      expect_equal(q$mid, 738.85)
      expect_identical(q$mktdata_type, 2L)
    })
})

test_that("account and symbol are programming contracts", {
  expect_error(previewStockOrder(NULL, "SPY", "BUY", 1, "market"))
  expect_error(stageStockOrder("U1804173", c("SPY", "QQQ"), "BUY", 1, "market"))
})
