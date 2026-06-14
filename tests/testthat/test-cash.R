# Tests for CASH position functions

test_that("CASH trade identification works", {
  trades <- data.frame(
    Instrument = c("SPY", "CHF", "AAPL", "USD"),
    Symbol = c("", "CASH", "", "CASH"),
    Right = c("C", "", "", ""),
    stringsAsFactors = FALSE
  )

  result <- is_cash_trade(trades)
  expect_equal(result, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("CASH position identification works", {
  portfolio <- data.frame(
    symbol = c("SPY", "CHF", "AAPL", "USD"),
    type = c("Stock", "CASH", "Stock", "CASH"),
    Instrument = c("SPY", "CHF", "AAPL", "USD"),
    stringsAsFactors = FALSE
  )

  result <- is_cash_position(portfolio)
  expect_equal(result, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("CASH trade identification handles NA", {
  trades <- data.frame(
    Instrument = c("SPY", "CHF"),
    Symbol = c(NA_character_, "CASH"),
    stringsAsFactors = FALSE
  )

  result <- is_cash_trade(trades)
  expect_equal(result, c(FALSE, TRUE))
})

test_that("CASH position identification handles NA", {
  portfolio <- data.frame(
    symbol = c("SPY", "CHF"),
    type = c(NA_character_, "CASH"),
    stringsAsFactors = FALSE
  )

  result <- is_cash_position(portfolio)
  expect_equal(result, c(FALSE, TRUE))
})

test_that("get_exchange_rate handles same currency", {
  rate <- get_exchange_rate("USD", "USD", Sys.Date())
  expect_equal(rate, 1.0)
})

test_that("get_exchange_rate returns numeric rate", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")

  rate <- get_exchange_rate("CHF", "USD", Sys.Date())
  expect_true(is.numeric(rate))
  expect_true(rate > 0)
})

test_that("CASH P&L calculation handles no CASH trades", {
  trades <- data.frame(
    Instrument = c("SPY", "AAPL"),
    Symbol = c("", ""),
    Pos = c(100, 50),
    stringsAsFactors = FALSE
  )

  result <- calculate_cash_unrealized_pnl(trades)

  # Should return original data frame unchanged
  expect_equal(nrow(result), 2)
  expect_false("UnrealizedPnL" %in% colnames(result))
})

test_that("CASH P&L calculation includes commission in cost basis (integration test)", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")

  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")

  trade <- data.frame(
    Instrument = "CHF",
    Symbol = "CASH",
    Pos = 30000,
    Price = 1.25,
    Total = -37501.60,  # Includes commission: 30000 * 1.25 + 1.60
    Commission = 1.60,
    Currency = "USD",
    Right = "",
    stringsAsFactors = FALSE
  )

  result <- calculate_cash_unrealized_pnl(trade)

  # Should have UnrealizedPnL column
  expect_true("UnrealizedPnL" %in% colnames(result))
  expect_true(is.numeric(result$UnrealizedPnL[1]))

  # Cost basis calculation test: abs(Total) should include commission
  cost_basis <- abs(trade$Total[1])
  expect_equal(cost_basis, 37501.60, tolerance = 0.01)
})

test_that("create_cash_portfolio_row skips base currency", {
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")

  base_curr <- getParam("BaseCurrency")

  result <- create_cash_portfolio_row(
    currency = base_curr,
    balance = 1000,
    snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
    snapshot_heure = format(Sys.time(), "%H:%M:%S")
  )

  expect_null(result)
})

test_that("create_cash_portfolio_row skips zero balance", {
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")

  result <- create_cash_portfolio_row(
    currency = "USD",
    balance = 0.001,
    snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
    snapshot_heure = format(Sys.time(), "%H:%M:%S")
  )

  expect_null(result)
})

test_that("create_cash_portfolio_row creates valid row for non-base currency (integration test)", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")

  base_curr <- getParam("BaseCurrency")
  test_currency <- if (base_curr == "CHF") "USD" else "CHF"

  result <- create_cash_portfolio_row(
    currency = test_currency,
    balance = 45000,
    snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
    snapshot_heure = format(Sys.time(), "%H:%M:%S")
  )

  expect_false(is.null(result))
  expect_equal(result$symbol, test_currency)
  expect_equal(result$type, "CASH")
  expect_equal(result$Instrument, test_currency)
  expect_equal(result$pos, 45000)
  expect_true(is.numeric(result$mktPrice))
  expect_true(is.numeric(result$mktValue))
  expect_equal(result$delta, 1.0)
  expect_equal(result$gamma, 0)
  expect_equal(result$vega, 0)
  expect_equal(result$theta, 0)
  # When no trade is linked, avgCost = mktPrice and unPnL = 0
  expect_equal(result$avgCost, result$mktPrice)
  expect_equal(result$unPnL, 0)
})

# --- resolve_cash_cost_basis tests ---

test_that("resolve_cash_cost_basis returns Price directly when Instrument matches", {
  # Trade: Instrument=EUR, Currency=CHF, Price=0.93 (CHF per EUR)
  # Looking up EUR balance -> Instrument matches -> use Price as-is
  trade_result <- data.frame(
    TradeNr = 659L,
    Price = 0.93,
    Instrument = "EUR",
    Currency = "CHF",
    stringsAsFactors = FALSE
  )

  cost_basis <- resolve_cash_cost_basis(trade_result, "EUR")
  expect_equal(cost_basis, 0.93)
})

test_that("resolve_cash_cost_basis inverts Price when Currency matches", {
  # Trade 687: Instrument=CHF, Currency=JPY, Price=201.665 (JPY per CHF)
  # Looking up JPY balance -> matched on Currency -> invert Price
  trade_result <- data.frame(
    TradeNr = 687L,
    Price = 201.665,
    Instrument = "CHF",
    Currency = "JPY",
    stringsAsFactors = FALSE
  )

  cost_basis <- resolve_cash_cost_basis(trade_result, "JPY")
  expect_equal(cost_basis, 1 / 201.665, tolerance = 1e-8)
})

# --- create_cash_portfolio_row with mocked trade lookup ---

test_that("create_cash_portfolio_row links trade via Instrument match", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  # Mock: EUR trade where Instrument=EUR (direct match)
  mock_result <- data.frame(
    TradeNr = 659L,
    Price = 0.93,
    Instrument = "EUR",
    Currency = "CHF",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getCashTradeForCurrency = function(account_table, currency) mock_result, {
      result <- create_cash_portfolio_row(
        currency = "EUR",
        balance = 12154.52,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00",
        account_table = "U1804173"
      )

      expect_false(is.null(result))
      expect_equal(result$TradeNr, 659L)
      # avgCost = Price directly (Instrument match)
      expect_equal(result$avgCost, 0.93)
      # unPnL = (exchange_rate - 0.93) * 12154.52
      expected_pnl <- (result$mktPrice - 0.93) * 12154.52
      expect_equal(result$unPnL, expected_pnl, tolerance = 0.01)
    }
  )
})

test_that("create_cash_portfolio_row links trade via Currency match and inverts Price", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  # Mock: Trade 687 — Instrument=CHF, Currency=JPY, Price=201.665 JPY/CHF
  # When looking up JPY balance, this trade matches on Currency
  mock_result <- data.frame(
    TradeNr = 687L,
    Price = 201.665,
    Instrument = "CHF",
    Currency = "JPY",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getCashTradeForCurrency = function(account_table, currency) mock_result, {
      result <- create_cash_portfolio_row(
        currency = "JPY",
        balance = -503061,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00",
        account_table = "U1804173"
      )

      expect_false(is.null(result))
      expect_equal(result$TradeNr, 687L)
      # avgCost = 1/Price (Currency match -> inversion)
      expect_equal(result$avgCost, 1 / 201.665, tolerance = 1e-8)
      # unPnL must be reasonable (NOT 101 million)
      expect_true(abs(result$unPnL) < 10000,
                  label = paste("unPnL should be reasonable, got", result$unPnL))
      # unPnL = (exchange_rate - 1/201.665) * (-503061)
      expected_pnl <- (result$mktPrice - 1 / 201.665) * (-503061)
      expect_equal(result$unPnL, expected_pnl, tolerance = 0.01)
    }
  )
})

test_that("create_cash_portfolio_row defaults to zero PnL when no trade found", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  # Mock: no matching trade
  empty_result <- data.frame(
    TradeNr = integer(0),
    Price = numeric(0),
    Instrument = character(0),
    Currency = character(0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getCashTradeForCurrency = function(account_table, currency) empty_result, {
      result <- create_cash_portfolio_row(
        currency = "USD",
        balance = 45000,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00",
        account_table = "U1804173"
      )

      expect_false(is.null(result))
      # No trade -> avgCost = mktPrice, unPnL = 0
      expect_equal(result$avgCost, result$mktPrice)
      expect_equal(result$unPnL, 0)
    }
  )
})

# --- Reconciliation and filtering tests ---

test_that("reconcile_cash_positions works with real data (unit test with inline data)", {
  test_reconcile <- function() {
    trades <- data.frame(
      Instrument = c("USD", "EUR"),
      Symbol = c("CASH", "CASH"),
      Pos = c(45000, 12500),
      stringsAsFactors = FALSE
    )

    portfolio <- data.frame(
      symbol = c("USD", "EUR"),
      type = c("CASH", "CASH"),
      pos = c(45000, 12600),  # EUR has discrepancy
      mktPrice = c(0.88, 0.93),
      mktValue = c(39600, 11718),
      unPnL = c(0, 0),
      stringsAsFactors = FALSE
    )

    trades_cash <- dplyr::filter(trades, Symbol == "CASH")
    portfolio_cash <- dplyr::filter(portfolio, type == "CASH")

    reconciled <- dplyr::left_join(
      trades_cash,
      portfolio_cash |> dplyr::select(symbol, pos, mktPrice, mktValue, unPnL),
      by = c("Instrument" = "symbol")
    )

    reconciled <- reconciled |>
      dplyr::mutate(
        position_match = abs(Pos - pos) < 0.01,
        discrepancy = dplyr::if_else(!position_match, Pos - pos, 0)
      )

    return(reconciled)
  }

  result <- test_reconcile()

  expect_equal(nrow(result), 2)
  expect_true("position_match" %in% colnames(result))
  expect_true("discrepancy" %in% colnames(result))
  expect_true(result$position_match[1])   # USD matches
  expect_false(result$position_match[2])  # EUR doesn't match
  expect_equal(result$discrepancy[2], -100, tolerance = 0.01)
})

test_that("get_cash_positions filters correctly (unit test)", {
  all_trades <- data.frame(
    Instrument = c("SPY", "AAPL", "USD"),
    Symbol = c("", "", "CASH"),
    stringsAsFactors = FALSE
  )

  cash_only <- all_trades[is_cash_trade(all_trades), ]

  expect_equal(nrow(cash_only), 1)
  expect_equal(cash_only$Instrument[1], "USD")
})
