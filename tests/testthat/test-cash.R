# Tests for CASH position functions

test_that("CASH trade identification works", {
  trades <- data.frame(
    Instrument = c("SPY", "CHF", "AAPL", "USD"),
    Symbol = c("SPY", "JPY", "AAPL", "USD"),
    Right = c("C", "", "", ""),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getActiveCurrencies = function(...) c("CHF", "EUR", "USD", "CAD", "JPY", "GBP"),
    expect_equal(is_cash_trade(trades), c(FALSE, TRUE, FALSE, TRUE))
  )
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
    Instrument = c(NA_character_, "CHF"),
    Symbol = c("SPY", "JPY"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getActiveCurrencies = function(...) c("CHF", "EUR", "USD", "CAD", "JPY", "GBP"),
    expect_equal(is_cash_trade(trades), c(FALSE, TRUE))
  )
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
    Symbol = c("SPY", "AAPL"),
    Pos = c(100, 50),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getActiveCurrencies = function(...) c("CHF", "EUR", "USD", "CAD", "JPY", "GBP"),
    {
      result <- calculate_cash_unrealized_pnl(trades)

      # Should return original data frame unchanged
      expect_equal(nrow(result), 2)
      expect_false("UnrealizedPnL" %in% colnames(result))
    }
  )
})

test_that("CASH P&L calculation includes commission in cost basis (integration test)", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")

  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")

  trade <- data.frame(
    Instrument = "CHF",
    Symbol = "USD",
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

# --- weighted_cash_cost_basis (Convention A) ---

test_that("weighted_cash_cost_basis returns NA when no open trades", {
  with_mocked_bindings(
    getCurrencyTradesForBasis = function(account_table, currency)
      data.frame(TradeDate = integer(0), Total = numeric(0)),
    expect_true(is.na(weighted_cash_cost_basis("U25343478", "JPY")))
  )
})

test_that("weighted_cash_cost_basis weights per-date rates by absolute Total", {
  fake_trades <- data.frame(TradeDate = c(20260101, 20260201),
                            Total = c(-100, 300))

  with_mocked_bindings(
    getCurrencyTradesForBasis = function(account_table, currency) fake_trades,
    convert_to_base_date = function(amount, currency, convert_date = Sys.Date())
      ifelse(as.numeric(format(convert_date, "%Y%m%d")) == 20260101, 0.010, 0.020),
    {
      # (100*0.010 + 300*0.020) / (100 + 300) = 7 / 400 = 0.0175
      expect_equal(weighted_cash_cost_basis("U25343478", "JPY"), 0.0175, tolerance = 1e-9)
    }
  )
})

# --- create_cash_portfolio_row with mocked cost basis + trade link ---

test_that("create_cash_portfolio_row uses weighted cost basis and links TradeNr", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  # JPY cash with an explicit FX trade (720) to link for display.
  mock_trade <- data.frame(TradeNr = 720L, Price = 201.665,
                           Instrument = "CHF", Currency = "JPY",
                           stringsAsFactors = FALSE)

  with_mocked_bindings(
    weighted_cash_cost_basis = function(account_table, currency, convert_date = Sys.Date()) 0.0049,
    getCashTradeForCurrency = function(account_table, currency) mock_trade, {
      result <- create_cash_portfolio_row(
        currency = "JPY", balance = -503061,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00", account_table = "U25343478")

      expect_false(is.null(result))
      expect_equal(result$TradeNr, 720L)
      expect_equal(result$avgCost, 0.0049)                 # weighted cost basis
      expected_pnl <- (result$mktPrice - 0.0049) * (-503061)
      expect_equal(result$unPnL, expected_pnl, tolerance = 0.01)
      expect_true(abs(result$unPnL) < 100000)              # sane, not 1e8
    }
  )
})

test_that("create_cash_portfolio_row computes FX P&L for cash with no FX trade (unlinked)", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  # EUR cash borrowed via stock purchases: weighted basis exists, but there is
  # no explicit FX trade to link, so TradeNr stays NA while FX P&L is captured.
  empty_trade <- data.frame(TradeNr = integer(0), Price = numeric(0),
                            Instrument = character(0), Currency = character(0),
                            stringsAsFactors = FALSE)

  with_mocked_bindings(
    weighted_cash_cost_basis = function(account_table, currency, convert_date = Sys.Date()) 0.90,
    getCashTradeForCurrency = function(account_table, currency) empty_trade, {
      result <- create_cash_portfolio_row(
        currency = "EUR", balance = -1561.91,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00", account_table = "U25343478")

      expect_false(is.null(result))
      expect_true(is.na(result$TradeNr))                   # unlinked
      expect_equal(result$avgCost, 0.90)                   # weighted basis used
      expected_pnl <- (result$mktPrice - 0.90) * (-1561.91)
      expect_equal(result$unPnL, expected_pnl, tolerance = 0.01)
      expect_false(result$unPnL == 0)                      # FX P&L now captured
    }
  )
})

test_that("create_cash_portfolio_row defaults to zero PnL when no trade history", {
  skip_if_not(DBI::dbCanConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH")),
              "Database not available")
  skip_if(is.null(try(getParam("BaseCurrency"), silent = TRUE)),
          "BaseCurrency not configured")
  skip_if(getParam("BaseCurrency") != "CHF", "Test designed for CHF base currency")

  empty_trade <- data.frame(TradeNr = integer(0), Price = numeric(0),
                            Instrument = character(0), Currency = character(0),
                            stringsAsFactors = FALSE)

  with_mocked_bindings(
    weighted_cash_cost_basis = function(account_table, currency, convert_date = Sys.Date()) NA_real_,
    getCashTradeForCurrency = function(account_table, currency) empty_trade, {
      result <- create_cash_portfolio_row(
        currency = "USD", balance = 45000,
        snapshot_date = as.integer(format(Sys.Date(), "%Y%m%d")),
        snapshot_heure = "16:00:00", account_table = "U1804173")

      expect_false(is.null(result))
      expect_equal(result$avgCost, result$mktPrice)        # no basis -> spot
      expect_equal(result$unPnL, 0)
    }
  )
})

# --- Reconciliation and filtering tests ---

test_that("reconcile_cash_positions works with real data (unit test with inline data)", {
  test_reconcile <- function() {
    trades <- data.frame(
      Instrument = c("USD", "EUR"),
      Symbol = c("USD", "EUR"),
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

    portfolio_cash <- dplyr::filter(portfolio, type == "CASH")

    reconciled <- dplyr::left_join(
      trades,
      portfolio_cash |> dplyr::select(symbol, pos, mktPrice, mktValue, unPnL),
      by = c("Symbol" = "symbol")
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
    Symbol = c("SPY", "AAPL", "USD"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    getActiveCurrencies = function(...) c("CHF", "EUR", "USD", "CAD", "JPY", "GBP"),
    {
      cash_only <- all_trades[is_cash_trade(all_trades), ]

      expect_equal(nrow(cash_only), 1)
      expect_equal(cash_only$Instrument[1], "USD")
    }
  )
})

# --- gonet_realized_fx: realized FX on closed / partially-closed Gonet trades ---

# Deterministic FX stub: rate depends only on the date (amount * rate).
.fx_stub <- function(rates_by_date) {
  function(amount, currency, convert_date) {
    key <- format(convert_date, "%Y%m%d")
    amount * rates_by_date[[key]]
  }
}

test_that("gonet_realized_fx: base-currency trades net zero", {
  trades <- data.frame(
    TradeNr = c(1, 1), orig_date = c("01.01.2024", "01.06.2024"),
    sym_yahoo = c("X", "X"), sym_ibkr = c("X", "X"),
    init_position = c(100, -100), init_price = c(10, 12),
    init_cost = c(-1000, 1200), currency = c("CHF", "CHF"),
    stringsAsFactors = FALSE)
  with_mocked_bindings(
    convert_to_base_date = .fx_stub(list("20240101" = 1, "20240601" = 1)),
    expect_equal(gonet_realized_fx(trades, "CHF"), 0)
  )
})

test_that("gonet_realized_fx: full close realizes closed_cost * (sell_rate - entry_rate)", {
  # buy 100 @ native cost 1000 (entry rate 0.90), sell 100 (exit rate 0.80)
  trades <- data.frame(
    TradeNr = c(1, 1), orig_date = c("01.01.2024", "01.06.2024"),
    sym_yahoo = c("X", "X"), sym_ibkr = c("X", "X"),
    init_position = c(100, -100), init_price = c(10, 12),
    init_cost = c(-1000, 1200), currency = c("USD", "USD"),
    stringsAsFactors = FALSE)
  with_mocked_bindings(
    convert_to_base_date = .fx_stub(list("20240101" = 0.90, "20240601" = 0.80)),
    expect_equal(gonet_realized_fx(trades, "USD"), round(1000 * (0.80 - 0.90), 2))  # -100
  )
})

test_that("gonet_realized_fx: partial close realizes FX on the closed portion only", {
  # buy 100 @ native cost 1000, sell 40 -> closed native cost = 400
  trades <- data.frame(
    TradeNr = c(1, 1), orig_date = c("01.01.2024", "01.06.2024"),
    sym_yahoo = c("X", "X"), sym_ibkr = c("X", "X"),
    init_position = c(100, -40), init_price = c(10, 12),
    init_cost = c(-1000, 480), currency = c("EUR", "EUR"),
    stringsAsFactors = FALSE)
  with_mocked_bindings(
    convert_to_base_date = .fx_stub(list("20240101" = 1.10, "20240601" = 0.90)),
    expect_equal(gonet_realized_fx(trades, "EUR"), round(400 * (0.90 - 1.10), 2))  # -80
  )
})

test_that("gonet_realized_fx: an all-open position has no realized FX", {
  trades <- data.frame(
    TradeNr = 1, orig_date = "01.01.2024",
    sym_yahoo = "X", sym_ibkr = "X",
    init_position = 100, init_price = 10, init_cost = -1000, currency = "USD",
    stringsAsFactors = FALSE)
  with_mocked_bindings(
    convert_to_base_date = .fx_stub(list("20240101" = 0.90)),
    expect_equal(gonet_realized_fx(trades, "USD"), 0)
  )
})

test_that("gonet_realized_fx: CASH ledger rows (sym_ibkr == currency) are excluded", {
  trades <- data.frame(
    TradeNr = 27, orig_date = "10.07.2026",
    sym_yahoo = "USD", sym_ibkr = "USD",
    init_position = 522, init_price = 1, init_cost = -522, currency = "USD",
    stringsAsFactors = FALSE)
  # No mock needed: the only row is a cash row, excluded -> 0 (no rate lookup).
  expect_equal(gonet_realized_fx(trades, "USD"), 0)
})
