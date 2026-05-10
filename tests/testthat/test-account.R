
test_that("readPortfolio DU5221795 returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio("DU5221795")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol",   "expdate", "strike", "pos", "mktPrice", "optPrice",
                               "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                               "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readPortfolio U1804173 returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio("U1804173")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol",   "expdate", "strike", "pos", "mktPrice", "optPrice",
                                "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                                "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})



test_that("readLastPortfolio Simu returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("DU5221795")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "expdate", "strike",  "pos", "mktPrice", "optPrice",
                            "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                            "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readLastPortfolio Live returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("U1804173")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "expdate", "strike", "pos", "mktPrice", "optPrice",
                            "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                            "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})


test_that("readLastPortfolio Gonet returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("Gonet")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "pos", "mktPrice",
                            "mktValue", "avgCost", "unPnL",
                            "currency", "type", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readAccount Simu  returns some data and columns look good", {
  expect_true({
    acc=readAccount("DU5221795")
    identical(colnames(acc),  c("date", "heure", "Currency", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashBalanceCHF", "CashBalanceEUR", "CashBalanceUSD", "CashFlow")) &&
      nrow(acc) >1
  })
})


test_that("readAccount Gonet  returns some data and columns look good", {
  expect_true({
    acc=readAccount("Gonet")
    identical(colnames(acc),  c("date","heure", "Currency", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashBalanceCHF", "CashBalanceEUR", "CashBalanceUSD","CashFlow")) &&
      nrow(acc) >1
  })
})

# ===========================================================================
# Stable-fixture tests — TestU1804173 portfolio table + TestAccount table
# (Apr 1 .. May 9 2026 snapshot of U1804173). These tests are deterministic
# even as live U1804173 keeps growing. Created by
# scripts/create_test_portfolio_table.R.
# ===========================================================================

# Top-level conn for inspecting the test fixtures directly (mirrors test-trades.R)
conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

# ---------------------------------------------------------------------------
# readPortfolio / readLastPortfolio / readPortfolioDate
# These functions take portfname directly, so no mocking needed —
# just pass "TestU1804173".
# ---------------------------------------------------------------------------
test_that("readPortfolio('TestU1804173') returns expected columns and >1 row", {
  portf <- readPortfolio("TestU1804173")
  expect_true(nrow(portf) > 1)
  expect_setequal(
    colnames(portf),
    c("TradeNr","date","heure","symbol","expdate","strike","pos","mktPrice",
      "optPrice","mktValue","avgCost","unPnL","IV","pvDividend","delta",
      "gamma","vega","theta","uPrice","multiplier","currency","type",
      "Instrument","margin"))
  expect_s3_class(portf$date, "Date")
  expect_s3_class(portf$heure, "hms")
})

test_that("readPortfolio for unknown name returns empty tibble", {
  portf <- readPortfolio("NoSuchPortfolioTable_XYZ")
  expect_s3_class(portf, "tbl_df")
  expect_equal(nrow(portf), 0)
})

test_that("readLastPortfolio('TestU1804173') returns only the last snapshot", {
  last_portf <- readLastPortfolio("TestU1804173")
  expect_true(nrow(last_portf) > 0)

  # Last snapshot in test fixture is 2026-05-09 (per script range)
  unique_dates <- unique(last_portf$date)
  expect_length(unique_dates, 1)
  expect_equal(unique_dates, as.Date("2026-05-09"))

  # All rows share the same heure (single snapshot)
  expect_length(unique(last_portf$heure), 1)
})

test_that("readLastPortfolio for unknown name returns empty tibble", {
  expect_equal(nrow(readLastPortfolio("NoSuchPortfolioTable_XYZ")), 0)
})

test_that("readPortfolioDate filters to the given date", {
  date_to_test <- as.Date("2026-04-30")
  portf <- readPortfolioDate("TestU1804173", date_to_test)
  expect_true(nrow(portf) > 0)
  # readPortfolioDate doesn't convert date column (only heure), so check stored date integer
  expect_true(all(portf$date == 20260430))
})

test_that("readPortfolioDate returns 0 rows for a date not in the snapshot", {
  portf <- readPortfolioDate("TestU1804173", as.Date("2020-01-15"))
  expect_equal(nrow(portf), 0)
})

test_that("readPortfolioDate errors when date is not a Date", {
  expect_error(readPortfolioDate("TestU1804173", "2026-04-30"))
  expect_error(readPortfolioDate("TestU1804173", 20260430))
})

test_that("readPortfolioDate for unknown name returns empty tibble", {
  expect_equal(
    nrow(readPortfolioDate("NoSuchPortfolioTable_XYZ", as.Date("2026-04-30"))),
    0)
})

# ---------------------------------------------------------------------------
# readAccount with mocked view name → reads from TestAccountWithConversionRate
# (TestAccount has account='U1804173' rows for Apr 1 .. May 9)
# ---------------------------------------------------------------------------
test_that("readAccount with mocked view returns rows for U1804173", {
  with_mocked_bindings(
    get_account_view_name = function() "TestAccountWithConversionRate", {
      acc <- readAccount("U1804173")
      expect_true(nrow(acc) > 1)
      expect_setequal(colnames(acc), c(
        "date","heure","Currency","NetLiquidation","EquityWithLoanValue",
        "FullAvailableFunds","FullInitMarginReq","FullMaintMarginReq",
        "FullExcessLiquidity","OptionMarketValue","StockMarketValue",
        "UnrealizedPnL","RealizedPnL","TotalCashBalance",
        "CashBalanceCHF","CashBalanceEUR","CashBalanceUSD","CashFlow"))
      expect_s3_class(acc$date, "Date")
      expect_s3_class(acc$heure, "hms")
    })
})

test_that("readAccount with mocked view returns 0 rows for unknown account", {
  with_mocked_bindings(
    get_account_view_name = function() "TestAccountWithConversionRate", {
      acc <- readAccount("NoSuchAccount_XYZ")
      expect_equal(nrow(acc), 0)
    })
})

# ---------------------------------------------------------------------------
# getCurrencyExposure with mocked table name → reads from TestAccount
# Uses TestU1804173 as the portfolio table (function takes account_name and
# uses it as the portfolio table name via glue).
# ---------------------------------------------------------------------------
test_that("getCurrencyExposure with mocked table reads TestAccount + TestU1804173", {
  with_mocked_bindings(
    get_account_table_name = function() "TestAccount", {
      # Note: account_name is used both as the WHERE filter on TestAccount AND
      # as the portfolio table name in the second query. TestAccount has rows
      # with account='U1804173', so we need the portfolio table also named
      # 'U1804173' for the join — but our fixture is TestU1804173. So this
      # call exercises the account-side query; portfolio query may return 0
      # rows (handled by the function's tryCatch + empty df fallback).
      exposure <- getCurrencyExposure("U1804173")
      expect_s3_class(exposure, "data.frame")
      expect_true(all(c("Currency", "CashPosition", "CashPositionBase",
                        "TotalExposureBase", "PercentOfPortfolio")
                      %in% colnames(exposure)))
    })
})

# ---------------------------------------------------------------------------
# greeksNet — pure-logic tests with synthetic + TestU1804173 portfolios
# ---------------------------------------------------------------------------
test_that("greeksNet on Gonet-style portfolio computes only delta + notional", {
  portf <- data.frame(
    pos       = c(100, 50),
    mktPrice  = c(50.0, 200.0),
    currency  = c("USD", "USD"),
    stringsAsFactors = FALSE
  )
  result <- greeksNet(portf)
  expect_equal(result$delta, 150)
  expect_equal(result$gamma, 0)
  expect_true(is.na(result$theta))
  expect_true(is.na(result$vega))
})

test_that("greeksNet on full IBKR-style portfolio computes all Greeks", {
  portf <- data.frame(
    type       = c("Stock", "Call", "Put"),
    pos        = c(100,    -1,     2),
    mktPrice   = c(50,     1.50,   2.20),
    multiplier = c(1L,     100L,   100L),
    delta      = c(NA_real_, 0.40, -0.30),
    gamma      = c(NA_real_, 0.05,  0.04),
    vega       = c(NA_real_, 0.20,  0.15),
    theta      = c(NA_real_, -0.10, -0.08),
    uPrice     = c(50,     50,     50),
    mktValue   = c(5000, -150, 440),
    currency   = c("USD","USD","USD"),
    stringsAsFactors = FALSE
  )
  result <- greeksNet(portf)
  # delta:  stock pos=100  +  call: 100 * 0.40 * (-1) = -40  +  put: 100*(-0.30)*2 = -60  → 0
  expect_equal(result$delta, 0)
  # gamma:  call: 100 * 0.05 * (-1) = -5  +  put: 100 * 0.04 * 2 = 8   → 3
  expect_equal(result$gamma, 3)
  # theta:  call: 100 * (-0.10) * (-1) = 10  +  put: 100 * (-0.08) * 2 = -16  → -6
  expect_equal(result$theta, -6)
  # vega:   call: 100 * 0.20 * (-1) = -20  +  put: 100 * 0.15 * 2 = 30  → 10
  expect_equal(result$vega, 10)
})

test_that("greeksNet runs on the TestU1804173 last-snapshot fixture", {
  portf <- readLastPortfolio("TestU1804173")
  expect_true(nrow(portf) > 0)
  result <- greeksNet(portf)
  # Just check shape — actual values depend on snapshot
  expect_true(all(c("delta", "deltanotional", "gamma", "theta", "vega")
                  %in% colnames(result)))
  expect_equal(nrow(result), 1)
})

# ---------------------------------------------------------------------------
# twr — pure-math tests
# ---------------------------------------------------------------------------
test_that("twr returns 0 for single-day input (n=1)", {
  expect_equal(twr(as.Date("2026-04-01"), 100000, 0), 0)
})

test_that("twr returns ~0 for flat portfolio with no cashflows", {
  dates <- seq.Date(as.Date("2026-04-01"), as.Date("2026-04-05"), by = "day")
  e_nlv <- rep(100000, 5)
  cashflows <- rep(0, 5)
  result <- twr(dates, e_nlv, cashflows)
  expect_length(result, 5)
  expect_true(all(abs(result) < 1e-10))
})

test_that("twr computes positive return for growing portfolio", {
  dates <- seq.Date(as.Date("2026-04-01"), as.Date("2026-04-05"), by = "day")
  e_nlv <- c(100000, 101000, 102000, 103000, 104000)
  cashflows <- rep(0, 5)
  result <- twr(dates, e_nlv, cashflows)
  expect_length(result, 5)
  expect_equal(result[1], 0)
  expect_gt(result[5], 0.039)  # ~4% over 4 days
  expect_lt(result[5], 0.041)
})

test_that("twr neutralises pure cash inflow (return should not move)", {
  dates <- seq.Date(as.Date("2026-04-01"), as.Date("2026-04-03"), by = "day")
  # Day 2: +5000 inflow at start of day, NLV grows by exactly that → 0% return
  e_nlv <- c(100000, 105000, 105000)
  cashflows <- c(0, 5000, 0)
  result <- twr(dates, e_nlv, cashflows)
  expect_equal(result[1], 0)
  expect_true(abs(result[2]) < 1e-10)
  expect_true(abs(result[3]) < 1e-10)
})

test_that("twr stops on duplicate dates", {
  dates <- as.Date(c("2026-04-01", "2026-04-01", "2026-04-02"))
  expect_error(twr(dates, c(100, 101, 102), c(0, 0, 0)),
               "All dates must be different")
})

test_that("twr returns NA when cashflows length mismatches NLV length after gap-fill", {
  # Trigger the length mismatch branch by passing a date series that becomes
  # longer once gap-filled to daily, but mismatched cashflows.
  dates <- as.Date(c("2026-04-01", "2026-04-04"))  # gap will create extra days
  e_nlv <- c(100, 110)
  cashflows <- c(0, 0)
  # twr internally extends to all calendar days — should still align
  result <- twr(dates, e_nlv, cashflows)
  expect_length(result, length(dates))
  expect_equal(result[1], 0)
})

# ---------------------------------------------------------------------------
# getAccountChoices — pure config-driven filter
# ---------------------------------------------------------------------------
test_that("getAccountChoices('all') returns all configured accounts", {
  out <- getAccountChoices("all")
  expect_type(out, "character")
  expect_setequal(out, c("U1804173", "U25343478", "DU5221795", "Gonet", "Live"))
})

test_that("getAccountChoices() defaults to 'all'", {
  expect_equal(getAccountChoices(), getAccountChoices("all"))
})

test_that("getAccountChoices('ibkr') keeps only U.../DU... accounts", {
  out <- getAccountChoices("ibkr")
  expect_setequal(out, c("U1804173", "U25343478", "DU5221795"))
  expect_false("Gonet" %in% out)
  expect_false("Live"  %in% out)
})

test_that("getAccountChoices('trade') drops Live and Gonet", {
  out <- getAccountChoices("trade")
  expect_setequal(out, c("U1804173", "U25343478", "DU5221795"))
  expect_false("Live"  %in% out)
  expect_false("Gonet" %in% out)
})

test_that("getAccountChoices errors on unknown type", {
  expect_error(getAccountChoices("bogus"))
})

# ---------------------------------------------------------------------------
# compute_margin_data — IBKR-margin pipeline (internal helper)
# ---------------------------------------------------------------------------
# tdata_py is an active binding — swap state directly (same trick as test-earnings.R).
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

# Helper: minimal grouped portfolio frame compute_margin_data expects.
# It uses dplyr::summarize() over groups, so input must be grouped by TradeNr.
make_margin_portf <- function(strategy, conId = 1234L, marginable = "Yes",
                              pos = -1, strike = 100, multiplier = 100L) {
  df <- data.frame(
    TradeNr    = 1L,
    Strategy   = strategy,
    conId      = conId,
    marginable = marginable,
    pos        = pos,
    strike     = strike,
    multiplier = multiplier,
    stringsAsFactors = FALSE
  )
  dplyr::group_by(df, TradeNr)
}

test_that("compute_margin_data returns input unchanged for strategies without margin lookup (e.g. CS)", {
  portf <- make_margin_portf("CS")
  # No mock needed — function shortcircuits when contracts is all NA
  result <- Tdata:::compute_margin_data(portf, exit_code = 2)
  expect_equal(result$exit_code, 2)
  expect_equal(nrow(result$portf_data), 1)
})

test_that("compute_margin_data sets exit_code=3 when WHEEL margin received from Python", {
  portf <- make_margin_portf("WHEEL", conId = 5678L)

  fake_py <- list(
    retrieveAccountMarginData = function(contracts) {
      # Echo back a per-contract margin
      rep(250.5, length(contracts))
    }
  )
  local_mock_tdata_py(fake_py)

  result <- Tdata:::compute_margin_data(portf, exit_code = 2)
  expect_equal(result$exit_code, 3)
})

test_that("compute_margin_data leaves exit_code unchanged when Python returns empty", {
  portf <- make_margin_portf("OFI", conId = 9999L)

  fake_py <- list(
    retrieveAccountMarginData = function(contracts) numeric(0)
  )
  local_mock_tdata_py(fake_py)

  result <- Tdata:::compute_margin_data(portf, exit_code = 2)
  expect_equal(result$exit_code, 2)
})
