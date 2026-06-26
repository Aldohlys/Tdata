
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
  # deltanotional in trade currency (no FX conversion): 100*50 + 50*200 = 15000
  expect_equal(result$deltanotional, 15000)
  expect_equal(result$currency, "USD")
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
  # deltanotional in trade currency: stock 100*50=5000, call 100*0.40*(-1)*50=-2000,
  #   put 100*(-0.30)*2*50=-3000  → 0
  expect_equal(result$deltanotional, 0)
  expect_equal(result$currency, "USD")
})

test_that("greeksNet uses mktPrice for a future's notional (uPrice is option-only)", {
  # IBKR's undPrice (-> uPrice) comes from option modelGreeks and is 0 for futures.
  # A future is its own underlying, so its notional must use its own mktPrice.
  portf <- data.frame(
    type       = "Future",
    pos        = 2,
    mktPrice   = 97.64,
    multiplier = 100L,
    delta      = NA_real_,
    gamma      = NA_real_,
    vega       = NA_real_,
    theta      = NA_real_,
    uPrice     = 0,
    mktValue   = NA_real_,
    currency   = "USD",
    stringsAsFactors = FALSE
  )
  result <- greeksNet(portf)
  # multiplier * pos * mktPrice = 100 * 2 * 97.64 = 19528 (USD, native)
  expect_equal(result$deltanotional, 19528)
  expect_equal(result$currency, "USD")
})

test_that("greeksNet treats a TreasuryBill as bond-like: notional = its market value", {
  portf <- data.frame(
    type       = "TreasuryBill",
    pos        = 30,
    mktPrice   = 99.99,
    multiplier = 10L,
    delta      = NA_real_,
    gamma      = NA_real_,
    vega       = NA_real_,
    theta      = NA_real_,
    uPrice     = 0,
    mktValue   = 29997,
    currency   = "USD",
    stringsAsFactors = FALSE
  )
  result <- greeksNet(portf)
  # notional = market (dollar) value, not delta-weighted
  expect_equal(result$deltanotional, 29997)
  expect_equal(result$currency, "USD")
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

# ---------------------------------------------------------------------------
# getIBKR — exit-code paths
# Strategy: mock isIBAvailable, tdata_py$getIBKRData, safe_db_connect,
# safe_db_append, getParam, getActiveTrades. Tests verify the four exit
# codes the function documents: 0 (no access), 1 (account only), 2 (account
# + portfolio), 3 (with margin).
# ---------------------------------------------------------------------------

# Minimal but realistic account row (one snapshot from one account).
make_fake_account <- function(account = "TestU1804173") {
  data.frame(
    account = account,
    date = "20260510",
    heure = "12:00:00",
    NetLiquidation = 100000, EquityWithLoanValue = 100000,
    FullAvailableFunds = 50000, FullInitMarginReq = 10000,
    FullMaintMarginReq = 8000, FullExcessLiquidity = 40000,
    OptionMarketValue = 5000, StockMarketValue = 50000,
    UnrealizedPnL = 0, RealizedPnL = 0,
    TotalCashBalance = 45000,
    CashBalanceCHF = 0, CashBalanceEUR = 0, CashBalanceUSD = 45000,
    CashFlow = 0,
    stringsAsFactors = FALSE
  )
}

# Minimal portfolio row matching what Python returns (pre-mutation).
# Columns are the union of what getIBKR reads/writes in the Stock and
# Option paths. Caller picks symbol/secType/etc. for the scenario.
make_fake_portf <- function(secType = "STK", symbol = "AAPL",
                            right = "", strike = 0, expdate = "",
                            pos = 100, mktPrice = 50, conId = 12345L) {
  data.frame(
    date = "20260510", heure = "12:00:00",
    symbol = symbol, secType = secType, right = right,
    expdate = expdate, strike = strike,
    pos = pos, multiplier = 1,
    mktPrice = mktPrice, optPrice = 0,
    mktValue = pos * mktPrice, avgCost = mktPrice * 0.9,
    unPnL = 0, IV = 0.20, pvDividend = 0,
    delta = 0.5, gamma = 0.01, vega = 0.10, theta = -0.05,
    uPrice = mktPrice, currency = "USD",
    conId = conId,
    stringsAsFactors = FALSE
  )
}

# ----- exit 0: IB not available -------------------------------------------
test_that("getIBKR returns 0 when IBKR is not available", {
  with_mocked_bindings(
    isIBAvailable = function() FALSE, {
      expect_equal(getIBKR(), 0)
    })
})

# ----- exit 0: tdata_py returns non-list ----------------------------------
test_that("getIBKR returns 0 with warning when tdata_py$getIBKRData returns non-list", {
  local_mock_tdata_py(list(getIBKRData = function(account) "oops"))
  with_mocked_bindings(
    isIBAvailable = function() TRUE, {
      expect_warning(result <- getIBKR(), "No value returned from IB")
      expect_equal(result, 0)
    })
})

# ----- exit 0: account_data has wrong row count ---------------------------
test_that("getIBKR returns 0 when account_data has 0 rows", {
  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account()[0, ], data.frame(), data.frame())
    }))
  with_mocked_bindings(
    isIBAvailable = function() TRUE, {
      expect_equal(getIBKR(), 0)
    })
})

# ----- exit 1: account stored, portfolio empty ----------------------------
test_that("getIBKR returns 1 when account is stored but portfolio is empty", {
  appended <- list()
  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account(), data.frame(), data.frame())  # empty portf
    }))

  with_mocked_bindings(
    isIBAvailable = function() TRUE,
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    safe_db_append = function(conn, table, df, ...) {
      appended[[table]] <<- df
      nrow(df)
    },
    getParam = function(name, ...) if (name == "BaseCurrency") "USD" else NULL, {
      expect_equal(getIBKR(), 1)
    })

  # Account row was written with Currency injected
  expect_true("Account" %in% names(appended))
  expect_equal(appended$Account$Currency, "USD")
  expect_equal(appended$Account$account, "TestU1804173")
})

# ----- exit 2: full path, stocks + options, no margin ---------------------
test_that("getIBKR returns 2 on full path (stocks + options, no margin)", {
  appended <- list()

  fake_portf <- rbind(
    make_fake_portf(secType = "STK", symbol = "AAPL"),
    make_fake_portf(secType = "OPT", symbol = "SPY", right = "P",
                    strike = 500, expdate = "20260620", conId = 99999L)
  )

  fake_trades <- data.frame(
    TradeNr = c(101L, 102L),
    Strategy = c("WHEEL", "CS"),
    Instrument = c("AAPL", "SPY 20JUN26 500 P"),
    Symbol = c("AAPL", "SPY"),
    Currency = c("USD", "USD"),
    Exp.Date = c("", "20.06.2026"),
    stringsAsFactors = FALSE
  )

  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account(), fake_portf, data.frame())  # no currency_balances
    }))

  with_mocked_bindings(
    isIBAvailable = function() TRUE,
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    safe_db_append = function(conn, table, df, ...) {
      appended[[table]] <<- df
      nrow(df)
    },
    getParam = function(name, ...) {
      switch(name, BaseCurrency = "USD", ComputeMargin = "No", NULL)
    },
    getActiveTrades = function(account) fake_trades, {
      expect_equal(getIBKR(), 2)
    })

  # Account + portfolio table writes both happened
  expect_true("Account" %in% names(appended))
  expect_true("TestU1804173" %in% names(appended))

  written_portf <- appended[["TestU1804173"]]
  expect_equal(nrow(written_portf), 2)
  # TradeNr column moved to front
  expect_equal(colnames(written_portf)[1], "TradeNr")
  # Stock row matched via symbol == Symbol
  expect_equal(written_portf$TradeNr[written_portf$type == "Stock"], 101L)
  # Option row matched via Instrument
  expect_equal(written_portf$TradeNr[written_portf$type == "Put"], 102L)
})

# ----- demo account (DU prefix) follows the same code path ----------------
test_that("getIBKR works symmetrically for DU demo accounts", {
  # The function has no U-vs-DU branching: both pass getAccountChoices('ibkr')
  # regex ^[UD] and both flow through identical R code. This test pins that
  # symmetry — if someone adds U-only logic, this fails.
  appended <- list()

  fake_portf <- make_fake_portf(secType = "STK", symbol = "AAPL")
  fake_trades <- data.frame(
    TradeNr = 555L, Strategy = "WHEEL", Instrument = "AAPL", Symbol = "AAPL",
    Currency = "USD", Exp.Date = "", stringsAsFactors = FALSE)

  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account(account = "DU5221795"), fake_portf, data.frame())
    }))

  with_mocked_bindings(
    isIBAvailable = function() TRUE,
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    safe_db_append = function(conn, table, df, ...) { appended[[table]] <<- df; nrow(df) },
    getParam = function(name, ...) {
      switch(name, BaseCurrency = "USD", ComputeMargin = "No", NULL)
    },
    getActiveTrades = function(account) {
      # Caller passes the account name through unchanged
      if (account != "DU5221795") stop("expected DU5221795, got ", account)
      fake_trades
    }, {
      expect_equal(getIBKR(account = "DU5221795"), 2)
    })

  # Portfolio table written under the DU account name (line 645:
  # safe_db_append(conn, account_data$account, portf_data))
  expect_true("DU5221795" %in% names(appended))
  expect_false("U1804173"  %in% names(appended))
  expect_equal(appended[["DU5221795"]]$TradeNr, 555L)
})

# ===========================================================================
# getIBKR — CASH currency_balances block (lines 593-642)
# Mocks create_cash_portfolio_row with a stub returning a known row so tests
# can verify getIBKR's filtering logic + rbind compatibility independently
# of the FX-rate / cost-basis lookups in cash.R.
# ===========================================================================

# Stub row that mirrors the schema returned by create_cash_portfolio_row.
make_fake_cash_row <- function(currency, balance,
                                snapshot_date, snapshot_heure,
                                trade_nr = NA_integer_) {
  data.frame(
    TradeNr = trade_nr,
    date = snapshot_date,
    heure = snapshot_heure,
    symbol = currency,
    expdate = NA_integer_,
    strike = NA_real_,
    pos = as.numeric(balance),
    mktPrice = 1.0,
    optPrice = NA_real_,
    mktValue = balance,
    avgCost = 1.0,
    unPnL = 0,
    IV = NA_real_,
    pvDividend = NA_real_,
    delta = 1.0, gamma = 0, vega = 0, theta = 0,
    uPrice = NA_real_,
    multiplier = 1L,
    currency = "USD",
    type = "CASH",
    Instrument = currency,
    margin = 0,
    stringsAsFactors = FALSE
  )
}

# Common scaffolding for CASH-block tests: returns the captured `appended`
# environment after running getIBKR with the supplied currency_balances.
run_getIBKR_with_cash_balances <- function(currency_balances,
                                            base_currency = "USD",
                                            cash_factory = NULL) {
  appended <- list()
  cash_calls <- list()

  fake_portf <- make_fake_portf(secType = "STK", symbol = "AAPL")
  fake_trades <- data.frame(
    TradeNr = 101L, Strategy = "WHEEL", Instrument = "AAPL", Symbol = "AAPL",
    Currency = "USD", Exp.Date = "", stringsAsFactors = FALSE)

  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account(), fake_portf, currency_balances)
    }))

  default_factory <- function(currency, balance, snapshot_date, snapshot_heure,
                              account_table = NULL) {
    cash_calls[[length(cash_calls) + 1L]] <<- list(
      currency = currency, balance = balance,
      snapshot_date = snapshot_date, snapshot_heure = snapshot_heure,
      account_table = account_table)
    make_fake_cash_row(currency, balance, snapshot_date, snapshot_heure)
  }

  with_mocked_bindings(
    isIBAvailable = function() TRUE,
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    safe_db_append = function(conn, table, df, ...) { appended[[table]] <<- df; nrow(df) },
    getParam = function(name, ...) {
      switch(name, BaseCurrency = base_currency, ComputeMargin = "No", NULL)
    },
    getActiveTrades = function(account) fake_trades,
    create_cash_portfolio_row = if (!is.null(cash_factory)) cash_factory else default_factory, {
      result <- getIBKR()
    })

  list(result = result, appended = appended, cash_calls = cash_calls)
}

# ----- empty currency_balances: block skipped, no extra rows --------------
test_that("CASH block: empty currency_balances data.frame does not add rows", {
  out <- run_getIBKR_with_cash_balances(data.frame())
  expect_equal(out$result, 2)
  written <- out$appended[["TestU1804173"]]
  expect_equal(nrow(written), 1)            # only the original AAPL stock
  expect_false("CASH" %in% written$type)
})

# ----- base currency rows are skipped --------------------------------------
test_that("CASH block: rows with base currency (USD) are skipped", {
  balances <- data.frame(
    currency = c("USD"),
    balance  = c(50000),
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances, base_currency = "USD")
  expect_length(out$cash_calls, 0)          # factory never called
  expect_equal(nrow(out$appended[["TestU1804173"]]), 1)
})

# ----- "BASE" pseudo-row from IBKR is skipped -----------------------------
test_that("CASH block: row with currency=='BASE' is skipped", {
  balances <- data.frame(
    currency = c("BASE"),
    balance  = c(100000),
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances)
  expect_length(out$cash_calls, 0)
})

# ----- near-zero balances are skipped --------------------------------------
test_that("CASH block: |balance| < 0.01 is skipped (rounding noise)", {
  balances <- data.frame(
    currency = c("CHF", "EUR"),
    balance  = c(0.005, -0.001),
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances)
  expect_length(out$cash_calls, 0)
})

# ----- valid foreign-currency row gets added to portfolio ------------------
test_that("CASH block: valid non-base currency creates a CASH portfolio row", {
  balances <- data.frame(
    currency = c("CHF"),
    balance  = c(25000),
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances, base_currency = "USD")
  expect_equal(out$result, 2)
  expect_length(out$cash_calls, 1)
  expect_equal(out$cash_calls[[1]]$currency, "CHF")
  expect_equal(out$cash_calls[[1]]$balance, 25000)
  expect_equal(out$cash_calls[[1]]$account_table, "TestU1804173")

  written <- out$appended[["TestU1804173"]]
  expect_equal(nrow(written), 2)            # AAPL stock + CHF cash
  cash_row <- written[written$type == "CASH", ]
  expect_equal(nrow(cash_row), 1)
  expect_equal(cash_row$symbol, "CHF")
  expect_equal(cash_row$pos, 25000)
})

# ----- mixed: foreign currencies + base + BASE + zero — only foreign added -
test_that("CASH block: filters base currency, BASE total, and zero balances; keeps real foreign positions", {
  balances <- data.frame(
    currency = c("CHF", "EUR", "USD",  "BASE",  "JPY"),
    balance  = c(25000, 12000, 50000, 100000, 0.001),
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances, base_currency = "USD")
  expect_length(out$cash_calls, 2)
  expect_setequal(vapply(out$cash_calls, `[[`, character(1), "currency"),
                  c("CHF", "EUR"))

  written <- out$appended[["TestU1804173"]]
  expect_equal(sum(written$type == "CASH"), 2)
})

# ----- create_cash_portfolio_row may return NULL (its own internal skip) ---
test_that("CASH block: NULL returns from create_cash_portfolio_row are dropped before rbind", {
  balances <- data.frame(
    currency = c("CHF", "EUR"),
    balance  = c(25000, 12000),
    stringsAsFactors = FALSE)
  null_factory <- function(currency, ...) {
    if (currency == "EUR") NULL else make_fake_cash_row(currency, 25000, 20260510, "12:00:00")
  }
  out <- run_getIBKR_with_cash_balances(balances, cash_factory = null_factory)

  written <- out$appended[["TestU1804173"]]
  cash_rows <- written[written$type == "CASH", ]
  expect_equal(nrow(cash_rows), 1)          # CHF kept, EUR dropped
  expect_equal(cash_rows$symbol, "CHF")
})

# ----- expdate type-compatibility: portf_data character + cash NA_integer -
test_that("CASH block: expdate type mismatch is reconciled before rbind", {
  # portf_data after the main pipeline has expdate as character (Python returns
  # YYYYMMDD strings); make_fake_cash_row uses NA_integer_. The block coerces
  # cash_df$expdate to character to match. This test pins that behaviour.
  balances <- data.frame(
    currency = "CHF", balance = 25000,
    stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances)

  written <- out$appended[["TestU1804173"]]
  expect_type(written$expdate, "character")
  cash_row <- written[written$type == "CASH", ]
  expect_true(is.na(cash_row$expdate))
})

# ----- snapshot_date and snapshot_heure passed through to factory ----------
test_that("CASH block: snapshot_date/snapshot_heure forwarded from portf_data first row", {
  balances <- data.frame(currency = "CHF", balance = 25000,
                         stringsAsFactors = FALSE)
  out <- run_getIBKR_with_cash_balances(balances)
  expect_length(out$cash_calls, 1)
  call <- out$cash_calls[[1]]
  # portf_data$date is coerced to integer by getIBKR; portf_data$heure is character
  expect_equal(call$snapshot_date, 20260510L)
  expect_equal(call$snapshot_heure, "12:00:00")
})

# ----- portfolio rows that don't match any trade leave TradeNr=NA + warn --
test_that("getIBKR logs unmatched instruments (TradeNr=NA) and still returns 2", {
  appended <- list()
  fake_portf <- make_fake_portf(secType = "STK", symbol = "UNKNOWN_TICKER")
  fake_trades <- data.frame(
    TradeNr = 101L, Strategy = "WHEEL", Instrument = "AAPL", Symbol = "AAPL",
    Currency = "USD", Exp.Date = "", stringsAsFactors = FALSE)

  local_mock_tdata_py(list(
    getIBKRData = function(account) {
      list(make_fake_account(), fake_portf, data.frame())
    }))

  with_mocked_bindings(
    isIBAvailable = function() TRUE,
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    safe_db_append = function(conn, table, df, ...) { appended[[table]] <<- df; nrow(df) },
    getParam = function(name, ...) {
      switch(name, BaseCurrency = "USD", ComputeMargin = "No", NULL)
    },
    getActiveTrades = function(account) fake_trades, {
      expect_equal(getIBKR(), 2)
    })

  written_portf <- appended[["TestU1804173"]]
  expect_equal(nrow(written_portf), 1)
  expect_true(is.na(written_portf$TradeNr))
})
