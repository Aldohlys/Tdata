# Tests for open-order retrieval and open-trade order coverage.
#
# buildTradeOrderCoverage is pure, so it is driven with fixtures rather than the
# live Trades table - trades change over time and would break these tests.

CURRENCIES <- c("CHF", "EUR", "USD", "CAD", "JPY", "GBP")

### One event row of the Trades table, with only the fields coverage reads.
trade_row <- function(TradeNr, Symbol, Instrument, Pos, Account = "U1804173",
                      Strategy = "BOT", Exp.Date = NA_character_, Currency = "USD") {
  data.frame(TradeNr = TradeNr, Account = Account, Strategy = Strategy,
             Symbol = Symbol, Instrument = Instrument, Pos = Pos,
             Exp.Date = Exp.Date, Currency = Currency,
             stringsAsFactors = FALSE, check.names = FALSE)
}

### One row of getOpenOrders() output.
order_row <- function(symbol, instrument, account = "U1804173", secType = "OPT",
                      action = "SELL", legAction = NA_character_, quantity = 1,
                      orderType = "LMT", price = 2.5, ocaGroup = NA_character_,
                      permId = 1, currency = "USD", status = "Submitted") {
  data.frame(account = account, permId = permId, symbol = symbol,
             instrument = instrument, secType = secType, action = action,
             legAction = legAction, quantity = quantity, orderType = orderType,
             price = price, tif = "GTC", status = status, ocaGroup = ocaGroup,
             currency = currency, stringsAsFactors = FALSE)
}

no_orders <- function() order_row("X", "X")[0, ]


test_that("an open trade with a matching order is reported as covered", {
  trades <- trade_row(741, "T", "T 11SEP26 24.5 C", 1, Exp.Date = "11.09.2026")
  orders <- order_row("T", "T 11SEP26 24.5 C")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(nrow(cv), 1)
    expect_equal(cv$Flag, "")
    expect_equal(cv$TradeNr, 741)
    expect_equal(cv$Match, "leg")
    expect_equal(cv$Contract, "T 11SEP26 24.5 C")
    expect_equal(cv$Price, 2.5)
  })
})


test_that("an open trade with no order is kept and flagged NO ORDER", {
  trades <- rbind(
    trade_row(741, "T", "T 11SEP26 24.5 C", 1),
    trade_row(511, "URA", "URA 15NOV26 28 P", -5))
  orders <- order_row("T", "T 11SEP26 24.5 C")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(nrow(cv), 2)
    naked <- cv[cv$Flag == "NO ORDER", ]
    expect_equal(naked$TradeNr, 511)
    expect_equal(naked$Symbol, "URA")
    expect_true(is.na(naked$Contract))
    ### Uncovered trades sort first - that is what the table exists to show
    expect_equal(cv$Flag[1], "NO ORDER")
  })
})


test_that("a cash/FX trade is flagged CASH, not NO ORDER", {
  trades <- trade_row(687, "JPY", "CHF", -1300, Currency = "CHF")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, no_orders())

    expect_equal(cv$Flag, "CASH")
  })
})


test_that("an order matching no open trade is flagged UNMATCHED", {
  trades <- trade_row(741, "T", "T 11SEP26 24.5 C", 1)
  orders <- order_row("TSLA", "TSLA 19JUN26 300 P", permId = 9)

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    loose <- cv[cv$Flag == "UNMATCHED", ]
    expect_equal(nrow(loose), 1)
    expect_true(is.na(loose$TradeNr))
    expect_equal(loose$Symbol, "TSLA")
    ### The trade is still reported, uncovered
    expect_equal(cv$Flag[cv$TradeNr %in% 741 & !is.na(cv$TradeNr)], "NO ORDER")
  })
})


test_that("an order matching two trades on the same root is flagged AMBIGUOUS", {
  trades <- rbind(
    trade_row(741, "SLB", "SLB 18SEP26 56 C", -1),
    trade_row(745, "SLB", "SLB 18SEP26 56 C", 2))
  orders <- order_row("SLB", "SLB 18SEP26 56 C")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(nrow(cv), 2)
    expect_true(all(cv$Flag == "AMBIGUOUS"))
    expect_setequal(cv$TradeNr, c(741, 745))
  })
})


test_that("an order on a strike the trade does not hold matches only at root level", {
  trades <- trade_row(742, "KO", "KO 18SEP26 90 C", 1)
  orders <- order_row("KO", "KO 18SEP26 95 C")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(cv$Match, "root")
  })
})


test_that("a stock order matches at leg level despite the company-name Instrument", {
  ### Trades.Instrument holds the IBKR company name for stocks, which no order
  ### field reproduces - Symbol is the only usable key there.
  trades <- trade_row(697, "CA", "CARREFOUR SA", 200, Currency = "EUR")
  orders <- order_row("CA", "CA", secType = "STK", quantity = 200,
                      orderType = "STP", price = 11.82, currency = "EUR")

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(cv$Flag, "")
    expect_equal(cv$Match, "leg")
  })
})


test_that("legs netting to zero are treated as closed and do not ask for cover", {
  trades <- rbind(
    trade_row(706, "ESTX50", "ESTX50 18SEP26 5200 P", 1),
    trade_row(706, "ESTX50", "ESTX50 18SEP26 5200 P", -1),
    trade_row(706, "ESTX50", "ESTX50 18DEC26 4600 P", -1))

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, no_orders())

    expect_equal(nrow(cv), 1)
    expect_equal(cv$Legs, 1)
    ### Single remaining leg, so the net position is meaningful again
    expect_equal(cv$Pos, -1)
  })
})


test_that("a multi-leg trade reports its leg count and no misleading net position", {
  trades <- rbind(
    trade_row(746, "BRK B", "BRK B 02OCT26 520 C", 2),
    trade_row(746, "BRK B", "BRK B 02OCT26 525 C", -2))

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, no_orders())

    expect_equal(nrow(cv), 1)
    expect_equal(cv$Legs, 2)
    ### Summing a vertical's legs would read as a flat, harmless zero
    expect_true(is.na(cv$Pos))
  })
})


test_that("a combo leg reports its own effective side, not the combo's", {
  trades <- rbind(
    trade_row(746, "BRK B", "BRK B 02OCT26 520 C", 2),
    trade_row(746, "BRK B", "BRK B 02OCT26 525 C", -2))
  orders <- rbind(
    order_row("BRK B", "BRK B 02OCT26 520 C", action = "SELL", legAction = "SELL"),
    order_row("BRK B", "BRK B 02OCT26 525 C", action = "SELL", legAction = "BUY"))

  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(trades, orders)

    expect_equal(nrow(cv), 2)
    expect_equal(cv$Action[cv$Contract == "BRK B 02OCT26 520 C"], "SELL")
    expect_equal(cv$Action[cv$Contract == "BRK B 02OCT26 525 C"], "BUY")
    expect_true(all(cv$Match == "leg"))
  })
})


test_that("no trades and no orders yields an empty coverage table", {
  with_mocked_bindings(getActiveCurrencies = function(...) CURRENCIES, {
    cv <- buildTradeOrderCoverage(data.frame(), no_orders())
    expect_equal(nrow(cv), 0)
  })
})


test_that("getOpenOrders reports TWS unreachable rather than an absence of orders", {
  with_mocked_bindings(getOpenOrderQuery = function(account) NULL, {
    orders <- getOpenOrders("U1804173")

    expect_equal(nrow(orders), 0)
    expect_false(attr(orders, "tws_available"))
  })
})


test_that("getOpenOrders distinguishes a reachable TWS with no resting orders", {
  empty <- emptyOpenOrders()[, setdiff(names(emptyOpenOrders()),
                                       c("expiryDate", "price", "instrument"))]

  with_mocked_bindings(getOpenOrderQuery = function(account) empty, {
    orders <- getOpenOrders("U1804173")

    expect_equal(nrow(orders), 0)
    expect_true(attr(orders, "tws_available"))
  })
})


test_that("getOpenOrders derives the operative price and the IBKR contract name", {
  raw <- data.frame(
    account = rep("U1804173", 4), permId = 1:4, orderId = 0, parentId = 0,
    ocaGroup = NA_character_, isCombo = FALSE, legCount = 1,
    legIndex = NaN, legRatio = NaN, legAction = NA_character_,
    secType = c("OPT", "STK", "STK", "STK"),
    symbol = c("BRK B", "CA", "CA", "CA"),
    localSymbol = c("BRKB  261002C00520000", "CA", "CA", "CA"),
    tradingClass = NA_character_,
    expiry = c("20261002", NA, NA, NA),
    strike = c(520, NaN, NaN, NaN),
    right = c("C", NA, NA, NA),
    multiplier = c(100, NaN, NaN, NaN),
    currency = "USD", exchange = "SMART",
    action = "SELL", quantity = 2,
    orderType = c("LMT", "STP", "TRAIL", "MKT"),
    lmtPrice = c(4, 0, 0, 0),
    auxPrice = c(0, 11.8, 12, 0),
    trailStopPrice = c(NaN, NaN, 12.5, NaN),
    tif = "GTC", status = "Submitted", filled = 0, remaining = 2,
    stringsAsFactors = FALSE)

  with_mocked_bindings(getOpenOrderQuery = function(account) raw, {
    orders <- getOpenOrders("U1804173")

    ### Option contracts rebuild the Trades.Instrument string
    expect_equal(orders$instrument[1], "BRK B 02OCT26 520 C")
    expect_equal(orders$instrument[2], "CA")
    expect_equal(orders$price[1], 4)      # LMT  -> limit
    expect_equal(orders$price[2], 11.8)   # STP  -> stop trigger
    expect_equal(orders$price[3], 12.5)   # TRAIL-> trailing stop, not auxPrice
    expect_true(is.na(orders$price[4]))   # MKT  -> no price
    expect_true(is.na(orders$strike[2]))  # NaN normalised to NA
    expect_true(attr(orders, "tws_available"))
  })
})


test_that("sortCoverage re-groups merged accounts so uncovered trades stay on top", {
  ### Two accounts' coverage concatenated: without a re-sort the second block's
  ### uncovered trade sits below the first block's covered rows.
  merged <- data.frame(
    Flag = c("NO ORDER", "", "NO ORDER", "CASH"),
    TradeNr = c(1, 2, 3, 4),
    Symbol = c("AAA", "BBB", "CCC", "DDD"),
    Contract = c(NA, "BBB", NA, NA),
    stringsAsFactors = FALSE)

  sorted <- sortCoverage(merged)

  expect_equal(sorted$Flag, c("NO ORDER", "NO ORDER", "", "CASH"))
  expect_equal(sorted$TradeNr, c(1, 3, 2, 4))
})


test_that("sortCoverage leaves an empty table alone", {
  expect_equal(nrow(sortCoverage(data.frame())), 0)
  expect_null(sortCoverage(NULL))
})
