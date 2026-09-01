# Tests for open-order retrieval and held-position order coverage.
#
# buildTradeOrderCoverage is pure, so it is driven with fixtures rather than the
# live Trades/portfolio tables - both change over time and would break these tests.

### One row of a portfolio snapshot, with only the fields coverage reads.
position_row <- function(TradeNr, symbol, Instrument, pos, expdate = NA_integer_,
                         type = "Call", currency = "USD") {
  data.frame(TradeNr = TradeNr, symbol = symbol, Instrument = Instrument,
             pos = pos, expdate = expdate, type = type, currency = currency,
             stringsAsFactors = FALSE)
}

### One event row of the Trades table (only Strategy and the TradeNr link matter).
trade_row <- function(TradeNr, Symbol, Instrument, Strategy = "BOT") {
  data.frame(TradeNr = TradeNr, Symbol = Symbol, Instrument = Instrument,
             Strategy = Strategy, stringsAsFactors = FALSE)
}

### One row of getOpenOrders() output.
order_row <- function(symbol, instrument, account = "U1804173", secType = "OPT",
                      action = "SELL", legAction = NA_character_, quantity = 1,
                      orderType = "LMT", algoStrategy = NA_character_, price = 2.5,
                      ocaGroup = NA_character_, permId = 1, currency = "USD",
                      status = "Submitted") {
  data.frame(account = account, permId = permId, symbol = symbol,
             instrument = instrument, secType = secType, action = action,
             legAction = legAction, quantity = quantity, orderType = orderType,
             algoStrategy = algoStrategy, price = price, tif = "GTC",
             status = status, ocaGroup = ocaGroup, currency = currency,
             stringsAsFactors = FALSE)
}

no_orders <- function() order_row("X", "X")[0, ]


test_that("a held position with a matching order is reported as covered", {
  positions <- position_row(742, "KO", "KO 18SEP26 90 C", 1, expdate = 20260918L)
  orders <- order_row("KO", "KO 18SEP26 90 C")

  cv <- buildTradeOrderCoverage(positions, orders, trade_row(742, "KO", "KO 18SEP26 90 C"),
                                "U1804173")

  expect_equal(nrow(cv), 1)
  expect_equal(cv$Flag, "")
  expect_equal(cv$TradeNr, 742)
  expect_equal(cv$Match, "leg")
  expect_equal(cv$Strategy, "BOT")
  expect_equal(cv$ExpDate, as.Date("2026-09-18"))
})


test_that("a held position with no order is kept and flagged NO ORDER", {
  positions <- rbind(
    position_row(742, "KO", "KO 18SEP26 90 C", 1),
    position_row(747, "GLD", "GLD 30SEP26 423 C", 2))
  orders <- order_row("KO", "KO 18SEP26 90 C")

  cv <- buildTradeOrderCoverage(positions, orders)

  expect_equal(nrow(cv), 2)
  naked <- cv[cv$Flag == "NO ORDER", ]
  expect_equal(naked$TradeNr, 747)
  expect_true(is.na(naked$Contract))
  ### Uncovered positions sort first - that is what the table exists to show
  expect_equal(cv$Flag[1], "NO ORDER")
})


test_that("a trade left open in the DB but no longer held is not reported at all", {
  ### Trade 511's URA legs expired in 2024; Status is still 'Ouvert' but nothing
  ### is held, so there is no position for an order to protect.
  stale <- rbind(
    trade_row(511, "URA", "URA 18OCT24 27 P"),
    trade_row(511, "URA", "URA 15NOV24 28 P"))
  positions <- position_row(742, "KO", "KO 18SEP26 90 C", 1)

  cv <- buildTradeOrderCoverage(positions, no_orders(), stale, "U1804173")

  expect_equal(nrow(cv), 1)
  expect_equal(cv$Symbol, "KO")
  expect_false("URA" %in% cv$Symbol)
})


test_that("cash/FX balances are excluded - an order cannot act on them", {
  positions <- rbind(
    position_row(742, "KO", "KO 18SEP26 90 C", 1),
    position_row(687, "JPY", "JPY", -1163, type = "CASH", currency = "JPY"),
    position_row(704, "USD", "USD", 152, type = "CASH"))

  cv <- buildTradeOrderCoverage(positions, no_orders())

  expect_equal(nrow(cv), 1)
  expect_equal(cv$Symbol, "KO")
})


test_that("the portfolio's TradeNr settles an attribution the Trades table muddles", {
  ### Trades carries an SLB leg mis-recorded under 741 (a T trade); the portfolio
  ### assigns the held SLB contract to 745, and that is authoritative.
  positions <- position_row(745, "SLB", "SLB 18SEP26 56 C", 1)
  trades <- rbind(
    trade_row(741, "SLB", "SLB 18SEP26 56 C"),
    trade_row(745, "SLB", "SLB 18SEP26 56 C"))
  orders <- order_row("SLB", "SLB 18SEP26 56 C")

  cv <- buildTradeOrderCoverage(positions, orders, trades, "U1804173")

  expect_equal(nrow(cv), 1)
  expect_equal(cv$TradeNr, 745)
  expect_equal(cv$Flag, "")
})


test_that("an exact contract match beats a root match instead of reading as ambiguous", {
  ### Two trades on the same underlying, but the order names a leg only one holds.
  positions <- rbind(
    position_row(741, "SLB", "SLB 11SEP26 50 C", 1),
    position_row(745, "SLB", "SLB 18SEP26 56 C", 2))
  orders <- order_row("SLB", "SLB 18SEP26 56 C")

  cv <- buildTradeOrderCoverage(positions, orders)

  covered <- cv[cv$Flag == "", ]
  expect_equal(nrow(covered), 1)
  expect_equal(covered$TradeNr, 745)
  expect_equal(covered$Match, "leg")
})


test_that("a root-only match against two trades is still flagged AMBIGUOUS", {
  positions <- rbind(
    position_row(741, "SLB", "SLB 11SEP26 50 C", 1),
    position_row(745, "SLB", "SLB 18SEP26 56 C", 2))
  ### An order on a third strike neither trade holds
  orders <- order_row("SLB", "SLB 18SEP26 60 C")

  cv <- buildTradeOrderCoverage(positions, orders)

  ambiguous <- cv[cv$Flag == "AMBIGUOUS", ]
  expect_equal(nrow(ambiguous), 2)
  expect_true(all(ambiguous$Match == "root"))
})


test_that("an order matching nothing held is flagged UNMATCHED", {
  positions <- position_row(742, "KO", "KO 18SEP26 90 C", 1)
  orders <- order_row("TSLA", "TSLA 19JUN26 300 P", permId = 9)

  cv <- buildTradeOrderCoverage(positions, orders)

  loose <- cv[cv$Flag == "UNMATCHED", ]
  expect_equal(nrow(loose), 1)
  expect_true(is.na(loose$TradeNr))
  expect_equal(loose$Symbol, "TSLA")
})


test_that("a stock order matches at leg level despite the company-name Instrument", {
  ### Trades.Instrument holds the IBKR company name for stocks, which no order
  ### field reproduces - the root is the only usable key there.
  positions <- position_row(697, "CA", "CA", 200, type = "Stock", currency = "EUR")
  orders <- order_row("CA", "CA", secType = "STK", quantity = 200,
                      orderType = "STP", price = 11.82, currency = "EUR")

  cv <- buildTradeOrderCoverage(positions, orders)

  expect_equal(cv$Flag, "")
  expect_equal(cv$Match, "leg")
})


test_that("a multi-leg trade reports its leg count and no misleading net position", {
  positions <- rbind(
    position_row(746, "BRK B", "BRK B 02OCT26 520 C", 2),
    position_row(746, "BRK B", "BRK B 02OCT26 525 C", -2))

  cv <- buildTradeOrderCoverage(positions, no_orders())

  expect_equal(nrow(cv), 1)
  expect_equal(cv$Legs, 2)
  ### Summing a vertical's legs would read as a flat, harmless zero
  expect_true(is.na(cv$Pos))
})


test_that("a combo leg reports its own effective side, not the combo's", {
  positions <- rbind(
    position_row(746, "BRK B", "BRK B 02OCT26 520 C", 2),
    position_row(746, "BRK B", "BRK B 02OCT26 525 C", -2))
  orders <- rbind(
    order_row("BRK B", "BRK B 02OCT26 520 C", action = "SELL", legAction = "SELL"),
    order_row("BRK B", "BRK B 02OCT26 525 C", action = "SELL", legAction = "BUY"))

  cv <- buildTradeOrderCoverage(positions, orders)

  expect_equal(nrow(cv), 2)
  expect_equal(cv$Action[cv$Contract == "BRK B 02OCT26 520 C"], "SELL")
  expect_equal(cv$Action[cv$Contract == "BRK B 02OCT26 525 C"], "BUY")
})


test_that("an algo order reports the algo TWS shows, not the bare order type", {
  ### TWS labels these "Adaptive LMT (IBKR)" while orderType stays "LMT"
  positions <- position_row(742, "KO", "KO 18SEP26 90 C", 1)
  orders <- order_row("KO", "KO 18SEP26 90 C", algoStrategy = "Adaptive")

  cv <- buildTradeOrderCoverage(positions, orders)

  expect_equal(cv$OrderType, "LMT (Adaptive)")
})


test_that("a position whose TradeNr the snapshot missed is recovered from Trades", {
  positions <- position_row(NA, "KO", "KO 18SEP26 90 C", 1)
  trades <- trade_row(742, "KO", "KO 18SEP26 90 C", Strategy = "WHEEL")

  cv <- buildTradeOrderCoverage(positions, no_orders(), trades)

  expect_equal(cv$TradeNr, 742)
  expect_equal(cv$Strategy, "WHEEL")
})


test_that("an unrecoverable TradeNr stays NA rather than being guessed", {
  positions <- position_row(NA, "KO", "KO 18SEP26 90 C", 1)
  ### Two open trades hold the same contract - nothing to pick between them
  trades <- rbind(
    trade_row(742, "KO", "KO 18SEP26 90 C"),
    trade_row(743, "KO", "KO 18SEP26 90 C"))

  cv <- buildTradeOrderCoverage(positions, no_orders(), trades)

  expect_equal(nrow(cv), 1)
  expect_true(is.na(cv$TradeNr))
})


test_that("no positions and no orders yields an empty coverage table", {
  expect_equal(nrow(buildTradeOrderCoverage(NULL, no_orders())), 0)
  expect_equal(nrow(buildTradeOrderCoverage(data.frame(), no_orders())), 0)
})


test_that("getLatestPositions returns only the newest snapshot of the newest day", {
  snapshots <- data.frame(
    date = as.Date(c("2026-08-30", "2026-08-31", "2026-08-31")),
    heure = c(3L, 1L, 2L),
    TradeNr = c(1, 2, 3), symbol = "KO", Instrument = "KO", pos = 1,
    expdate = NA_integer_, type = "Call", currency = "USD",
    stringsAsFactors = FALSE)

  with_mocked_bindings(getPositionsQuery = function(account) snapshots, {
    latest <- getLatestPositions("U1804173")

    expect_equal(nrow(latest), 1)
    expect_equal(latest$TradeNr, 3)
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
    algoStrategy = NA_character_,
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


test_that("sortCoverage re-groups merged accounts so uncovered rows stay on top", {
  ### Two accounts' coverage concatenated: without a re-sort the second block's
  ### uncovered position sits below the first block's covered rows.
  merged <- data.frame(
    Flag = c("NO ORDER", "", "NO ORDER", "UNMATCHED"),
    TradeNr = c(1, 2, 3, 4),
    Symbol = c("AAA", "BBB", "CCC", "DDD"),
    Contract = c(NA, "BBB", NA, "DDD"),
    stringsAsFactors = FALSE)

  sorted <- sortCoverage(merged)

  expect_equal(sorted$Flag, c("NO ORDER", "NO ORDER", "", "UNMATCHED"))
  expect_equal(sorted$TradeNr, c(1, 3, 2, 4))
})


test_that("sortCoverage leaves an empty table alone", {
  expect_equal(nrow(sortCoverage(data.frame())), 0)
  expect_null(sortCoverage(NULL))
})
