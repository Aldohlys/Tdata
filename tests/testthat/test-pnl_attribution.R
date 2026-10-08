## attributePnL: inline snapshots and fills, no DB.

snap <- function(date, heure, Instrument, pos, mktPrice, uPrice = 100, IV = 0.30,
                 delta = 0.5, gamma = 0.02, vega = 0.10, theta = -0.02,
                 type = "Call", strike = 100, expdate = 20261217,
                 symbol = "XYZ", multiplier = 100) {
  data.frame(date = as.integer(date), heure = heure, symbol = symbol, type = type,
             Instrument = Instrument, strike = strike, expdate = as.integer(expdate),
             pos = pos, uPrice = uPrice, mktPrice = mktPrice, multiplier = multiplier,
             currency = "USD", IV = IV, delta = delta, gamma = gamma, vega = vega,
             theta = theta, stringsAsFactors = FALSE)
}

fill <- function(TradeDate, time, Instrument, Pos, Price, Commission = 1,
                 multiplier = 100, UnderlyingPrice = NA_real_, Symbol = "XYZ",
                 Strike = 100, Right = "C", EventType = "Open") {
  data.frame(TradeDate = as.integer(TradeDate),
             DateTime = paste(format(as.Date(as.character(TradeDate), "%Y%m%d")), time),
             TimeZoneSource = "America/New_York", Instrument = Instrument, Symbol = Symbol,
             Pos = Pos, Price = Price, Commission = Commission,
             Total = -Pos * Price * multiplier - Commission,
             Strike = Strike, Right = Right, EventType = EventType,
             UnderlyingPrice = UnderlyingPrice, stringsAsFactors = FALSE)
}

C100 <- "XYZ 17DEC26 100 C"
ALL <- as.Date("2099-01-01")

test_that("stock: every move is delta, residual is zero, total reconciles", {
  s <- rbind(snap(20260105, "22:00:00", "XYZ", 10, 100, type = "Stock", multiplier = 1),
             snap(20260106, "22:00:00", "XYZ", 10, 103, type = "Stock", multiplier = 1),
             snap(20260107, "22:00:00", "XYZ", 10, 101, type = "Stock", multiplier = 1))
  f <- fill(20260105, "15:00:00", "XYZ", 10, 99.5, multiplier = 1, Strike = NA, Right = "")
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(nrow(r), 3)
  expect_equal(r$delta, c(0, 30, -20))
  expect_equal(r$residual, c(0, 0, 0))
  expect_equal(r$execution[1], 10 * 100 - 995 - 1)      # fill vs first mark
  expect_equal(sum(r$actual), 10 * 101 + f$Total)        # open trade: value + cash
  expect_true(all(c(r$gamma, r$vega, r$theta) == 0))
})

test_that("option: Greek terms from the start-of-interval Greeks", {
  s <- rbind(snap(20260105, "22:00:00", C100, 2, 5.00, uPrice = 100, IV = 0.30),
             snap(20260107, "22:00:00", C100, 2, 6.00, uPrice = 102, IV = 0.32,
                  delta = 0.6, gamma = 0.03, vega = 0.12, theta = -0.06))
  f <- fill(20260105, "15:00:00", C100, 2, 4.90)
  r <- attributePnL(s, f, as_of = ALL)
  q <- 2 * 100
  expect_equal(r$delta[2], q * 0.5 * 2)
  expect_equal(r$gamma[2], q * 0.5 * 0.02 * 4)
  expect_equal(r$vega[2], q * 0.10 * 0.02 * 100)
  expect_equal(r$theta[2], q * -0.02 * 2)                 # two calendar days
  expect_equal(r$actual[2], q * (6 - 5))
  expect_equal(r$residual[2], r$actual[2] - r$delta[2] - r$gamma[2] - r$vega[2] - r$theta[2])
  expect_equal(r$dS[2], 2)
})

test_that("missing Greeks (IBKR -2 / NA) are recomputed with Black-Scholes", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5, delta = NA, vega = -2, theta = -2),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101))
  f <- fill(20260105, "15:00:00", C100, 1, 5)
  r <- attributePnL(s, f, rate = 0.04, as_of = ALL)
  g <- Tbasics::getBSOptGreeks(type = "Call", S = 100, K = 100,
                               DTE = as.numeric(as.Date("2026-12-17") - as.Date("2026-01-05")),
                               sig = 0.30, r = 0.04, div = 0)
  expect_equal(r$delta[2], 100 * g$delta * 1)
  expect_match(r$flag[2], "BS")
})

test_that("an IBKR theta far above the model's is replaced by the Black-Scholes theta", {
  s <- rbind(snap(20260402, "22:00:00", C100, 1, 5, theta = -25),
             snap(20260414, "22:00:00", C100, 1, 4.5))
  r <- attributePnL(s, fill(20260402, "15:00:00", C100, 1, 5), rate = 0.03, as_of = ALL)
  g <- Tbasics::getBSOptGreeks(type = "Call", S = 100, K = 100,
                               DTE = as.numeric(as.Date("2026-12-17") - as.Date("2026-04-02")),
                               sig = 0.30, r = 0.03, div = 0)
  expect_equal(r$theta[2], 100 * g$theta * 12)
  expect_match(r$flag[2], "theta")
  ## A plausible IBKR theta is kept as is.
  s$theta[1] <- g$theta * 1.5
  r <- attributePnL(s, fill(20260402, "15:00:00", C100, 1, 5), rate = 0.03, as_of = ALL)
  expect_equal(r$theta[2], 100 * g$theta * 1.5 * 12)
  expect_false(grepl("theta", r$flag[2]))
})

test_that("a snapshot without IV or underlying is skipped, not used", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "21:00:00", C100, 1, 5.2, uPrice = 100.5),
             snap(20260106, "23:00:00", C100, 1, 9.9, uPrice = NA, IV = 0),
             snap(20260107, "22:00:00", C100, 1, 5.4, uPrice = 101))
  f <- fill(20260105, "15:00:00", C100, 1, 5)
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(r$S, c(100, 100.5, 101))                  # 23:00 row dropped, 21:00 kept
  expect_equal(r$actual[2], 100 * 0.2)
})

test_that("several snapshots in a day: the last usable one is kept", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "09:00:00", C100, 1, 5.1, uPrice = 100),
             snap(20260106, "22:00:00", C100, 1, 5.6, uPrice = 101),
             snap(20260107, "22:00:00", C100, 1, 5.7, uPrice = 101.5))
  r <- attributePnL(s, fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(nrow(r), 3)
  expect_equal(r$S[2], 101)
})

test_that("split guard: an underlying move above 25% goes to residual", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5, uPrice = 400),
             snap(20260106, "22:00:00", C100, 1, 5, uPrice = 100))
  r <- attributePnL(s, fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(r$delta[2], 0)
  expect_match(r$flag[2], "split")
})

test_that("partial close: Greek terms on the starting position, realised cash in actual", {
  s <- rbind(snap(20260105, "22:00:00", C100, 2, 5),
             snap(20260107, "22:00:00", C100, 1, 6, uPrice = 101))
  f <- rbind(fill(20260105, "15:00:00", C100, 2, 5),
             fill(20260106, "15:00:00", C100, -1, 5.8, EventType = "Adjust"))
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(r$delta[2], 2 * 100 * 0.5 * 1)
  expect_equal(r$actual[2], 1 * 100 * 6 - 2 * 100 * 5 + f$Total[2])
  expect_equal(sum(r$actual), 100 * 6 + sum(f$Total))
})

test_that("exit with an underlying price: Greeks to the fill, commission is execution", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101))
  f <- rbind(fill(20260105, "15:00:00", C100, 1, 5),
             fill(20260107, "15:00:00", C100, -1, 6.2, Commission = 1.5,
                  UnderlyingPrice = 102, EventType = "Close"))
  r <- attributePnL(s, f, as_of = ALL)
  ex <- r[nrow(r), ]
  expect_equal(ex$date, as.Date("2026-01-07"))
  expect_equal(ex$kind, "exit")
  expect_equal(r$kind[1:2], c("entry", "day"))
  expect_equal(ex$S, 102)
  expect_equal(ex$delta, 100 * 0.5 * 1)
  expect_equal(ex$execution, -1.5)
  expect_equal(ex$actual, -550 + f$Total[2])
  expect_true(ex$vega != 0)                               # exit IV implied from the fill
  expect_equal(sum(r$actual), sum(f$Total))               # closed: P&L = cash
})

test_that("exit without an underlying price: whole interval is execution, flagged", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101))
  f <- rbind(fill(20260105, "15:00:00", C100, 1, 5),
             fill(20260107, "15:00:00", C100, -1, 6.2, EventType = "Close"))
  ex <- tail(attributePnL(s, f, as_of = ALL), 1)
  expect_equal(ex$execution, ex$actual)
  expect_equal(ex$residual, 0)
  expect_match(ex$flag, "noS")
})

test_that("option + stock trade: the stock fill is the underlying price at exit", {
  st <- function(d, px) snap(d, "22:00:00", "XYZ", 100, px, type = "Stock", multiplier = 1, uPrice = px)
  s <- rbind(snap(20260105, "22:00:00", C100, -1, 5), st(20260105, 100),
             snap(20260106, "22:00:00", C100, -1, 5.5, uPrice = 101), st(20260106, 101))
  f <- rbind(fill(20260105, "15:00:00", C100, -1, 5), fill(20260105, "15:00:00", "XYZ", 100, 100, multiplier = 1, Strike = NA, Right = ""),
             fill(20260107, "15:00:00", C100, 1, 6, EventType = "Close"),
             fill(20260107, "15:00:00", "XYZ", -100, 102, multiplier = 1, Strike = NA, Right = "", EventType = "Close"))
  ex <- tail(attributePnL(s, f, as_of = ALL), 1)
  expect_equal(ex$S, 102)
  expect_false(grepl("noS", ex$flag))
  expect_equal(ex$delta, -100 * 0.5 * 1 + 100 * 1)
})

test_that("future leg: linear on its own price with its multiplier", {
  s <- rbind(snap(20260105, "22:00:00", "ES MAR26", 1, 5000, type = "Future", multiplier = 50, symbol = "ES"),
             snap(20260106, "22:00:00", "ES MAR26", 1, 5010, type = "Future", multiplier = 50, symbol = "ES"))
  f <- fill(20260105, "15:00:00", "ES MAR26", 1, 5000, multiplier = 50, Symbol = "ES", Strike = NA, Right = "")
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(r$delta[2], 500)
  expect_equal(r$residual[2], 0)
})

test_that("cash-only trades and single-day trades give NULL", {
  cash <- rbind(snap(20260105, "22:00:00", "USD", 1000, 0.8, type = "CASH", symbol = "USD", multiplier = 1),
                snap(20260106, "22:00:00", "USD", 1000, 0.81, type = "CASH", symbol = "USD", multiplier = 1))
  expect_null(attributePnL(cash, fill(20260105, "15:00:00", "USD", 1000, 0.8, Symbol = "USD")))
  one <- snap(20260105, "22:00:00", C100, 1, 5)
  expect_null(attributePnL(one, fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL))
})

test_that("snapshot legs no fill names (stale TradeNr) are ignored", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260105, "22:00:00", "XYZ 17DEC26 90 P", -1, 2, type = "Put", strike = 90),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101),
             snap(20260106, "22:00:00", "XYZ 17DEC26 90 P", -1, 1, type = "Put", strike = 90))
  r <- attributePnL(s, fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(r$actual[2], 50)
})

test_that("date-only fills go before the day's snapshot when it already holds them", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "10:00:00", C100, 1, 5.5, uPrice = 101))
  f <- rbind(fill(20260105, "00:00:00", C100, 1, 4.8),
             fill(20260106, "00:00:00", C100, -1, 6, EventType = "Close", UnderlyingPrice = 101.5))
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(r$execution[1], 500 + f$Total[1])          # open fill sorted before the 05.01 snapshot
  expect_equal(nrow(r), 3)                                 # close fill after the 06.01 snapshot
  expect_equal(sum(r$actual), sum(f$Total))
})

test_that("as_of cuts the path", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101),
             snap(20260107, "22:00:00", C100, 1, 5.7, uPrice = 101.5))
  r <- attributePnL(s, fill(20260105, "15:00:00", C100, 1, 5), as_of = as.Date("2026-01-06"))
  expect_equal(max(r$date), as.Date("2026-01-06"))
})

test_that("getTradePnLAttribution reads through the two query functions", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101))
  with_mocked_bindings(
    getTradeSnapshots = function(table, trade_nr) s,
    getTradeFills = function(trade_nr, account) fill(20260105, "15:00:00", C100, 1, 5),
    getLastRate = function(currency, DTE = 30) NA,
    getLastDivYield = function(names) NA, {
      r <- getTradePnLAttribution(1, "U0000000", as_of = ALL)
      expect_equal(nrow(r), 2)
    })
  with_mocked_bindings(getTradeSnapshots = function(table, trade_nr) s[0, ], {
    expect_null(getTradePnLAttribution(1, "U0000000"))
  })
})

test_that("getUnderlyingPriceAt: day close for date-only and expiry rows, NA on no bar", {
  asked <- NULL
  py <- list(get_price_at = function(sym, when) { asked <<- when; list(price = 123.4, bar_time = when) })
  with_mocked_bindings(get_tdata_py = function() py, {
    expect_equal(getUnderlyingPriceAt("XYZ", "2026-01-07 18:37:57"), 123.4)
    expect_equal(asked, "2026-01-07 18:37:57")
    getUnderlyingPriceAt("XYZ", "2026-01-07 00:00:00")
    expect_equal(asked, "2026-01-07 23:59:00")
    getUnderlyingPriceAt("XYZ", "2026-01-07 16:00:00", day_close = TRUE)
    expect_equal(asked, "2026-01-07 23:59:00")
  })
  with_mocked_bindings(get_tdata_py = function() list(get_price_at = function(sym, when) NULL), {
    expect_true(is.na(getUnderlyingPriceAt("XYZ", "2026-01-07 18:37:57")))
  })
})

test_that("an underlying price typed on one leg serves every leg closed with it", {
  P95 <- "XYZ 17DEC26 95 C"
  s <- rbind(snap(20260105, "22:00:00", C100, -1, 5), snap(20260105, "22:00:00", P95, 1, 8, strike = 95),
             snap(20260106, "22:00:00", C100, -1, 5.5, uPrice = 101), snap(20260106, "22:00:00", P95, 1, 8.7, uPrice = 101, strike = 95))
  f <- rbind(fill(20260105, "15:00:00", C100, -1, 5), fill(20260105, "15:00:00", P95, 1, 8, Strike = 95),
             fill(20260107, "15:00:00", C100, 1, 6, EventType = "Close", UnderlyingPrice = 102),
             fill(20260107, "15:00:00", P95, -1, 9.4, Strike = 95, EventType = "Close"))
  ex <- tail(attributePnL(s, f, as_of = ALL), 1)
  expect_false(grepl("noS", ex$flag))
  expect_equal(ex$S, 102)
})
