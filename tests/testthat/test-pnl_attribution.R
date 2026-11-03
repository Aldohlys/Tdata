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
             snap(20260414, "22:00:00", C100, 1, 4.5, uPrice = 99))
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

test_that("a snapshot with a frozen uPrice next to moving marks is dropped", {
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5.00, uPrice = 100),
             snap(20260106, "22:00:00", C100, 1, 5.50, uPrice = 101),
             snap(20260107, "10:00:00", C100, 1, 7.00, uPrice = 101),   # pre-market: marks +27%, uPrice frozen
             snap(20260107, "22:00:00", C100, 1, 5.60, uPrice = 101.2))
  r <- attributePnL(s, fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(r$S, c(100, 101, 101.2))
  expect_equal(r$actual[3], 100 * (5.60 - 5.50))
  ## Today's pre-market snapshot is dropped too: the path ends at the last
  ## consistent one.
  r <- attributePnL(s[1:3, ], fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(max(r$date), as.Date("2026-01-06"))
  ## Same uPrice with a small mark change (decay, IV) is kept.
  s$mktPrice[3] <- 5.52
  r <- attributePnL(s[1:3, ], fill(20260105, "15:00:00", C100, 1, 5), as_of = ALL)
  expect_equal(max(r$date), as.Date("2026-01-07"))
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

## Daily completion: marks instead of a DB, TWS or Yahoo.

close_rows <- function(dates, close, sym = "XYZ", source = "bar") {
  data.frame(leg = sym, date = as.Date(dates), close = close, iv = NA_real_, source = source,
             stringsAsFactors = FALSE)
}
stock_leg <- data.frame(key = "XYZ", symbol = "XYZ", type = "Stock", strike = NA_real_,
                        expiry = as.Date(NA), multiplier = 1, currency = "USD",
                        stringsAsFactors = FALSE)
call_leg <- data.frame(key = C100, symbol = "XYZ", type = "Call", strike = 100,
                       expiry = as.Date("2026-12-17"), multiplier = 100, currency = "USD",
                       stringsAsFactors = FALSE)
no_snaps <- snap(20260105, "22:00:00", "XYZ", 1, 100, type = "Stock", multiplier = 1)[0, ]
stock_fill <- function(...) fill(..., Strike = NA, Right = "", multiplier = 1, Commission = 0)

test_that("an option fill no snapshot names stays off the stock leg (trade 737, roll of 31.08)", {
  stk <- "XYZ"
  s <- rbind(snap(20260105, "22:00:00", stk, 100, 100, type = "Stock", multiplier = 1),
             snap(20260105, "22:00:00", C100, -1, 5, uPrice = 100),
             snap(20260120, "22:00:00", stk, 100, 101, type = "Stock", multiplier = 1),
             snap(20260120, "22:00:00", C100, -1, 5.5, uPrice = 101))
  C110 <- "XYZ 18DEC26 110 C"
  f <- rbind(fill(20260105, "15:00:00", stk, 100, 100, multiplier = 1, Strike = NA, Right = ""),
             fill(20260105, "15:00:00", C100, -1, 5),
             fill(20260112, "15:00:00", C110, -1, 10.82, Strike = 110),
             fill(20260112, "15:00:00", C110, 1, 11.00, Strike = 110))
  r <- attributePnL(s, f, as_of = ALL)
  expect_false(any(grepl("split", r$flag)))
  ## The stock's delta is its own move, not moved by the option's cash.
  expect_equal(r$delta[2], 100 * 1 + -100 * 0.5 * 1)
  expect_equal(.fill_keys(f[3, ]), C110)
})

test_that("stock-only trade: one row per session from closes, no snapshot needed", {
  days <- c("2026-01-05", "2026-01-06", "2026-01-07", "2026-01-08")
  f <- stock_fill(20260105, "15:00:00", "XYZ", 10, 99.5)
  r <- attributePnL(no_snaps, f, as_of = as.Date("2026-01-08"),
                    daily = list(legs = stock_leg, marks = close_rows(days, c(100, 103, 101, 102))))
  expect_true(attr(r, "stock_only"))
  expect_equal(as.character(r$date), days)
  expect_equal(r$delta, c(0, 30, -20, 10))
  expect_equal(r$execution[1], 10 * 100 - 995)
  expect_true(all(c(r$gamma, r$vega, r$theta, r$residual) == 0))
  expect_equal(sum(r$actual), 10 * 102 + f$Total)
  expect_true(all(r$flag == ""))
})

test_that("stock-only: a fill inside an interval is execution, residual stays zero", {
  days <- c("2026-01-05", "2026-01-06", "2026-01-07")
  f <- rbind(stock_fill(20260105, "15:00:00", "XYZ", 10, 100),
             stock_fill(20260106, "15:00:00", "XYZ", 10, 104))
  r <- attributePnL(no_snaps, f, as_of = as.Date("2026-01-07"),
                    daily = list(legs = stock_leg, marks = close_rows(days, c(100, 103, 105))))
  expect_true(all(r$residual == 0))
  expect_equal(r$delta + r$execution, r$actual)
  expect_equal(sum(r$actual), 20 * 105 + sum(f$Total))
})

test_that("option trade: gaps filled, model days flagged, total still reconciles", {
  days <- c("2026-01-05", "2026-01-06", "2026-01-07", "2026-01-08")
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5, uPrice = 100),
             snap(20260108, "22:00:00", C100, 1, 6, uPrice = 102))
  f <- fill(20260105, "15:00:00", C100, 1, 4.9)
  marks <- rbind(close_rows(days, c(100, 101, 100.5, 102)),
                 data.frame(leg = C100, date = as.Date(days[2:3]), close = c(5.5, 5.2),
                            iv = c(0.30, 0.31), source = c("bar", "model"), stringsAsFactors = FALSE))
  r <- attributePnL(s, f, as_of = as.Date("2026-01-08"), daily = list(legs = call_leg, marks = marks))
  expect_equal(as.character(r$date), days)
  expect_equal(r$actual[2:4], 100 * c(0.5, -0.3, 0.8))
  expect_equal(r$flag[2], "")
  expect_equal(r$flag[3], "model")          # ends on the model mark
  expect_equal(r$flag[4], "model")          # starts from it
  expect_equal(sum(r$actual), 100 * 6 + f$Total)
})

test_that("a Saturday snapshot leaves the path; a day with an unmarked leg is not added", {
  days <- c("2026-01-09", "2026-01-12", "2026-01-13")
  s <- rbind(snap(20260109, "22:00:00", C100, 1, 5, uPrice = 100),
             snap(20260110, "10:00:00", C100, 1, 5.6, uPrice = 100),
             snap(20260113, "22:00:00", C100, 1, 6, uPrice = 102))
  f <- fill(20260109, "15:00:00", C100, 1, 5)
  r <- attributePnL(s, f, as_of = as.Date("2026-01-13"),
                    daily = list(legs = call_leg, marks = close_rows(days, c(100, 101, 102))))
  expect_equal(as.character(r$date), c("2026-01-09", "2026-01-13"))
  expect_equal(sum(r$actual), 100 * 6 + f$Total)
})

test_that("a closing fill with no underlying price takes the day's close", {
  days <- c("2026-01-05", "2026-01-06", "2026-01-07")
  s <- rbind(snap(20260105, "22:00:00", C100, 1, 5, uPrice = 100),
             snap(20260106, "22:00:00", C100, 1, 5.5, uPrice = 101))
  f <- rbind(fill(20260105, "15:00:00", C100, 1, 5),
             fill(20260107, "15:00:00", C100, -1, 6, EventType = "Close"))
  r <- attributePnL(s, f, as_of = ALL,
                    daily = list(legs = call_leg, marks = close_rows(days, c(100, 101, 103))))
  expect_false(grepl("noS", r$flag[nrow(r)]))
  expect_equal(r$dS[nrow(r)], 2)
})

test_that(".trade_legs: legs from fills; NULL for what cannot be completed", {
  tk <- data.frame(Name = c("XYZ", "CL"), Type = c("STK", "FUT"), Currency = "USD",
                   Multiplier = 100, OptExchange = NA_character_, stringsAsFactors = FALSE)
  f <- rbind(fill(20260105, "15:00:00", "XYZ INC", 100, 100, Strike = NA, Right = ""),
             fill(20260105, "15:00:00", C100, -1, 5))
  legs <- .trade_legs(no_snaps, f, tk)
  expect_equal(legs$key, c("XYZ", C100))
  expect_equal(legs$type, c("Stock", "Call"))
  expect_equal(legs$expiry[2], as.Date("2026-12-17"))
  expect_equal(legs$multiplier, c(1, 100))

  combo <- fill(20260105, "15:00:00", "XYZ DEC26 100/110 CS", 1, 5)
  expect_null(.trade_legs(no_snaps, combo, tk))
  fut <- fill(20260105, "15:00:00", "CL 16OCT25 60 P", 1, 5, Symbol = "CL")
  expect_null(.trade_legs(no_snaps, fut, tk))
  unknown <- fill(20260105, "15:00:00", "ABC 17DEC26 100 C", 1, 5, Symbol = "ABC")
  expect_null(.trade_legs(no_snaps, unknown, tk))
})

test_that(".closes_consistent rejects a split or another quoting unit", {
  closes <- close_rows("2026-01-05", 50)
  s <- data.frame(symbol = "XYZ", day = as.Date("2026-01-05"), S = 100)
  f <- stock_fill(20260105, "15:00:00", "XYZ", 10, 51)
  expect_false(.closes_consistent(closes, s, f))
  expect_true(.closes_consistent(closes, s[0, ], f))
  expect_false(.closes_consistent(closes, s[0, ], stock_fill(20260105, "15:00:00", "XYZ", 10, 5000)))
})

test_that(".model_marks interpolates IV between real marks, flat beyond them", {
  days <- as.Date(c("2026-01-05", "2026-01-06", "2026-01-07", "2026-01-08"))
  closes <- close_rows(days, rep(100, 4))
  bars <- data.frame(leg = C100, date = days[c(1, 3)], close = c(5, 6), iv = c(0.20, 0.40),
                     source = "bar", stringsAsFactors = FALSE)
  f <- fill(20260105, "15:00:00", C100, 1, 0)   # price 0 gives no anchor
  s <- data.frame(key = character(), day = as.Date(character()), uPrice = numeric(), mktPrice = numeric())
  m <- .model_marks(call_leg, days[c(2, 4)], bars, closes, s, f, 0, 0)
  expect_equal(m$iv, c(0.30, 0.40))
  expect_equal(m$bracketed, c(TRUE, FALSE))
  expect_equal(m$close[1], Tbasics::getBSCallPrice(S = 100, K = 100, r = 0, DTE = 345, sig = 0.30, div = 0))
  expect_null(.model_marks(call_leg, days[2], bars[0, ], closes, s, f, 0, 0))
})

test_that("getTradeDailyMarks: stored marks need no TWS; expired legs are not asked", {
  expired <- "XYZ 16JAN26 100 C"
  f <- rbind(fill(20260105, "15:00:00", expired, 1, 5, UnderlyingPrice = 100),
             fill(20260112, "15:00:00", expired, -1, 6, UnderlyingPrice = 102, EventType = "Close"))
  days <- as.Date(c("2026-01-05", "2026-01-06", "2026-01-07", "2026-01-08", "2026-01-09", "2026-01-12"))
  store <- close_rows(days, seq(100, 102, length.out = 6))
  written <- NULL
  tk <- data.frame(Name = "XYZ", Type = "STK", Currency = "USD", Multiplier = 100,
                   OptExchange = NA_character_, stringsAsFactors = FALSE)
  res <- with_mocked_bindings(
    .leg_tickers = function(symbols) tk,
    readLegMarks = function(legs, from, to) store[store$leg %in% legs & store$date >= from & store$date <= to, ],
    writeLegMarks = function(marks) { written <<- rbind(written, marks); invisible(nrow(marks)) },
    isIBAvailable = function() stop("TWS must not be touched"),
    getTradeDailyMarks(no_snaps, f, as_of = as.Date("2026-01-12")))
  m <- res$marks[res$marks$leg == expired, ]
  expect_equal(m$date, days[1:5])                    # closed on the 12th: not held at that close
  expect_true(all(m$source == "model"))
  expect_equal(nrow(written), 5)                     # a closed trade: all of them final
  expect_true(all(written$source == "model"))

  ## Seen again: everything stored, nothing written.
  store <- rbind(store, m)
  written <- NULL
  with_mocked_bindings(
    .leg_tickers = function(symbols) tk,
    readLegMarks = function(legs, from, to) store[store$leg %in% legs & store$date >= from & store$date <= to, ],
    writeLegMarks = function(marks) { written <<- rbind(written, marks); invisible(nrow(marks)) },
    isIBAvailable = function() stop("TWS must not be touched"),
    getTradeDailyMarks(no_snaps, f, as_of = as.Date("2026-01-12")))
  expect_null(written)
})

test_that("writeLegMarks: a bar replaces a row, a model mark or a Yahoo close does not", {
  path <- tempfile(fileext = ".db")
  on.exit(unlink(path), add = TRUE)
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(conn, "CREATE TABLE LegDailyMark (leg TEXT NOT NULL, date INTEGER NOT NULL,
                        close REAL NOT NULL, iv REAL, source TEXT NOT NULL,
                        PRIMARY KEY (leg, date)) WITHOUT ROWID")
  DBI::dbDisconnect(conn)
  row <- function(close, source) data.frame(leg = C100, date = as.Date("2026-01-05"), close = close,
                                            iv = 0.3, source = source, stringsAsFactors = FALSE)
  with_mocked_bindings(safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), path), {
    writeLegMarks(row(1, "model"))
    writeLegMarks(row(2, "bar"))
    writeLegMarks(row(3, "model"))
    writeLegMarks(row(4, "yahoo"))
    r <- readLegMarks(C100, as.Date("2026-01-01"), as.Date("2026-01-31"))
  })
  expect_equal(nrow(r), 1)
  expect_equal(r$close, 2)
  expect_equal(r$source, "bar")
  expect_equal(r$date, as.Date("2026-01-05"))
})

test_that("a fill stamped midnight New York is date-only: placed by the snapshot's position (trade 760)", {
  s <- rbind(snap(20261019, "22:01:58", "XYZ", 300, 41.29, type = "Stock", multiplier = 1),
             snap(20261021, "09:16:44", "XYZ", 300, 40.90, type = "Stock", multiplier = 1))
  f <- rbind(stock_fill(20261019, "04:00:00", "XYZ", 300, 41.32),
             stock_fill(20261021, "04:00:00", "XYZ", -300, 40.13))
  r <- attributePnL(s, f, as_of = ALL)
  expect_equal(sum(r$actual), sum(f$Total))
  expect_equal(r$kind[nrow(r)], "exit")
})

test_that("bought one day, sold the next: one close and the exit make a path (trade 769)", {
  days <- c("2026-01-05", "2026-01-06")
  f <- rbind(stock_fill(20260105, "15:00:00", "XYZ", 10, 100),
             stock_fill(20260106, "15:00:00", "XYZ", -10, 102))
  r <- attributePnL(no_snaps, f, as_of = ALL,
                    daily = list(legs = stock_leg, marks = close_rows(days, c(101, 103))))
  expect_equal(r$kind, c("entry", "exit"))
  expect_equal(sum(r$actual), sum(f$Total))
})
