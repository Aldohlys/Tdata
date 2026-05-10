# Tests for trend.R — isTrendContinuation
#
# Strategy: mock get_har_price_data with synthetic xts in Yahoo column naming
# convention (SYM.High, SYM.Low, SYM.Close) so the regex grep on column suffixes
# matches. Tests cover insufficient-data branches, default-list shape, and
# representative trend scenarios.

# Synthetic price-bar generator. Returns an xts with Open/High/Low/Close columns
# named SYM.<Field> so trend.R's grep-by-suffix lookups succeed.
make_bars <- function(sym, n, drift, sigma = 0.01, seed = 1L,
                      end = as.Date("2026-05-10"), start_price = 100) {
  set.seed(seed)
  rets <- rnorm(n, mean = drift, sd = sigma)
  closes <- start_price * cumprod(1 + rets)
  highs <- closes * (1 + abs(rnorm(n, 0, 0.003)))
  lows  <- closes * (1 - abs(rnorm(n, 0, 0.003)))
  opens <- c(start_price, utils::head(closes, -1))

  dates <- seq(end - n + 1, end, by = "day")
  m <- cbind(opens, highs, lows, closes)
  colnames(m) <- paste0(sym, c(".Open", ".High", ".Low", ".Close"))
  xts::xts(m, order.by = dates)
}

# ---------------------------------------------------------------------------
# Insufficient-data branches
# ---------------------------------------------------------------------------
test_that("isTrendContinuation returns default-FALSE list when bars is NULL", {
  with_mocked_bindings(
    get_har_price_data = function(...) NULL, {
      r <- isTrendContinuation("AAPL")
      expect_false(r$passes)
      expect_match(r$reason, "need >=200 bars, got NULL")
      expect_equal(r$sym, "AAPL")
    })
})

test_that("isTrendContinuation returns default-FALSE list when bars has <200 rows", {
  bars <- make_bars("AAPL", n = 100, drift = 0.001)
  with_mocked_bindings(
    get_har_price_data = function(...) bars, {
      r <- isTrendContinuation("AAPL")
      expect_false(r$passes)
      expect_match(r$reason, "need >=200 bars, got 100")
    })
})

test_that("isTrendContinuation returns default-FALSE when get_har_price_data errors", {
  with_mocked_bindings(
    get_har_price_data = function(...) stop("yfinance offline"), {
      r <- isTrendContinuation("AAPL")
      expect_false(r$passes)
      expect_match(r$reason, "need >=200 bars, got NULL")
    })
})

test_that("isTrendContinuation rejects non-character or vector sym", {
  expect_error(isTrendContinuation(c("AAPL", "MSFT")))
  expect_error(isTrendContinuation(123))
})

# ---------------------------------------------------------------------------
# Default-list shape (smoke) — verify all expected fields are present
# ---------------------------------------------------------------------------
test_that("default-FALSE list contains all expected diagnostic fields", {
  with_mocked_bindings(
    get_har_price_data = function(...) NULL, {
      r <- isTrendContinuation("AAPL")
      expect_setequal(names(r), c(
        "sym", "passes", "reason",
        "stage2", "ma_stack_ok", "sma200_rising",
        "pct_from_52wh", "in_52wh_band",
        "hh_count", "hl_count", "hh_hl_ok",
        "rsi14", "rsi_in_trend",
        "rs_vs_bench_3m", "rs_positive",
        "pullback_state", "pullback_ok",
        "price", "sma20", "sma50", "sma150", "sma200"
      ))
    })
})

# ---------------------------------------------------------------------------
# Real-shape outputs — uptrend / downtrend / flat scenarios.
# These tests drive sufficient bars through the full pipeline and check the
# diagnostic fields are populated (not asserting passes=TRUE, since matching
# every Stage-2 rule with synthetic data is fragile — see comments).
# ---------------------------------------------------------------------------
test_that("uptrend scenario populates all numeric diagnostics", {
  sym_bars   <- make_bars("AAPL", n = 300, drift = 0.0008, sigma = 0.012, seed = 42)
  bench_bars <- make_bars("SPY",  n = 300, drift = 0.0003, sigma = 0.008, seed = 7)

  with_mocked_bindings(
    get_har_price_data = function(sym, ...) {
      if (sym == "SPY") bench_bars else sym_bars
    }, {
      r <- isTrendContinuation("AAPL")

      expect_true(is.numeric(r$pct_from_52wh))
      expect_true(is.numeric(r$rsi14))
      expect_true(is.numeric(r$rs_vs_bench_3m))
      expect_true(is.integer(r$hh_count))
      expect_true(is.integer(r$hl_count))
      expect_true(r$pullback_state %in% c(
        "strong (near SMA20 + RSI retraced)", "at SMA20", "RSI retraced", "none"))

      # SMAs all populated and ordered (uptrend → 20 >= 50 >= 150 >= 200, roughly)
      expect_true(is.numeric(r$sma20) && !is.na(r$sma20))
      expect_true(is.numeric(r$sma200) && !is.na(r$sma200))
      expect_gt(r$sma20, r$sma200)
    })
})

test_that("flat scenario fails stage2 (price not above SMA50)", {
  flat <- make_bars("XYZ", n = 300, drift = 0, sigma = 0.005, seed = 1)
  bench <- make_bars("SPY", n = 300, drift = 0, sigma = 0.005, seed = 2)

  with_mocked_bindings(
    get_har_price_data = function(sym, ...) {
      if (sym == "SPY") bench else flat
    }, {
      r <- isTrendContinuation("XYZ")
      expect_false(r$passes)
      expect_match(r$reason, "fails:")
    })
})

test_that("downtrend scenario fails stage2 and yields negative RS", {
  down  <- make_bars("DOWN", n = 300, drift = -0.001, sigma = 0.012, seed = 5,
                     start_price = 200)
  bench <- make_bars("SPY",  n = 300, drift = 0.0003, sigma = 0.008, seed = 7)

  with_mocked_bindings(
    get_har_price_data = function(sym, ...) {
      if (sym == "SPY") bench else down
    }, {
      r <- isTrendContinuation("DOWN")
      expect_false(r$passes)
      expect_false(isTRUE(r$stage2))
      expect_false(isTRUE(r$rs_positive))
    })
})

test_that("missing benchmark data leaves rs_vs_bench_3m as NA", {
  sym_bars <- make_bars("AAPL", n = 300, drift = 0.0008, seed = 11)

  with_mocked_bindings(
    get_har_price_data = function(sym, ...) {
      if (sym == "SPY") NULL else sym_bars
    }, {
      r <- isTrendContinuation("AAPL")
      expect_true(is.na(r$rs_vs_bench_3m))
      expect_true(is.na(r$rs_positive))
      # Composite cannot pass without RS info (rs_positive is NA → not isTRUE)
      expect_false(r$passes)
    })
})

test_that("custom benchmark argument is honored in get_har_price_data calls", {
  sym_bars   <- make_bars("AAPL", n = 300, drift = 0.0008, seed = 13)
  bench_bars <- make_bars("QQQ",  n = 300, drift = 0.0003, seed = 17)
  fetched <- character()

  with_mocked_bindings(
    get_har_price_data = function(sym, ...) {
      fetched <<- c(fetched, sym)
      if (sym == "QQQ") bench_bars else sym_bars
    }, {
      isTrendContinuation("AAPL", bench = "QQQ")
    })

  expect_setequal(fetched, c("AAPL", "QQQ"))
})
