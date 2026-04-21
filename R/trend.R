#' isTrendContinuation
#' Evaluate whether a symbol is in a confirmed uptrend with pullback characteristics
#' suitable for a "2nd/3rd inning" breakout-continuation option play.
#'
#' Returns a diagnostic list (not a single pass/fail flag) so the caller can surface
#' individual criteria and let the trader make the discretionary call on trap vs. valid pullback.
#'
#' Pass criteria (Stage-2 à la Minervini/Weinstein):
#'   - Price above SMA50; SMA50 within 5% of SMA150; SMA150 within 5% of SMA200 (qualitative stack)
#'   - SMA200 rising over the last 20 bars (~4 weeks)
#'   - Price within 1-25% of 52-week high (trend mature but room to extend)
#'   - HH/HL structure: at least 7 higher highs and 7 higher lows (out of 14 diffs over 15 bars)
#'   - RSI(14) in [40, 75] — in-trend, allows momentum names with some heat
#'   - RS vs SPY over 3 months: stock outperforming SPY
#'   - Pullback active: price within 5% of SMA20, OR RSI recently retraced from >65 to 40-55
#'
#' @param sym character — single symbol
#' @param bench character — benchmark ticker for RS (default "SPY")
#' @param lookback_days numeric — how much calendar history to pull (default 400, yields ~275 trading bars)
#' @return named list with all diagnostic fields, plus a composite $passes boolean
#' @export
isTrendContinuation <- function(sym, bench = "SPY", lookback_days = 400) {
  stopifnot(is.character(sym), length(sym) == 1L)

  default <- list(
    sym            = sym,
    passes         = FALSE,
    reason         = "insufficient data",
    stage2         = NA,
    ma_stack_ok    = NA,
    sma200_rising  = NA,
    pct_from_52wh  = NA_real_,
    in_52wh_band   = NA,
    hh_count       = NA_integer_,
    hl_count       = NA_integer_,
    hh_hl_ok       = NA,
    rsi14          = NA_real_,
    rsi_in_trend   = NA,
    rs_vs_bench_3m = NA_real_,
    rs_positive    = NA,
    pullback_state = NA_character_,
    pullback_ok    = NA,
    price          = NA_real_,
    sma20          = NA_real_,
    sma50          = NA_real_,
    sma150         = NA_real_,
    sma200         = NA_real_
  )

  bars <- tryCatch(
    get_har_price_data(sym, lookback_days = lookback_days, source = "yahoo"),
    error = function(e) NULL
  )

  if (is.null(bars) || nrow(bars) < 200) {
    default$reason <- sprintf("need >=200 bars, got %s", if (is.null(bars)) "NULL" else nrow(bars))
    return(default)
  }

  close_col  <- grep("\\.Close$", colnames(bars), value = TRUE)[1]
  high_col   <- grep("\\.High$",  colnames(bars), value = TRUE)[1]
  low_col    <- grep("\\.Low$",   colnames(bars), value = TRUE)[1]
  closes <- as.numeric(bars[, close_col])
  highs  <- as.numeric(bars[, high_col])
  lows   <- as.numeric(bars[, low_col])
  n      <- length(closes)

  price  <- utils::tail(closes, 1)
  sma20  <- utils::tail(TTR::SMA(closes,  20), 1)
  sma50  <- utils::tail(TTR::SMA(closes,  50), 1)
  sma150 <- utils::tail(TTR::SMA(closes, 150), 1)
  sma200 <- utils::tail(TTR::SMA(closes, 200), 1)

  ### MA stack: price > 50 > 150 > 200, with 5% tolerance on inter-MA comparisons
  ### Rationale: converging stacks during a base-to-trend transition can have MAs within
  ### a couple of percent of each other; strict ordering rejects valid early-trend setups.
  ### Qualitative evaluation — user filters further downstream.
  ma_tol <- 0.95
  ma_stack_ok <- !any(is.na(c(sma50, sma150, sma200))) &&
                 (price > sma50) &&
                 (sma50 >= sma150 * ma_tol) &&
                 (sma150 >= sma200 * ma_tol)

  ### SMA200 rising: today > 20 bars ago
  sma200_full <- TTR::SMA(closes, 200)
  sma200_rising <- !is.na(sma200_full[n]) && !is.na(sma200_full[n - 20]) &&
                   (sma200_full[n] > sma200_full[n - 20])

  ### Distance from 52w high (last 252 bars)
  hi_window <- max(utils::tail(highs, min(n, 252)), na.rm = TRUE)
  pct_from_52wh <- round(100 * (price - hi_window) / hi_window, 2)
  in_52wh_band <- pct_from_52wh >= -25 && pct_from_52wh <= -1  # 1-25% below

  stage2 <- ma_stack_ok && sma200_rising && in_52wh_band

  ### HH/HL structure: count higher highs and higher lows over last 15 bars
  last15_highs <- utils::tail(highs, 15)
  last15_lows  <- utils::tail(lows, 15)
  hh_count <- sum(diff(last15_highs) > 0, na.rm = TRUE)
  hl_count <- sum(diff(last15_lows)  > 0, na.rm = TRUE)
  hh_hl_ok <- hh_count >= 7 && hl_count >= 7  # >half of 14 diffs

  ### RSI(14) — in-trend band [40, 75]. Upper end widened beyond 70 to accommodate
  ### momentum names with pullback already in progress but not yet mean-reverted.
  rsi14 <- utils::tail(TTR::RSI(closes, 14), 1)
  rsi_in_trend <- !is.na(rsi14) && rsi14 >= 40 && rsi14 <= 75

  ### Relative strength vs benchmark over last ~63 bars (3 months)
  bench_bars <- tryCatch(
    get_har_price_data(bench, lookback_days = lookback_days, source = "yahoo"),
    error = function(e) NULL
  )
  rs_vs_bench_3m <- NA_real_
  rs_positive <- NA
  if (!is.null(bench_bars) && nrow(bench_bars) >= 63) {
    bench_close_col <- grep("\\.Close$", colnames(bench_bars), value = TRUE)[1]
    bench_closes <- as.numeric(bench_bars[, bench_close_col])
    sym_ret   <- utils::tail(closes, 1) / utils::head(utils::tail(closes, 63), 1) - 1
    bench_ret <- utils::tail(bench_closes, 1) / utils::head(utils::tail(bench_closes, 63), 1) - 1
    rs_vs_bench_3m <- round(100 * (sym_ret - bench_ret), 2)
    rs_positive <- rs_vs_bench_3m > 0
  }

  ### Pullback state — two indicators, either one counts as "pullback active"
  ### (a) price within 5% of SMA20 from above (retracement to dynamic support)
  ### (b) RSI pulled back from >65 within last 10 bars to now 40-55
  pct_from_sma20 <- if (!is.na(sma20) && sma20 > 0) 100 * (price - sma20) / sma20 else NA_real_
  rsi_series <- TTR::RSI(closes, 14)
  rsi_recent_max <- max(utils::tail(rsi_series, 10), na.rm = TRUE)

  at_sma20 <- !is.na(pct_from_sma20) && pct_from_sma20 >= -2 && pct_from_sma20 <= 5
  rsi_retraced <- !is.na(rsi14) && !is.na(rsi_recent_max) &&
                  rsi_recent_max > 65 && rsi14 >= 40 && rsi14 <= 55

  pullback_state <- if (at_sma20 && rsi_retraced) "strong (near SMA20 + RSI retraced)"
                    else if (at_sma20)              "at SMA20"
                    else if (rsi_retraced)          "RSI retraced"
                    else                            "none"
  pullback_ok <- at_sma20 || rsi_retraced

  ### Composite pass: stage2 + HH/HL + RSI + RS + pullback
  passes <- isTRUE(stage2) && isTRUE(hh_hl_ok) && isTRUE(rsi_in_trend) &&
            isTRUE(rs_positive) && isTRUE(pullback_ok)

  reason <- if (passes) "all criteria met" else {
    fails <- c(
      if (!isTRUE(stage2))       "stage2"        else NULL,
      if (!isTRUE(hh_hl_ok))     "HH/HL"         else NULL,
      if (!isTRUE(rsi_in_trend)) "RSI"           else NULL,
      if (!isTRUE(rs_positive))  "RS<=bench"     else NULL,
      if (!isTRUE(pullback_ok))  "no pullback"   else NULL
    )
    paste("fails:", paste(fails, collapse = ", "))
  }

  list(
    sym            = sym,
    passes         = passes,
    reason         = reason,
    stage2         = stage2,
    ma_stack_ok    = ma_stack_ok,
    sma200_rising  = sma200_rising,
    pct_from_52wh  = pct_from_52wh,
    in_52wh_band   = in_52wh_band,
    hh_count       = hh_count,
    hl_count       = hl_count,
    hh_hl_ok       = hh_hl_ok,
    rsi14          = round(rsi14, 2),
    rsi_in_trend   = rsi_in_trend,
    rs_vs_bench_3m = rs_vs_bench_3m,
    rs_positive    = rs_positive,
    pullback_state = pullback_state,
    pullback_ok    = pullback_ok,
    price          = round(price, 2),
    sma20          = round(sma20, 2),
    sma50          = round(sma50, 2),
    sma150         = round(sma150, 2),
    sma200         = round(sma200, 2)
  )
}
