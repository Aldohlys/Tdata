# ATR% and ATR-based expected-move projection (empirical, asymmetric)
# ------------------------------------------------------------------
# Third lens on "expected move", complementing IV (forward, market-priced)
# and HV (realized sigma). ATR is model-free and path/range-based.
#
# Design (validated 2026-06-09, NewTrading/Reports/atr_move_envelope_*.md):
#   move% ~ C(q) * ATR% * sqrt(N)
# C(q) is the empirical quantile of the asset's OWN standardized signed
# N-session moves. Each historical move is normalized by the ATR% prevailing
# AT THAT TIME (regime-independent "shape"); the band is then re-inflated by
# the CURRENT ATR% (regime-responsive "scale"). Signed quantiles capture
# asymmetry (equities lean up, FX symmetric) that the symmetric IV/HV bands
# cannot. Any confidence level is just two quantiles of the shape vector.
#
# Validity envelope: coefficients hold for equities + equity/commodity ETFs,
# ATR 1-5%, horizon 3-25 sessions. Flags are raised outside that band.

# Wilder ATR series (EMA, alpha = 1/n, adjust = FALSE) — matches the pandas
# ewm form used to validate the coefficients. Returns NA until seeded.
#'@noRd
.wilder_atr <- function(high, low, close, n = 14) {
  m <- length(close)
  prev_close <- c(NA_real_, close[-m])
  tr <- pmax(high - low, abs(high - prev_close), abs(low - prev_close))
  tr[1] <- high[1] - low[1]
  atr <- rep(NA_real_, m)
  if (m < n) return(atr)
  atr[1] <- tr[1]
  for (i in 2:m) atr[i] <- atr[i - 1] + (tr[i] - atr[i - 1]) / n
  # First (n-1) values are seed-contaminated; treat as warm-up
  if (n > 1) atr[1:(n - 1)] <- NA_real_
  atr
}

# Fetch daily OHLC, split/dividend-adjusted via the Adjusted/Close ratio so
# multi-year ATR% is not corrupted by splits.
#'@noRd
.fetch_adjusted_ohlc <- function(sym, lookback_years = 8) {
  from_date <- Sys.Date() - ceiling(lookback_years * 365.25)
  df <- tryCatch(getSymIntervalDate(sym, from_date, Sys.Date()),
                 error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 60) return(NULL)
  df <- df[!is.na(df$Close) & !is.na(df$Adjusted) &
           !is.na(df$High) & !is.na(df$Low) & df$Close > 0, ]
  df <- df[order(df$date), ]
  if (nrow(df) < 60) return(NULL)
  factor <- df$Adjusted / df$Close
  list(date = df$date,
       high = df$High * factor,
       low  = df$Low  * factor,
       close = df$Adjusted)
}

#' ATR as a percentage of price
#'
#' Current Wilder ATR(n) divided by last close, in percent.
#'
#' @param sym IBKR-style ticker
#' @param n ATR window (default 14)
#' @param lookback_years history to fetch (default 2; enough for a stable ATR)
#' @return numeric ATR% (e.g. 2.3 means 2.3%), or NA on data failure
#' @examples
#' \dontrun{ atr_pct("SPY") }
#' @export
atr_pct <- function(sym, n = 14, lookback_years = 2) {
  ohlc <- .fetch_adjusted_ohlc(sym, lookback_years)
  if (is.null(ohlc)) return(NA_real_)
  atr <- .wilder_atr(ohlc$high, ohlc$low, ohlc$close, n)
  last_atr <- utils::tail(atr[!is.na(atr)], 1)
  if (length(last_atr) == 0) return(NA_real_)
  round(last_atr / utils::tail(ohlc$close, 1) * 100, 3)
}

#' Build the standardized signed-move distribution for ATR projection
#'
#' Heavy step: fetches history once and returns the asset's own standardized
#' signed N-session moves plus the current ATR% scale. The cheap
#' \code{atr_expected_move_from_dist()} then reads any confidence level off it
#' without re-fetching — so a UI can cache this per (sym, horizon).
#'
#' @param sym IBKR-style ticker
#' @param horizon_sessions forecast horizon in trading sessions (not calendar days)
#' @param n ATR window (default 14)
#' @param lookback_years history to fetch (default 8)
#' @param spot optional live spot to project around; defaults to last close
#' @return list(sym, spot, horizon_sessions, atr_pct, n_obs, standardized) or NULL
#' @export
atr_move_distribution <- function(sym, horizon_sessions, n = 14,
                                  lookback_years = 8, spot = NULL) {
  stopifnot(is.numeric(horizon_sessions), horizon_sessions >= 1)
  N <- round(horizon_sessions)
  ohlc <- .fetch_adjusted_ohlc(sym, lookback_years)
  if (is.null(ohlc)) {
    logger::log_warn("atr_move_distribution: no data for {sym}", namespace = "Tdata")
    return(NULL)
  }
  close <- ohlc$close
  m <- length(close)
  if (m <= N + n) return(NULL)

  atr <- .wilder_atr(ohlc$high, ohlc$low, close, n)
  atr_pct_series <- atr / close                       # fraction, not percent

  # Signed forward N-session move (fraction), aligned to its start day t
  fwd <- c(close[(N + 1):m] / close[1:(m - N)] - 1, rep(NA_real_, N))

  # Standardize each move by the ATR% that prevailed at its start
  scale_t <- atr_pct_series * sqrt(N)
  standardized <- fwd / scale_t
  standardized <- standardized[is.finite(standardized)]

  cur_atr_pct <- utils::tail(atr_pct_series[is.finite(atr_pct_series)], 1)
  last_close <- utils::tail(close, 1)
  list(
    sym = sym,
    spot = if (is.null(spot)) last_close else spot,
    horizon_sessions = N,
    atr_pct = round(cur_atr_pct * 100, 3),
    n_obs = length(standardized),
    standardized = standardized
  )
}

#' Project an asymmetric ATR expected-move band at a confidence level
#'
#' Cheap: reads quantiles off a distribution from \code{atr_move_distribution()}.
#' An L-confidence interval uses the (1-L)/2 and (1+L)/2 quantiles, so the lower
#' and upper reach can differ (skew). Works for any L in (0,1).
#'
#' @param dist list from \code{atr_move_distribution()}
#' @param conf confidence level in (0,1), e.g. 0.80 or 0.70 (default 0.80)
#' @return list with coefficients, signed move %s, price bounds, asymmetry,
#'   and validity flags; or NULL if dist is NULL/empty
#' @export
atr_expected_move_from_dist <- function(dist, conf = 0.80) {
  if (is.null(dist) || dist$n_obs < 60) return(NULL)
  stopifnot(conf > 0, conf < 1)
  s <- dist$standardized
  q_lo <- (1 - conf) / 2
  q_hi <- (1 + conf) / 2
  c_lo <- as.numeric(stats::quantile(s, q_lo, names = FALSE))
  c_md <- as.numeric(stats::quantile(s, 0.5,  names = FALSE))
  c_hi <- as.numeric(stats::quantile(s, q_hi, names = FALSE))

  scale <- (dist$atr_pct / 100) * sqrt(dist$horizon_sessions)
  spot <- dist$spot

  flags <- character(0)
  if (dist$horizon_sessions > 25)
    flags <- c(flags, "horizon>25: sqrt(N) scaling breaks (drift/mean-reversion)")
  if (dist$atr_pct < 1)
    flags <- c(flags, "ATR<1%: thin tails, band may overstate")
  if (dist$atr_pct > 5)
    flags <- c(flags, "ATR>5%: long-horizon mean-reversion risk")
  if (dist$n_obs < 250)
    flags <- c(flags, paste0("thin history (", dist$n_obs, " obs): quantiles noisy"))

  list(
    sym = dist$sym,
    spot = round(spot, 2),
    horizon_sessions = dist$horizon_sessions,
    conf = conf,
    atr_pct = dist$atr_pct,
    n_obs = dist$n_obs,
    coef_lower = round(c_lo, 3),
    coef_median = round(c_md, 3),
    coef_upper = round(c_hi, 3),
    move_lower_pct = round(c_lo * scale * 100, 2),
    move_median_pct = round(c_md * scale * 100, 2),
    move_upper_pct = round(c_hi * scale * 100, 2),
    price_lower = round(spot * (1 + c_lo * scale), 2),
    price_median = round(spot * (1 + c_md * scale), 2),
    price_upper = round(spot * (1 + c_hi * scale), 2),
    asymmetry = if (c_lo != 0) round(c_hi / abs(c_lo), 2) else NA_real_,
    flags = flags
  )
}

#' ATR expected-move band for a symbol (convenience wrapper)
#'
#' Fetches history and projects the asymmetric band in one call. For repeated
#' confidence levels on the same symbol, call \code{atr_move_distribution()}
#' once and reuse it with \code{atr_expected_move_from_dist()}.
#'
#' @param sym IBKR-style ticker
#' @param horizon_sessions forecast horizon in trading sessions
#' @param conf confidence level in (0,1) (default 0.80)
#' @param n ATR window (default 14)
#' @param lookback_years history to fetch (default 8)
#' @param spot optional live spot; defaults to last close
#' @return list as in \code{atr_expected_move_from_dist()}, or NULL
#' @examples
#' \dontrun{
#'   atr_expected_move("SPY", horizon_sessions = 10, conf = 0.80)
#'   atr_expected_move("NVDA", 20, conf = 0.90)
#' }
#' @export
atr_expected_move <- function(sym, horizon_sessions, conf = 0.80,
                              n = 14, lookback_years = 8, spot = NULL) {
  dist <- atr_move_distribution(sym, horizon_sessions, n, lookback_years, spot)
  atr_expected_move_from_dist(dist, conf)
}
