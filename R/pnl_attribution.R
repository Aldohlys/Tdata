## P&L attribution of a multi-day trade: delta / gamma / vega / theta /
## residual / execution, day by day, from the daily portfolio snapshots.
##
## Per leg and per interval between two consecutive daily snapshots, Greeks at
## the start of the interval (IBKR units: vega per vol point, theta per
## calendar day, IV decimal):
##   delta = q * delta * dS        gamma = q * 0.5 * gamma * dS^2
##   vega  = q * vega * dIV * 100  theta = q * theta * days      (q = pos * mult)
## Stock, CFD, future, T-bill and cash legs carry delta = q * dS on their own
## price and nothing else.
## actual = value change + fill cash flows; residual = actual - Greek terms -
## execution. Execution is what the Greeks cannot see: fill vs first mark for a
## leg opened in the interval, commissions for a leg closed in it.

.linear_types <- c("Stock", "CFD", "Future", "TreasuryBill", "CASH")
.split_guard <- 0.25    ## |dS|/S above this is a split, not a move (uPrice is unadjusted)

#' Snapshot rows of one trade
#'
#' @param table Snapshot table name (an IBKR account: U1804173, U25343478, DU5221795)
#' @param trade_nr Trade number
#' @return data.frame of the trade's snapshot rows, all dates and times
#' @export
getTradeSnapshots <- function(table, trade_nr) {
  stopifnot(grepl("^[A-Za-z0-9]+$", table))
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, sprintf(
    "SELECT date, heure, symbol, type, Instrument, strike, expdate, pos, uPrice,
            mktPrice, multiplier, currency, IV, delta, gamma, vega, theta
       FROM %s WHERE TradeNr = ?", table),
    params = list(as.integer(trade_nr)))
}

#' Fills of one trade
#'
#' @param trade_nr Trade number
#' @param account Account of the trade
#' @return data.frame of the trade's Trades rows
#' @export
getTradeFills <- function(trade_nr, account) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn,
    "SELECT TradeDate, DateTime, TimeZoneSource, Instrument, Symbol, Pos, Price,
            Commission, Total, Strike, Right, EventType, UnderlyingPrice
       FROM Trades WHERE TradeNr = ? AND Account = ?",
    params = list(as.integer(trade_nr), account))
}

## Snapshot rows -> one row per leg per kept snapshot. Missing Greeks (NA, or
## IBKR's -2 placeholder, recognisable as a negative vega; or the all-zero row
## IBKR writes when it has no model) are recomputed from IV and uPrice when
## both are there. Per date, the last snapshot in which every leg is usable is
## kept.
.prepare_snapshots <- function(snaps, rate, div_yield) {
  s <- snaps
  s$ts <- as.POSIXct(paste(s$date, s$heure), format = "%Y%m%d %H:%M:%S", tz = "Europe/Zurich")
  s$day <- as.Date(as.character(s$date), "%Y%m%d")
  s$linear <- s$type %in% .linear_types
  s$key <- ifelse(s$linear & s$type != "Future", s$symbol, s$Instrument)
  s$flag <- ""

  missing <- !s$linear & (is.na(s$delta) | is.na(s$gamma) | is.na(s$vega) |
                            is.na(s$theta) | (!is.na(s$vega) & s$vega < 0) |
                            (s$delta %in% 0 & s$vega %in% 0))
  fixable <- missing & !is.na(s$IV) & s$IV > 0 & !is.na(s$uPrice) & s$uPrice > 0
  if (any(fixable)) {
    dte <- as.numeric(as.Date(as.character(s$expdate[fixable]), "%Y%m%d") - s$day[fixable])
    g <- Tbasics::getBSOptGreeks(type = s$type[fixable], S = s$uPrice[fixable],
                                 K = s$strike[fixable], DTE = dte, sig = s$IV[fixable],
                                 r = rate, div = div_yield)
    s[fixable, c("delta", "gamma", "vega", "theta")] <- g[, c("delta", "gamma", "vega", "theta")]
    s$flag[fixable] <- "BS"
  }
  s$S <- ifelse(s$linear, s$mktPrice, s$uPrice)
  s$usable <- !is.na(s$mktPrice) & !is.na(s$S) & s$S > 0 &
    (s$linear | (!(missing & !fixable) & !is.na(s$IV) & s$IV > 0))

  ok_ts <- tapply(s$usable, s$ts, all)
  s <- s[s$ts %in% as.POSIXct(names(ok_ts)[ok_ts], tz = "Europe/Zurich"), ]
  last_ts <- tapply(s$ts, s$day, max)
  s <- s[s$ts %in% as.POSIXct(last_ts, origin = "1970-01-01", tz = "Europe/Zurich"), ]
  s <- s[!duplicated(s[c("ts", "key")]), ]
  s$value <- s$pos * s$multiplier * s$mktPrice
  s[order(s$ts, s$key), ]
}

## Fill timestamps in Europe/Zurich, the snapshot clock. Trades.DateTime is
## stored in UTC (parse_ibkr_datetime); TimeZoneSource only records its origin.
.prepare_fills <- function(fills, snap_keys) {
  f <- fills
  f$ts <- lubridate::with_tz(lubridate::ymd_hms(f$DateTime, tz = "UTC", truncated = 3, quiet = TRUE),
                             "Europe/Zurich")
  no_time <- is.na(f$ts)
  f$ts[no_time] <- as.POSIXct(paste(f$TradeDate[no_time], "12:00:00"),
                              format = "%Y%m%d %H:%M:%S", tz = "UTC")
  f$day <- as.Date(as.character(f$TradeDate), "%Y%m%d")
  f$key <- ifelse(f$Instrument %in% snap_keys, f$Instrument, f$Symbol)
  f$Commission[is.na(f$Commission)] <- 0
  f
}

## Fills stored with a date and no time (00:00:00 UTC, migrated rows) can sit
## on either side of that day's snapshot. Per leg and day they go before it
## when the snapshot position already includes them, after it otherwise.
.place_dateonly_fills <- function(f, s) {
  utc <- lubridate::with_tz(f$ts, "UTC")
  dateonly <- !is.na(utc) & format(utc, "%H:%M:%S") == "00:00:00"
  if (!any(dateonly)) return(f)
  for (g in unique(paste(f$key[dateonly], f$day[dateonly]))) {
    i <- dateonly & paste(f$key, f$day) == g
    k <- f$key[i][1]; d <- f$day[i][1]
    late <- as.POSIXct(paste(d, "23:59:59"), tz = "Europe/Zurich")
    snap <- s[s$day == d, ]
    if (nrow(snap)) {
      prior <- sum(f$Pos[f$key == k & f$day < d])
      held <- sum(snap$pos[snap$key == k])
      if (abs(prior + sum(f$Pos[i]) - held) < 1e-9) late <- min(snap$ts) - 1
    }
    f$ts[i] <- late
  }
  f
}

## Underlying price at a set of fills: the UnderlyingPrice stored on any row
## filled at the same time (a price typed in later may sit on one leg only),
## else the fill price of a stock / future leg filled at the same time.
.fill_underlying <- function(fl, all_fills, linear_keys) {
  same <- all_fills[all_fills$DateTime %in% fl$DateTime, ]
  s <- same$UnderlyingPrice[!is.na(same$UnderlyingPrice)]
  if (length(s)) return(s[length(s)])
  same <- same[same$key %in% linear_keys, ]
  if (nrow(same)) return(same$Price[nrow(same)])
  NA_real_
}

## One leg over one interval. `a` is the leg at the start snapshot; the end is
## either the leg at the next snapshot (`b`) or, when the leg was closed in the
## interval, its closing fills.
.leg_interval <- function(a, b, fl, all_fills, linear_keys, rate, div_yield) {
  q <- a$pos * a$multiplier
  cf <- sum(fl$Total, na.rm = TRUE)
  out <- list(delta = 0, gamma = 0, vega = 0, theta = 0, execution = 0, flag = "",
              S_end = NA_real_)
  if (!is.null(b)) {
    out$actual <- b$value - a$value + cf
    S_end <- b$S; iv_end <- b$IV; days <- as.numeric(b$day - a$day)
  } else if (nrow(fl) == 0) {
    ## The leg left the snapshots without a fill: a gap in the data.
    out$actual <- -a$value
    out$execution <- out$actual
    out$flag <- "gap"
    return(out)
  } else {
    out$actual <- -a$value + cf
    out$execution <- -sum(fl$Commission)
    S_end <- if (a$linear) fl$Price[nrow(fl)] else .fill_underlying(fl, all_fills, linear_keys)
    days <- as.numeric(max(fl$day) - a$day)
    if (is.na(S_end) || S_end <= 0) {
      out$execution <- out$actual
      out$flag <- "noS"
      return(out)
    }
    iv_end <- NA_real_
    if (!a$linear) {
      dte <- as.numeric(as.Date(as.character(a$expdate), "%Y%m%d") - max(fl$day))
      px <- fl$Price[nrow(fl)]
      if (dte > 0 && !is.na(px) && px > 0)
        iv_end <- tryCatch(Tbasics::getImpliedVolOpt(type = a$type, S = S_end, K = a$strike,
                                                     r = rate, DTE = dte, div = div_yield, price = px),
                           error = function(e) NA_real_)
    }
  }
  out$S_end <- S_end
  dS <- S_end - a$S
  if (abs(dS) / a$S > .split_guard) {
    out$flag <- "split"
    return(out)
  }
  if (a$linear) {
    out$delta <- q * dS
  } else {
    out$delta <- q * a$delta * dS
    out$gamma <- q * 0.5 * a$gamma * dS^2
    out$vega <- if (is.na(iv_end)) 0 else q * a$vega * (iv_end - a$IV) * 100
    out$theta <- q * a$theta * days
  }
  out$flag <- a$flag
  out
}

#' Split a trade's P&L into Greek components, day by day
#'
#' Pure function over the trade's snapshot rows and fills; see the file header
#' for the formulas.
#'
#' @param snaps Snapshot rows, as returned by \code{getTradeSnapshots()}
#' @param fills Trades rows, as returned by \code{getTradeFills()}
#' @param rate Interest rate (decimal) for recomputed Greeks and the exit IV
#' @param div_yield Dividend yield (decimal), same use
#' @param as_of Last date included (Date)
#' @return data.frame, one row per day: date, kind, S (underlying), dS,
#'   delta, gamma, vega, theta, residual, execution, actual, flag. kind is
#'   \code{entry} for the first row (fills against the first marks),
#'   \code{exit} for the last one when the trade was closed after its last
#'   snapshot (it can share its date with that snapshot), \code{day}
#'   otherwise. Flags: \code{BS} Greeks recomputed, \code{split}
#'   underlying move > 25\% (all to residual), \code{noS} no underlying price
#'   at the exit (all to execution), \code{gap} leg gone without a fill.
#'   NULL for a cash-only trade or fewer than two usable snapshot days.
#' @export
attributePnL <- function(snaps, fills, rate = 0, div_yield = 0, as_of = Sys.Date()) {
  if (all(snaps$type == "CASH")) return(NULL)
  ## A snapshot row can carry a stale TradeNr: keep the legs the trade's fills
  ## name. Legacy trades booked as one combo row name no leg; keep all then.
  leg_key <- ifelse(snaps$type %in% c("Stock", "CFD"), snaps$symbol, snaps$Instrument)
  named <- leg_key %in% c(fills$Instrument, fills$Symbol)
  if (any(named)) snaps <- snaps[named, ]
  ## Same for a closed trade: rows after its last fill belong to a later
  ## position in the same contract.
  if (nrow(fills) && all(abs(tapply(fills$Pos, fills$Instrument, sum)) < 1e-9))
    snaps <- snaps[snaps$date <= max(fills$TradeDate), ]
  s <- .prepare_snapshots(snaps, rate, div_yield)
  s <- s[s$day <= as_of, ]
  days <- sort(unique(s$day))
  if (length(days) < 2) return(NULL)

  f <- .prepare_fills(fills, unique(s$key))
  f <- f[f$day <= as_of, ]
  f <- .place_dateonly_fills(f, s)
  linear_keys <- unique(c(s$key[s$linear], f$key[is.na(f$Strike) & !(f$key %in% s$key[!s$linear])]))
  ts_of <- vapply(days, function(d) as.numeric(max(s$ts[s$day == d])), numeric(1))

  row_for <- function(day, kind, legs_res, entry = 0, S = NA_real_, dS = NA_real_) {
    sum_of <- function(n) sum(vapply(legs_res, `[[`, numeric(1), n))
    r <- data.frame(date = day, kind = kind, S = S, dS = dS,
                    delta = sum_of("delta"), gamma = sum_of("gamma"),
                    vega = sum_of("vega"), theta = sum_of("theta"),
                    execution = sum_of("execution") + entry,
                    actual = sum_of("actual") + entry)
    r$residual <- r$actual - r$delta - r$gamma - r$vega - r$theta - r$execution
    flags <- unique(unlist(lapply(legs_res, `[[`, "flag")))
    r$flag <- paste(flags[flags != ""], collapse = " ")
    r[c("date", "kind", "S", "dS", "delta", "gamma", "vega", "theta", "residual",
        "execution", "actual", "flag")]
  }
  ref_S <- function(legs) if (nrow(legs)) legs$S[which.max(!legs$linear)] else NA_real_

  ## Entry: fills up to the first kept snapshot, against its marks.
  first <- s[s$day == days[1], ]
  before <- f[f$ts <= ts_of[1], ]
  entry <- sum(first$value) + sum(before$Total, na.rm = TRUE)
  out <- list(row_for(days[1], "entry", list(), entry = entry, S = ref_S(first)))

  ## Intervals between kept snapshots, then the exit after the last one.
  ends <- c(ts_of[-1], Inf)
  for (k in seq_along(days)) {
    A <- s[s$day == days[k], ]
    B <- if (k < length(days)) s[s$day == days[k + 1], ] else NULL
    in_iv <- f[as.numeric(f$ts) > ts_of[k] & as.numeric(f$ts) <= ends[k], ]
    if (is.null(B)) {
      ## Only legs closed after the last snapshot form an exit interval.
      closed <- vapply(A$key, function(kk) {
        isTRUE(abs(A$pos[A$key == kk] + sum(in_iv$Pos[in_iv$key == kk])) < 1e-9)
      }, logical(1))
      if (!any(closed)) break
      A <- A[closed, ]
      in_iv <- in_iv[in_iv$key %in% A$key, ]
    }
    res <- lapply(seq_len(nrow(A)), function(i) {
      a <- A[i, ]
      b <- if (!is.null(B) && a$key %in% B$key) B[B$key == a$key, ] else NULL
      .leg_interval(a, b, in_iv[in_iv$key == a$key, ], f, linear_keys, rate, div_yield)
    })
    ## Legs opened in the interval: fill vs first mark is execution.
    if (!is.null(B)) {
      for (kk in setdiff(B$key, A$key)) {
        v <- B$value[B$key == kk] + sum(in_iv$Total[in_iv$key == kk], na.rm = TRUE)
        res[[length(res) + 1]] <- list(delta = 0, gamma = 0, vega = 0, theta = 0,
                                       execution = v, actual = v, flag = "")
      }
    }
    ## Legs opened and closed between two snapshots: all execution.
    flash <- setdiff(in_iv$key, c(A$key, if (!is.null(B)) B$key))
    for (kk in flash) {
      v <- sum(in_iv$Total[in_iv$key == kk], na.rm = TRUE)
      res[[length(res) + 1]] <- list(delta = 0, gamma = 0, vega = 0, theta = 0,
                                     execution = v, actual = v, flag = "")
    }
    end_day <- if (!is.null(B)) days[k + 1] else max(in_iv$day)
    S_end <- if (!is.null(B)) ref_S(B) else {
      se <- vapply(res, function(r) if (is.null(r$S_end)) NA_real_ else r$S_end, numeric(1))
      se[!is.na(se)][1]
    }
    out[[length(out) + 1]] <- row_for(end_day, if (is.null(B)) "exit" else "day", res,
                                      S = S_end, dS = S_end - ref_S(A))
  }
  do.call(rbind, out)
}

#' P&L attribution of one trade
#'
#' Reads the trade's fills and snapshots and calls \code{attributePnL()}.
#' Rate and dividend yield are today's values: they only feed recomputed
#' Greeks and the exit IV.
#'
#' @param trade_nr Trade number
#' @param account IBKR account of the trade (the snapshot table)
#' @param as_of Last date included (Date)
#' @return see \code{attributePnL()}
#' @export
getTradePnLAttribution <- function(trade_nr, account, as_of = Sys.Date()) {
  snaps <- getTradeSnapshots(account, trade_nr)
  if (nrow(snaps) == 0) return(NULL)
  fills <- getTradeFills(trade_nr, account)
  ccy <- snaps$currency[1]
  rate <- suppressWarnings(getLastRate(ccy))
  div <- suppressWarnings(getLastDivYield(snaps$symbol[1]))
  attributePnL(snaps, fills,
               rate = if (is.na(rate)) 0 else rate,
               div_yield = if (is.na(div)) 0 else div,
               as_of = as_of)
}

#' Underlying price at a past moment
#'
#' Last traded price from IBKR 1-minute bars (regular trading hours) at the
#' given UTC moment -- the time of a closing fill, stored in Trades.DateTime.
#' A date-only DateTime (00:00:00, migrated rows) means "that day": the day's
#' last bar is used, as with \code{day_close = TRUE}. Needs TWS; the caller
#' checks \code{isIBAvailable()} first.
#'
#' @param symbol Underlying symbol (a Tickers row, e.g. "GOOG", "SPX")
#' @param datetime_utc Character "YYYY-MM-DD HH:MM:SS" in UTC
#' @param day_close TRUE for the day's last price whatever the time -- an
#'   expiry row, whose 16:00:00 is New York time, not UTC
#' @return Numeric price, NA_real_ when IBKR returns no bar
#' @export
getUnderlyingPriceAt <- function(symbol, datetime_utc, day_close = FALSE) {
  stopifnot(length(symbol) == 1, length(datetime_utc) == 1)
  when <- if (day_close || endsWith(datetime_utc, "00:00:00"))
    paste(substr(datetime_utc, 1, 10), "23:59:00") else datetime_utc
  res <- tryCatch(get_tdata_py()$get_price_at(symbol, when), error = function(e) {
    logger::log_warn("getUnderlyingPriceAt({symbol}, {datetime_utc}): {conditionMessage(e)}", namespace = "Tdata")
    NULL
  })
  if (is.null(res)) return(NA_real_)
  logger::log_info("Underlying price {symbol} at {datetime_utc} UTC: {res$price} (bar {res$bar_time})", namespace = "Tdata")
  res$price
}
