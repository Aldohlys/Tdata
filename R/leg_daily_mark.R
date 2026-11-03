## End-of-day marks that complete a trade's P&L path on days without a portfolio
## snapshot, stored in LegDailyMark so that each is fetched or solved only once.
##
## An underlying or stock close comes from IBKR daily bars, else Yahoo. For an
## option, in order: the mid of IBKR's last hourly bar of the session, else a
## model mark, Black-Scholes on the underlying close at the IV interpolated
## between the leg's real marks (snapshots, bars, fills). Hold-out on 1,178 equity
## option leg-days: median error 14 per leg against a median move of 46.
##
## IBKR holds no daily option bars, hourly quotes for about 38 sessions, and
## nothing on an expired contract, which is why open legs are collected daily
## (scripts/collect_leg_marks.R).

.ibkr_quote_days <- 60          ## calendar days of option quotes IBKR still holds
.iv_bounds <- c(0.01, 5)        ## an IV solved outside this is a bad input, not a mark
.leg_regex <- "^(.+) ([0-9]{2})([A-Z]{3})([0-9]{2}) ([0-9.]+) ([CP])$"

.ymd <- function(d) as.integer(format(d, "%Y%m%d"))

#' Stored daily marks
#'
#' @param legs Option Instruments and / or symbols
#' @param from,to First and last date (Date)
#' @return data.frame: leg, date (Date), close, iv, source
#' @export
readLegMarks <- function(legs, from, to) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  sql <- sprintf("SELECT leg, date, close, iv, source FROM LegDailyMark
                   WHERE leg IN (%s) AND date BETWEEN ? AND ?",
                 paste(rep("?", length(legs)), collapse = ", "))
  res <- DBI::dbGetQuery(conn, sql, params = c(as.list(legs), .ymd(from), .ymd(to)))
  res$date <- as.Date(as.character(res$date), "%Y%m%d")
  res
}

#' Store daily marks
#'
#' An IBKR bar replaces whatever is there; a Yahoo close or a model mark never
#' replaces an existing row.
#'
#' @param marks data.frame: leg, date (Date), close, iv, source
#' @return Number of rows offered, invisibly
#' @export
writeLegMarks <- function(marks) {
  if (nrow(marks) == 0) return(invisible(0L))
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 30000")
  params <- function(m) list(m$leg, .ymd(m$date), m$close, m$iv, m$source)
  bar <- marks$source == "bar"
  sql <- "%s INTO LegDailyMark (leg, date, close, iv, source) VALUES (?, ?, ?, ?, ?)"
  if (any(bar)) DBI::dbExecute(conn, sprintf(sql, "INSERT OR REPLACE"), params = params(marks[bar, ]))
  if (any(!bar)) DBI::dbExecute(conn, sprintf(sql, "INSERT OR IGNORE"), params = params(marks[!bar, ]))
  invisible(nrow(marks))
}

## The last session whose close is known: today once New York is past 16:15.
.last_complete_session <- function(now = Sys.time()) {
  ny <- as.POSIXlt(now, tz = "America/New_York")
  d <- as.Date(format(now, "%Y-%m-%d", tz = "America/New_York"))
  if (ny$hour * 60 + ny$min < 16 * 60 + 15) d <- d - 1
  while (as.POSIXlt(d)$wday %in% c(0, 6)) d <- d - 1
  d
}

## Tickers rows of a trade's symbols. A leg must have one to be completed.
.leg_tickers <- function(symbols) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, sprintf(
    "SELECT Name, Type, Currency, Multiplier, OptExchange FROM Tickers WHERE Name IN (%s)",
    paste(rep("?", length(symbols)), collapse = ", ")), params = as.list(symbols))
}

## "18", "DEC", "26" -> Date. Month by hand: %b follows the locale.
.leg_expiry <- function(dd, mon, yy) {
  as.Date(sprintf("20%s-%02d-%s", yy, match(mon, toupper(month.abb)), dd))
}

## One row per leg the fills name: key (as in attributePnL), symbol, type, strike,
## expiry (Date, NA for stock), multiplier, currency. NULL when the trade cannot be
## completed: a legacy combo row, a futures leg, a cash trade, a symbol not in Tickers.
.trade_legs <- function(snaps, fills, tickers) {
  opt <- .is_option_fill(fills)
  if (any(opt & !grepl(.leg_regex, fills$Instrument))) return(NULL)
  if (any(snaps$type %in% c("Future", "TreasuryBill", "CASH"))) return(NULL)
  symbols <- unique(fills$Symbol)
  if (!all(symbols %in% tickers$Name)) return(NULL)
  if (!all(tickers$Type[match(symbols, tickers$Name)] %in% c("STK", "IND"))) return(NULL)

  stock <- unique(fills$Symbol[!opt])
  if (!all(tickers$Type[match(stock, tickers$Name)] %in% "STK")) return(NULL)
  rows <- lapply(stock, function(sym) {
    tk <- tickers[tickers$Name == sym, ]
    sn <- snaps[snaps$symbol == sym & snaps$type %in% c("Stock", "CFD"), ]
    data.frame(key = sym, symbol = sym, type = if (nrow(sn)) sn$type[1] else "Stock",
               strike = NA_real_, expiry = as.Date(NA), multiplier = 1,
               currency = tk$Currency[1], stringsAsFactors = FALSE)
  })
  for (inst in unique(fills$Instrument[opt])) {
    m <- regmatches(inst, regexec(.leg_regex, inst))[[1]]
    sym <- fills$Symbol[fills$Instrument == inst][1]
    tk <- tickers[tickers$Name == sym, ]
    sn <- snaps[snaps$Instrument == inst, ]
    mult <- if (nrow(sn)) sn$multiplier[1] else tk$Multiplier[1]
    rows[[length(rows) + 1]] <- data.frame(
      key = inst, symbol = sym, type = if (m[7] == "C") "Call" else "Put",
      strike = as.numeric(m[6]), expiry = .leg_expiry(m[3], m[4], m[5]),
      multiplier = if (is.na(mult)) 100 else as.numeric(mult),
      currency = tk$Currency[1], stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

## Option mark -> IV, NA when it cannot be solved or is implausible.
.solve_iv <- function(type, S, K, dte, price, rate, div_yield) {
  if (is.na(S) || S <= 0 || is.na(price) || price <= 0 || dte <= 0) return(NA_real_)
  iv <- tryCatch(suppressWarnings(
    Tbasics::getImpliedVolOpt(type = type, S = S, K = K, r = rate, DTE = dte,
                              div = div_yield, price = price)),
    error = function(e) NA_real_)
  if (length(iv) != 1 || is.na(iv) || iv < .iv_bounds[1] || iv > .iv_bounds[2]) NA_real_ else iv
}

.bs_price <- function(type, S, K, dte, iv, rate, div_yield) {
  if (dte <= 0) return(if (type == "Call") max(S - K, 0) else max(K - S, 0))
  fn <- if (type == "Call") Tbasics::getBSCallPrice else Tbasics::getBSPutPrice
  fn(S = S, K = K, r = rate, DTE = dte, sig = iv, div = div_yield)[1]
}

## Sessions on which a leg is held at the close, from the fills: its position
## summed up to that day, and not past expiry.
.held_sessions <- function(leg, fills, sessions) {
  f <- fills[.fill_keys(fills) == leg$key, ]
  days <- as.Date(as.character(f$TradeDate), "%Y%m%d")
  held <- vapply(sessions, function(d) abs(sum(f$Pos[days <= d])) > 1e-9, logical(1))
  if (!is.na(leg$expiry)) held <- held & sessions <= leg$expiry
  sessions[held]
}

## Closes against the prices the trade already records: fills of the stock and the
## snapshots' underlying. IBKR and Yahoo adjust history for splits and quote some
## exchanges in another unit (LSE pence); snapshots and fills do neither.
.closes_consistent <- function(closes, s, fills, guard = .split_guard) {
  check <- function(sym, d, px) {
    cl <- closes$close[closes$leg == sym & closes$date == d]
    length(cl) == 0 || is.na(px) || px <= 0 || abs(cl[1] / px - 1) <= guard
  }
  for (i in seq_len(nrow(s)))
    if (!check(s$symbol[i], s$day[i], s$S[i])) return(FALSE)
  f <- fills[!.is_option_fill(fills), ]
  for (i in seq_len(nrow(f)))
    if (!check(f$Symbol[i], as.Date(as.character(f$TradeDate[i]), "%Y%m%d"), f$Price[i])) return(FALSE)
  TRUE
}

## Model marks of one option leg on `days`, at the IV interpolated between its real
## marks and held flat beyond the first and the last. Real marks are its bars, its
## snapshots, and its fills against the underlying of that day. `bracketed`: real
## marks on both sides, so the interpolation will not change and the row can be kept.
.model_marks <- function(leg, days, bars, closes, s, fills, rate, div_yield) {
  if (length(days) == 0) return(NULL)
  dte_of <- function(d) as.numeric(leg$expiry - d)
  S_of <- function(d) {
    x <- closes$close[closes$leg == leg$symbol & closes$date == d]
    if (length(x)) x[1] else NA_real_
  }
  sn <- s[s$key == leg$key, ]
  sn_iv <- vapply(seq_len(nrow(sn)), function(i)
    .solve_iv(leg$type, sn$uPrice[i], leg$strike, dte_of(sn$day[i]), sn$mktPrice[i], rate, div_yield),
    numeric(1))
  f <- fills[.fill_keys(fills) == leg$key, ]
  f_days <- as.Date(as.character(f$TradeDate), "%Y%m%d")
  f_iv <- vapply(seq_len(nrow(f)), function(i) {
    S <- if (!is.na(f$UnderlyingPrice[i])) f$UnderlyingPrice[i] else S_of(f_days[i])
    .solve_iv(leg$type, S, leg$strike, dte_of(f_days[i]), f$Price[i], rate, div_yield)
  }, numeric(1))
  ## A snapshot or a bar outranks a fill on the same day: later rows win.
  a <- data.frame(day = c(f_days, sn$day, bars$date), iv = c(f_iv, sn_iv, bars$iv))
  a <- a[!is.na(a$iv), ]
  if (nrow(a) == 0) return(NULL)
  a <- a[!duplicated(a$day, fromLast = TRUE), ]
  a <- a[order(a$day), ]
  iv <- if (nrow(a) == 1) rep(a$iv, length(days))
        else stats::approx(as.numeric(a$day), a$iv, xout = as.numeric(days), rule = 2)$y
  S <- vapply(seq_along(days), function(i) S_of(days[i]), numeric(1))
  keep <- !is.na(S)
  if (!any(keep)) return(NULL)
  days <- days[keep]; iv <- iv[keep]; S <- S[keep]
  close <- vapply(seq_along(days), function(i)
    .bs_price(leg$type, S[i], leg$strike, dte_of(days[i]), iv[i], rate, div_yield), numeric(1))
  data.frame(leg = leg$key, date = days, close = close, iv = iv, source = "model",
             bracketed = days > min(a$day) & days < max(a$day), stringsAsFactors = FALSE)
}

## Closes of a symbol over [from, to]: stored, then IBKR, then Yahoo for what is
## missing. Missing means past the stored range or a hole of more than five days (a
## holiday leaves a shorter one). What is stored is returned when both sources fail.
.underlying_closes <- function(symbol, from, to, ib_ok) {
  stored <- readLegMarks(symbol, from, to)
  gaps <- diff(sort(c(from - 1, stored$date, to + 1)))
  if (nrow(stored)) {
    after <- seq(max(stored$date), to, by = "day")[-1]
    if (!any(!as.POSIXlt(after)$wday %in% c(0, 6)) && all(gaps <= 5)) return(stored)
  }

  got <- NULL
  if (ib_ok()) {
    days <- as.numeric(Sys.Date() - from) + 5
    duration <- if (days <= 360) paste(days, "D") else paste(ceiling(days / 365), "Y")
    bars <- tryCatch(get_tdata_py()$get_historical_bars(sym = symbol, duration = duration, bar_size = "1 day"),
                     error = function(e) NULL)
    if (is.data.frame(bars) && nrow(bars))
      got <- data.frame(leg = symbol, date = as.Date(substr(as.character(bars$datetime), 1, 10)),
                        close = bars$close, iv = NA_real_, source = "bar", stringsAsFactors = FALSE)
  }
  if (is.null(got)) {
    yh <- tryCatch(getYahooData(getYahooName(symbol), from_date = from, to_date = to + 1),
                   error = function(e) NULL)
    if (is.data.frame(yh) && nrow(yh))
      got <- data.frame(leg = symbol, date = as.Date(yh$date), close = yh$Close, iv = NA_real_,
                        source = "yahoo", stringsAsFactors = FALSE)
  }
  if (is.null(got)) return(stored)
  got <- got[got$date >= from & got$date <= to & !is.na(got$close) & got$close > 0, ]
  writeLegMarks(got)
  readLegMarks(symbol, from, to)
}

## IBKR mids of an option leg with their IV, stored. Empty when IBKR has none.
.option_bars <- function(leg, from, to, closes, opt_exchange, rate, div_yield) {
  res <- tryCatch(get_tdata_py()$get_option_daily_mids(
    sym = leg$symbol, expiration = format(leg$expiry, "%Y%m%d"), strike = leg$strike,
    right = substr(leg$type, 1, 1), currency = leg$currency,
    exchange = if (is.na(opt_exchange) || !nzchar(opt_exchange)) "SMART" else opt_exchange,
    days = .ibkr_quote_days), error = function(e) NULL)
  empty <- data.frame(leg = character(), date = as.Date(character()), close = numeric(),
                      iv = numeric(), source = character(), stringsAsFactors = FALSE)
  if (is.null(res)) return(empty)
  m <- data.frame(leg = leg$key, date = as.Date(unlist(res$date)), close = unlist(res$close),
                  source = "bar", stringsAsFactors = FALSE)
  m <- m[m$date >= from & m$date <= to, ]
  if (nrow(m) == 0) return(empty)
  m$iv <- vapply(seq_len(nrow(m)), function(i) {
    S <- closes$close[closes$leg == leg$symbol & closes$date == m$date[i]]
    .solve_iv(leg$type, if (length(S)) S[1] else NA_real_, leg$strike,
              as.numeric(leg$expiry - m$date[i]), m$close[i], rate, div_yield)
  }, numeric(1))
  m <- m[c("leg", "date", "close", "iv", "source")]
  writeLegMarks(m)
  m
}

#' Daily marks of a trade, completing its snapshots
#'
#' Returns what is stored and fetches or computes only what is missing, so a trade
#' seen again costs one query per symbol. Touches TWS only when a close is missing or
#' an unexpired option leg has a missing day IBKR can still answer.
#'
#' @param snaps Snapshot rows, as returned by \code{getTradeSnapshots()}
#' @param fills Trades rows, as returned by \code{getTradeFills()}
#' @param rate Interest rate (decimal)
#' @param div_yield Dividend yield (decimal)
#' @param as_of Last date included (Date)
#' @return list(legs, marks) for \code{attributePnL(daily = )}. NULL when the trade
#'   cannot be completed (a legacy combo row, a futures or cash leg, a symbol not in
#'   Tickers) or its closes disagree with its recorded prices (a split, another
#'   quoting unit): its path then stays on the snapshots
#' @export
getTradeDailyMarks <- function(snaps, fills, rate = 0, div_yield = 0, as_of = Sys.Date()) {
  if (nrow(fills) == 0) return(NULL)
  tickers <- .leg_tickers(unique(fills$Symbol))
  legs <- .trade_legs(snaps, fills, tickers)
  if (is.null(legs)) return(NULL)

  fill_days <- as.Date(as.character(fills$TradeDate), "%Y%m%d")
  first <- min(fill_days)
  closed <- all(abs(tapply(fills$Pos, fills$Instrument, sum)) < 1e-9)
  last <- if (closed) max(fill_days) else min(as_of, .last_complete_session())
  if (last <= first) return(NULL)

  ib_state <- NULL
  ib_ok <- function() {
    if (is.null(ib_state)) ib_state <<- isTRUE(tryCatch(isIBAvailable(), error = function(e) FALSE))
    ib_state
  }

  closes <- do.call(rbind, lapply(unique(legs$symbol), .underlying_closes,
                                  from = first, to = last, ib_ok = ib_ok))
  if (nrow(closes) == 0) return(NULL)
  s <- .trade_snapshots(snaps, fills, rate, div_yield)
  s <- s[s$day <= as_of, ]
  if (!.closes_consistent(closes, s, fills)) {
    logger::log_warn("Closes disagree with recorded prices of {paste(unique(legs$symbol), collapse = ', ')}: path stays on its snapshots",
                     namespace = "Tdata")
    return(NULL)
  }
  sessions <- sort(unique(closes$date))
  options <- legs[!is.na(legs$strike), ]
  if (nrow(options) == 0) return(list(legs = legs, marks = closes))

  all_marks <- list(closes)
  stored_all <- readLegMarks(options$key, first, last)
  window_start <- Sys.Date() - .ibkr_quote_days
  for (i in seq_len(nrow(options))) {
    leg <- options[i, ]
    days <- .held_sessions(leg, fills, sessions)
    stored <- stored_all[stored_all$leg == leg$key, ]
    missing <- days[!days %in% stored$date]
    fetched <- FALSE
    if (leg$expiry >= Sys.Date() && any(missing >= window_start) && ib_ok()) {
      bars <- .option_bars(leg, min(missing), last, closes,
                           tickers$OptExchange[tickers$Name == leg$symbol][1], rate, div_yield)
      fetched <- TRUE
      stored <- rbind(stored[!stored$date %in% bars$date, ], bars)
      missing <- days[!days %in% stored$date]
    }
    m <- .model_marks(leg, missing, stored[stored$source == "bar", ], closes, s, fills, rate, div_yield)
    if (!is.null(m)) {
      ## A day IBKR can still answer waits for it, unless it was just asked. An open
      ## leg's last days move with its next real mark: kept once bracketed.
      keep <- (m$bracketed | closed) & (fetched | leg$expiry < Sys.Date() | m$date < window_start)
      writeLegMarks(m[keep, c("leg", "date", "close", "iv", "source")])
      stored <- rbind(stored, m[c("leg", "date", "close", "iv", "source")])
    }
    ## A bar whose IV could not be solved takes the interpolated one for its Greeks.
    no_iv <- is.na(stored$iv)
    if (any(no_iv) && any(!no_iv))
      stored$iv[no_iv] <- if (sum(!no_iv) == 1) stored$iv[!no_iv]
                          else stats::approx(as.numeric(stored$date[!no_iv]), stored$iv[!no_iv],
                                             xout = as.numeric(stored$date[no_iv]), rule = 2)$y
    all_marks[[length(all_marks) + 1]] <- stored
  }
  list(legs = legs, marks = do.call(rbind, all_marks))
}

#' Store the marks of every open option trade
#'
#' Runs each open trade's path, which stores what is missing. IBKR's quote history
#' is about 38 sessions, so a run after a long gap still catches up.
#'
#' @return data.frame of the trades visited: TradeNr, Account, ok
#' @export
collectLegMarks <- function() {
  conn <- safe_db_connect()
  open <- tryCatch(DBI::dbGetQuery(conn,
    "SELECT TradeNr, Account, Instrument, SUM(Pos) AS net FROM Trades
      WHERE Strike IS NOT NULL AND Strike <> '' AND TradeNr IS NOT NULL
      GROUP BY TradeNr, Account, Instrument HAVING ABS(net) > 1e-9"),
    finally = DBI::dbDisconnect(conn))
  open <- open[grepl("^D?U[0-9]+$", open$Account) & grepl(.leg_regex, open$Instrument), ]
  m <- regmatches(open$Instrument, regexec(.leg_regex, open$Instrument))
  expiry <- as.Date(vapply(m, function(x) format(.leg_expiry(x[3], x[4], x[5])), character(1)))
  trades <- unique(open[expiry >= Sys.Date(), c("TradeNr", "Account")])
  trades$ok <- vapply(seq_len(nrow(trades)), function(i) {
    !inherits(tryCatch(getTradePnLAttribution(trades$TradeNr[i], trades$Account[i]),
                       error = function(e) {
                         logger::log_error("Leg marks of trade {trades$TradeNr[i]}: {conditionMessage(e)}", namespace = "Tdata")
                         e
                       }), "error")
  }, logical(1))
  trades
}
