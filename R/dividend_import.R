#### Import of IBKR cash dividends into the Trades table ####################
##
## TWS exposes no cash ledger -- the dividend tick (456) only gives announced
## per-share amounts, not what was credited to the account. What was actually
## paid comes from IBKR statements: the "Dividends" and "Withholding Tax"
## sections of an Activity Statement CSV (single account or MULTI).
##
## Each payment becomes ONE Trades row, net of withholding tax, booked on the
## trade that holds the stock: EventType "Dividend", Pos 0, Total = net amount
## in the dividend currency, TradeDate = pay date. Accrued-but-unpaid dividends
## (the statement's "Change in Dividend Accruals") are not booked: a dividend
## enters Trades when IBKR pays it.
##
## A Pos-0 row leaves positions, cost basis and Risk untouched; realized P&L
## picks it up everywhere it is computed from the trade's cash (RReporting
## compute_open_realized, the Tuser All view, closed-trade PnL = sum(Total)).

## Parses the dividend payments of an IBKR Activity Statement CSV.
## Returns one row per payment: account, currency, date (Date), symbol, desc,
## rate, gross, tax, net. Reversals inside the statement net out per payment.
ibkr_statement_dividends <- function(file) {
  empty <- data.frame(account = character(), currency = character(),
                      date = as.Date(character()), symbol = character(),
                      desc = character(), rate = numeric(), gross = numeric(),
                      tax = numeric(), net = numeric(), stringsAsFactors = FALSE)
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  lines <- sub("^\ufeff", "", lines)
  keep  <- grepl("^(Account Information,Data,Account,|Dividends,Data,|Withholding Tax,Data,|Payment In Lieu Of Dividends,Data,)", lines)
  if (!any(keep)) return(empty)

  ### Sections follow the account they belong to: a MULTI statement repeats the
  ### whole layout once per account, each opened by its Account Information.
  account <- NA_character_
  rows <- list()
  for (ln in lines[keep]) {
    f <- utils::read.csv(text = ln, header = FALSE, stringsAsFactors = FALSE,
                         colClasses = "character")
    if (f$V1 == "Account Information") { account <- f$V4; next }
    ccy <- f$V3
    if (!grepl("^[A-Z]{3}$", ccy)) next                  # Total / Total in CHF lines
    amount <- as.numeric(gsub(",", "", f$V6))
    kind <- if (f$V1 == "Withholding Tax") "tax" else "gross"
    rows[[length(rows) + 1]] <- data.frame(
      account = account, currency = ccy, date = as.Date(f$V4),
      ### A dividend line ends "(Ordinary Dividend)" / "(Bonus Dividend)", its
      ### tax line " - FR Tax": strip both so the two pair up on one key.
      desc = trimws(sub(" \\([A-Za-z ]*Dividend\\)$", "", sub(" - [A-Z]{2} Tax$", "", f$V5))),
      kind = kind,
      amount = amount, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(empty)
  x <- do.call(rbind, rows)

  key <- paste(x$account, x$currency, x$date, x$desc, sep = "|")
  out <- lapply(split(seq_len(nrow(x)), key), function(i) {
    d <- x[i, , drop = FALSE]
    gross <- sum(d$amount[d$kind == "gross"])
    tax   <- sum(d$amount[d$kind == "tax"])
    desc  <- d$desc[1]
    data.frame(account = d$account[1], currency = d$currency[1], date = d$date[1],
               symbol = sub("\\.[A-Z]+$", "", sub("\\(.*$", "", desc)),
               desc = desc,
               rate = suppressWarnings(as.numeric(sub(".* ([0-9.]+) per Share.*", "\\1", desc))),
               gross = round(gross, 2), tax = round(tax, 2),
               net = round(gross + tax, 2), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out <- out[abs(out$gross) > 0.005, , drop = FALSE]   # fully reversed payments
  rownames(out) <- NULL
  out[order(out$date, out$account), , drop = FALSE]
}

## The trade holding `symbol` on `date`: the TradeNr whose stock legs dated on
## or before `date` leave a positive position. The paying account's trades are
## preferred; a stock moved between accounts (CRST, U1804173 -> U25343478) is
## found in the other one. Returns a one-row data.frame of that trade's last
## stock leg, or NULL.
dividend_target_trade <- function(trades, symbol, date, account) {
  d <- as.integer(format(date, "%Y%m%d"))
  t <- trades[!is.na(trades$Symbol) & trades$Symbol == symbol &
              (is.na(trades$EventType) | trades$EventType != "Dividend") &
              trades$TradeDate <= d, , drop = FALSE]
  if (nrow(t) == 0) return(NULL)
  held <- stats::aggregate(t$Pos, by = list(TradeNr = t$TradeNr), FUN = sum)
  held <- held[held$x > 0, , drop = FALSE]
  if (nrow(held) == 0) return(NULL)
  cand <- t[t$TradeNr %in% held$TradeNr, , drop = FALSE]
  own  <- cand[cand$Account == account, , drop = FALSE]
  if (nrow(own) > 0) cand <- own
  cand <- cand[order(cand$TradeDate, cand$DateTime), , drop = FALSE]
  last_nr <- cand$TradeNr[nrow(cand)]
  leg <- cand[cand$TradeNr == last_nr, , drop = FALSE]
  leg[nrow(leg), , drop = FALSE]
}

#'   importIBKRDividends
#'
#' Books the cash dividends of an IBKR Activity Statement CSV into the Trades
#' table: one row per payment, net of withholding tax, on the trade holding the
#' stock (EventType "Dividend", Pos 0, Total = net, TradeDate = pay date).
#' Works on single-account and MULTI statements. Running it again books nothing
#' twice: a payment already present (same TradeNr, date, currency and net) is
#' skipped. Unpaid accruals are ignored; they are booked once paid.
#'
#' @param file path to the Activity Statement CSV.
#' @param apply FALSE (default) only reports what would be booked.
#' @returns data.frame of the payments with the trade matched and an `action`
#'   column: "insert", "exists" or "no trade".
#' @export
#' @examples
#' \dontrun{
#' importIBKRDividends("MULTI_20260101_20260921.csv")          # dry run
#' importIBKRDividends("MULTI_20260101_20260921.csv", apply = TRUE)
#' }
importIBKRDividends <- function(file, apply = FALSE) {
  divs <- ibkr_statement_dividends(file)
  if (nrow(divs) == 0) {
    logger::log_info("No dividend payments in {file}", namespace = "Tdata")
    return(invisible(divs))
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  trades <- DBI::dbGetQuery(conn, "SELECT * FROM Trades")

  divs$TradeNr <- NA_integer_; divs$booked_account <- NA_character_; divs$action <- NA_character_
  new_rows <- list()
  for (i in seq_len(nrow(divs))) {
    p   <- divs[i, ]
    leg <- dividend_target_trade(trades, p$symbol, p$date, p$account)
    if (is.null(leg)) {
      divs$action[i] <- "no trade"
      logger::log_warn("Dividend {p$desc} of {p$date} ({p$account}): no open trade holds {p$symbol}",
                       namespace = "Tdata")
      next
    }
    divs$TradeNr[i] <- leg$TradeNr; divs$booked_account[i] <- leg$Account
    d <- as.integer(format(p$date, "%Y%m%d"))
    dup <- trades[!is.na(trades$EventType) & trades$EventType == "Dividend" &
                  trades$TradeNr == leg$TradeNr & trades$TradeDate == d &
                  trades$Currency == p$currency & abs(trades$Total - p$net) < 0.005, , drop = FALSE]
    if (nrow(dup) > 0) { divs$action[i] <- "exists"; next }
    divs$action[i] <- "insert"

    note <- sprintf("Dividend %s: gross %.2f, tax %.2f %s%s", p$desc, p$gross, p$tax, p$currency,
                    if (p$account != leg$Account) paste0(", paid into ", p$account) else "")
    new_rows[[length(new_rows) + 1]] <- data.frame(
      TradeNr = leg$TradeNr, Account = leg$Account, TradeDate = d,
      DateTime = paste(format(p$date, "%Y-%m-%d"), "00:00:00"),
      TimeZoneSource = leg$TimeZoneSource, Strategy = leg$Strategy,
      Instrument = leg$Instrument, Symbol = leg$Symbol, Pos = 0L,
      Price = if (is.na(p$rate)) NA_real_ else p$rate, Commission = 0,
      Total = p$net, Risk = 0, Reward = 0, PnL = 0, Status = leg$Status,
      Currency = p$currency, Notes = note, EventType = "Dividend",
      stringsAsFactors = FALSE)
  }

  if (apply && length(new_rows) > 0) {
    ins <- do.call(rbind, new_rows)
    DBI::dbAppendTable(conn, "Trades", ins)
    logger::log_info("Booked {nrow(ins)} dividend row(s) into Trades", namespace = "Tdata")
  }
  divs
}
