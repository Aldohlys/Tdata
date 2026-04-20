#' Get next earnings date for a symbol
#'
#' Queries Wall Street Horizon via IBKR. Returns NA for non-NA tickers
#' (WSH free tier covers North America only) or when WSH has no data.
#'
#' @param name Ticker symbol (e.g. "AAPL")
#' @return Character "YYYYMMDD" or NA_character_
#' @export
#' @examples
#' \dontrun{
#' getNextEarningsDate("AAPL")
#' }
getNextEarningsDate <- function(name) {
  py <- tdata_py
  if (is.null(py)) {
    logger::log_error("tdata_py module not available", namespace = "Tdata")
    return(NA_character_)
  }

  result <- tryCatch(
    py$getNextEarningsDate(symbol = name),
    error = function(e) {
      logger::log_warn(
        paste("getNextEarningsDate failed for", name, ":", e$message),
        namespace = "Tdata")
      NULL
    })

  if (is.null(result) || length(result) == 0) return(NA_character_)
  as.character(result)
}


#' Batch-update NextEarnings column in Tickers table
#'
#' Sequential calls to WSH (API constraint — no parallelism).
#' Always updates EarningsLastUpdate; NextEarnings may be NA for non-NA tickers.
#'
#' @param names Character vector of ticker symbols
#' @return Integer: number of rows updated
#' @export
updateEarnings <- function(names) {
  if (length(names) == 0) {
    message("No tickers provided")
    return(0L)
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  today <- format(Sys.Date(), "%Y%m%d")
  updated <- 0L

  for (n in names) {
    next_date <- getNextEarningsDate(n)

    result <- DBI::dbExecute(conn,
      "UPDATE Tickers SET NextEarnings = ?, EarningsLastUpdate = ? WHERE Name = ?",
      params = list(
        if (is.na(next_date)) NA_character_ else next_date,
        today,
        n))

    if (result > 0) {
      updated <- updated + 1L
      message(sprintf("  %-8s -> %s",
                      n,
                      if (is.na(next_date)) "(no data)" else next_date))
    } else {
      message(sprintf("  %-8s -> SKIPPED (ticker not in Tickers table)", n))
    }
  }

  message(sprintf("Earnings updated for %d / %d tickers", updated, length(names)))
  invisible(updated)
}


#' Refresh earnings dates only where stale
#'
#' Refresh rules (covers all IV='YES', Type='STK' rows):
#'   - NextEarnings is past today → always retry (date rolled over)
#'   - NextEarnings IS NULL (never had data) → retry only if EarningsLastUpdate
#'     is older than 7 days. Avoids re-hitting yfinance daily for ETFs and
#'     non-US tickers with broken YahooName — they're permanently NA.
#'
#' @return Integer: number of rows updated
#' @export
updateStaleEarnings <- function() {
  conn <- safe_db_connect()
  today         <- format(Sys.Date(), "%Y%m%d")
  seven_days_ago <- format(Sys.Date() - 7, "%Y%m%d")

  stale <- DBI::dbGetQuery(conn,
    "SELECT Name FROM Tickers
     WHERE IV = 'YES' AND Type = 'STK'
       AND (
         (NextEarnings IS NOT NULL AND NextEarnings < ?)
         OR
         (NextEarnings IS NULL
          AND (EarningsLastUpdate IS NULL OR EarningsLastUpdate < ?))
       )
     ORDER BY Name",
    params = list(today, seven_days_ago))

  DBI::dbDisconnect(conn)

  if (nrow(stale) == 0) {
    message("Earnings dates: all current, nothing to refresh")
    return(0L)
  }

  message(sprintf("Refreshing earnings for %d stale tickers:", nrow(stale)))
  updateEarnings(stale$Name)
}
