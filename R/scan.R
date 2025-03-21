
#####  scan.R
##### All utilities related to scanned tickers

#' getTickers
#'
#' This function is used by scanner functions to get all tickets to scan
#'
#'@return A data frame with columns \code{Name, YahooName, TradingClass, Multiplier, Type, Currency,  Exchange, OptExchange}
#' sorted by alphabetical order, equal to the list of current tickers stored in DB
#'@export
getTickers = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  tickers = DBI::dbReadTable(conn, "Tickers")
  DBI::dbDisconnect(conn)

  return(dplyr::arrange(tickers, tickers$Name))
}

#' addTicker
#'
#' This function is used by scanner functions to add one ticker in DB
#'
#'@param name IBKR security name
#'@param yahoo_name Yahoo finances download security name.  If not provided, will be equal to \code{name}.
#'@param trading_class chain trading class that will be used to retrieve option data from IBKR. If not provided, will be equal to \code{name}.
#'@param multiplier chain multiplier, default is 100
#'@param type IBKR security type, default is STK (stock). Could be also FUT (future), IND (Index),...
#'@param currency IBKR currency value, i.e. USD, EUR, CHF,... Default is USD
#'@param exchange exchange name where security price can be retrieved, default is SMART
#'@param opt_exchange exchange name where option price for the security can be retrieved, default is SMART
#'@return number of records added, i.e. 1 (normal case) or 0 (no line deleted, error case)
#'@export
addTicker <- function(name, yahoo_name, trading_class, multiplier = 100, type="STK", currency="USD", exchange="SMART", opt_exchange="SMART") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  if (missing(trading_class)) trading_class=name
  if (missing(yahoo_name)) yahoo_name=name
  beta <- calculate_beta_vs_spx_periods(yahoo_name)
  result <- DBI::dbAppendTable(conn, "Tickers", data.frame(Name = name, YahooName = yahoo_name, Type = type,
                                              Currency = currency, TradingClass = trading_class, Multiplier = multiplier,
                                              Exchange = exchange, OptExchange = opt_exchange,
                                              Beta_3m = beta$beta_3m, Beta_6m = beta$beta_6m,
                                              Beta_1y = beta$beta_1y, Beta_3y = beta$beta_3y))
  # Check how many row were added
  print(paste("Rows added:", result))

  DBI::dbDisconnect(conn)
  return(result)
}

#' getTicker
#'
#' This function is used by scanner functions to get one specific ticker defined by name from DB.
#'
#' If no name is retrieved a data frame with columns
#' \code{Name, YahooName, Type, Currency, TradingClass, Multiplier, Exchange, OptExchange} and 0 line is returned
#'
#' N.B. It could be that the same ticker name is used for different securities - I assume it is not the case in my scans
#'
#'@return A data frame with one line and columns
#' \code{Name, YahooName, Type, Currency, TradingClass, Multiplier, Exchange, OptExchange}
getTicker <- function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  ticker = DBI::dbGetQuery(conn,
                                 "Select * from Tickers WHERE Name = ?",
                                 params=list(name))
  DBI::dbDisconnect(conn)
  return(ticker)
}

#' removeTicker
#'
#' This function is used by scanner functions to remove one specific ticker defined by name in the DB.
#'
#' If no name is retrieved then "Row deleted: 0" will be displayed, and 0 is returned
#'
#' N.B. It could be that the same ticker name is used for different securities - I assume it is not the case in my scans
#'
#'@return number of records deleted, i.e. 1 (normal case) or 0 (no line deleted, error case)
#'@export
removeTicker = function(ticker) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  # Delete the row where Name equals ticker
  # Delete using parameterized query
  result <- DBI::dbExecute(conn,
            "DELETE FROM Tickers WHERE Name = ?",
            params = list(ticker))

  # Check how many rows were affected
  # This shows how many rows were deleted
  print(paste("Rows deleted:", result))
  DBI::dbDisconnect(conn)
  return(result)
}

