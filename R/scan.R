
#####  scan.R
##### All utilities related to scanned tickers

#' getAllTickers
#'
#' This function is used by scanner functions to get all tickers to scan
#'
#'@return A data frame with columns \code{Name, YahooName, TradingClass, Multiplier, Type, Currency,  Exchange, OptExchange}
#' sorted by alphabetical order, equal to the list of current tickers stored in DB
#'@examples getAllTickers()
#'@export
getAllTickers = function() {
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
#'@examples addTicker("SPY")
#'@export
addTicker <- function(name, yahoo_name, trading_class, multiplier = 100, type="STK", currency="USD", exchange="SMART", opt_exchange="SMART") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  ticker = DBI::dbGetQuery(conn,
                           "Select * from Tickers WHERE Name = ?",
                           params=list(name))
  if (nrow(ticker) != 0) {
    message("Ticker ", name," already exists in DB")
    return(0)
  }

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

#' getTickers
#'
#' This function is used by scanner functions to get one specific ticker or a character vector of tickers defined by name from DB.
#'
#' If no name is retrieved a data frame with columns
#' \code{Name, YahooName, Type, Currency, TradingClass, Multiplier, Exchange, OptExchange} and 0 line is returned
#'
#' N.B. It could be that the same ticker name is used for different securities - I assume it is not the case in my scans
#'
#'@param name string or character vector made by IBKR security name
#'@return A data frame with one line per ticker and columns
#' \code{Name, YahooName, Type, Currency, TradingClass, Multiplier, Exchange, OptExchange}
#'@examples getTickers("SPY")
#'@examples getTickers(c("SPY", "SLV"))
#'@export
getTickers <- function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  # Check if name is a vector with multiple elements
  if (length(name) > 1) {
    # Create a parameterized query for multiple names
    placeholders <- paste(rep("?", length(name)), collapse = ", ")
    query <- paste0("SELECT * FROM Tickers WHERE Name IN (", placeholders, ")")
    tickers <- DBI::dbGetQuery(conn, query, params = as.list(name))
  } else {
    # Original query for single name
    tickers <- DBI::dbGetQuery(conn,
                              "SELECT * FROM Tickers WHERE Name = ?",
                              params = list(name))
  }
  DBI::dbDisconnect(conn)
  return(tickers)
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
#'@param name string
#'@return A data frame with one line per ticker and columns
#' \code{Name, YahooName, Type, Currency, TradingClass, Multiplier, Exchange, OptExchange}
#'@examples getTicker("SPY")
#'@export
getTicker <- function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  # Check if name is a vector with multiple elements
  if (length(name) == 1) {
    # Original query for single name
    tickers <- DBI::dbGetQuery(conn,
                               "SELECT * FROM Tickers WHERE Name = ?",
                               params = list(name))
  }

  else Tbasics::display_message("getTicker must be used with only one ticker name")

  DBI::dbDisconnect(conn)
  return(tickers)
}

#' removeTicker
#'
#' This function is used by scanner functions to remove one specific ticker defined by name in the DB.
#'
#' If no name is retrieved then "Row deleted: 0" will be displayed, and 0 is returned
#'
#' N.B. It could be that the same ticker name is used for different securities - I assume it is not the case in my scans
#'
#'@param name IBKR security name
#'@return number of records deleted, i.e. 1 (normal case) or 0 (no line deleted, error case)
#'@examples removeTicker("ABT")
#'@export
removeTicker = function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  # Delete the row where Name equals ticker
  # Delete using parameterized query
  result <- DBI::dbExecute(conn,
            "DELETE FROM Tickers WHERE Name = ?",
            params = list(name))

  # Check how many rows were affected
  # This shows how many rows were deleted
  print(paste("Rows deleted:", result))
  DBI::dbDisconnect(conn)
  return(result)
}

