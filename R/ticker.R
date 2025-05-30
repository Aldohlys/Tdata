
#####  ticker.R
##### All utilities related to scanned tickers

#' getAllTickers
#'
#' This function is used by scanner functions to get all tickers to scan
#'
#'@return A data frame with columns \code{Name, YahooName, TradingClass, Multiplier, Type, Currency,  Exchange, OptExchange}
#' sorted by alphabetical order, equal to the list of current tickers stored in DB
#'@examples
#'\dontrun{
#'getAllTickers()
#'}
#'@export
getAllTickers = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  tickers = DBI::dbReadTable(conn, "Tickers")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

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
#'@param IV String character - whether IV should be looked at or not , default is YES
#'@param expiration String character - should be a date for YYYYMM for futures or YYYYMMDD for bills, bonds. Default is empty string
#'@return record added and display number of records added, i.e. 1 (normal case) or 0 (no line deleted, error case)
#'@examples
#'\dontrun{
#'addTicker("SPY")
#'}
#'@export
addTicker <- function(name, yahoo_name, trading_class, multiplier = 100, type="STK",
                      currency="USD", exchange="SMART", opt_exchange="SMART", IV="YES", expiration = "") {

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ticker = DBI::dbGetQuery(conn,
                           "Select * from Tickers WHERE Name = ?",
                           params=list(name))
  if (nrow(ticker) != 0) {
    message("Ticker ", name," already exists in DB")
    return(0)
  }

  #### Standard case for US stocks
  if (missing(trading_class)) trading_class=name
  if (missing(yahoo_name)) yahoo_name=name

  ### Initialize beta and div_yield to NA
  beta = list(
    beta_3m = NA,
    beta_6m = NA,
    beta_1y = NA,
    beta_3y = NA
  )
  div_yield = -1

  ### Beta gets computed only if yahoo_name parameter is not equal to NA and security type is either STK, IND or CASH
  ### There is historical data for TBILL under Yahoo but with current yield and it does not relate to price
  ###
  if ((!is.na(yahoo_name)) && (type %in% c("STK", "IND", "CASH"))) beta <- calculate_beta_vs_spx_periods(ticker=yahoo_name, benchmark = "^GSPC", currency=currency)

  ### it is not possible to compute dividend yield as we do not know price data at this point of time
  ### if (type %in% c("STK", "IND")) div_yield <- getSingleDivYield(type=type, name=name, yahoo_name=yahoo_name, currency=currency, exchange=exchange)

  ### Build df for storage in DB
  df <- data.frame(Name = name, YahooName = yahoo_name, Type = type,
                   Currency = currency, TradingClass = trading_class, Multiplier = multiplier,
                   Exchange = exchange, OptExchange = opt_exchange,
                   Beta_3m = round(beta$beta_3m, 4), Beta_6m = round(beta$beta_6m, 4),
                   Div_yield = div_yield,
                   Beta_1y = round(beta$beta_1y, 4), Beta_3y = round(beta$beta_3y, 4),
                   IV = IV, Expiration = expiration,
                   LastUpdate = format(Sys.time(), "%Y%m%d %H:%M"))

  result <- DBI::dbAppendTable(conn, "Tickers", df)
  # Check how many row were added
  print(paste("Rows added:", result))

  return(df)
}


#' updateTicker
#'
#' This function allows to update one or a vector of tickers in DB, looking at dividend yields (for STK and IND type of securities)
#' and at beta computations for STK, CASH and IND type of securities.
#'
#'@param name one or a vector of IBKR security names
#'@return number of records updated, i.e. length(name) (normal case) or less (partial or no update, error case)
#'@examples
#'\dontrun{
#'updateTicker("SPY")
#'}
#'@export
updateTicker <- function(name) {

  # Check if name contains at least one element
  if (length(name) == 0) {
    Tbasics::display_error_message("At least one ticker must be provided")
    return(0)
  }

  ### Retrieve ticker data
  tickers <- getTickers(name)

  ### This function works only if at least on of the tickers already exist -
  if (nrow(tickers) == 0) {
    message("No tickers to update")
    return(0)
  }

  rows_affected <- 0

  ### Open connection and prepare when exiting function
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  sql <- "UPDATE Tickers
            SET Beta_3m = ?, Beta_6m = ?, Beta_1y = ?, Beta_3y = ?, Div_Yield = ?, LastUpdate = ?
            WHERE Name = ?"

  ### Keep only beta numbers in list returned by calculate beta function
  ### Round these numbers to the 4th decimal digit

  updateOneTicker <- function(ticker) {
    sym <- ticker$Name

    ### Default value
    div_yield_data <- NA
    b3m <- b6m <- b1y <- b3y <- NA

    ### Manage dividend
    if (ticker$Type %in% c("STK", "IND")) div_yield_data <- getDividendYield(ticker)

    ### Manage beta computations
    if (ticker$Type %in% c("STK", "IND", "CASH")) {
      betas <- suppressWarnings(as.numeric(calculate_beta_vs_spx_periods(ticker$YahooName)))
      b3m <- round(betas[4], 4)
      b6m <- round(betas[5], 4)
      b1y <- round(betas[6], 4)
      b3y <- round(betas[7], 4)
    }

    ### Returns params for the SQL statement
    return(list(b3m, b6m, b1y, b3y, div_yield_data, format(Sys.time(), "%Y%m%d %H:%M"), sym))
  }

  ### Case with only one ticker
  if (nrow(tickers) == 1) {
      params <- updateOneTicker(tickers)
      query <- DBI::dbSendStatement(conn, sql)
      DBI::dbBind(query, params)

      ### Update number of updated records and clear query
      rows_affected <- DBI::dbGetRowsAffected(query)
      DBI::dbClearResult(query)
  }

  ### Case with multiple tickers
  else {
    for (i in 1:nrow(tickers)) {

      ### One ticker at a time for each query
      params <- updateOneTicker(tickers[i,])
      query <- DBI::dbSendStatement(conn, sql)
      DBI::dbBind(query, params)

      ### Update number of updated records
      rows_affected <- rows_affected + DBI::dbGetRowsAffected(query)

      ## Clear query to be ready for next update
      DBI::dbClearResult(query)
    }
  }



  # Check how many row were added
  print(paste("Rows updated:", rows_affected))

  return(rows_affected)
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
#'@examples
#'\dontrun{
#'getTickers("SPY")
#'getTickers(c("SPY", "SLV"))
#'}
#'@export
getTickers <- function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

  # Check if name is a vector with multiple elements
  if (length(name) > 1) {
    # Create a parameterized query for multiple names
    placeholders <- paste(rep("?", length(name)), collapse = ", ")
    query <- paste0("SELECT * FROM Tickers WHERE Name IN (", placeholders, ")")
    tickers <- DBI::dbGetQuery(conn, query, params = as.list(name))

    # Reorder results to match input order
    if (nrow(tickers) > 0) {
      # Create a factor with levels in the same order as the input vector
      tickers$OriginalOrder <- factor(tickers$Name, levels = name)
      # Sort by this factor
      tickers <- tickers[order(tickers$OriginalOrder), ]
      # Remove the temporary column
      tickers$OriginalOrder <- NULL
    }
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
#'@examples
#'\dontrun{
#'getTicker("SPY")
#'}
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

  else Tbasics::display_error_message("getTicker must be used with only one ticker name")

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
#'@examples
#'\dontrun{
#'removeTicker("ABT")
#'}
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

