
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
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  tickers = DBI::dbReadTable(conn, "Tickers")

  return(dplyr::arrange(tickers, tickers$Name))
}

#' addTicker
#'
#' This function is used by scanner functions to add one ticker in DB
#'
#'@param name IBKR security name
#'@param yahoo_name Yahoo finances download security name.  If not provided, will be equal to \code{name}.
#'@param sector GCIS (i.e. S&P) industry name for a given stock, e.g. Industrial, Financial,... Denominated as "US Stocks" or "US Bonds" or "Forex" for non-stock cases. Default is "Unclassified"
#'@param trading_class chain trading class that will be used to retrieve option data from IBKR. If not provided, will be equal to \code{name}.
#'@param multiplier chain multiplier, default is 100
#'@param type IBKR security type, default is STK (stock). Could be also FUT (future), IND (Index),...
#'@param currency IBKR currency value, i.e. USD, EUR, CHF,... Default is USD
#'@param exchange exchange name where security price can be retrieved, default is SMART
#'@param opt_exchange exchange name where option price for the security can be retrieved, default is SMART
#'@param IV String character - whether IV should be looked at or not , default is YES
#'@param expiration String character - should be a date for YYYYMM for futures or YYYYMMDD for bills, bonds. Default is empty string
#'@return record added and display number of records added, i.e. 1 (normal case) or 0 (no line added, error case)
#'@examples
#'\dontrun{
#'addTicker("SPY")
#'}
#'@export
addTicker <- function(name, yahoo_name, sector, trading_class, multiplier = 100, type="STK",
                      currency="USD", exchange="SMART", opt_exchange="SMART", IV="YES", expiration = "", ConId = NULL) {

  conn <- safe_db_connect()
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
  if (missing(sector)) sector="Unclassified"

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
  df <- data.frame(Name = name, YahooName = yahoo_name, Type = type, Sector = sector,
                   Currency = currency, TradingClass = trading_class, Multiplier = multiplier,
                   Exchange = exchange, OptExchange = opt_exchange,
                   Beta_3m = round(beta$beta_3m, 4), Beta_6m = round(beta$beta_6m, 4),
                   Div_yield = div_yield,
                   Beta_1y = round(beta$beta_1y, 4), Beta_3y = round(beta$beta_3y, 4),
                   IV = IV, Expiration = expiration,
                   LastUpdate = format(Sys.time(), "%Y%m%d %H:%M"),
                   ConId = if (!is.null(ConId)) as.integer(ConId) else NA_integer_)

  result <- safe_db_append(conn, "Tickers", df)
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
  conn <- safe_db_connect()
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
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

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

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Check if name is a vector with multiple elements
  if (length(name) == 1) {
    # Original query for single name
    tickers <- DBI::dbGetQuery(conn,
                               "SELECT * FROM Tickers WHERE Name = ?",
                               params = list(name))
  }

  else Tbasics::display_error_message("getTicker must be used with only one ticker name")

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
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Delete the row where Name equals ticker
  # Delete using parameterized query
  result <- DBI::dbExecute(conn,
            "DELETE FROM Tickers WHERE Name = ?",
            params = list(name))

  # Check how many rows were affected
  # This shows how many rows were deleted
  print(paste("Rows deleted:", result))
  return(result)
}

#' Get name from Yahoo service
#'
#' This function is used by getStockPrice or getSymIntervalDate functions
#' to get Yahoo symbol names before calling getYahooData function. NOT EXPORTED.
#'
#' If no symbol name is found in Ticker DB then NA is returned.
#' If only a given symbol is not found, then NA will be returned for this symbol.
#'
#'@param sym IBKR security name or a vector of names
#'@return named vector of Yahoo names or NA, where names are argument names (i.e. sym) and values are Yahoo result names
#'@examples
#'\dontrun{
#'getYahooName(c("ABT", "ESTX50", "US-T", "SOFR3"))
#'getYahooName(c("EUR.USD", "USD.CAD"))
#'}
getYahooName <- function(sym) {

  sym_yahoo = purrr::map_chr(sym, \(x){
    logger::log_debug("ticker: {x}", namespace="Tdata")

    ## Case where x=NA
    if (is.na(x)) return(NA)

    # First, try to find the ticker in the Tickers table
    ticker <- getTicker(x)

    # If ticker exists, we'll use it (even if it matches base currency name)
    # This handles cases like CHF options where "CHF" is a valid ticker symbol
    if (nrow(ticker) > 0) {
      # Ticker found - will process it below (after currency pair fallback logic)
      logger::log_debug("Found ticker {x} in Tickers table", namespace="Tdata")
    } else {
      # Ticker not found - check if it's the base currency
      if (grepl("^[A-Z]{3}$", x)) {
        base_currency <- getParam("BaseCurrency")
        if (x == base_currency) {
          logger::log_debug("Symbol {x} is the base currency (not in Tickers), no conversion needed", namespace="Tdata")
          return("BASE_CURRENCY")  # Return special marker instead of NA
        }
      }
    }

    ### If ticker does not exist in Ticker table, try currency pair format if applicable
    if (nrow(ticker) == 0) {
      logger::log_debug("Ticker {x} is not in Ticker DB", namespace="Tdata")

      # If symbol looks like a currency code (3 uppercase letters), try currency pair
      if (grepl("^[A-Z]{3}$", x)) {
        # Get base currency from config (already retrieved above, but keep for clarity)
        base_currency <- getParam("BaseCurrency")

        # First try: symbol.BaseCurrency (e.g., USD.CHF if BaseCurrency is CHF)
        logger::log_debug("Symbol {x} looks like currency, trying {x}.{base_currency}", namespace="Tdata")
        currency_pair <- paste0(x, ".", base_currency)
        ticker <- getTicker(currency_pair)

        # Second try: inverse pair (e.g., CHF.USD instead of USD.CHF)
        # Note: Caller will need to invert the rate (rate_inverse = 1 / rate)
        if (nrow(ticker) == 0) {
          inverse_pair <- paste0(base_currency, ".", x)
          logger::log_debug("Currency pair {currency_pair} not found, trying inverse {inverse_pair}", namespace="Tdata")
          ticker <- getTicker(inverse_pair)
          if (nrow(ticker) > 0) {
            logger::log_warn("Using inverse pair {inverse_pair} - caller should invert rates", namespace="Tdata")
          }
        }

        # Third try: symbol.USD (fallback, most Yahoo pairs are quoted against USD)
        if (nrow(ticker) == 0 && base_currency != "USD") {
          logger::log_debug("Inverse pair not found, trying {x}.USD as fallback", namespace="Tdata")
          currency_pair <- paste0(x, ".USD")
          ticker <- getTicker(currency_pair)
        }

        if (nrow(ticker) > 0) {
          logger::log_debug("Found currency pair {currency_pair} in Ticker DB", namespace="Tdata")
          # Continue with this ticker (don't return yet, check Type and YahooName below)
        } else {
          logger::log_debug("No currency pair found for {x}", namespace="Tdata")
          return(NA)
        }
      } else {
        return(NA)
      }
    }

    ### One ticker has been found - if type equals FUT or TBILL no Yahoo search
    if (ticker$Type %in% c("STK", "CASH", "IND"))  {
      ### Case where no Yahoo historical data available
      if (is.na(ticker$YahooName) || ticker$YahooName == "") return(NA)
      else return(ticker$YahooName)
    }

    ### Else no Yahoo search possible or does not make sense (i.e. T-Bill)
    else {
      logger::log_debug("Ticker type {ticker$Type} is outside of allowed types for Yahoo search", namespace="Tdata")
      return (NA)
    }
  })

  logger::log_debug("(getYahooName) Yahoo retrieved/taken : {sym_yahoo}", namespace="Tdata")
  return(sym_yahoo)
}

#' setTicker
#'
#' This function allows to set specific parameters for one given ticker in the DB.
#'
#' If ticker name does not exist then NA will be returned.
#'
#'
#'@param Name IBKR security name
#'@param ... Any of the following parameters: \code{YahooName Type  Sector
#'Currency TradingClass Multiplier Exchange OptExchange Beta_3m Beta_6m Div_yield
#'Beta_1y Beta_3y  IV Expiration}
#'@return Update result: 1 is successful, 0 is failure
#'@examples
#'\dontrun{
#'setTicker("ABT", Sector="Healthcare", TradingClass="ABT")
#'}
#'@export
setTicker = function(Name, ...) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  params <- update_ticker_params(Name, ...)

  if (length(params$updates) == 0) {
    logger::log_info("No valid update parameters provided", namespace="Tdata")
    return(0)
  }

  # Add LastUpdate timestamp - always updated
  timestamp <- format(Sys.time(), "%Y%m%d %H:%M")
  params$updates$LastUpdate <- timestamp

  # Build SET clause with named placeholders
  set_clauses <- paste0(names(params$updates), " = :", names(params$updates))
  set_clause <- paste(set_clauses, collapse = ", ")

  # Build SQL with named placeholders
  sql <- paste0("UPDATE Tickers SET ", set_clause, " WHERE Name = :Name")

  # Create named parameter list
  param_list <- params$updates
  param_list$Name <- params$Name  # Add the WHERE parameter

  # Execute
  result <- DBI::dbExecute(conn, sql, param_list)

  # Check how many rows were affected
  # This shows how many rows were deleted
  print(paste("Row modified:", result))

  return(list(
    sql = sql,
    params = param_list,
    rows_affected = result
  ))

  return(result)
}

#' Update Ticker parameters
#'
#' This function is used by setTicker function only to validate and extract
#'  the parameters from what end user has provided
#' NOT EXPORTED.
#' If only a given symbol is not found, then NA will be returned for this symbol.
#'
#'@param Name IBKR security name or a vector of names
#'@param ... Key/value pairs to be updated for security
#'@return list with Name and updated arguments list
update_ticker_params <- function(Name, ...) {
  kwargs <- list(...)

  # Define exact column names that can be updated
  updatable_columns <- c(
    "YahooName", "Type", "Sector", "Currency", "TradingClass",
    "Multiplier", "Exchange", "OptExchange", "Beta_3m", "Beta_6m",
    "Div_yield", "Beta_1y", "Beta_3y", "IV", "Expiration", "ConId"
  )

  # Filter to only valid columns - exact match required
  valid_updates <- kwargs[names(kwargs) %in% updatable_columns]

  return(list(
    Name = Name,
    updates = valid_updates
  ))
}

#' Reload Ticker Cache from Database
#'
#' Forces the Python ticker cache to reload from the database.
#' Call this function after adding or updating tickers to make them
#' immediately available to Python-based functions without restarting R.
#'
#' @return Logical. TRUE if reload successful, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Add a new ticker
#' addTicker("GTT", "Gonet SA", "STK", "SMART", "EUR")
#'
#' # Reload cache to recognize new ticker
#' reloadTickerCache()
#'
#' # Now Python functions can access GTT
#' updateTicker("GTT")
#' }
reloadTickerCache <- function() {
  # Check if Python module is available
  if (is.null(tdata_py)) {
    logger::log_error("Python module not available - cannot reload ticker cache",
                     namespace = "Tdata")
    return(FALSE)
  }

  # Call Python method to reload tickers
  tryCatch({
    tdata_py$core$ticker_db$load_tickers()
    ticker_count <- length(tdata_py$core$ticker_db$tickers)
    logger::log_info(paste0("Reloaded ticker cache: ", ticker_count, " tickers"),
                    namespace = "Tdata")
    return(TRUE)
  }, error = function(e) {
    logger::log_error(paste0("Failed to reload ticker cache: ", e$message),
                     namespace = "Tdata")
    return(FALSE)
  })
}

# ════════════════════════════════════════════════════════════════════════════════
# ScannerUniverse functions
# ════════════════════════════════════════════════════════════════════════════════

#' Get Scanner Universe
#'
#' Returns filtered symbols from the ScannerUniverse table.
#' Used by swing scanner and macro context scripts to load their ticker lists
#' from DB instead of hardcoded vectors.
#'
#' @param role Optional filter: "scanner", "etf", "macro", or NULL for all
#' @param sector Optional filter: sector name (e.g. "Energy", "Macro"), or NULL for all
#' @param active_only Logical. If TRUE (default), only return active symbols (IsActive = 1)
#' @return A data.frame with columns Symbol, Sector, Role, IsActive, Notes
#' @examples
#' \dontrun{
#' getScannerUniverse()                              # all active symbols
#' getScannerUniverse(role = "macro")                # macro tickers only
#' getScannerUniverse(sector = "Energy")             # Energy sector (stocks + ETF)
#' getScannerUniverse(role = "scanner", sector = "Technology")
#' }
#' @export
getScannerUniverse <- function(role = NULL, sector = NULL, active_only = TRUE) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  sql <- "SELECT * FROM ScannerUniverse WHERE 1=1"
  params <- list()

  if (active_only) {
    sql <- paste(sql, "AND IsActive = ?")
    params <- c(params, 1L)
  }
  if (!is.null(role)) {
    sql <- paste(sql, "AND Role = ?")
    params <- c(params, role)
  }
  if (!is.null(sector)) {
    sql <- paste(sql, "AND Sector = ?")
    params <- c(params, sector)
  }

  sql <- paste(sql, "ORDER BY Sector, Symbol")
  DBI::dbGetQuery(conn, sql, params = params)
}

#' Add Symbol to Scanner Universe
#'
#' Inserts a new symbol into the ScannerUniverse table.
#' If the symbol already exists, a message is shown and 0 is returned.
#'
#' @param symbol Ticker symbol (e.g. "AAPL")
#' @param sector Sector name (e.g. "Technology")
#' @param role Role: "scanner", "etf", or "macro". Default "scanner"
#' @param notes Optional notes string
#' @return Integer: 1 if inserted, 0 if already exists
#' @examples
#' \dontrun{
#' addScannerSymbol("CRWD", "Technology")
#' addScannerSymbol("XLE", "Energy", role = "etf", notes = "Sector ETF")
#' }
#' @export
addScannerSymbol <- function(symbol, sector, role = "scanner", notes = NULL) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  existing <- DBI::dbGetQuery(conn,
    "SELECT Symbol FROM ScannerUniverse WHERE Symbol = ?",
    params = list(symbol))

  if (nrow(existing) > 0) {
    message("Symbol ", symbol, " already exists in ScannerUniverse")
    return(0L)
  }

  df <- data.frame(
    Symbol = symbol, Sector = sector, Role = role,
    IsActive = 1L, Notes = if (is.null(notes)) NA_character_ else notes,
    stringsAsFactors = FALSE)

  safe_db_append(conn, "ScannerUniverse", df)
  message("Added ", symbol, " to ScannerUniverse (", sector, "/", role, ")")
  return(1L)
}

#' Remove Symbol from Scanner Universe (Soft Delete)
#'
#' Sets IsActive = 0 for the given symbol. Does not physically delete the row.
#'
#' @param symbol Ticker symbol to deactivate
#' @return Integer: number of rows affected (1 if found, 0 if not)
#' @examples
#' \dontrun{
#' removeScannerSymbol("BIOX")
#' }
#' @export
removeScannerSymbol <- function(symbol) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- DBI::dbExecute(conn,
    "UPDATE ScannerUniverse SET IsActive = 0 WHERE Symbol = ?",
    params = list(symbol))

  if (result == 0) message("Symbol ", symbol, " not found in ScannerUniverse")
  else message("Deactivated ", symbol, " in ScannerUniverse")
  return(result)
}

#' Sync Tickers to ScannerUniverse
#'
#' Finds Tickers with IV=YES and Type=STK that are missing from
#' ScannerUniverse and inserts them. Skips tickers whose latest
#' price exceeds \code{max_price}.
#'
#' @param max_price Numeric. Skip tickers with price above this (default 500)
#' @param dry_run Logical. If TRUE, only report what would be added (default FALSE)
#' @return Integer: number of symbols added
#' @export
syncTickersToScanner <- function(max_price = 500, dry_run = FALSE) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  missing <- DBI::dbGetQuery(conn, "
    SELECT t.Name, t.Sector, t.Type
    FROM Tickers t
    LEFT JOIN ScannerUniverse s ON t.Name = s.Symbol
    WHERE t.IV = 'YES'
      AND t.Type = 'STK'
      AND s.Symbol IS NULL
    ORDER BY t.Sector, t.Name
  ")

  if (nrow(missing) == 0) {
    message("ScannerUniverse is in sync with Tickers")
    return(0L)
  }

  # Check prices to skip expensive tickers
  if (nrow(missing) > 0 && max_price < Inf) {
    placeholders <- paste0("'", missing$Name, "'", collapse = ",")
    prices <- DBI::dbGetQuery(conn, sprintf("
      SELECT sym, price FROM Prices
      WHERE sym IN (%s)
      GROUP BY sym HAVING ROWID = MAX(ROWID)
    ", placeholders))

    expensive <- prices$sym[!is.na(prices$price) & prices$price > max_price]
    if (length(expensive) > 0) {
      message("Skipping (price > $", max_price, "): ", paste(expensive, collapse = ", "))
      missing <- missing[!missing$Name %in% expensive, ]
    }
  }

  if (nrow(missing) == 0) return(0L)

  # Detect ETFs by common patterns
  etf_patterns <- c("^XL[A-Z]$", "^IW[A-Z]$", "^QQQ$", "^SPY$", "^GLD$",
                     "^SLV$", "^SMH$", "^ITB$", "^EWJ$", "^EW[A-Z]$",
                     "^VXX$", "^FXC$", "^XLRE$", "^XLU$")
  is_etf <- grepl(paste(etf_patterns, collapse = "|"), missing$Name)

  missing$Role <- ifelse(is_etf, "etf", "scanner")

  message("Sync: ", nrow(missing), " tickers to add to ScannerUniverse:")
  for (i in seq_len(nrow(missing))) {
    message("  + ", missing$Name[i], " (", missing$Sector[i], "/", missing$Role[i], ")")
  }

  if (dry_run) {
    message("Dry run — no changes made")
    return(0L)
  }

  added <- 0L
  for (i in seq_len(nrow(missing))) {
    df <- data.frame(
      Symbol = missing$Name[i], Sector = missing$Sector[i],
      Role = missing$Role[i], IsActive = 1L, Notes = NA_character_,
      stringsAsFactors = FALSE)
    safe_db_append(conn, "ScannerUniverse", df)
    added <- added + 1L
  }

  message("Added ", added, " symbols to ScannerUniverse")
  return(added)
}
