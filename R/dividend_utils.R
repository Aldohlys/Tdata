### For a single ticker
### This is internal only
## It returns dividend yield in percent terms, either from IBKR (if name is not NA) or from Yahoo or using default values for indexes
## It returns -1 in case of errors
getSingleDivYield <- function(type="STK", name=NA, yahoo_name=NA, currency="USD", exchange="SMART") {

  ### test first that correct arguments were given, if not returns -1 - name is mandatory to retrieve a price
  if ((length(type) != 1) | (length(name) != 1)| (length(yahoo_name) != 1) | is.na(name)) {
    logger::log_info("getSingleDivYield does not have the right arguments: type={type}, name={name}, yahoo_name={yahoo_name}", namespace="Tdata")
    return (-1)
  }
  price_data <- getStockPrice(name)
  logger::log_debug("Price data {price_data}", namespace="Tdata")

  if (is.null(price_data) || is.na(price_data$price) || price_data$price <= 0) {
    logger::log_error("Invalid {name} price - returns -1", namespace="Tdata")
    return(-1)
  }

  ### Simple cases for stock type of securities
  if (type == "STK") {

      ### first try to retrieve next 12 months estimation from IBKR if available -
      ### getNTMDividend returns -1 if it does not know how to handle
      sum_div <- tdata_py$getNTMDividend(symbol=name, secType=type, currency=currency, exchange = exchange)

      ### If IBKR TWS API unable to provide next12months, then try past 12 month data from Yahoo and sum
      if (sum_div == -1) {
        tryCatch({
          ### If no dividends are paid during the last year (e.g. SLV, GLD) then sum is equal to 0 because no records
          sum_div <- sum(quantmod::getDividends(yahoo_name, from = Sys.Date() - 365, to = Sys.Date()))
        }, error = function(e) {
          # This will execute instead
          logger::log_error("Error with Yahoo in getSingleDivYield for", list(name=name), namespace="Tdata")
          return(-1)
        })
      }

      #### Data is rounded to 2 decimals - x.yz% expected
      result <- round(100*sum_div / price_data$price, 2)
      logger::log_debug("dividend yield: type={type}, result={result}", namespace="Tdata")
      return(result)
  }

  #### For index options such as ESTX50, SPX, RUT, etc...
  else if (type == "IND") {
    result <- switch(name,
                     ESTX50 = 2.86,
                     SPX=,
                     XSP= round(7.1655/5.2641, 2), ### Using SPY ETF as approximation
                     -1 ### Unknown index
    )
    logger::log_debug("dividend yield: type={type}, result={result}", namespace="Tdata")
    return(result)
  }

  ### Other cases like TBILL, FUT, CASH,... - normally should not be sent
  else {
    logger::log_info("type {type} different from STK and IND - no dividend", namespace="Tdata")
    return(0)
  }
}

#' getDividendYield
#'
#' @description Returns dividend yield for one or a vector of symbols, looking at IBKR or Yahoo through internal getSingleDivYield function.
#' Dividend yield is expressed in percent terms, not in decimal terms.
#' @param tickers data frame of tickers
#' @return a vector of double, including -1 for tickers where no value could be computed, even with Yahoo, and
#' 0 for dividends non paying securities like FUT, TBILL, CASH,...
#' @export
getDividendYield <- function(tickers) {

  ### Test that tickers is of the right type and there should be at least 1 ticker, with Type, Name and YahooName columns
  if (!is.data.frame(tickers) || (!all(c("Type", "Name", "YahooName") %in% names(tickers))) || nrow(tickers) == 0) return (NA)

  ##
  logger::log_debug("Tickers:", tickers, namespace="Tdata")

  if (nrow(tickers) == 1) {
    return(getSingleDivYield(type=tickers$Type, name=tickers$Name, yahoo_name=tickers$YahooName,
                                 currency=tickers$Currency, exchange=tickers$Exchange))
  }

  else return(
    # Creates a list of values for each row
    purrr::pmap_dbl(tickers, function(Type, Name, YahooName, Currency, Exchange,...) {
      getSingleDivYield(type=Type, name=Name, yahoo_name=YahooName, currency=Currency, exchange=Exchange)
    })
  )
}

#' getLastDivYield
#'
#' @description Looks up at DB and returns dividend yield for one or a vector of symbols.
#' It is expressed in decimal terms - i.e. divided by 100 after retrieved from DB
#' @param names vector of tickers names - therefore all of the same type
#' @return a vector of double, or NA if any missing ticker in DB or if names is not of character type
#' @export
getLastDivYield <- function(names) {

  #### names should be coercible to character
  if (!is.character(names)) return(NA)

  ## Retrieve tickers from DB
  tickers = getTickers(names)

  ### Make sure that if any missing ticker then return NA else return yield
  if (nrow(tickers) != length(names)) return(NA)

  ### Convert to numeric to get a vector of doubles
  else return(as.numeric(tickers$Div_yield/100))
}

