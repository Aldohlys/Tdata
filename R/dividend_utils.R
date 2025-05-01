### For a single ticker
### This is internal only
## It returns dividend yield, either from IBKR (if name is not NA) or from Yahoo or using default values for indexes
getSingleDivYield <- function(type="STK", name=NA, yahoo_name=NA, currency="USD", exchange="SMART") {

  ### test first that correct arguments were given, if not returns -1
  if ((length(type) != 1) | (length(name) != 1)| (length(yahoo_name) != 1)) return (-1)

  ### Simple cases for stock type of securities
  if (type == "STK") {

    ### first retrieve data from IBKR if available -
    if (!is.na(name)) ttm_div <- tdata_py$getTTMDividend(symbol=name, secType=type, currency=currency, exchange = exchange)
    else {
      ttm_div = NA
      name <- yahoo_name
    }

    ### If not available retrieve 12 month data from Yahoo and sum
    if (is.na(ttm_div) | ttm_div == -1) {
      tryCatch({
        ### If no dividends are paid during the last year (e.g. SLV, GLD) then sum is equal to 0 because no records
        ttm_div <- sum(quantmod::getDividends(yahoo_name, from = Sys.Date() - 365, to = Sys.Date()))
      }, error = function(e) {
        message("Error retrieving getDividendYield for: ", yahoo_name, "\n", e$message)
        return(-1)
      })
    }
    #### Data is rounded to 4 decimals - 0.xy% expected
    return(round(ttm_div / getStockPrice(name)$price, 4))
  }

  #### For index options such as ESTX50, SPX, RUT, etc...
  else if (type == "IND") {
    return(switch(name,
                  ESTX50 = 0.0286,
                  SPX=,
                  XSP= round(7.1655/526.41, 2), ### Using SPY ETF as approximation
                  -1 ### Unknown indices
    ))
  }

  ### Other cases like TBILL
  return(0)
}

#' getDividendYield
#'
#' @description Returns dividend yield for one or a vector of symbols, looking at IBKR or Yahoo through internal getSingleDivYield function.
#' @param tickers data frame of tickers
#' @return a list of double, -1 for tickers where no value could be computed, even with Yahoo
#' @export
getDividendYield <- function(tickers, verbose = FALSE) {
  message("getDividendYield")

  ### Test that tickers is of the right type and there should be at least 1 ticker, with Type, Name and YahooName columns
  if (!is.data.frame(tickers) || (!all(c("Type", "Name", "YahooName") %in% names(tickers))) || nrow(tickers) == 0) return (NA)

  ##
  if (verbose) print(tickers$Name)

  if (nrow(tickers) == 1) return(getSingleDivYield(type=tickers$Type, name=tickers$Name, yahoo_name=tickers$YahooName),
                                 currency=tickers$Currency, exchange=tickers$Exchange)

  else return(
    # Creates a list of values for each row
    purrr::pmap_dbl(tickers, function(Type, Name, YahooName, Currency, Exchange,...) {
      getSingleDivYield(type=Type, name=Name, yahoo_name=YahooName, currency=Currency, exchange=Exchange)
    })
  )
}

#' getLastDivYield
#'
#' @description Looks up at DB and returns dividend yield for one or a vector of symbols
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

