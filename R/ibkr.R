

#' isIBAvailable
#'
#' This function tries to open a connection to TWS API on 7496 port. If successful, it returns TRUE and disconnect.
#'  Otherwise returns FALSE
#'
#'
#'@returns TRUE or FALSE
#'@export
#'@examples
#'\dontrun{
#'isIBAvailable()
#'}
isIBAvailable <- function() {
  ### Check if tdata_py is available (Python module loaded successfully)
  if (is.null(tdata_py)) {
    logger::log_warn("tdata_py Python module not available - Python environment not initialized", namespace="Tdata")
    return(FALSE)
  }

  ### Open a connection and then close it with IBKR TWS API
  is_api_available <- tdata_py$isIBAvailable()

  if (is_api_available) {
    logger::log_debug("Getting IBKR TWS API Access: IBKR_API={is_api_available}", namespace="Tdata")
    logger::log_info("Connected", namespace="Tdata")
  }

  else logger::log_info("No IBKR TWS API Access: IBKR_API={is_api_available}", namespace="Tdata")
  return(is_api_available)
}


#### Used by Gonet.R script and RAnalysis
###
#'getIBKRMetrics
#'
#'Retrieves a price or a list of price from IBKR and
#'returns a data frame with date and time (formatted), the list of tickers and corresponding prices, equal to Nan if no price found.
#'Additionally it stores prices in DB Prices table (only if price different from NaN)
#'
#'For a given ticker or a list of tickers, this function returns most recent market prices from IBKR,
#'using Tickers table from DB within Python code called.
#'If IBKR service is not available, it will return an error code and display an error message: 0 if no IBKR service, -1 if contract is unknown.
#'@param sym string or vector of strings - IBKR style of ticker, if unknown then function returns -1
#'@param reqType integer - Market data type: 1=Live, 2=Frozen (default), 3=Delayed, 4=Delayed Frozen
#'@returns a data frame with the following fields: \code{datetime} (formatted date and time), \code{sym} (ticker name),
#' \code{price} (price as double),  \code{iv180} IV for 6 months expiration, \code{iv30} IV for 1 month expiration,
#' \code{ivp} IV percentile, \code{rv30}, realized volatility for the last month,  \code{rvp} realized volatility percentile.
#'@examples
#'\dontrun{
#'getIBKRMetrics("SPY")
#'getIBKRMetrics(c("ESTX50", "USO", "ABBN"))
#'getIBKRMetrics("SPY", reqType=1)  # Live data
#'}
#'@export
getIBKRMetrics <- function(sym, reqType=NULL) {

  ### Automatically determine reqType if not provided (delayed data for LSEETF exchange)
  if (is.null(reqType)) {
    reqType <- determine_req_type(sym)
  }

  ### This will work even if sym is a vector and not the other IBKR contract components
  IBKRPrice <- tdata_py$getValue(list_sym=sym, ib=NULL, reqType=reqType)
  logger::log_info("Retrieved price data from IBKR: {IBKRPrice}", namespace="Tdata")

  ### Error case do not go further on
  if (length(IBKRPrice) == 1) {
    logger::log_warn("IBKR data retrieval did not work - either no connection or contract does not exist", namespace="Tdata")
    return (IBKRPrice)
  }

  ### Remove all empty prices if any, print resulting data
  ### Only prices that are different from NaN will be stored in DB
  LastIBKRPrice <- IBKRPrice[!is.nan(IBKRPrice$price),]
  logger::log_debug("Filtered IBKRPrice: {LastIBKRPrice}", namespace="Tdata")

  ### Retrieve tickers returned by IBKRPrice and filter out non relevant tickers
  tickers <- getTickers(LastIBKRPrice$sym) |> dplyr::filter(IV == "YES")

  ### Retrieve 30days IV for each ticker, each time applicable - -1 or NA if not
  ### Do nothing if no ticker has IV
  if (nrow(tickers) != 0) {

    #tickers_30d_iv <- get30dIV(tickers)
    logger::log_info("Build IV180 from near/next option chains IVs...", namespace="Tdata")
    tickers_180d_iv <- get180dIV(tickers, LastIBKRPrice)

    logger::log_info("Get IV30 and RV30 through IBKR historical data...", namespace="Tdata")
    vol_metrics <- getVolMetrics(tickers$Name)
    ### Keep only 3 significant digits
    tickers_vol_metrics <- dplyr::mutate(vol_metrics, dplyr::across(where(is.numeric), ~signif(.x, 3)))

    LastIBKRPrice <- dplyr::left_join(LastIBKRPrice, tickers_180d_iv, by = dplyr::join_by(sym == Name)) |>
      dplyr::left_join(dplyr::select(tickers_vol_metrics, sym, iv30, ivp,
                                     rv30, rvp),
                       by = dplyr::join_by(sym))

    tryCatch({
      myconn <- safe_db_connect()
      on.exit(DBI::dbDisconnect(myconn), add=TRUE)
      safe_db_append(myconn, "Prices", LastIBKRPrice)
    },
    error = function(cond) {
      logger::log_error("Error while trying to write to DB", cond, namespace="Tdata")
      NA
    },
    warning = function(cond) {
      logger::log_warn(conditionMessage(cond), namespace="Tdata")
      # Choose a return value in case of warning
      NULL
    })
    return(LastIBKRPrice)
  }
  else logger::log_info("All IBKR Prices equal to NA (did not get any data from exchange) or IV set to NO", namespace="Tdata")

  ### Available readily if needed
  return(IBKRPrice)
}

########################## Get data from IBKR
#' getSliceAllIBKRMetrics
#'
#' Retrieve Market Data for a Subset of Tickers from IBKR TWS API
#'
#' This function retrieves market data metrics for a specified slice of tickers from Interactive Brokers
#' Trader Workstation (TWS) API. It filters tickers based on exchange criteria and processes each ticker
#' sequentially to gather metrics.
#'
#' @param first Numeric, start index of the ticker slice to process. Default is 1 (first ticker).
#' @param last Numeric, end index of the ticker slice to process. Default is 0, which processes all tickers.
#'          When specified, tickers from index first to last will be processed.
#'
#' @details
#' The function performs the following operations:
#' 1. Retrieves all available tickers using `getAllTickers()`
#' 2. Optionally slices the ticker list based on parameters first and last
#' 3. Filters tickers to include only those from exchanges "SMART", "EUREX", or "CBOE"
#' 4. Processes each filtered ticker sequentially using `getIBKRMetrics()`
#' 5. Combines all results into a single data frame
#'
#' The function outputs progress messages indicating:
#' - Which ticker range is being processed
#' - The list of tickers before filtering
#' - The list of tickers after filtering by exchange
#'
#' @return A data frame containing combined metrics data for all processed tickers. Each row
#'         represents data returned by `getIBKRMetrics()` for a single ticker.
#'
#' @note This function requires an active connection to IBKR TWS API. The underlying `getIBKRMetrics()`
#'       function handles the actual data retrieval.
#'
#' @examples
#' \dontrun{
#' # Requires IBKR TWS connection and Python environment
#' # Process all tickers
#' all_metrics <- getSliceAllIBKRMetrics()
#'
#' # Process only the first 5 tickers
#' first_five <- getSliceAllIBKRMetrics(last=5)
#'
#' # Process tickers from index 10 to 20
#' subset_metrics <- getSliceAllIBKRMetrics(first=10, last=20)
#' }
#' @seealso
#' \code{\link{getAllTickers}} for retrieving the complete list of available tickers
#' \code{\link{getIBKRMetrics}} for retrieving metrics for a single ticker
#'
#' @export
getSliceAllIBKRMetrics <- function(first=1, last=0) {

  tickers = getAllTickers()
  max = nrow(tickers)

  ### Verify last value
  if (first == 0)  Tbasics::display_error_message("First index ticker cannot be 0 !")
  if (last > max) Tbasics::display_error_message("Last index ticker exceeds number of tickers !")
  if (last == 0) last = max

  ### Process new prices for tickers - that should include also underlyings part of the portfolio
  message(paste0("\n#####  Retrieving price data from ticker nr.",first," to ticker nr.",last," ..."))
  tickers = tickers[first:last,]

  message(paste0("\nUnfiltered: ",paste(tickers$Name, collapse=" ")))
  ### Do not load any security related to exchange like LSEETF or EBS
  tickers <- dplyr::filter(tickers, Exchange == "SMART" | Exchange == "EUREX" | Exchange == "CBOE")
  message(paste0("\nFiltered: ",paste(tickers$Name, collapse=" ")))

  ### Store in DB all IBKR prices from tickers$Name, return final result

  result = data.frame()
  nb_tickers = nrow(tickers)

  ### seq_len will work also if nb_tickers = 0, unlike 1:nb_tickers
  for (i in seq_len(nb_tickers)) {
    name <- tickers[i, "Name"]
    message(paste0("\n#####  Retrieving price data for ticker ", name, " nr.", i," out of ", nb_tickers," ..."))
    metrics <- getIBKRMetrics(name)
    result <- dplyr::bind_rows(result, metrics)
  }
  return(result)
}

#### Used by Ligne module
###
#'getOptPrice
#'
#'Retrieves an option price from IBKR. This function is not vectorized.
#'
#'For a given contract, this function returns a price from IBKR - it may be a last price or a mid price
#'If IBKR service is not available, or option price not available, or contract does not exist, it will return an error code -1.
#'@param sym string - IBKR style of ticker, if unknown then function returns -1
#'@param tradingClass string - optional to retrieve the correct chain for determining price - default is Ticker table in DB
#'@param right string - can be either P, C or Put, Call
#'@param expiration number, date or string - expiration date, format is Y/M/D
#'@param strike string or numeric - option strike
#'@param currency string - either EUR, CHF or USD - default is  - default is Ticker table in DB
#'@param exchange string - exchange like SMART, EUREX, CBOE,..., if unknown then function returns -1 -  - default is Ticker table in DB
#'@returns a number, either option value or -1 is not found
#'@examples
#'\dontrun{
#'getOptPrice("SPX", "SPX", "P", 5000, "20291220", "USD", "SMART")
#'getOptPrice("SPY", "SPY", "Call", 500.0, as.Date("2026-12-18"), "USD", "SMART")
#'getOptPrice(sym="SPY", right="Call", strike=500.0, expiration=as.Date("2026-12-18"))
#'}
#'@export
getOptPrice = function(sym, tradingClass, right, strike, expiration, currency="USD", exchange="SMART") {
  if (tradingClass == "Stock") {
    Tbasics::display_error_message("A valid Trading Class must be provided!")
    return(NA)
  }

  ### If necessary modify right
  if (right =="Put") right = "P"
  if (right == "Call") right = "C"

  ### Case where expiration is a number
  if (is.numeric(expiration)) expiration = as.character(expiration)

  ### Case where expiration has a date class - convert it into a string with IBKR format for expiration date
  else if (inherits(expiration,"Date")) expiration = format(expiration,"%Y%m%d")
  else if (!is.character(expiration)) stop("expiration date must be either a date, a number or a character string!")

  strike=as.numeric(strike)
  # message("NBBO Option value Sym:",sym," Type:",right," Strike:",strike," Expiration:",expiration,
  #         " Currency:",currency," Exchange:",exchange," tradingClass:",tradingClass)
  val = tdata_py$getOptValue(sym=sym,expiration=expiration,strikes=strike,
                             right=right)$value

  if (is.null(val) || is.nan(val)) val=-1
  return(val)
}


