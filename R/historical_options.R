#' Historical Option Data Retrieval and Tracking
#'
#' R wrappers for on-demand historical option data retrieval from IBKR,
#' plus contract tracking for incremental daily data accumulation.

#' Get tdata_py module via package active binding (ensures Python path is set up)
#' @return tdata_py module or NULL if unavailable
#' @keywords internal
get_tdata_py <- function() {
  py <- tdata_py
  if (is.null(py)) {
    logger::log_error("Python module 'tdata_py' not available", namespace = "Tdata")
  }
  py
}

#' Get or Retrieve Historical Option Data
#'
#' Retrieve historical option data with automatic on-demand fetching from IBKR.
#' This function first checks if data exists in storage, and if not found,
#' automatically fetches from IBKR and optionally caches the results.
#'
#' @param symbol Character. Underlying symbol (e.g., "SPY", "AAPL")
#' @param trading_class Character. Option trading class (often same as symbol)
#' @param expiration Character. Expiration date in YYYYMMDD format (e.g., "20250321")
#' @param strike Numeric. Strike price
#' @param right Character. Option type: "C" for Call, "P" for Put
#' @param exchange Character. Exchange for routing (e.g., "SMART", "CBOE"). Default: "SMART"
#' @param data_type Character. Type of data: "historical", "intraday", or "combined". Default: "historical"
#' @param what_to_show Character. IBKR data type: "TRADES", "BID_ASK", "MIDPOINT". Default: "TRADES"
#' @param include_archived Logical. Check archived data if not in active storage. Default: TRUE
#' @param auto_cache Logical. Automatically cache retrieved data for reuse. Default: TRUE
#' @param force_refresh Logical. Skip cache and fetch fresh data from IBKR. Default: FALSE
#'
#' @return A tibble with historical option data, or NULL if unavailable
#'
#' @details
#' The function workflow:
#' \enumerate{
#'   \item Check if data exists in storage (unless force_refresh=TRUE)
#'   \item If not found, automatically fetch from IBKR
#'   \item Optionally cache the retrieved data (auto_cache=TRUE)
#'   \item Return data as R tibble
#' }
#'
#' @examples
#' \dontrun{
#' # Simple retrieval
#' data <- get_or_retrieve_option_historical(
#'   symbol = "SPY",
#'   trading_class = "SPY",
#'   expiration = "20250321",
#'   strike = 450.0,
#'   right = "C"
#' )
#'
#' # Force fresh data
#' fresh_data <- get_or_retrieve_option_historical(
#'   symbol = "AAPL",
#'   trading_class = "AAPL",
#'   expiration = "20250418",
#'   strike = 180.0,
#'   right = "P",
#'   force_refresh = TRUE
#' )
#'
#' # Get intraday data only
#' intraday <- get_or_retrieve_option_historical(
#'   symbol = "ITB",
#'   trading_class = "ITB",
#'   expiration = "20251128",
#'   strike = 110,
#'   right = "C",
#'   data_type = "intraday"
#' )
#' }
#'
#' @export
get_or_retrieve_option_historical <- function(
  symbol,
  trading_class,
  expiration,
  strike,
  right,
  exchange = "SMART",
  data_type = "historical",
  what_to_show = "TRADES",
  include_archived = TRUE,
  auto_cache = TRUE,
  force_refresh = FALSE
) {

  # Input validation
  if (missing(symbol) || missing(trading_class) || missing(expiration) ||
      missing(strike) || missing(right)) {
    logger::log_error("Missing required parameters: symbol, trading_class, expiration, strike, right", namespace="Tdata")
    return(NULL)
  }

  # Validate right parameter
  if (!right %in% c("C", "P", "Call", "Put")) {
    logger::log_error("Invalid 'right' parameter. Must be 'C', 'P', 'Call', or 'Put'", namespace="Tdata")
    return(NULL)
  }

  # Normalize right to single letter
  right_normalized <- if (right %in% c("Call", "C")) "C" else "P"

  # Validate data_type
  if (!data_type %in% c("historical", "intraday", "combined")) {
    logger::log_error("Invalid 'data_type'. Must be 'historical', 'intraday', or 'combined'", namespace="Tdata")
    return(NULL)
  }

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(NULL)

    logger::log_info(paste0(
      "Retrieving option data: ", symbol, " ", strike, right_normalized, " ", expiration,
      " (data_type=", data_type, ", force_refresh=", force_refresh, ")"
    ))

    # Call Python function (imported directly into tdata_py namespace)
    result <- tdata_py$get_or_retrieve_option_historical_data(
      symbol = symbol,
      trading_class = trading_class,
      expiration = expiration,
      strike = as.numeric(strike),
      right = right_normalized,
      exchange = exchange,
      data_type = data_type,
      what_to_show = what_to_show,
      include_archived = include_archived,
      auto_cache = auto_cache,
      force_refresh = force_refresh
    )

    # Check if result is NULL or None
    if (is.null(result)) {
      logger::log_warn("No data returned from Python function", namespace="Tdata")
      return(NULL)
    }

    # Convert to tibble (reticulate auto-converts pandas DataFrame to R data.frame)
    if (inherits(result, "data.frame")) {
      data_tibble <- tibble::as_tibble(result)
    } else {
      # If still a Python object, convert explicitly
      data_tibble <- tibble::as_tibble(reticulate::py_to_r(result))
    }

    logger::log_info(paste0("Retrieved ", nrow(data_tibble), " data points"), namespace="Tdata")

    return(data_tibble)

  }, error = function(e) {
    logger::log_error(paste0("Error in get_or_retrieve_option_historical: ", e$message), namespace="Tdata")
    return(NULL)
  })
}


#' Clear On-Demand Option Data Cache
#'
#' Clean up cached on-demand option data files to free disk space.
#'
#' @param symbol Character. If specified, only clear cache for this symbol. Default: NULL (all symbols)
#' @param older_than_days Integer. Remove files older than this many days. Default: 30
#'
#' @return A list with cleanup summary:
#' \itemize{
#'   \item files_removed: Number of files removed
#'   \item space_freed_mb: Disk space freed in megabytes
#' }
#'
#' @examples
#' \dontrun{
#' # Clear all cache older than 30 days
#' summary <- clear_on_demand_cache()
#'
#' # Clear cache for SPY only
#' spy_summary <- clear_on_demand_cache(symbol = "SPY")
#'
#' # Clear all cache older than 7 days
#' recent_summary <- clear_on_demand_cache(older_than_days = 7)
#' }
#'
#' @export
clear_on_demand_cache <- function(symbol = NULL, older_than_days = 30) {

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(list(error = "Python module not available"))

    logger::log_info(paste0(
      "Clearing on-demand cache",
      if (!is.null(symbol)) paste0(" for symbol: ", symbol) else "",
      " (older than ", older_than_days, " days)"
    ))

    # Call Python function (imported directly into tdata_py namespace)
    result <- tdata_py$clear_on_demand_cache(
      symbol = symbol,
      older_than_days = as.integer(older_than_days)
    )

    # Convert result to R list
    result_list <- reticulate::py_to_r(result)

    logger::log_info(paste0(
      "Cache cleanup complete: ",
      result_list$files_removed, " files removed, ",
      result_list$space_freed_mb, " MB freed"
    ))

    return(result_list)

  }, error = function(e) {
    logger::log_error(paste0("Error in clear_on_demand_cache: ", e$message), namespace="Tdata")
    return(list(error = e$message))
  })
}


#' Qualify an Option Contract via IBKR
#'
#' Asks IBKR to resolve the correct tradingClass, conId, and exchange for an
#' option contract. Essential for futures options (FOP) where the tradingClass
#' varies by contract series and cannot be looked up statically.
#'
#' @param symbol Character. Underlying symbol (e.g., "CHF", "SOFR3", "SPY")
#' @param expiration Character. Expiration date in YYYYMMDD format
#' @param strike Numeric. Strike price
#' @param right Character. "C" for Call, "P" for Put
#' @param exchange Character. Exchange for routing. Default: NULL (auto from Tickers)
#' @param currency Character. Currency. Default: NULL (auto from Tickers)
#' @param ib Python IB connection object. If NULL, creates and disconnects own connection.
#'   Pass an existing connection to avoid repeated connect/disconnect overhead.
#'
#' @return A list with qualified contract fields (conId, tradingClass, exchange, etc.)
#'   or NULL on error
#'
#' @examples
#' \dontrun{
#' qualify_contract("CHF", "20260605", 1.3, "C", exchange = "CME")
#' # Returns list(conId=..., tradingClass="CHU", exchange="CME", ...)
#' }
#'
#' @export
qualify_contract <- function(symbol, expiration, strike, right,
                             exchange = NULL, currency = NULL, ib = NULL) {

  right_normalized <- if (right %in% c("Call", "C")) "C" else "P"

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(NULL)

    result <- tdata_py$qualify_contract(
      sym = symbol,
      expiration = as.character(expiration),
      strike = as.numeric(strike),
      right = right_normalized,
      exchange = exchange,
      currency = currency,
      ib = ib
    )

    result_list <- reticulate::py_to_r(result)

    # Use [[ ]] for exact matching - avoid R partial matching on $
    if (!is.null(result_list[["error"]])) {
      logger::log_error(
        "qualify_contract failed: {result_list[['error']]}",
        namespace = "Tdata"
      )
      return(NULL)
    }

    logger::log_info(
      "Qualified: {symbol} {expiration} {strike}{right_normalized} -> tradingClass={result_list[['tradingClass']]}, exchange={result_list[['exchange']]}",
      namespace = "Tdata"
    )

    return(result_list)

  }, error = function(e) {
    logger::log_error(paste0("Error in qualify_contract: ", e$message), namespace = "Tdata")
    return(NULL)
  })
}


#' Add Option Contract to Tracking
#'
#' Add an option contract to the historical tracking configuration for
#' incremental daily data collection. Tracked contracts accumulate data
#' over time via scheduled updates.
#'
#' @param symbol Character. Underlying symbol (e.g., "SPY", "6SM6")
#' @param trading_class Character. Option trading class
#' @param expiration Character. Expiration date in YYYYMMDD format
#' @param strike Numeric. Strike price
#' @param right Character. "C" for Call, "P" for Put
#' @param exchange Character. Exchange for routing. Default: "SMART"
#'
#' @return Logical. TRUE if contract was added successfully
#'
#' @examples
#' \dontrun{
#' add_option_tracking("SPY", "SPY", "20260918", 680, "C", "SMART")
#' }
#'
#' @export
add_option_tracking <- function(symbol, trading_class, expiration, strike, right, exchange = "SMART") {

  # Normalize right
  right_normalized <- if (right %in% c("Call", "C")) "C" else "P"

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(FALSE)

    contract_config <- list(
      symbol = symbol,
      trading_class = trading_class,
      expiration = expiration,
      strike = as.numeric(strike),
      right = right_normalized,
      exchange = exchange
    )

    result <- tdata_py$add_historical_tracking(contract_config)
    success <- isTRUE(result)

    if (success) {
      logger::log_info(
        "Added tracking: {symbol} {strike}{right_normalized} {expiration} ({exchange})",
        namespace = "Tdata"
      )
    } else {
      logger::log_warn(
        "Failed to add tracking: {symbol} {strike}{right_normalized} {expiration}",
        namespace = "Tdata"
      )
    }

    return(success)

  }, error = function(e) {
    logger::log_error(paste0("Error in add_option_tracking: ", e$message), namespace = "Tdata")
    return(FALSE)
  })
}


#' Remove Option Contract from Tracking
#'
#' Remove an option contract from the historical tracking configuration.
#'
#' @param symbol Character. Underlying symbol
#' @param trading_class Character. Option trading class
#' @param expiration Character. Expiration date in YYYYMMDD format
#' @param strike Numeric. Strike price
#' @param right Character. "C" for Call, "P" for Put
#'
#' @return Logical. TRUE if contract was removed successfully
#'
#' @examples
#' \dontrun{
#' remove_option_tracking("SPY", "SPY", "20260918", 680, "C")
#' }
#'
#' @export
remove_option_tracking <- function(symbol, trading_class, expiration, strike, right) {

  right_normalized <- if (right %in% c("Call", "C")) "C" else "P"

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(FALSE)

    result <- tdata_py$manage_contracts(
      action = "remove",
      symbol = symbol,
      trading_class = trading_class,
      expiration = expiration,
      strike = as.numeric(strike),
      right = right_normalized
    )

    success <- isTRUE(result)

    if (success) {
      logger::log_info(
        "Removed tracking: {symbol} {strike}{right_normalized} {expiration}",
        namespace = "Tdata"
      )
    }

    return(success)

  }, error = function(e) {
    logger::log_error(paste0("Error in remove_option_tracking: ", e$message), namespace = "Tdata")
    return(FALSE)
  })
}


#' Update Tracked Option Contracts
#'
#' Collect incremental data for all tracked option contracts. This function
#' is designed to be called by scheduled scripts (e.g., daily via Windows Task Scheduler)
#' to accumulate historical data over time.
#'
#' @param data_type Character. "historical", "intraday", or "both". Default: "both"
#'
#' @return A list with update results:
#' \itemize{
#'   \item contracts_processed: Number of contracts updated
#'   \item contracts_errors: Number of contracts that failed
#'   \item files_updated: Number of data files written
#'   \item errors: Character vector of error messages
#' }
#'
#' @examples
#' \dontrun{
#' # Update all tracked contracts
#' result <- update_tracked_options()
#'
#' # Update historical data only
#' result <- update_tracked_options("historical")
#' }
#'
#' @export
update_tracked_options <- function(data_type = "both") {

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(list(error = "Python module not available"))

    logger::log_info("Updating tracked options (data_type={data_type})", namespace = "Tdata")

    result <- tdata_py$update_historical_data(data_type)
    result_list <- reticulate::py_to_r(result)

    if (!is.null(result_list[["error"]])) {
      logger::log_error("Update failed: {result_list[['error']]}", namespace = "Tdata")
    } else {
      logger::log_info(
        "Update complete: {result_list$contracts_processed} contracts, {result_list$files_updated} files",
        namespace = "Tdata"
      )
    }

    return(result_list)

  }, error = function(e) {
    logger::log_error(paste0("Error in update_tracked_options: ", e$message), namespace = "Tdata")
    return(list(error = e$message))
  })
}


#' List Tracked Option Contracts
#'
#' Display the current tracking configuration including all tracked contracts
#' and collection settings.
#'
#' @return A list with tracking configuration:
#' \itemize{
#'   \item contracts: List of tracked contracts with their details
#'   \item settings: Collection settings (durations, frequencies, etc.)
#' }
#'
#' @examples
#' \dontrun{
#' config <- list_tracked_options()
#' config$contracts
#' }
#'
#' @export
list_tracked_options <- function() {

  tryCatch({
    tdata_py <- get_tdata_py()
    if (is.null(tdata_py)) return(list(error = "Python module not available"))

    result <- tdata_py$list_historical_config(return_dict = TRUE)
    return(reticulate::py_to_r(result))

  }, error = function(e) {
    logger::log_error(paste0("Error in list_tracked_options: ", e$message), namespace = "Tdata")
    return(list(error = e$message))
  })
}
