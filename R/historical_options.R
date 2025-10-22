#' Historical Option Data Retrieval
#'
#' R wrappers for on-demand historical option data retrieval from IBKR.
#' These functions provide seamless access to historical option data with automatic
#' caching and retrieval.

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
    t_log_error("Missing required parameters: symbol, trading_class, expiration, strike, right")
    return(NULL)
  }

  # Validate right parameter
  if (!right %in% c("C", "P", "Call", "Put")) {
    t_log_error("Invalid 'right' parameter. Must be 'C', 'P', 'Call', or 'Put'")
    return(NULL)
  }

  # Normalize right to single letter
  right_normalized <- if (right %in% c("Call", "C")) "C" else "P"

  # Validate data_type
  if (!data_type %in% c("historical", "intraday", "combined")) {
    t_log_error("Invalid 'data_type'. Must be 'historical', 'intraday', or 'combined'")
    return(NULL)
  }

  tryCatch({
    # Import Python module
    if (!reticulate::py_module_available("tdata_py")) {
      t_log_error("Python module 'tdata_py' not available")
      return(NULL)
    }

    tdata_py <- reticulate::import("tdata_py")

    t_log_info(paste0(
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
      t_log_warn("No data returned from Python function")
      return(NULL)
    }

    # Convert to tibble (reticulate auto-converts pandas DataFrame to R data.frame)
    if (inherits(result, "data.frame")) {
      data_tibble <- tibble::as_tibble(result)
    } else {
      # If still a Python object, convert explicitly
      data_tibble <- tibble::as_tibble(reticulate::py_to_r(result))
    }

    t_log_info(paste0("Retrieved ", nrow(data_tibble), " data points"))

    return(data_tibble)

  }, error = function(e) {
    t_log_error(paste0("Error in get_or_retrieve_option_historical: ", e$message))
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
    # Import Python module
    if (!reticulate::py_module_available("tdata_py")) {
      t_log_error("Python module 'tdata_py' not available")
      return(list(error = "Python module not available"))
    }

    tdata_py <- reticulate::import("tdata_py")

    t_log_info(paste0(
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

    t_log_info(paste0(
      "Cache cleanup complete: ",
      result_list$files_removed, " files removed, ",
      result_list$space_freed_mb, " MB freed"
    ))

    return(result_list)

  }, error = function(e) {
    t_log_error(paste0("Error in clear_on_demand_cache: ", e$message))
    return(list(error = e$message))
  })
}
