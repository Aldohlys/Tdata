



###################  Retrieve prices functions #######################

#'   getSymIntervalDate
#'
#'This function gets from Yahoo service all values (Open, High, Low, Close, Volume and Adjusted)
#'for one symbol or a vector of symbols.
#'
#'It is based upon getYahooData,itself relying upon [quantmod::getSymbols] function.
#'It converts IBKR-style tickers into Yahoo-style tickers first,
#'by calling [Tdata::getYahooName] that looks up into Tickers table.
#'If not found in Tickers table, then returns NA.
#'
#'@param sym one or a vector of symbols
#'@param from_date a start date from which to retrieve symbols
#'@param to_date a end date to which to retrieve symbols - Default is today
#'@param sym_yahoo Optional argument default value is NULL.
#'@return Data frame with columns: date, ticker, Open, High, Low, Close, Adjusted, and Volume
#'@keywords Yahoo
#'@export
getSymIntervalDate = function(sym, from_date, to_date = Sys.Date(), sym_yahoo=NULL) {

  if (length(from_date) != 1) stop("length from_date must be equal to 1!")
  if (length(to_date) != 1) stop("length to_date must be equal to 1!")

  ### Get sym_yahoo if not already provided
  if (is.null(sym_yahoo)) sym_yahoo <- getYahooName(sym)

  if (length(sym) != length(sym_yahoo)) {
    tdata_log_error("sym_yahoo and sym must have the same length!!")
    return(NA)
  }

  ### Check first if there are some NA in Yahoo Names (e.g. futures)
  ### If yes then return NA and do not process further
  if (any(is.na(sym_yahoo))) {
    ### This assumes that sym order is the same as sym_yahoo order - i.e. sapply does not change order, should be Ok
    sym_list = paste(sym[is.na(sym_yahoo)], collapse = " ")
    tdata_log_info(sprintf("One or several tickers cannot be analyzed through Yahoo service: %s", sym_list))
    return(NA)
  }

  else {
    # Create a named vector for efficient lookup
    # This is faster than repeated indexing for large vectors
    lookup_table <- sym
    names(lookup_table) <- sym_yahoo

    df <- getYahooData(sym_yahoo, from_date, to_date)
    tdata_log_debug("Yahoo Data:", df)
    # Replace symbols using vector indexing - ticker is returned by YahooData function
    return(dplyr::mutate(df, ticker = lookup_table[ticker]))
  }

}

#' getSymMetricIntervalDate
#'
#' This function retrieves a metric from \code{from_date} till \code{to_date} for one or a vector of symbols
#' This function calls \code{getSymFromDate} with one or a vector of tickers.
#'@param sym one or a vector of symbols
#'@param from_date a start date from which to retrieve symbols
#'@param to_date a end date to which to retrieve symbols - Default is today
#'@param metric column to retrieve from Yahoo service, can be any of: "Open" "High" "Low" "Close" "Volume" "Adjusted". Default value is "Adjusted".
#'@returns a tibble: columns are date, symbol names.
#'Each (non date) column lists \code{metric} values for a given symbol and all dates between  \code{from_date} till \code{to_date}.
#'@examples getSymMetricIntervalDate(sym = c("SPY","USO"), as.Date("2023-01-02"))
#'@export
getSymMetricIntervalDate = function(sym, from_date, to_date = Sys.Date(), metric = "Adjusted"){

  if (length(from_date) != 1) stop("length from_date must be equal to 1!")
  if (length(to_date) != 1) stop("length to_date must be equal to 1!")

  ### Get corresponding Yahoo results
  sym_yahoo <- getYahooName(sym)

  ### Get only the non NA
  sub_sym <- sym[!is.na(sym_yahoo)]
  sub_sym_yahoo <- sym_yahoo[!is.na(sym_yahoo)]

  ### Retrieve first Yahoo data
  sub_res = getSymIntervalDate(sub_sym, from_date, to_date, sub_sym_yahoo)
  tdata_log_debug("getSymMetricIntervalDate: ", sub_res)

  ### Build NA data for non-Yahoo symbols - recycling NA and date
  sub_na <- dplyr::tibble(date = Sys.Date(), ticker=sym[is.na(sym_yahoo)],
                       Open=NA, High=NA, Low=NA, Close=NA, Adjusted=NA, Volume=NA)

  res <- dplyr::bind_rows(sub_res, sub_na)


  ### Extract column equal to OHLCVA value (by default equal to "Adjusted")
  df_subcols = res[, c("date", "ticker", metric)]

  ### Pivot data to from long to wide format
  wide_df <- tidyr::pivot_wider(
    data = df_subcols,
    id_cols = date,
    names_from = ticker,
    values_from = dplyr::all_of(metric) ## This to evaluate metric
  )

  # Ensure date is properly formatted
  wide_df$date <- as.Date(wide_df$date)

  # Sort by date
  wide_df <- wide_df[order(wide_df$date), ]

  return(wide_df)
}


#'   getSymPrice
#'
#'This function retrieves an historical price of one or several tickers
#'from Yahoo download service for one date or for a vector of dates.
#'
#'It will look for adjusted price from Yahoo. This function works only for previous days, not for today.
#'It will look around \code{report_date} to make sure it grasps at least one date with values from Yahoo.
#'It can be a closed day. In this case, nearest day will be taken (i.e. Monday for Sunday and Friday for Saturday).
#'
#'
#'@param sym ticker name or list of tickers, as known by IBKR or Yahoo, If necessary, will be converted to Yahoo ticker name.
#'@param report_date date ot list of dates, any date prior to today. Default is yesterday.
#'It can be also numeric or character, in which case it will be converted to Date format.
#'@param metric column to retrieve from Yahoo service, can be any of: "Open" "High" "Low" "Close" "Volume" "Adjusted". Default value is "Adjusted".
#'@return a vector of prices (even if multiple dates and multiple symbols), ordered by symbols and then by date for each symbol.
#'If no data could be retrieved (for instance report_date is today) then display error message and returns NA
#'@examples \dontrun{
#'getSymPrice("SPY")
#'getSymPrice(c("SPY","XSP"))
#'getSymPrice(c("ESTX50","DTLA"),as.Date("2024-04-15"))
#'getSymPrice(c("SPY","USO"),c(as.Date("2024-04-15"), as.Date("2024-04-17")))
#'}
#'@export
getSymPrice = function(sym, report_date = Sys.Date() - 1, metric = "Adjusted"){

  if (is.numeric(report_date)) report_date = as.character(report_date)
  if (is.character(report_date)) report_date = as.Date(report_date, "%Y%m%d")

  ### Keep only report_date values that are prior to today
  report_date <- report_date[report_date < Sys.Date()]

  ### test if there is no report_date prior to today - in this case display error message and stops
  if (length(report_date) == 0) {
    Tbasics::display_message("report_date must be prior today!")
    return(NA)
  }

  ### Recycling describes the concept of repeating elements of one vector to match the size of another.
  ### There are two rules that underlie the “tidyverse” recycling rules:
  ### - Vectors of size 1 will be recycled to the size of any other vector
  ### - Otherwise, all vectors must have the same size
  l <- vctrs::vec_recycle_common(sym, report_date, metric)

  ### Define a function for getSymPrice purpose as getSymMetricIntervalDate cannot be vectorized over dates
  getSymPriceOne <- function(one_sym, one_date, metric) {
    ### First case - requested date is an holiday or requested date is not today
    ### Get last close price in this case
    ### Take prices list 5 days before one_date to be sure to grasp at least one business day among these 5 days
    if (one_date == Sys.Date() - 1) prices_list <- getSymMetricIntervalDate(one_sym, one_date - 5, one_date + 1, metric)
    else prices_list <- getSymMetricIntervalDate(one_sym, one_date - 5, one_date + 2, metric)

    tdata_log_debug("Prices returned by getSymMetricIntervalDate", prices_list)
    ### Find nearest date to one_date, one_date becomes the nearest recorded day in Yahoo
    ### Monday date will be taken for Sunday, and Friday for Saturday
    one_date = Tbasics::findNearestNumberOrDate(prices_list$date, one_date)

    ### Extract prices line for one_date
    prices = prices_list[prices_list$date == one_date, 2]

    ### Convert data frame  to numeric vector
    prices_num = as.numeric(prices)

    ### No round as precision is unknown at this point - could be a currency or a stock
    return(prices_num)
  }

  l_prices <- unlist(purrr::pmap_dbl(l, getSymPriceOne))
  return(l_prices)

}


#'   getLastSymPrice
#'
#'This function retrieves the last available adjusted prices of a vector of tickers from Yahoo service
#'
#'It is based upon \code{getSymIntervalDate}
#' To achieve this, it will give argument "today - 5" date to this function- to be sure to get at least one valid date,
#' even if there are week ends and closed days. It takes then last open date if current date provided does not work.
#'
#'@param sym symbol name or list of symbols, as known by IBKR or Yahoo.
#'If it belongs to Tickers table, it will be converted to Yahoo ticker name.
#'Otherwise it is assumed to be good for Yahoo service.
#'@return a tibble with column names: \code{date, sym, value}, sym column contains the original sym list.
#'Known also as long data frame format as opposed to wide format.
#'@examples
#'getLastSymPrice(c("SPY","XSP"))
getLastSymPrice <- function(sym) {
  data = getSymIntervalDate(sym, from_date=Sys.Date()-5)

  if (all(is.na(data))) {
    tdata_log_info("Yahoo service did not find any data for: ", list(sym=sym))
    line <- data.frame(
      datetime = format(Sys.Date(),"%Y-%m-%d"),
      sym = sym,
      value = NA
    )
    return(line)
  }

  else {
    # Subset and rename columns
    ## Rows already filtered by getSymIntervalDate
    df <- data[, c("date", "ticker", "Adjusted")]
    names(df) <- c("date", "sym", "value")

    # Create a factor in the original sym order before processing
    original_order <- unique(df$sym)
    df$sym <- factor(df$sym, levels = original_order)

    # Apply the slicing operation (default is 1, i.e. get max slice)
    # Use base R pipe operator instead of magrittr %>%
    result <- df |>
      dplyr::group_by(sym) |>
      dplyr::slice_max(date) |>
      dplyr::ungroup()

    return(result)
  }
}


###
#' getLastAdjustedPrice
#'
#' This function takes one ticker (or a vector of tickers) as input and returns the last available adjusted value from Yahoo service.
#'
#' It calls \code{getLastSymPrice} and returns value column from this data frame.
#' If ticker is NA or NULL or equal to All or STOCK, it returns NA -
#' Same behavior if any ticker belonging to tickers vector is unknown by Yahoo
#'
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of values (rounded to 2 decimals) corresponding to last values of tickers
#'@examples getLastAdjustedPrice("SPY")
#'@export
getLastAdjustedPrice = function(ticker) {
  if (length(ticker) == 0) return(NA)
  if ( (length(ticker) == 1 && is.na(ticker)) || any(ticker %in% c("","All","STOCK")) ) return(NA)

  ### This will return NA if any ticker is unknown by Yahoo
  return(getLastSymPrice(ticker)$value)
}

###
#' getLastPriceDate
#'
#' This function takes one ticker (or a vector of tickers) as input and returns from Yahoo service the last available
#' date with an available price - see also \code{getLastAdjustedPrice}.
#'
#' It calls \code{getLastSymPrice} and returns date column from this data frame.
#' If ticker is NA or NULL or equal to All or STOCK, it returns NA also.
#' Same if any of ticker does not have any Yahoo name.
#'
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of dates for each ticker. It calls \code{getSymFromDate} to get the dates.
#'@examples getLastPriceDate("SPY")
#'@export
getLastPriceDate = function(ticker) {
  if (length(ticker) == 0) return(as.Date(NA))
  if ( (length(ticker) == 1 && is.na(ticker)) || any(ticker %in% c("","All","STOCK")) ) return(as.Date(NA))

  ### This may return current date and time if ticker is unknown
  return(getLastSymPrice(ticker)$date)

}

### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
### Get last price and last change (J/J-1)

###
#'getLastTickerData
#'
#'For a given ticker this function returns the last known closed value (adjusted) and its change from previous day.
#'
#'This function is not vectorized and accepts only one ticker at a time.
#'It calls Yahoo service through \code{getSymFromDate} to obtains necessary value.
#'@param ticker string - IBKR style of ticker, if equal to "STOCK" or "All" then returns a list of NA values
#'@returns a list of 2 fields: \code{last} which contains last value of the ticker,
#'and \code{change} that contains a string giving the percentage of change since previous day.
#'@examples getLastTickerData("SPY")
#'@export
getLastTickerData = function(ticker) {

  if (length(ticker) > 1) stop("getLastTickerData must be called with only one ticker")

  if (is.null(ticker) ||
      ### NA shall work as a numeric for future computations like round(x,2)
      any(ticker %in% c("", "All", "STOCK"))) return(list(last = NA, change = NA))

  tryCatch({
    ticks = getSymIntervalDate(ticker, Sys.Date() - 5) ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    if (is.na(ticks)) return(list(last = NA, change = NA))
    else {
      last_data = ticks[nrow(ticks), "Adjusted"]
      p_last_data = ticks[nrow(ticks) - 1, "Adjusted"]
      return(list(
        last = round(last_data, 2),
        change = scales::label_percent(accuracy = 0.01)(Tbasics::change(p_last_data, last_data))
      ))
    }
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last = NA, change = NA))
  })
}

#### Used by Gonet.R script and RAnalysis
###
#'getStockPrice
#'
#'For a given ticker or a vector of tickers, this function returns either the last close price from Yahoo service
#'or the last stored price (latest from both).
#'
#'This will depend upon close parameter.
#'If close is TRUE then Yahoo service is used, otherwise data is retrieved from prices DB.
#'If no data can be found in prices DB (e.g. sym is not found in DB), then last available Yahoo price is returned.
#'
#'This function is vectorized, if no price ticker exists in DB it will then return an empty line for the corresponding ticker.
#'@param sym string - IBKR style of ticker.
#'@param close Boolean TRUE/FALSE if true then retrieve last close price else retrieve last stored price.
#'@returns a value
#'@examples
#'\dontrun{
#'getStockPrice(sym="SPY")
#'getStockPrice(sym="ESTX50")
#'getStockPrice(sym="USO",close=TRUE)
#'}
#'@export
getStockPrice = function(sym = NULL, close = FALSE) {
  tdata_log_info("getStockPrice: using Yahoo and DB, not IBKR")

  if (is.null(sym) || all(is.na(sym)) || any(!is.character(sym)) ||
      any(sym %in% c("", "All", "STOCK"))){
    tdata_log_info("getStockPrice: There must be at least one valid ticker name")
    return(NA)
  }

  prices_list = purrr::map_dbl(sym, \(s) {
    sym_yahoo = getYahooName(s)
    if (is.na(sym_yahoo)) return(NA)
    else return(getLastAdjustedPrice(s))
  })

  logger::log_debug("prices_list:", prices_list, namespace="Tdata")

  ### Initialize all lines with data from Yahoo,
  ###   dated yesterday close of business for US stocks (maybe different for European stocks)
  ### This will have as many lines as sym
  line <- data.frame(
    datetime = paste(format(Sys.Date() - 1,"%Y-%m-%d"), "22:00"),
    sym = sym,
    ### price is field name in Prices DB
    price =  prices_list ### This may include NA if unknown by Yahoo
  )
  logger::log_debug("line:", line, namespace="Tdata")

  ### Just retrieve latest price from DB or Yahoo but no try to update using IBKR
  if (!close) {
    conn <- DBI::dbConnect(RSQLite::SQLite(),  config::get("DB"))
    on.exit(DBI::dbDisconnect(conn), add=TRUE)

    ### Retrieve last price stored in DB
    line_db <- DBI::dbGetQuery(conn, "SELECT datetime, sym, price FROM Prices
                          WHERE sym = ? ORDER BY ROWID DESC LIMIT 1;", params = list(sym))

    ### Add this info to line info retrieved from Yahoo
    line <- dplyr::left_join(line, line_db, dplyr::join_by(sym))
    line <- dplyr::mutate(line,
                          dt_x = as.POSIXct(datetime.x),
                          dt_y = as.POSIXct(datetime.y, format="%Y%m%d %H:%M"),

                          ### Takes the more recent datetime - datetime.x will always be yesterday's close datetime
                          ### i.e. datetime.x cannot be NA
                          datetime = dplyr::if_else(is.na(datetime.y),
                                                    datetime.x,
                                                    dplyr::if_else(dt_x > dt_y, datetime.x, datetime.y)),

                          ### Looks first if DB price is NA (no price returned) and then takes Yahoo price
                          ### Otherwise takes the most recent price as well
                          price = dplyr::if_else(is.na(price.y), price.x,
                                          dplyr::if_else(dt_x > dt_y, price.x, price.y)))

    line <- dplyr::select(line, datetime, sym, price)
    tdata_log_debug("From DB: ", line)
  }

  return(line)
}

#'@export
getStoredMetrics = function(name) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  # Check if name is a vector with multiple elements
  if (length(name) > 1) {
    # Create a parameterized query for multiple names
    placeholders <- paste(rep("?", length(name)), collapse = ", ")
    query <- paste0("SELECT * FROM Prices WHERE sym IN (", placeholders, ") ORDER BY datetime DESC LIMIT ",length(name))
    prices <- DBI::dbGetQuery(conn, query, params = as.list(name))

    # Reorder results to match input order
    if (nrow(prices) > 0) {
      # Create a factor with levels in the same order as the input vector
      prices$OriginalOrder <- factor(prices$sym, levels = name)
      # Sort by this factor
      prices <- prices[order(prices$OriginalOrder), ]
      # Remove the temporary column
      prices$OriginalOrder <- NULL
    }
  } else {
    # Original query for single name
    prices <- DBI::dbGetQuery(conn,
                               "SELECT * FROM Prices WHERE sym = ? ORDER BY ROWID DESC LIMIT 1;",
                               params = list(name))
  }
  return(prices)
}

#' Ultra-robust Yahoo Finance data retrieval for problematic tickers
#'
#' @param tickers Data frame from getTickers() or character vector
#' @param from_date Start date
#' @param to_date End date (defaults to current date)
#' @param max_retries Maximum number of retry attempts for each ticker
#' @param retry_delay Seconds to wait between retries
#' @param timeout Seconds for connection timeout
#' @param chunk_size Number of tickers to process in each batch
#' @return Data frame with columns: date, ticker, Open, High, Low, Close, Adjusted, and Volume
#' @export
getYahooData <- function(tickers, from_date = Sys.Date() - 5, to_date = Sys.Date(),
                               max_retries = 5, retry_delay = 2,
                               timeout = 10, chunk_size = 5) {

  # Process input to get ticker names
  if (is.data.frame(tickers) && "YahooName" %in% colnames(tickers)) {
    ### Test if there is at least one ticker to retrieve
    if (nrow(tickers) == 0) Tbasics::display_error_message("At least one ticker should have non-NA Yahoo name declared")
    else ticker_names <- tickers$YahooName
  } else if (is.character(tickers)) {
    ticker_names <- tickers
  } else {
    Tbasics::display_error_message("Tickers must be either a character vector or a data frame with YahooName column")
  }


  # If any duplicate then stop - there should be no duplicate in call
  if (any(duplicated(ticker_names))) stop("Tickers cannot be duplicated")

  # Store original timeout setting
  original_timeout <- getOption("timeout")
  on.exit(options(timeout = original_timeout), add = TRUE)

  # Set custom timeout
  options(timeout = timeout)

  # Initialize results
  all_results <- list()
  failed_tickers <- character(0)

  # Split tickers into smaller chunks to process
  ticker_chunks <- split(ticker_names, ceiling(seq_along(ticker_names) / chunk_size))

  # Process each chunk
  for (chunk_idx in seq_along(ticker_chunks)) {
    chunk <- ticker_chunks[[chunk_idx]]
    tdata_log_debug(sprintf("Processing chunk %d of %d (%d tickers)",
                  chunk_idx, length(ticker_chunks), length(chunk)))

    # Process each ticker in the chunk
    for (ticker in chunk) {
      tdata_log_debug(sprintf("  Fetching %s... ", ticker))

      success <- FALSE
      last_error <- NULL

      # Special handling for known problematic ticker formats
      is_special_ticker <- grepl("^\\^|=$", ticker)

      # Try multiple times with increasing timeouts for problem tickers
      for (attempt in 1:max_retries) {
        # Increase timeout for problematic tickers on later attempts
        if (is_special_ticker && attempt > 1) {
          options(timeout = timeout * attempt)
        }

        result <- tryCatch({
          # Test special ticker values first
          if (is.na(ticker) || ticker == "All" || ticker == "STOCK") ticker_data = NA

          # Try to fetch this ticker
          else {
            ticker_data <- quantmod::getSymbols(ticker,
                                              from = from_date,
                                              to = to_date,
                                              auto.assign = FALSE,
                                              warnings = FALSE)

            # Verify we got actual data
            if (is.null(ticker_data) || nrow(ticker_data) == 0) {
              stop("Retrieved empty dataset")
            }
          }
          # Success!
          all_results[[ticker]] <- ticker_data
          success <- TRUE
          tdata_log_debug("Success for {ticker}")
          break  # Exit retry loop
        },
        error = function(e) {
          last_error <- e
          if (attempt < max_retries) {
            tdata_log_debug(sprintf("attempt %d failed, retrying... ", attempt))
            Sys.sleep(retry_delay * attempt)  # Exponential backoff
          } else {
            tdata_log_debug("Fail for {ticker}")
          }
          return(NULL)
        })

        if (success) break
      }

      # Reset timeout to original setting for this function
      options(timeout = timeout)

      if (!success) {
        failed_tickers <- c(failed_tickers, ticker)
        warning(sprintf("Failed to retrieve %s after %d attempts: %s",
                        ticker, max_retries, as.character(last_error)))
      }
    }

    # Small pause between chunks to avoid overwhelming the API
    if (chunk_idx < length(ticker_chunks)) {
      Sys.sleep(1)
    }
  }

  # Handle case where all tickers failed
  if (length(all_results) == 0) {
    warning("Failed to retrieve any data")
    return(NULL)
  }

  # Report on overall success rate
  success_rate <- (length(ticker_names) - length(failed_tickers)) / length(ticker_names)
  tdata_log_debug(sprintf("Retrieved data for %.1f%% of tickers (%d/%d)",
                success_rate * 100,
                length(ticker_names) - length(failed_tickers),
                length(ticker_names)))
  if (length(failed_tickers) > 0)
    tdata_log_debug("Failed tickers: ", paste(failed_tickers, collapse=", "))

  # Process results into a consolidated XTS object with ticker as column
  if (length(all_results) == 0) {
    return(NULL)
  }

  # Create an empty list to store data frames
  all_ticker_data <- list()

  # Process each ticker's data
  for (ticker in names(all_results)) {
    ticker_data <- all_results[[ticker]]

    # Convert to data frame while preserving dates
    df <- data.frame(date = zoo::index(ticker_data),
                     zoo::coredata(ticker_data),
                     row.names = NULL,
                     stringsAsFactors = FALSE)

    # Extract the column name pattern - Yahoo Finance typically uses TICKER.OHLCV format
    col_pattern <- paste0("^", ticker, "\\.")

    # Rename columns to standard format: Open, High, Low, Close, Adjusted, Volume
    colnames(df) <- gsub(col_pattern, "", colnames(df))

    # Add ticker column
    df$ticker <- ticker

    # Ensure we have the expected columns
    expected_cols <- c("date", "Open", "High", "Low", "Close", "Adjusted", "Volume", "ticker")
    for (col in expected_cols) {
      if (!col %in% colnames(df)) {
        # Handle missing columns
        if (col == "Adjusted" && "Adj.Close" %in% colnames(df)) {
          # Sometimes Yahoo returns "Adj.Close" instead of "Adjusted"
          df$Adjusted <- df$Adj.Close
          df$Adj.Close <- NULL
        } else if (col != "date" && col != "ticker") {
          # Create empty columns for any missing data
          df[[col]] <- NA
          warning(sprintf("Missing %s column for ticker %s, filled with NA", col, ticker))
        }
      }
    }

    # Store the processed data frame
    all_ticker_data[[ticker]] <- df
  }

  # Combine all data frames
  combined_df <- do.call(rbind, all_ticker_data)

  # Arrange columns in requested order
  col_order <- c("date", "ticker", "Open", "High", "Low", "Close", "Adjusted", "Volume")
  final_df <- combined_df[, col_order]

  # Ensure date is a proper date object
  final_df$date <- as.Date(final_df$date)

  # Sort by date
  final_df <- final_df[order(final_df$date), ]

  ### Remove fancy row names
  rownames(final_df) <- NULL
  return(final_df)
}


