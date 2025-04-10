
###################  Retrieve prices functions #######################

#'   getSymIntervalDate
#'
#'This function gets from Yahoo service all values (Open, High, Low, Close, Volume and Adjusted)
#'for one of a vector of symbols.
#'
#'It is based upon getYahooDataRobust, based upon quantmod getSymbols function.
#'It converts IBKR-style tickers into Yahoo-style tickers first, by looking up into Tickers table.
#'If not found in Tickers table, then just returns what has been given in \code{sym} argument.
#'
#'@param sym one or a vector of symbols
#'@param from_date a start date from which to retrieve symbols
#'@param to_date a end date to which to retrieve symbols - Default is today
#'@return Data frame with columns: date, ticker, Open, High, Low, Close, Adjusted, and Volume
#'@keywords Yahoo
#'@export
getSymIntervalDate = function(sym, from_date, to_date = Sys.Date()) {

  if (length(from_date) != 1) stop("length from_date must be equal to 1!")
  if (length(to_date) != 1) stop("length to_date must be equal to 1!")

  # lookup_yahoo = c("ESTX50"="^STOXX50E","MC"="MC.PA","OR"="OR.PA","TTE"="TTE.PA","AI"="AI.PA", "SGO"="SGO.PA", "BN"="BN.PA",
  #                  "SPX"="^SPX","XSP"="^XSP","RUT"="^RUT","NESN"="NESN.SW", "ABBN" = "ABBN.SW", "HOLN"="HOLN.SW","SLHN"="SLHN.SW",
  #                  "ROG"="ROG.SW", "SXLV"="SXLV.L", "SXLY"="SXLY.L","SXLK"="SXLK.L","SXLC"="SXLC.L",
  #                  "CSBGU0"="CSBGU0.SW","DTLA"="DTLA.L","TRE7"="TRE7.L",
  #                  "U.UN"="U-UN.TO", "USD.CAD"="USDCAD=X",
  #                  "EUR.USD"="EURUSD=X","CHF.USD"="CHFUSD=X","EUR.CHF"="EURCHF=X")
  ####  sym = dplyr::if_else(sym %in% names(lookup_yahoo), lookup_yahoo[sym], sym)

  sym_yahoo = sapply(sym, \(x){
                ticker <- getTickers(x)
                ### If ticker does not exist in Scan table then just return the name
                if (nrow(ticker) == 0) return(x)
                ### Else return Yahoo name
                else return(ticker$YahooName)
                })

  # Create a named vector for efficient lookup
  # This is faster than repeated indexing for large vectors
  lookup_table <- sym
  names(lookup_table) <- sym_yahoo

  #
  # ### sapply would not work here as it tries to simplify the returned structure which does not work with getSymbols function
  #### auto.assign=TRUE is necessary if multiple symbols at the same time
  # lapply(as.character(sym_yahoo), function(x) {
  #                             suppressMessages(quantmod::getSymbols(x, from = from_date, to = to_date,
  #                             auto.assign = F, warnings=FALSE))
  #                             }
  #        )
  df <- getYahooDataRobust(sym_yahoo, from_date, to_date)
  # Replace symbols using vector indexing
  dplyr::mutate(df, ticker = lookup_table[ticker])
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
#'@examples getSymMetricIntervalDate(sym = c("SPY","FNV","USO"), as.Date("2023-01-02"))
#'@export
getSymMetricIntervalDate = function(sym, from_date, to_date = Sys.Date(), metric = "Adjusted"){

  if (length(from_date) != 1) stop("length from_date must be equal to 1!")
  if (length(to_date) != 1) stop("length to_date must be equal to 1!")

  sym_all = getSymIntervalDate(sym, from_date, to_date)

  ### Extract column equal to OHLCVA value (by default equal to "Adjusted")
  df_subset = sym_all[, c("date", "ticker", metric)]

  ### Pivot data to from long to wide format
  wide_df <- tidyr::pivot_wider(
    data = df_subset,
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


######################

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

  ### Define a function for getSymPrice purpose as getSymPriceIntervalDate cannot be vectorized over dates
  getSymPriceOne <- function(one_sym, one_date, metric) {
    ### First case - requested date is an holiday or requested date is not today
    ### Get last close price in this case
    ### Take prices list 5 days before one_date to be sure to grasp at least one business day among these 5 days
    if (one_date == Sys.Date() - 1) prices_list <- getSymMetricIntervalDate(one_sym, one_date - 5, one_date + 1, metric)
    else prices_list <- getSymMetricIntervalDate(one_sym, one_date - 5, one_date + 2, metric)

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
## print(l_prices)
## Other possible return value types: as dor now it is a simple vector of double
## xts::xts(l_prices, order.by = report_date)
## data.frame(l_prices, row.names = NULL)
}


######################

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
#'@export
getLastSymPrice <- function(sym) {
  data = getSymIntervalDate(sym, from_date=Sys.Date()-5)

  # Subset and rename columns
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


###
#' getLastAdjustedPrice
#'
#' This function takes one ticker (or a vector of tickers) as input and returns the last available adjusted value from Yahoo service.
#'
#' It calls \code{getLastSymPrice} and returns value column from this data frame.
#' If ticker is NA or NULL or equal to All or STOCK, it returns NA also
#'
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of values (rounded to 2 decimals) corresponding to last values of tickers
#'@examples getLastAdjustedPrice("SPY")
#'@export
getLastAdjustedPrice = function(ticker) {
  if (length(ticker) == 0) return(NA)
  if ((length(ticker) == 1) && (is.na(ticker) || ticker %in% c("","All","STOCK") )) return(NA)

  getLastSymPrice(ticker)$value

}

###
#' getLastPriceDate
#'
#' This function takes one ticker (or a vector of tickers) as input and returns from Yahoo service the last available
#' date with an available price - see also \code{getLastAdjustedPrice}.
#'
#' It calls \code{getLastSymPrice} and returns date column from this data frame.
#' If ticker is NA or NULL or equal to All or STOCK, it returns NA also
#'
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of dates for each ticker. It calls \code{getSymFromDate} to get the dates.
#'@examples getLastPriceDate("SPY")
#'@export
getLastPriceDate = function(ticker) {
  if (length(ticker) == 0) return(as.Date(NA))
  if ((length(ticker) == 1) && (is.na(ticker) || ticker %in% c("","All","STOCK") )) return(as.Date(NA))

  getLastSymPrice(ticker)$date
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
      ticker %in% c("", "All", "STOCK")) return(list(last = NA, change = NA))

  tryCatch({
    ticks = getSymIntervalDate(ticker, Sys.Date() - 5) ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    last_data = ticks[nrow(ticks), "Adjusted"]
    p_last_data = ticks[nrow(ticks) - 1, "Adjusted"]
    return(list(
      last = round(last_data, 2),
      change = scales::label_percent(accuracy = 0.01)(Tbasics::change(p_last_data, last_data))
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last = NA, change = NA))
  })
}

#### Used by Gonet.R script and RAnalysis
###
#'getStockPrice
#'
#'For a given ticker or a vector of tickers, this function returns either the last close price from Yahoo service or the last stored price.
#'
#'This will depend upon close parameter. If close is TRUE then Yahoo service is used, otherwise data is retrieved from prices DB
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
getStockPrice = function(sym, close = FALSE) {
  message("getStockPrice")

  if (close) {
    line <- data.frame(
      datetime = paste(format(Sys.Date() - 1,"%Y%m%d"), "22:00:00"),
      sym = sym,
      price = getLastAdjustedPrice(sym)
    )
  }

  ### Just retrieve last price from DB but no update from DB
  else {
    mydb <- DBI::dbConnect(RSQLite::SQLite(),  config::get("DB"))
    line <- DBI::dbGetQuery(mydb, "SELECT * FROM Prices
                          WHERE sym = ? ORDER BY ROWID DESC LIMIT 1;", params = list(sym))
    DBI::dbDisconnect(mydb)
  }

  return(line)
}

#### Used by Gonet.R script and RAnalysis
###
#'getIBKRPrice
#'
#'Retrieves a price or a list of price from IBKR and
#'returns a data frame with date and time (formatted), the list of tickers and corresponding prices, equal to Nan if no price found.
#'Additionally it stores prices in DB Prices table (only if price different from NaN and not a close price)
#'
#'For a given ticker or a list of tickers, this function returns prices from IBKR, using also Tickers table from DB within Python code called.
#'If IBKR service is not available, it will return an error code and display an error message: 0 if no IBKR service, -1 if contract is unknown.
#' if unknown then function returns -1
#'@param sym string or vector of strings- IBKR style of ticker, if unknown then function returns -1
#'@param reqType integer - should be either 2 or 4, default is 2
#'@param close Boolean TRUE/FALSE, default is FALSE. if TRUE then retrieve last close price from IBKR, if FALSE then retrieve market price from IBKR
#'@param verbose Boolean TRUE/FALSE, default is FALSE. if TRUE then print result of retrieved data in IBKR
#'@returns a data frame with the following fields: \code{datetime} (formatted date and time), \code{sym} (ticker name), \code{price} (price as double)
#'@examples
#'\dontrun{
#'getIBKRPrice("SPY")
#'getIBKRPrice(c("ESTX50", "USO", "ABBN"))
#'getIBKRPrice("USO", close=TRUE)
#'}
#'@export
getIBKRPrice <- function(sym, reqType=2, close=FALSE, verbose = FALSE) {

  ### This will work even if list_sym is a vector and not the other IBKR contract components
  IBKRPrice <- reticulate::py$getValue(sym, reqType, close)

  ### Error case do not go further on###
  if (length(IBKRPrice) == 1) {
    Tbasics::display_message("IBKR data retrieval did not work - either no connection or contract does not exist")
    return (IBKRPrice)
  }

  if (verbose) print(IBKRPrice)

  ### Last close price should not be stored as date and time will be wrong (IBKR returned last day close data...)
  ### Also only prices that are different from NaN will be stored in DB
  if (!close) {
    ### Remove all empty prices if any, print resulting data
    StoredIBKRPrice <- IBKRPrice[!is.nan(IBKRPrice$price),]

    myconn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    DBI::dbAppendTable(myconn, "Prices", StoredIBKRPrice)
    DBI::dbDisconnect(myconn)
  }

  ### Available readily if needed
  return(IBKRPrice)
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
  val = reticulate::py$getOptValue(sym=sym,expiration=expiration,strikes=strike,
                                 right=right)$value

  if (is.null(val) || is.nan(val)) val=-1
  return(val)
}

#' Enhanced Yahoo Finance service check with retry logic
#'
#' @param tickers Data frame from getTickers() containing YahooName column
#' @param sample_size Number of tickers to test
#' @param asset_coverage Ensure coverage across different asset types if sampling
#' @param timeout Seconds to wait before timeout
#' @param retries Number of retry attempts for each ticker
#' @param retry_delay Seconds to wait between retries
#' @return List with status and detailed diagnostics
#' @export
check_yahoo_service_multi <- function(tickers, sample_size = 8, asset_coverage = TRUE,
                                      timeout = 5, retries = 2, retry_delay = 1) {
  # Ensure we have the right data format
  if (!("YahooName" %in% colnames(tickers))) {
    stop("Input must be a data frame with 'YahooName' column")
  }

  # Extract unique ticker symbols
  yahoo_tickers <- unique(tickers$YahooName)

  # If we're sampling and want asset coverage, try to get diverse instruments
  test_tickers <- yahoo_tickers
  if (length(yahoo_tickers) > sample_size) {
    if (asset_coverage && "Type" %in% colnames(tickers)) {
      # Ensure we sample across different asset types
      types <- unique(tickers$Type)
      test_tickers <- c()

      # Take at least one from each type, then distribute remaining slots
      for (type in types) {
        type_tickers <- tickers$YahooName[tickers$Type == type]
        sample_count <- max(1, round(sample_size * length(type_tickers) / nrow(tickers)))
        sample_count <- min(sample_count, length(type_tickers))

        if (length(type_tickers) > 0) {
          test_tickers <- c(test_tickers, sample(type_tickers, sample_count))
        }
      }

      # Ensure we don't exceed sample size
      if (length(test_tickers) > sample_size) {
        test_tickers <- sample(test_tickers, sample_size)
      }
    } else {
      # Simple random sampling if we don't need asset coverage
      test_tickers <- sample(yahoo_tickers, sample_size)
    }
  }

  # Always include indices with special characters like ^STOXX50E
  # as these often cause problems
  special_chars_tickers <- grep("^\\^", yahoo_tickers, value = TRUE)
  if (length(special_chars_tickers) > 0 &&
      !any(special_chars_tickers %in% test_tickers)) {
    # Replace one random ticker with a special char ticker
    if (length(test_tickers) > 0) {
      test_tickers[sample(length(test_tickers), 1)] <- sample(special_chars_tickers, 1)
    } else {
      test_tickers <- sample(special_chars_tickers, 1)
    }
  }

  # Set timeout temporarily
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = timeout)

  # Test each ticker with retry logic
  results <- lapply(test_tickers, function(ticker) {
    for (attempt in 1:retries) {
      result <- tryCatch({
        # Use a shorter time window to minimize data transfer
        test_data <- quantmod::getSymbols(ticker,
                                          src = "yahoo",
                                          from = Sys.Date() - 3,
                                          to = Sys.Date(),
                                          auto.assign = FALSE,
                                          warnings = FALSE)

        # Perform basic validation on returned data
        if (is.null(test_data) || nrow(test_data) == 0) {
          return(list(ticker = ticker, status = FALSE,
                      message = "Empty data returned", attempt = attempt))
        }

        return(list(ticker = ticker, status = TRUE,
                    message = "OK", attempt = attempt))
      },
      error = function(e) {
        err_msg <- as.character(e)
        return(list(ticker = ticker, status = FALSE,
                    message = err_msg, attempt = attempt))
      })

      # If successful, return result
      if (result$status) {
        return(result)
      }

      # Otherwise, wait and retry
      if (attempt < retries) {
        Sys.sleep(retry_delay)
      }
    }

    # Return the last failed result if all attempts failed
    return(result)
  })

  # Analyze results
  working_tickers <- sapply(results, function(x) x$status)

  # Calculate success metrics
  success_rate <- sum(working_tickers) / length(working_tickers)
  failing_tickers <- sapply(results[!working_tickers], function(x) x$ticker)

  # Categorize failures if possible
  failure_patterns <- list()
  for (result in results[!working_tickers]) {
    if (!is.null(result$message)) {
      # Extract error pattern
      if (grepl("404", result$message)) {
        pattern <- "404 Not Found"
      } else if (grepl("401", result$message)) {
        pattern <- "401 Unauthorized"
      } else if (grepl("time", result$message, ignore.case = TRUE)) {
        pattern <- "Timeout"
      } else if (grepl("SSL", result$message)) {
        pattern <- "SSL/TLS Error"
      } else if (grepl("Empty", result$message)) {
        pattern <- "Empty Data"
      } else {
        pattern <- "Other"
      }

      # Add to the appropriate category
      if (is.null(failure_patterns[[pattern]])) {
        failure_patterns[[pattern]] <- c(result$ticker)
      } else {
        failure_patterns[[pattern]] <- c(failure_patterns[[pattern]], result$ticker)
      }
    }
  }

  # Determine overall status based on success rate
  overall_status <- success_rate >= 0.7  # 70% threshold for "service working"

  return(list(
    status = overall_status,
    message = sprintf("%.1f%% of tested tickers accessible (%d/%d)",
                      success_rate * 100, sum(working_tickers), length(working_tickers)),
    failing_tickers = failing_tickers,
    failure_patterns = failure_patterns,
    details = results
  ))
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
#' @param verbose Whether to print progress messages
#' @return Data frame with columns: date, ticker, Open, High, Low, Close, Adjusted, and Volume
#' @export
getYahooDataRobust <- function(tickers, from_date, to_date = Sys.Date(),
                               max_retries = 5, retry_delay = 2,
                               timeout = 10, chunk_size = 5,
                               verbose = FALSE) {

  # Process input to get ticker names
  if (is.data.frame(tickers) && "YahooName" %in% colnames(tickers)) {
    ticker_names <- tickers$YahooName
  } else if (is.character(tickers)) {
    ticker_names <- tickers
  } else {
    stop("Tickers must be either a character vector or a data frame with YahooName column")
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
    if (verbose) {
      cat(sprintf("Processing chunk %d of %d (%d tickers)\n",
                  chunk_idx, length(ticker_chunks), length(chunk)))
    }

    # Process each ticker in the chunk
    for (ticker in chunk) {
      if (verbose) {
        cat(sprintf("  Fetching %s... ", ticker))
      }

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
          # Test special ticker values
          if ((ticker == "All") | (ticker == "STOCK")) ticker_data = NA

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
          if (verbose) cat("OK\n")
          break  # Exit retry loop
        },
        error = function(e) {
          last_error <- e
          if (attempt < max_retries) {
            if (verbose) cat(sprintf("attempt %d failed, retrying... ", attempt))
            Sys.sleep(retry_delay * attempt)  # Exponential backoff
          } else {
            if (verbose) cat("FAILED\n")
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
  if (verbose) {
    success_rate <- (length(ticker_names) - length(failed_tickers)) / length(ticker_names)
    cat(sprintf("\nRetrieved data for %.1f%% of tickers (%d/%d)\n",
                success_rate * 100,
                length(ticker_names) - length(failed_tickers),
                length(ticker_names)))

    if (length(failed_tickers) > 0) {
      cat("Failed tickers: ", paste(failed_tickers, collapse=", "), "\n")
    }
  }

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
