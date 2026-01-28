#### CURRENCY management

#' getActiveCurrencies
#'
#' This function retrieves all active currencies in DB.
#'
#' It looks into Currencies table and requests all corresponding records.
#'
#' N.B: This will not work for currencies that have been used in the past, but are not active any more.
#'@returns a character vector with all active currencies names
#'@examples
#'\dontrun{
#'getActiveCurrencies()
#'}
#'@export
getActiveCurrencies <- function(currency) {

  ### Look at DB
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  currency_list <- DBI::dbGetQuery(conn, "SELECT Name FROM Currencies WHERE Active = 'Yes'")
  return(currency_list$Name)
}



#' getCurrencyAttrib
#'
#' This function retrieves all attributes of a given currency from DB.
#' This function can be vectorized.
#'
#' It looks into Currencies table and requests
#' the corresponding record.
#'@param currency string or vector of strings - possible values are EUR, CHF, USD...
#'@returns a data frame with \code{
#' "Name", "DirectConversion","IBKRPair","Active","Display","ir1week","ir1month","ir3months",
#' "ir6months","ir1year","ir2years","last_ir_update"
#'}
#'@examples
#'\dontrun{
#'getCurrencyAttrib("USD")
#'getCurrencyAttrib(c("EUR", "CHF"))
#'getCurrencyAttrib("CAD")
#'getCurrencyAttrib("CHF")
#'}
#'@export
getCurrencyAttrib <- function(currency) {

  ### Look at DB
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  currency_detail <- DBI::dbGetQuery(conn, "SELECT * FROM Currencies WHERE Name = ?", params=list(currency))
  return(currency_detail)

}



#' getAllCurrenciesCHFValues
#'
#' Internal function to retrieve all currency values in CHF from DB
getAllCurrenciesCHFValues <- function() {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  all_values <- DBI::dbReadTable(conn, "ConvertToCHF")
  return(all_values)
}

#' getAllCurrenciesUSDValues
#'
#' Internal function to retrieve all currency values in USD from DB
getAllCurrenciesUSDValues = function() {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  all_values <- DBI::dbReadTable(conn, "ConvertToUSD")
  return(all_values)
}

#' getStoredCHFValue
#'
#' Retrieves last CHF value of given currency from DB (vectorized)
#'
#' @param currency string or character vector - EUR, USD, CAD...
#' @returns data frame with date, currency, chf_value fields
#' @export
getStoredCHFValue <- function(currency) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  placeholders <- paste(rep("?", length(currency)), collapse = ",")

  # Add CHF to union only if requested
  ## Notice it is a constant string returned by SELECT
  chf_union <- if("CHF" %in% currency) {
    sprintf("SELECT 'CHF' as currency, %s as date, 1.0 as chf_value UNION ALL",
            as.numeric(format(Sys.Date(),"%Y%m%d")))
  } else ""

  query <- sprintf("
    SELECT max(date) as date, currency, chf_value
    FROM (
      %s
      SELECT currency, date, chf_value FROM ConvertToCHF WHERE currency IN (%s)
    )
    GROUP BY currency",
    chf_union,
    placeholders
  )

  result <- DBI::dbGetQuery(conn, query, params = as.list(currency))
  return(result)
}

#'  getStoredUSDValue
#'
#' This function retrieves last USD value of a given currency from DB. This function can be vectorized.
#'
#' It looks into ConvertToUSD table and requests
#' the last record. It does not try to retrieve more up to date value from IBKR or other sources.
#' See \code{getIBKR()} to get more up to date data from IBKR
#'@param currency string or character vector - possible values are EUR, CHF, USD...
#'@returns a data frame with \code{date, currency, usd_value} fields,
#'where \code{date} is an integer format of YYYYMMDD and \code{usd_value} a numeric.
#'@examples
#'\dontrun{
#'getStoredUSDValue("USD")
#'getStoredUSDValue(c("EUR", "CHF"))
#'getStoredUSDValue("CAD")
#'getStoredUSDValue("CHF")
#'}
#'@export
getStoredUSDValue = function(currency) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  placeholders <- paste(rep("?", length(currency)), collapse = ",")

  # Add USD to union only if requested
  usd_union <- if("USD" %in% currency) {
    sprintf("SELECT 'USD' as currency, %s as date, 1.0 as usd_value UNION ALL",
            as.numeric(format(Sys.Date(),"%Y%m%d")))
  } else ""

  query <- sprintf("
    SELECT max(date) as date,  currency, usd_value
    FROM (
      %s
      SELECT currency, date,  usd_value FROM ConvertToUSD WHERE currency IN (%s)
    )
    GROUP BY currency",
    usd_union, placeholders
  )

  result <- DBI::dbGetQuery(conn, query, params = as.list(currency))
  return(result)
}

#' getLastCHFValue
#'
#' Returns converted value of currency to CHF, updating from Yahoo if needed
#'
#' @param currency string or character vector - EUR, USD, CAD...
#' @returns data frame with date, chf_value fields
#' @export
getLastCHFValue <- function(currency) {

  # Handle CHF currency separately - don't fetch from Yahoo for CHF
  chf_currencies <- currency[currency == "CHF"]
  other_currencies <- currency[currency != "CHF"]

  result <- data.frame()

  # For CHF, return 1.0 directly
  if (length(chf_currencies) > 0) {
    chf_result <- data.frame(
      date = as.integer(format(Sys.Date(), "%Y%m%d")),
      currency = "CHF",
      chf_value = 1.0
    )
    result <- rbind(result, chf_result)
  }

  # Process other currencies normally
  if (length(other_currencies) > 0) {
    Tbasics::display_message("Retrieve currencies from DB...")
    conn <- safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    # Get currency details including DirectConversion flag
    placeholders <- paste(rep("?", length(other_currencies)), collapse = ",")
    query <- sprintf("
      SELECT Name, YahooName, DirectConversion FROM Currencies
      WHERE Name IN (%s)", placeholders)
    currency_detail <- DBI::dbGetQuery(conn, query, params = as.list(other_currencies))

    if (nrow(currency_detail) == 0){
      Tbasics::display_error_message(paste0(other_currencies, " currency undefined, not able to retrieve from Yahoo!"))
      return(data.frame(date = as.Date(""), chf_value = NA))
    }

    # Create Yahoo tickers for CHF pairs - handle direct vs cross rates
    yahoo_tickers <- purrr::map_chr(1:nrow(currency_detail), function(i) {
      curr <- currency_detail$Name[i]

      if (curr == "EUR") {
        return("EURCHF=X")  # Direct EUR/CHF pair
      } else if (curr == "USD") {
        return("CHFUSD=X")  # CHF/USD pair (will invert)
      } else {
        return(currency_detail$YahooName[i])  # Use original ticker for cross-rate
      }
    })

    # Get Yahoo data for CHF pairs and cross-rates
    price_list <- getYahooData(yahoo_tickers, from_date = Sys.Date() - 3)

    if (nrow(price_list) == 0) {
      logger::log_info("No currency data found!", namespace="Tdata")
      return(data.frame(date = as.Date(""), chf_value = NA))
    }

    # Get latest price per ticker
    last_price <- price_list |>
      dplyr::group_by(ticker) |>
      dplyr::slice_max(date, n = 1) |>
      dplyr::select(date, ticker, Adjusted) |>
      dplyr::ungroup()

    # Map back to currencies and calculate CHF values
    result_prices <- data.frame()

    for (i in 1:nrow(currency_detail)) {
      curr <- currency_detail$Name[i]
      expected_ticker <- yahoo_tickers[i]

      ticker_data <- last_price |> dplyr::filter(ticker == expected_ticker)

      if (nrow(ticker_data) == 0) {
        logger::log_warn("No data for {curr}", namespace="Tdata")
        next
      }

      chf_value <- if (curr == "EUR") {
        ticker_data$Adjusted  # Direct EUR/CHF rate
      } else if (curr == "USD") {
        1 / ticker_data$Adjusted  # Invert CHF/USD to get USD/CHF
      } else {
        # For other currencies, need cross-rate via USD
        # Get USD/CHF rate for cross-calculation
        usd_chf_data <- last_price |> dplyr::filter(ticker == "CHFUSD=X")
        if (nrow(usd_chf_data) > 0) {
          ticker_data$Adjusted / usd_chf_data$Adjusted  # Currency/USD divided by CHF/USD
        } else {
          NA  # Cannot calculate without USD/CHF rate
        }
      }

      if (!is.na(chf_value)) {
        result_prices <- rbind(result_prices, data.frame(
          date = as.integer(format(ticker_data$date, "%Y%m%d")),
          currency = curr,
          chf_value = round(chf_value, 4)
        ))
      }
    }

    # Check for updates needed
    if (nrow(result_prices) > 0) {
      stored_values <- getStoredCHFValue(result_prices$currency)
      stored_dates <- setNames(stored_values$date, stored_values$currency)

      updates_needed <- result_prices |>
        dplyr::filter(date > stored_dates[currency] | is.na(stored_dates[currency]))

      # Update DB if needed
      if(nrow(updates_needed) > 0) {
        safe_db_append(conn, "ConvertToCHF", updates_needed)
        logger::log_info("Updated {nrow(updates_needed)} CHF currency rates", namespace="Tdata")
      } else {
        logger::log_info("No CHF updates needed - stored data is current", namespace="Tdata")
      }
    }

    # Return latest values from DB for other currencies
    placeholders <- paste(rep("?", length(other_currencies)), collapse = ", ")
    query <- sprintf("
      SELECT max(date) as date, currency, chf_value
      FROM ConvertToCHF
      WHERE currency IN (%s)
      GROUP BY currency",
                     placeholders
    )

    other_result <- DBI::dbGetQuery(conn, query, params = as.list(other_currencies))
    result <- rbind(result, other_result)
  }

  return(result)
}

#'  getLastUSDValue
#'
#' This function returns converted value of a given currency to USD
#' by trying to get from Yahoo the latest available value and by default
#' returning last value available in DB.
#'
#'
#' It looks into ConvertToUSD table and retrieves also last available value from Yahoo.
#' If Yahoo value is different from NA and if it is are more recent than stored value, then
#' it will update ConvertToUSD table in DB. It returns the stored value anyhow.
#'
#' It does not try to retrieve more up to date value from IBKR.
#'@param currency string or character vector - possible values are EUR, CHF,...
#'Any USD value from input will trigger an error message
#'@returns a data frame with \code{date, usd_value} fields,
#'where \code{date} is an integer format of YYYYMMDD and \code{usd_value} a numeric.
#'@examples
#'\dontrun{
#'getLastUSDValue("EUR")
#'getLastUSDValue(c("CAD", "EUR"))
#'getLastUSDValue("CHF")
#'}
#'@export
getLastUSDValue = function(currency) {

  if (any(currency == "USD")) {
    Tbasics::display_error_message("There cannot be USD value returned by getLastUSDValue if currency is already USD !!")
    return(NA)
  }

  Tbasics::display_message("Retrieve currencies from DB...")
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  # Create placeholders for IN clause based on vector length
  placeholders <- paste(rep("?", length(currency)), collapse = ",")
  query <- sprintf("
    SELECT Name, YahooName, DirectConversion FROM Currencies
    WHERE Name IN (%s)", placeholders)
  currency_detail <- DBI::dbGetQuery(conn, query, params=as.list(currency))

  if (nrow(currency_detail) == 0){
    Tbasics::display_error_message(paste0(currency, " currency undefined, not able to retrieve in Yahoo !!"))
  }

  ### Direct call to YahooData to get last value as there is no known ticker
  ### 3 last days returned
  price_list <- getYahooData(currency_detail$YahooName, from_date=Sys.Date()-3)
  last_nr = nrow(price_list)

  if ((last_nr == 0) | all(is.na(price_list$Adjusted))) {
    logger::log_info("No currency data for {currency} found !", namespace="Tdata")
    return(data.frame(date = as.Date(""), value = NA))
  }

  ### There is at least one value returned per currency
  last_price <- price_list |>
    dplyr::group_by(ticker) |>
    dplyr::slice_max(date, n = 1) |>
    dplyr::select(date, currency=ticker, Adjusted) |>
    dplyr::ungroup()

  ### COnvert Yahoo names back to currency names
  last_price$currency <- currency_detail$Name[match(last_price$currency, currency_detail$YahooName)]

  ### convert date to integer to compare more easily
  last_price$date <- as.integer(format(max(last_price$date),"%Y%m%d"))

  ### Get last record from CurrencyPairs table
  stored_values <- getStoredUSDValue(last_price$currency)

  # Create lookup vectors for efficient comparison
  stored_dates <- setNames(stored_values$date, stored_values$currency)

  # Identify updates needed without join
  updates_needed <- last_price |>
    dplyr::filter(date > stored_dates[currency] | is.na(stored_dates[currency])) |>
    dplyr::mutate(usd_value = round(Adjusted, 4)) |>
    dplyr::select(date, currency, usd_value)

  # Insert/update records if any updates needed
  if(nrow(updates_needed) > 0) {
    safe_db_append(conn, "ConvertToUSD", updates_needed)
    logger::log_info("Updated {nrow(updates_needed)} currency rates", namespace="Tdata")
  }

  else {
    logger::log_info("No updates needed - stored data is current", namespace="Tdata")
  }

  ### DB is now up to date using Yahoo data - retrieve last values
  placeholders <- paste(rep("?", length(currency)), collapse = ", ")
  query <- sprintf("
    SELECT max(date) as date, currency, usd_value
    FROM ConvertToUSD
    WHERE currency IN (%s)
    GROUP BY currency",
  placeholders
  )

  result <- DBI::dbGetQuery(conn, query, params = as.list(currency))
  return(result)
}

#'  currency_sign
#'
#' This function returns display sign for a given currency
#'
#' It looks into Currencies table from DB and then returns corresponding currency sign in table.
#'@param currency a string or a vector of strings - possible values are EUR, CHF,...
#'@returns a character or a vector of characters
#'@examples
#'\dontrun{
#'currency_sign("USD")
#'currency_sign("EUR")
#'currency_sign("CAD")
#'currency_sign(c("CHF", "USD"))
#'}
#'@export
currency_sign <- function(currency) {

  ### Open connection to DB
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  # Create temporary table with currency (preserving duplicates)
  temp_df <- data.frame(requested_currency = currency)
  DBI::dbWriteTable(conn, "temp_requests", temp_df, temporary = TRUE)

  # JOIN to get duplicates preserved
  currency_detail <- DBI::dbGetQuery(conn, "
  SELECT c.*
  FROM temp_requests t
  INNER JOIN Currencies c ON t.requested_currency = c.Name
")

  return(currency_detail$Display)
}

#'  base_currency_format
#'
#' This function returns a string with converted amount into base currency and base currency symbol
#'
#' It defines a labeling functions for base currency.
#'
#' Currency symbols (CHF, $) are also taken into consideration.
#'
#' Only 2 digits after decimal are displayed. Big mark (3 digits separator) equal to space.

#'@param amount is the number to be converted, can be also a character if convertible into a number
#'@examples
#'\dontrun{
#'base_currency_format(100.45)
#'base_currency_format(10000)
#'base_currency_format(1000)
#'base_currency_format(758.458)
#'base_currency_format(100000.455)
#'base_currency_format(c(100,40))
#'}
#'@export
base_currency_format <- function(amount) {
  tryCatch(
    {
      ## Try to convert to numeric if not already the case
      amount <- as.numeric(amount)

      currency_sign <- currency_sign(getParam("BaseCurrency"))

      ### Compute display functions based upon currency signs
      display_currency <- scales::label_currency(
          prefix = "",
          suffix = paste0(" ", currency_sign), #### EURO sign = \u20ac", ...
          big.mark = " ",
          accuracy=0.01
      )

      display_currency(amount)
    },

    error = function(cond) {
      logger::log_error("Currency formatting error: {cond}", namespace="Tdata")
      NA
    },

    warning = function(cond) {
      logger::log_warn("Currency formatting warning: {cond}", namespace="Tdata")
      NA
    }
  )
}


#'  currency_format
#'
#' This function returns a string with amount and currency symbol
#'
#' It defines a labeling functions for each currency.
#'
#' Currency symbols (for EUR and USD) are also taken into consideration.
#'
#' Only 2 digits after decimal are displayed. Big mark (3 digits separator) equal to space.

#'@param currency a string whose value has to be defined in Currencies table from DB
#'@param amount is the number to be displayed, can be also a character if convertible into a number
#'If length(currency) is 1, then it is recycled
#'@examples
#'\dontrun{
#'currency_format(100.45,"EUR")
#'currency_format(10000,"CHF")
#'currency_format(1000,"CAD")
#'currency_format(758.458,"USD")
#'currency_format(100000.455,"EUR")
#'currency_format(c(100,40),c("EUR","USD"))
#'currency_format(c(100,40),"EUR")
#'}
#'@export
currency_format = function(amount, currency, accuracy = 0.01){
  #Returns the amount values formatted with their respective currency sign, based on the currency argument
  ## Amounts are rounded to accuracy (default 0.01)

  ### If length currency equals 1 and length arguments differ
  ### then recycled otherwise error is raised
  tryCatch(
    {
      ## Try to convert to numeric if not already the case
      amount <- as.numeric(amount)

      ### Retrieve currency signs in one single shot
      ### Duplicate signs if necessary for each currency
      currency_signs <- currency_sign(currency)

      ### Compute display functions based upon currency signs
      display_currencies <- purrr::map(currency_signs, \(curr) {
        scales::label_currency(
          prefix = "",
          suffix = paste0(" ", curr), #### EURO sign = \u20ac"
          big.mark = " ",
          accuracy = accuracy
        )
      })

      ### apply these functions on amount and return this value
      purrr::map2_chr(amount, display_currencies, \(am, disp){disp(am)})
    },

    error = function(cond) {
     logger::log_error("Currency formatting error: {cond}", namespace="Tdata")
      NA
    },

    warning = function(cond) {
      logger::log_warn("Currency formatting warning: {cond}", namespace="Tdata")
      NA
    }
  )

}

#' c_to_base
#'
#' Converts amount of currency into base currency using stored DB values. Wrapper for USD or CHF base currencies.
#'
#' @param amount numeric or vector - amounts to convert
#' @param currency string or vector - currency codes (EUR, USD, CAD...)
#' @return double, amount in base currency
#' @examples
#'\dontrun{
#' c_to_base(100.45,"EUR")
#' c_to_base(c(10000,500),c("CHF","EUR"))
#' c_to_base(c(750.543,10),c("USD","EUR"))
#' c_to_base(c(500,10),c("CAD","EUR"))
#' }
#' @export
c_to_base <- function(amount, currency) {
  base_currency <- getParam("BaseCurrency")
  if (base_currency == "CHF") return(c_to_chf(amount, currency))
  else if (base_currency == "USD") return(c_to_usd(amount, currency))
  else Tbasics::display_message("Wrong base currency in DB!!!")
  return(NA)
}

#' c_to_chf
#'
#' Converts amount of currency into CHF using stored DB values
#' It merely performs a multiplication of the amount by currency pair value, using \link{getStoredCHFValue}.
#' This function can be vectorized
#'
#' @param amount numeric or vector - amounts to convert
#' @param currency string or vector - currency codes (EUR, USD, CAD...)
#' @return double, amount in CHF
#' @export
c_to_chf <- function(amount, currency) {
  data <- data.frame(am = amount, cur = currency)
  data <- dplyr::mutate(data, res = am * getStoredCHFValue(cur)$chf_value)
  return(data$res)
}

#'  c_to_usd
#'
#' This function converts the amount of currency into USD, using currency pairs values stored in DB
#'
#' It merely performs a multiplication of the amount by currency pair value, using \link{getStoredUSDValue}.
#' This function can be vectorized
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@return double, amount in USD
#'@examples
#'\dontrun{
#'c_to_usd(100.45,"EUR")
#'c_to_usd(c(10000,500),c("CHF","EUR"))
#'c_to_usd(c(750.543,10),c("USD","EUR"))
#'c_to_usd(c(500,10),c("CAD","EUR"))
#'}
#'@export
c_to_usd <- function(amount, currency) {
  data <- data.frame(am=amount, cur=currency)
  data <- dplyr::mutate(data, res = am*getStoredUSDValue(cur)$usd_value)
  return(data$res)
}


#' convert_to_base_date
#'
#' Converts currency amounts to base currency for specific date. Wrapper for USD or CHF base currencies.
#'
#' @param amount numeric or vector - amounts to convert
#' @param currency string or vector - currency codes
#' @param convert_date date, character, or numeric - conversion date
#' @export
convert_to_base_date <- function(amount, currency, convert_date = Sys.Date()) {
  base_currency <- getParam("BaseCurrency")
  if (base_currency == "CHF") return(convert_to_chf_date(amount, currency, convert_date))
  else if (base_currency == "USD") return(convert_to_usd_date(amount, currency, convert_date))
  else Tbasics::display_message("Wrong base currency in DB!!!")
  return(NA)
}


#' convert_to_chf_date
#'
#' Converts currency amounts to CHF for specific date(s). Can be vectorized using \link{vctrs}, i.e. tidyverse rules.
#'
#' @param amount numeric or vector - amounts to convert
#' @param currency string or vector - currency codes
#' @param convert_date date, character, or numeric - conversion date
#' @export
convert_to_chf_date <- function(amount, currency, convert_date = Sys.Date()) {

  # Apply vctrs recycling rules to inputs
  recycled <- vctrs::vec_recycle_common(amount = amount,
                                        currency = currency,
                                        convert_date = convert_date)

  amount <- recycled$amount
  currency <- recycled$currency
  convert_date <- recycled$convert_date

  # Convert dates to numeric format - handle each element individually
  numeric_date <- vapply(convert_date, function(x) {
    if (is.numeric(x)) {
      as.numeric(x)  # Already numeric, use as-is
    } else if (inherits(x, "Date")) {
      as.numeric(format(x, "%Y%m%d"))  # Convert Date to YYYYMMDD
    } else if (is.character(x)) {
      as.numeric(x)  # Convert character to numeric
    } else {
      NA_real_
    }
  }, FUN.VALUE = numeric(1))

  # Get unique currency/date combinations for efficient querying
  unique_lookups <- unique(data.frame(currency = currency, date = numeric_date))

  # Handle empty case
  if (nrow(unique_lookups) == 0) {
    return(numeric(0))
  }

  logger::log_debug("Unique lookups: {nrow(unique_lookups)} rows", namespace="Tdata")

  # Connect to database and execute optimized SQL query
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))  # Ensure connection cleanup

  # Alternative approach: Use temp table which is more reliable
  temp_table_name <- paste0("temp_lookup_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  DBI::dbWriteTable(conn, temp_table_name, unique_lookups, temporary = TRUE)

  logger::log_debug("Created temp table: {temp_table_name}", namespace="Tdata")

  # Use simple query with temp table
  query <- sprintf("
    WITH ranked_rates AS (
      SELECT
        l.currency,
        l.date as lookup_date,
        c.chf_value,
        ROW_NUMBER() OVER (
          PARTITION BY l.currency, l.date
          ORDER BY ABS(c.date - l.date), c.date
        ) as rn
      FROM %s l
      JOIN ConvertToCHF c ON c.currency = l.currency
    )
    SELECT currency, lookup_date, chf_value
    FROM ranked_rates
    WHERE rn = 1",
                   temp_table_name)

  logger::log_debug("Generated query: {query}", namespace="Tdata")

  chf_rates <- DBI::dbGetQuery(conn, query)

  # Create lookup table with CHF hardcoded
  lookup_table <- chf_rates |>
    dplyr::bind_rows(
      data.frame(currency = "CHF",
                 lookup_date = unique(numeric_date),
                 chf_value = 1.0)  # CHF to CHF rate
    )

  # Join rates back to original vectors
  result_df <- data.frame(currency = currency, date = numeric_date, amount = amount) |>
    dplyr::left_join(lookup_table, by = c("currency", "date" = "lookup_date"))

  # DEFENSIVE: Check data types before multiplication
  if (!is.numeric(result_df$amount)) {
    logger::log_error("result_df$amount is not numeric! Class: {class(result_df$amount)}, Values: {paste(head(result_df$amount), collapse=', ')}", namespace="Tdata")
    stop("result_df$amount must be numeric but got ", class(result_df$amount))
  }
  if (!is.numeric(result_df$chf_value)) {
    logger::log_error("result_df$chf_value is not numeric! Class: {class(result_df$chf_value)}, Values: {paste(head(result_df$chf_value), collapse=', ')}", namespace="Tdata")
    stop("result_df$chf_value must be numeric but got ", class(result_df$chf_value))
  }

  # Return vectorized conversion maintaining input order
  return(as.numeric(result_df$amount * result_df$chf_value))
}
#'  convert_to_usd_date
#'
#' This function converts the amount of currency into USD, using CAD, EUR and CHF currency pairs values for a given date
#'
#' First it loads all currency pairs that have been stored for a while, then looks up for the nearest date in the CurrencyPairs table, compared with input date.
#' It retrieves the EUR and CHF corresponding values.
#'
#' Last it calls the convert_to_usd function.
#'
#' This function can be vectorized for \code{amount}, \code{currency}, and \code{date}.
#'@param amount is the number to be converted
#'@param currency is a string whose value is either EUR, CHF, CAD, etc..
#'@param convert_date can be a date, or character, or integer(numeric). By default it is today.
#'If type is character/date, then \code{convert_date} argument will be first converted to an integer type with Y/M/D format,
#'this is the format in CurrencyPairs DB table
#'@keywords currency trading
#'@examples
#'\dontrun{
#'convert_to_usd_date(100.45,"EUR",as.Date("2023-10-15"))
#'convert_to_usd_date(200, "CHF", 20240421)
#'convert_to_usd_date(200, "EUR", "20240421")
#'convert_to_usd_date(200, "CAD")
#'convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09"))
#'convert_to_usd_date(c(750.543,10),c("USD","EUR"),as.Date("2023-12-03"))
#'convert_to_usd_date(c(750.543,10),"EUR",as.Date("2023-12-03"))
#'convert_to_usd_date(10,c("CHF","EUR"),c(as.Date("2025-06-03"), Sys.Date()))
#'convert_to_usd_date(10,"CHF",c(as.Date("2025-06-03"), Sys.Date()))
#'}
#'@export
convert_to_usd_date <- function(amount, currency, convert_date = Sys.Date()) {

  # Apply vctrs recycling rules to inputs
  ### If currency is of length 1 - it will be recycled
  ### If amount is of length 1 - it will be recycled
  ### If convert_date is of length 1 - it will be recycled
  #### tidyverse rules should apply :
  ####  Recycling describes the concept of repeating elements of one vector to match the size of another.
  #### There are two rules that underlie the tidyverse recycling rules:
  #### -  Vectors of size 1 will be recycled to the size of any other vector
  #### - Otherwise, all vectors must have the same size
  recycled <- vctrs::vec_recycle_common(amount = amount,
                                        currency = currency,
                                        convert_date = convert_date)

  amount <- recycled$amount
  currency <- recycled$currency
  convert_date <- recycled$convert_date

  # Convert dates to numeric format - handle each element individually
  numeric_date <- vapply(convert_date, function(x) {
    if (is.numeric(x)) {
      as.numeric(x)  # Already numeric, use as-is
    } else if (inherits(x, "Date")) {
      as.numeric(format(x, "%Y%m%d"))  # Convert Date to YYYYMMDD
    } else if (is.character(x)) {
      as.numeric(x)  # Convert character to numeric
    } else {
      NA_real_
    }
  }, FUN.VALUE = numeric(1))


  # Get unique currency/date combinations for efficient querying
  unique_lookups <- unique(data.frame(currency = currency, date = numeric_date))

  # Handle empty case
  if (nrow(unique_lookups) == 0) {
    return(numeric(0))
  }
  # Connect to database and execute optimized SQL query
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))  # Ensure connection cleanup


  # Alternative approach: Use temp table which is more reliable
  temp_table_name <- paste0("temp_lookup_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  DBI::dbWriteTable(conn, temp_table_name, unique_lookups, temporary = TRUE)

  # Use simple query with temp table
  query <- sprintf("
    WITH ranked_rates AS (
      SELECT
        l.currency,
        l.date as lookup_date,
        c.usd_value,
        ROW_NUMBER() OVER (
          PARTITION BY l.currency, l.date
          ORDER BY ABS(c.date - l.date), c.date
        ) as rn
      FROM %s l
      JOIN ConvertToUSD c ON c.currency = l.currency
    )
    SELECT currency, lookup_date, usd_value
    FROM ranked_rates
    WHERE rn = 1",
  temp_table_name)

  usd_rates <- DBI::dbGetQuery(conn, query)

  # Create lookup table with USD hardcoded
  lookup_table <- usd_rates |>
    dplyr::bind_rows(
      data.frame(currency = "USD",
                 lookup_date = unique(numeric_date),
                 usd_value = 1.0)  # USD to USD rate
    )

  # Join rates back to original vectors
  result_df <- data.frame(currency = currency, date = numeric_date, amount = amount) |>
    dplyr::left_join(lookup_table, by = c("currency", "date" = "lookup_date"))

  # Return vectorized conversion maintaining input order
  return(as.numeric(result_df$amount * result_df$usd_value))
}


