#### CURRENCY management


### This function work only for IBKR accounts not for Gonet account
### This function is NOT exported
# getAllCurrencyPairs = function() {
#   suppressMessages(read_delim(file=config::get("CurrencyPairs"),
#                                      delim=";",locale=locale(date_names="en",decimal_mark=".",
#                                                              grouping_mark="",encoding="UTF-8")))
# }


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
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  currency_detail <- DBI::dbGetQuery(conn, "SELECT * FROM Currencies WHERE Name = ?", params=list(currency))
  return(currency_detail)

}



### This assumes that mydb is declared
getAllCurrenciesUSDValues = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  all_values <- DBI::dbReadTable(conn, "ConvertToUSD")
  return(all_values)
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
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
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
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
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

  if (last_nr == 0) {
    t_log_info("No currency data for {currency} found !")
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
    t_log_info("Updated {nrow(updates_needed)} currency rates")
  }

  else {
    t_log_info("No updates needed - stored data is current")
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
#'currency_sign("USD")
#'currency_sign("EUR")
#'currency_sign("CAD")
#'currency_sign(c("CHF", "USD"))
#'@export
currency_sign <- function(currency) {

  ### Open connection to DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
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
#'currency_format(100.45,"EUR")
#'currency_format(10000,"CHF")
#'currency_format(1000,"CAD")
#'currency_format(758.458,"USD")
#'currency_format(100000.455,"EUR")
#'currency_format(c(100,40),c("EUR","USD"))
#'currency_format(c(100,40),"EUR")
#'@export
currency_format = function(amount, currency){
  #Returns the amount values formatted with their respective currency sign, based on the currency argument
  ## Amounts are rounded to 0.01

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
        scales::label_dollar(
          prefix = "",
          suffix = paste0(" ", curr), #### EURO sign = \u20ac"
          big.mark = " ",
          accuracy=0.01
        )
      })

      ### apply these functions on amount and return this value
      purrr::map2_chr(amount, display_currencies, \(am, disp){disp(am)})
    },

    error = function(cond) {
      message("Tdata::currency_format ERROR")
      message(conditionMessage(cond))
      NA
    },

    warning = function(cond) {
      message("Tdata::currency_format WARNING")
      message(conditionMessage(cond))
      NA
    }
  )

}

#'  c_to_usd
#'
#' This function converts the amount of currency into USD, using currency pairs values stored in DB
#'
#' It merely performs a multiplication of the amount by currency pair value, using getStoredValue.
#' This function can be vectorized
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@keywords currency trading
#'@examples
#'c_to_usd(100.45,"EUR")
#'c_to_usd(c(10000,500),c("CHF","EUR"))
#'c_to_usd(c(750.543,10),c("USD","EUR"))
#'c_to_usd(c(500,10),c("CAD","EUR"))
#'@export
c_to_usd <- function(amount, currency) {
  data <- data.frame(am=amount, cur=currency)
  data <- dplyr::mutate(data, res = am*getStoredUSDValue(cur)$usd_value)
  return(data$res)
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
#' This function can be vectorized for \code{amount} and \code{currency}, but \code{date} MUST be unique.
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF, CAD, etc..
#'@param convert_date Can be a date, or character, or integer(numeric). By default it is today.
#'If type is character/date, then \code{convert_date} argument will be first converted to an integer type with Y/M/D format
#'- this is the format in CurrencyPairs DB table
#'@keywords currency trading
#'@examples
#'convert_to_usd_date(100.45,"EUR",as.Date("2023-10-15"))
#'convert_to_usd_date(200, "CHF", 20240421)
#'convert_to_usd_date(200, "EUR", "20240421")
#'convert_to_usd_date(200, "CAD")
#'convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09"))
#'convert_to_usd_date(c(750.543,10),c("USD","EUR"),as.Date("2023-12-03"))
#'convert_to_usd_date(c(750.543,10),"EUR",as.Date("2023-12-03"))
#'@export
convert_to_usd_date = function(amount, currency, convert_date = Sys.Date()) {

  if (length(convert_date) != 1) stop("convert_date must be of length 1!")

  ### If currency is of length 1 - it will be recycled
  ### If amount is of length 1 - it will be recycled
  #### tidyverse rules should apply :
  ####  Recycling describes the concept of repeating elements of one vector to match the size of another.
  #### There are two rules that underlie the tidyverse recycling rules:
  #### -  Vectors of size 1 will be recycled to the size of any other vector
  #### - Otherwise, all vectors must have the same size
  result = data.frame(amount = amount, currency = currency)
  numeric_date = numeric(0)

  ### If convert_date is of Date type or character type then convert it
  if (is.numeric(convert_date)) numeric_date <- convert_date
  else if (inherits(convert_date,"Date")) numeric_date <- as.numeric(format(convert_date,"%Y%m%d"))
  else if (is.character(convert_date)) numeric_date <- as.numeric(convert_date)

  ### Retrieve all currency pairs since beginning
  ### It is assumed here that dates are stored in integer/character format in CurrencyPairs table
  usd = getAllCurrenciesUSDValues()

  ### This works only if date is of length 1
  ### if date is not recorded yet, it will provide the values of yesterday or before
  ### If it falls on a closed day and day before and after are business days, then it provides the oldest day
  # Find nearest date for each currency using vectorized operations
  usd <- usd |>
    dplyr::group_by(currency) |>
    dplyr::slice_min(abs(date - numeric_date), n = 1) |>  # Get row with minimum date difference
    dplyr::slice_head(n = 1) |>  # Remove duplicates (take first/oldest if tie)
    dplyr::ungroup() |>
    dplyr::select(currency, usd_value)

  # Create lookup vector with USD hardcoded
  currency_rates <- setNames(usd$usd_value, usd$currency)
  currency_rates["USD"] <- 1.0  # Add USD rate directly

  # Vectorized lookup and calculation
  rates <- currency_rates[currency]  # Direct vector indexing
  return(as.numeric(amount * rates))
}


