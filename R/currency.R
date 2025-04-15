#### CURRENCY management ##################


### This function work only for IBKR accounts not for Gonet account
### This function is NOT exported
# getAllCurrencyPairs = function() {
#   suppressMessages(read_delim(file=config::get("CurrencyPairs"),
#                                      delim=";",locale=locale(date_names="en",decimal_mark=".",
#                                                              grouping_mark="",encoding="UTF-8")))
# }

### This assumes that mydb is declared
getAllCurrenciesUSDValues = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  all_values <- DBI::dbReadTable(conn, "ConvertToUSD")
  DBI::dbDisconnect(conn)
  all_values
}

#'  getStoredUSDValue
#'
#' This function retrieves last USD value of a given currency from DB. This function can be vectorized.
#'
#' It looks into ConvertToUSD table and requests
#' the last record. It does not try to retrieve more up to date value from IBKR or other sources.
#' See \code{getIBKR()} to get more up to date data from IBKR
#'@param currency string - possible values are EUR, CHF, USD...
#'@returns a data frame with \code{date, usd_value} fields,
#'where \code{date} is an integer format of YYYYMMDD and \code{usd_value} a numeric.
#'@examples
#'getStoredUSDValue("USD")
#'getStoredUSDValue("EUR")
#'getStoredUSDValue("CAD")
#'getStoredUSDValue("CHF")
#'@export
getStoredUSDValue = function(currency) {
  dplyr::if_else (currency == "USD",
           data.frame(date = as.numeric(format(Sys.Date(),"%Y%m%d")), usd_value = 1.0),
           {
             conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
             usd <- DBI::dbGetQuery(conn, "SELECT max(date) as date, usd_value FROM ConvertToUSD WHERE currency = ?",
                                    params=list(currency))
             DBI::dbDisconnect(conn)
             usd
           })
}


  # usd = getAllCurrencyPairs()
  # usd <- usd[usd$currency == currency,]
  # usd_last = dplyr::filter(usd, date == max(date))
  #
  # ### This will remove duplicate for currency and date
  # usd_last = usd_last[!duplicated(usd_last[, 1:2]),]
  #
  # ### This is an approximation - hopefully dates at which currencies were retrieved are not too different
  # ### - ideally they should be all equal !!
  # ### So looking at this date one knows worse case for currency value accuracy is this date
  # usd_last$date = min(usd_last$date)
  # usd_last = tidyr::pivot_wider(usd_last, names_from="currency", values_from="usd_value")
  # return(usd_last)


#'  getLastUSDValue
#'
#' This function returns converted value of a given currency to USD
#' by trying to get from Yahoo the latest available value and by default
#' returning last value available in DB. This function cannot be vectorized.
#'
#'
#' It looks into ConvertToUSD table and then retrieves last available value from Yahoo.
#' If Yahoo value is different from NA and if it is are more recent than stored value, then
#' it will update ConvertToUSD table in DB. It returns the stored value anyhow.
#'
#' It does not try to retrieve more up to date value from IBKR.
#' See \code{getIBKR()} to get more up to date data from IBKR
#'@param currency string - possible values are EUR, CHF,...
#'@returns a data frame with \code{date, usd_value} fields,
#'where \code{date} is an integer format of YYYYMMDD and \code{usd_value} a numeric.
#'@examples
#'getLastUSDValue("USD")
#'getLastUSDValue("EUR")
#'getLastUSDValue("CAD")
#'getLastUSDValue("CHF")
#'@export
getLastUSDValue = function(currency) {

  usd = data.frame(date=NA, usd_value=NA)

  if (currency == "USD") {
    usd = data.frame(date = as.numeric(format(Sys.Date(),"%Y%m%d")), usd_value = 1.0)
    return(usd)
  }

  Tbasics::display_message("Retrieve currencies from DB...")
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  currency_detail <- DBI::dbGetQuery(conn, "SELECT YahooPair, DirectConversion FROM Currencies WHERE Name = ?", params=list(currency))
  DBI::dbDisconnect(conn)

  if (nrow(currency_detail) == 0){
    Tbasics::display_error_message(paste0(currency, " currency undefined, not able to retrieve in Yahoo !!"))
  }

  last_price <- getLastSymPrice(currency_detail[,1])

   ### convert date to compare
  last_price$date <- as.integer(format(max(last_price$date),"%Y%m%d"))

  ### Get last record from CurrencyPairs table
  usd <- getStoredUSDValue(currency)

  if (last_price$date > usd$date) {
      ### Only new prices are to be stored
      new_price <- dplyr::left_join(last_price, currency_detail, by = c("sym" = "YahooPair"))
      new_price <- dplyr::mutate(new_price, value=round(dplyr::if_else(DirectConversion == "Yes", value, 1/value), 4))
      new_price <- dplyr::mutate(new_price, date=date, currency=currency, usd_value=value, .keep="none")

      conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
      DBI::dbAppendTable(conn, "ConvertToUSD", new_price)
      DBI::dbDisconnect(conn)
      usd <- dplyr::select(new_price, date, usd_value)
  }
  return(usd)
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
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  currencies <- DBI::dbReadTable(conn, "Currencies")
  DBI::dbDisconnect(conn)
  sub_currencies <- suppressMessages(dplyr::left_join(data.frame(Name=currency), currencies))
  sub_currencies$Display
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
  #### There are two rules that underlie the “tidyverse” recycling rules:
  #### -  Vectors of size 1 will be recycled to the size of any other vector
  #### - Otherwise, all vectors must have the same size
  result = data.frame(amount = amount, currency = currency)
  numeric_date = numeric(0)

  ### If convert_date is of Date type or character type then convert it
  if (is.numeric(convert_date)) numeric_date = convert_date
  if (inherits(convert_date,"Date")) numeric_date <- as.numeric(format(convert_date,"%Y%m%d"))
  if (is.character(convert_date)) numeric_date <- as.numeric(convert_date)

  ### Retrieve all currency pairs since beginning
  ### It is assumed here that dates are stored in integer/character format in CurrencyPairs table
  usd = getAllCurrenciesUSDValues()

  ### This works only if date is of length 1
  ### if date is not recorded yet, it will provide the values of yesterday or before
  ### If it falls on a closed day and day before and after are business days, then it provides the oldest day
  usd = dplyr::group_by(usd, currency)
  usd = dplyr::ungroup(dplyr::filter(usd, abs(date-numeric_date) == min(abs(date-numeric_date))))
  usd$date = NULL

  ### Remove duplicated currencies (take first one i.e. oldest same date)
  usd = usd[!duplicated(usd[,1]),]

  ### If convert to USD - always equal to 1
  usd = dplyr::add_row(usd, currency="USD", usd_value = 1)

  result = dplyr::left_join(result, usd)
  return(as.numeric(result$amount * result$usd_value))
}


