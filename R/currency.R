#### CURRENCY management ##################


### This function work only for IBKR accounts not for Gonet account
### This function is NOT exported
# getAllCurrencyPairs = function() {
#   suppressMessages(read_delim(file=config::get("CurrencyPairs"),
#                                      delim=";",locale=locale(date_names="en",decimal_mark=".",
#                                                              grouping_mark="",encoding="UTF-8")))
# }

### This assumes that mydb is declared
getAllCurrencyPairs = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  all_pairs <- DBI::dbReadTable(conn, "CurrencyPairs")
  DBI::dbDisconnect(conn)
  all_pairs
}

#'  getCurrencyPairs
#'
#' This function returns pairs EUR/USD, CHF/USD, CAD/USD.
#'
#' It looks into CurrencyPairs table and requests
#' the last record. It does not try to retrieve more up to date value from IBKR or other sources.
#' See \code{getIBKR()} to get more up to date data from IBKR
#'@export
getCurrencyPairs = function() {
  ### euro_usd and chf_usd data frames - values for the day- are already retrieved
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

  ### Get last record from CurrencyPairs table
  usd <- DBI::dbGetQuery(conn, "SELECT *
                                      FROM CurrencyPairs
                                      WHERE ROWID = (SELECT MAX(ROWID)  FROM CurrencyPairs);")
  DBI::dbDisconnect(conn)
  return(usd)
}

#'  getLastCurrencyPairs
#'
#' This function returns pairs EUR/USD, CHF/USD, CAD/USD and tries to get from Yahoo the latest available pair.
#'
#' It looks into CurrencyPairs table and retrieves last available pairs from Yahoo. If Yahoo pairs are more recent,
#' it will update CurrencyPairs table in DB and return it. Otherwise it just returns
#' the last record from DB.
#'
#' It does not try to retrieve more up to date value from IBKR.
#' See \code{getIBKR()} to get more up to date data from IBKR
#'@export
getLastCurrencyPairs = function() {
  last_prices <- getLastSymPrice(c("EUR.USD", "CHF.USD", "USD.CAD"))

  ### Rename sym for possible future storage in DB
  last_prices$sym <- c("EUR", "CHF", "CAD")
  last_prices$date <- as.integer(format(max(last_prices$date),"%Y%m%d"))

  ### Get CAD/USD value instead of USD/CAD
  where_cad = (last_prices$sym == "CAD")
  last_prices[where_cad, "value"] = 1 / last_prices[where_cad, "value"]

  ### For future storage in DB
  last_prices$value = round(last_prices$value, 4)

  ### Most recent date retrieved
  last_date = max(last_prices$date)

  ### Get last record from CurrencyPairs table
  usd <- getCurrencyPairs()

  if (usd$date >= last_date) return(usd)
  else {
    ### Only new prices are to be stored
    new_prices = last_prices[last_prices$date > usd$date,]
    usd <- tidyr::pivot_wider(new_prices, names_from="sym", values_from="value")
    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    DBI::dbAppendTable(conn, "CurrencyPairs", usd)
    DBI::dbDisconnect(conn)
    return(usd)
  }
}

#'  currency_format
#'
#' This function returns a string with amount and currency symbol
#'
#' It defines local labeling functions for CHF, EUR, CAD and USD.
#'
#' Currency symbol (for EUR and USD) are also taken into consideration.
#'
#' Only 2 digits after decimal are displayed. Big mark (3 digits separator) is empty.

#'@param amount,currency amount is the number to be displayed, currency is a string whose value is either EUR, CHF or USD.
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
currency_format = function(amount,currency){
  #Returns the amount values formatted with their respective currency sign, based on the currency argument
  ## Amounts are rounded to 0.01

  euro <- scales::label_dollar(
    prefix = "",
    suffix = " \u20ac",
    big.mark = "",
    accuracy=0.01
  )
  chf <- scales::label_dollar(
    prefix = "",
    suffix = " CHF",
    big.mark = "",
    accuracy=0.01
  )
  cad <- scales::label_dollar(
    prefix = "",
    suffix = " CAD",
    big.mark = "",
    accuracy=0.01
  )
  dollar <- scales::label_dollar(
    prefix = "",
    suffix = " $",
    big.mark = "",
    accuracy=0.01
  )

  ### If length currency equals 1 and length arguments differ
  ### then recycled otherwise error is raised
  if (length(currency) != length(amount)) {
    if (length(currency) == 1) currency <- rep(currency, length(amount))
    else stop("amount and currency do not have same length AND currency length is not equal to 1 !")
  }

  dplyr::if_else (is.na(amount), "", {
    dplyr::case_match(currency,
           "EUR"~euro(amount),
           "CHF"~chf(amount),
           "CAD"~cad(amount),
           "USD"~dollar(amount))
  })
}

#'  convert_to_usd
#'
#' This function converts the amount of currency into USD, using EUR, CAD and CHF currency pairs values
#'
#' It merely performs a multiplication of the amount by currency pair value. This function can be vectorized
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@param EUR,CHF,CAD EUR (resp. CHF, CAD) is the value of 1 euro (resp. CHF, CAD) in USD
#'@keywords currency trading
#'@examples
#'convert_to_usd(100.45,"EUR",1.09,1.14,0.785)
#'convert_to_usd(c(10000,500),c("CHF","EUR"),1.09,1.14,0.785)
#'convert_to_usd(c(750.543,10),c("USD","EUR"),1.09,1.14,0.785)
#'convert_to_usd(c(500,10),c("CAD","EUR"),0.788,1.14,0.785)
#'@export
convert_to_usd = function(amount, currency, EUR, CHF, CAD) {
  round(dplyr::case_match(currency,
                   "EUR" ~amount*EUR,
                   "CHF" ~amount*CHF,
                   "CAD" ~amount*CAD,
                   "USD" ~amount),2)
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
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@param date Can be a date, or character, or integer(numeric). By defaut it is today.
#'If type is character/numeric, then \code{date} argument will be converted first to a date type using Y/M/D format.
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
convert_to_usd_date = function(amount, currency, date = Sys.Date()) {

  if (length(date) != 1) stop("date must be of length 1!")

  ### Prepare call to bind_cols
  amount = data.frame(amount = amount)

  ### If date is of numeric (i.e. integer) or character type then convert it
  if (is.numeric(date)) date <- as.Date(as.character(date), "%Y%m%d")
  if (is.character(date)) date <- as.Date(date, "%Y%m%d")

  ### Retrieve all currency pairs since beginning
  usd = getAllCurrencyPairs()

  ### It is assumed here that dates are stored in integer/character format in CurrencyPairs table
  usd$date = as.Date(as.character(usd$date),"%Y%m%d")

  ### This works only if date is of length 1
  ### because which.min returns one single index, even if date is a vector
  ### if date is not recorded yet, it will provide the values of yesterday or before
  ### If it falls on a closed day and day before and after are business days, then it provides the oldest day
  nearest_index = which.min(abs(usd$date-date))

  usd_nearest = usd[nearest_index,]

  ### Prepare call to bind_cols - remove date column
  currency = data.frame(currency = currency)
  usd_nearest$date = NULL

  #### tidyverse rules should apply :
  ####  Recycling describes the concept of repeating elements of one vector to match the size of another.
  #### There are two rules that underlie the “tidyverse” recycling rules:
  #### -  Vectors of size 1 will be recycled to the size of any other vector
  #### - Otherwise, all vectors must have the same size
  data <- dplyr::bind_cols(amount, currency, usd_nearest)

  do.call(convert_to_usd, data)
}


