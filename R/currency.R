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
#' This function provides a pair EUR/USD, CHF/USD.
#'
#' It first retrieves the CurrencyPairs CSV file
#' and looks for the last record.
#'
#' If last record date is today then returns it.
#'
#' Otherwise tries to connect to IBKR TWS API and retrieve pairs value.
#'
#' If it does not succeed, it will then ask the end-user using enter_numerical_data  function
#' Once new data is obtained, then write it to CurrencyPairs file
#'@keywords currency trading
#'@export
getCurrencyPairs = function() {
  message("getCurrencyPairs")
  ### euro_usd and chf_usd data frames - values for the day- are already retrieved
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  usd <- DBI::dbReadTable(conn, "CurrencyPairs")

  ### Convert string to date
  last_usd=dplyr::last(usd)
  last_date=as.Date(last_usd$date)

  if (last_date == Sys.Date())  {
    #print(usd)
    return(last_usd)
  }

  ### No EUR or CHF values for today => to be retrieved from IBKR
  Tbasics::display_message("Retrieving EUR/USD !")
  EUR = reticulate::py$getCurrencyPairValue("EURUSD",reqType=2)
  if (is.null(EUR)) EUR=Tbasics::enter_numerical_data("EUR/USD")
  else if (is.na(EUR)) EUR=Tbasics::enter_numerical_data("EUR/USD")

  Tbasics::display_message("Retrieving CHF/USD !")
  CHF = reticulate::py$getCurrencyPairValue("CHFUSD",reqType=2)
  if (is.null(CHF)) CHF=Tbasics::enter_numerical_data("CHF/USD")
  else if (is.na(CHF)) CHF=Tbasics::enter_numerical_data("CHF/USD")

  Sys.sleep(1)

  usd = data.frame(date=Sys.Date(),EUR=EUR,CHF=CHF)
  #print(usd)

  DBI::dbAppendTable(conn, "CurrencyPairs", usd)
  ### write.table seems to be the only one working simply -
  ### to be revisited as utils package is not the most current and efficient
  ### utils::write.table(usd,config::get("CurrencyPairs"),sep=";",dec=".",row.names=F,append=T,col.names=F)
  DBI::dbDisconnect(conn)

  return(usd)
}


#'  currency_format
#'
#' This function returns a string with amount and currency symbol
#'
#' It defines local labeling functions for CHF, EUR and USD.
#'
#' Currency symbol (for EUR and USD) are also taken into consideration.
#'
#' Only 2 digits after decimal are displayed. Big mark (3 digits separator) is empty.

#'@param amount,currency amount is the number to be displayed, currency is a string whose value is either EUR, CHF or USD.
#'If length(currency) is 1, then it is recycled
#'@examples
#'currency_format(100.45,"EUR")
#'currency_format(10000,"CHF")
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
    dplyr::case_match(currency, "EUR"~euro(amount),
           "CHF"~chf(amount),
           "USD"~dollar(amount))
  })
}

#'  currency_convert
#'
#' This function converts the amount of currency into USD, using getCurrencyPairs
#'
#' First it retrieves today's currency pairs by calling getCurrencyPairs() function.
#'
#' Then it just performs a multiplication of the amount by currency pair value. This function can be vectorized
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@keywords currency trading
#'Examples
#'currency_convert(100.45,"EUR")
#'currency_convert(c(10000,500),c("CHF","EUR"))
#'currency_convert(c(750.543,10),c("USD","EUR"))
#'@export
currency_convert = function(amount,currency) {

  ### Suppress warning that close is only current close and not final one for today
  usd=getCurrencyPairs()
  cur_convert= dplyr::case_match(currency, "EUR"~usd$EUR,
                      "CHF"~usd$CHF,
                      "USD"~1)

  cur_convert*amount
}

#'  convert_to_usd
#'
#' This function converts the amount of currency into USD, using EUR and CHF currency pairs values
#'
#' It merely performs a multiplication of the amount by currency pair value. This function can be vectorized
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@param EUR,CHF EUR is the value of 1 euro in USD, CHF is the value of 1 CHF in USD
#'@keywords currency trading
#'Examples
#'convert_to_usd(100.45,"EUR",1.09,1.14)
#'convert_to_usd(c(10000,500),c("CHF","EUR"),1.09,1.14)
#'convert_to_usd(c(750.543,10),c("USD","EUR"),1.09,1.14)
#'@export
convert_to_usd = function(amount,currency,EUR,CHF) {
  round(dplyr::case_match(currency,
                   "EUR" ~amount*EUR,
                   "CHF" ~amount*CHF,
                   "USD" ~amount),2)
}

#'  convert_to_usd_date
#'
#' This function converts the amount of currency into USD, using EUR and CHF currency pairs values for a given date
#'
#' First it loads all currency pairs that have been stored for a while, then looks up for the nearest date in the CurrencyPairs table, compared with input date.
#' It retrieves the EUR and CHF corresponding values.
#'
#' Last it calls the convert_to_usd function.
#'
#' This function can be vectorized.
#'@param amount,currency amount is the number to be converted, currency is a string whose value is either EUR, CHF
#'@param date date is the requested date, it must be in Date format
#'@keywords currency trading
#'Examples
#'convert_to_usd_date(100.45,"EUR",as.Date("2023-10-15"))
#'convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09"))
#'convert_to_usd_date(c(750.543,10),c("USD","EUR"),as.Date("2023-12-03"))
#'@export
convert_to_usd_date = function(amount,currency,date) {
  usd=getAllCurrencyPairs()
  usd$date=as.Date(usd$date)

  nearest_index = which.min(abs(usd$date-date))
  EUR= usd$EUR[nearest_index]
  CHF= usd$CHF[nearest_index]

  convert_to_usd(amount,currency,EUR,CHF)

}


