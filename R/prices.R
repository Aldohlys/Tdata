
###################  Retrieve prices functions #######################

#### auto.assign=TRUE is necessary if multiple symbols at the same time
#'   getSymFromDate
#'
#'This function gets from Yahoo service all values (Open, High, Low, Close, Volume and Adjusted)
#'for one of a vector of symbols. It is based upon quantmod getSymbols function.
#'It converts IBKR-style tickers into Yahoo-style tickers first.
#'@param sym one or a vector of symbols
#'@param date a start date from which to retrieve symbols
#'@returns a list of list: each list contains the list of prices and volume: Open, Close, Low, High, Volume and Adjusted
#'@keywords Yahoo
#'@examples getSymFromDate("SPY",as.Date("2023-12-01"))
#'@export
getSymFromDate = function(sym, date) {

  if (length(date) != 1) stop("date must be equal to 1!")

  lookup_yahoo = c("ESTX50"="^STOXX50E","MC"="MC.PA","OR"="OR.PA","TTE"="TTE.PA","AI"="AI.PA",
                   "SPX"="^SPX","XSP"="^XSP","RUT"="^RUT","NESN"="NESN.SW","HOLN"="HOLN.SW","SLHN"="SLHN.SW",
                   "EUR.USD"="EURUSD=X","EUR.CHF"="EURCHF=X")
  sym = dplyr::if_else(sym %in% names(lookup_yahoo), lookup_yahoo[sym], sym)
  lapply(sym, function(x) { suppressMessages(quantmod::getSymbols(x, from = date, auto.assign = F, warnings=FALSE))})
}

#' getSym
#'
#' This function retrieves all values from Yahoo starting from CurrentTradesInitialDate - see config.yml file at user level directory
#' This function calls \code{getSymFromDate} with one or a vector of tickers.
#'@param sym one or a vector of symbols
#'@returns a list of list: each list contains the list of prices and volume: Open, Close, Low, High, Volume and Adjusted
#'@examples getSym("SPY")
#'@export
getSym = function(sym){
  getSymFromDate(sym, as.Date(config::get("CurrentTradesInitialDate")))
}


#' getSymPriceFromDate
#'
#' This function retrieves all values from Yahoo starting from CurrentTradesInitialDate - see config.yml file at user level directory
#' This function calls \code{getSymFromDate} with one or a vector of tickers.
#'@param sym_list one or a vector of symbols
#'@param date a start date from which to retrieve symbols
#'@returns a xts matrix: each column of data contains the adjusted prices - column name is the symbol name
#'@examples getSymPriceFromDate(sym_list=c("SPY","FNV","USO"),as.Date("2023-01-02"))
#'@export
getSymPriceFromDate = function(sym_list, date){

  if (length(date) != 1) stop("date must be equal to 1!")

  sym_OHLC = getSymFromDate(sym_list, date)

  sym_all = purrr::reduce(sym_OHLC, \(acc, x) cbind(acc, x) )
  adj_sym = sym_all[,grepl("Adjusted", names(sym_all))]
  colnames(adj_sym) = sub(".Adjusted","", colnames(adj_sym))
  adj_sym
}


#' getPriceAllDates
#' This function is a helper function that helps to deal with \code{getSymFromDate} returned values
#' It extracts the first list of the list of lists returned by \code{getSymFromDate},
#' Takes the 6th column (adjusted values) and then convert it into a tibble with one column \code{date} and one column \code{value}.
#'@param sym_list a list of list, each list having 6 fields.
#'@returns a tibble with one column \code{date} and one column \code{value}.
#'@examples getPriceAllDates(getSym("SPY"))
#'@export
getPriceAllDates= function(sym_list) {
  ### Takes only the first element of the sym list
  ### This is for compatibility with getSymFromDate
  sym = sym_list[[1]]
  price = dplyr::tibble(date = as.Date(zoo::index(sym)), value = as.numeric(sym[,6]))
  colnames(price) = c("date","value")
  return(price)
}

######################

#'   getSymPrice
#'
#'This function retrieves the price of one ticker from an exchange or from Yahoo download service.
#'
#'It tries first on Yahoo (close price) - this works only for previous days, not for today
#'Then in Prices table and if not available returns NA. It cannot work on a vectorized report_date
#'
#'@param sym ticker name, as known by IBKR, If necessary, will be converted to Yahoo ticker name.
#'@param report_date either today, then it will look at IBKR by calling getStockPrice, or a past day, then will look at Yahoo service-
#'\code{report_date} can be a closed day. In this case, nearest day is taken in the list.
#'Prices list goes back only to CurrentTradesInitialDate )set by config::get() for IBKR so currently begin of 2023
#'This is not suited for Gonet account
#'@examples getSymPrice("SPY")
#'@export
getSymPrice = function(sym, report_date){

  if (length(report_date) != 1) stop("report_date must be equal to 1!")

  ### First case - requested date is an holiday or requested date is not today
  ### Get last close price in this case
  if ((!RQuantLib::isBusinessDay("UnitedStates", report_date)) | report_date < Sys.Date()) {
    ### Take prices list 5 days before report_date to be sure to grasp at least one business day among these 5 days
    prices_list=getPriceAllDates(getSymFromDate(sym, report_date - 5))

    #### report_date becomes the nearest recorded day in Yahoo
    report_date = Tbasics::findNearestNumberOrDate(prices_list$date, report_date)
    price = dplyr::filter(prices_list,date==report_date)
    return(price$value)
  }

  #### If report_date is today and is not an holiday
  ####  then retrieve most current market price from DB
  return(getStockPrice(sym=sym))

}

###
#' getLastAdjustedPrice
#'
#' This function takes one ticker (or a vector of tickers) as input and returns the last available adjusted value from Yahoo service.
#'
#' To achieve this, it will call \code{getSymFromDate} with "today - 5" date - so to be sure to get at least one valid date,
#' even if there are week ends and closed days. It takes last open date if current date provided does not work.
#' See also \code{getLastPriceDate}
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of values (rounded to 2 decimals) corresponding to last values of tickers
#'@examples getLastAdjustedPrice("SPY")
#'@export
getLastAdjustedPrice = function(ticker) {
  dplyr::if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
                  NA,
                  {
                    ## Case date is Tuesday morning and US market not yet opened + Monday and Friday were off -> Get THur data
                    ### This returns all last data available
                    ticker=getSymFromDate(ticker,Sys.Date()-5)
                    ### Last column is "ticker.Adjusted"
                    sapply(ticker, function(x) round(x[[nrow(x),6]],2))
                  }
  )
}

###
#' getLastPriceDate
#'
#' This function takes one ticker (or a vector of tickers) as input and returns the last available
#' date with an available price - see also \code{getLastAdjustedPrice}.
#'
#'@param ticker ticker name, as known by IBKR - can be one name or a vector of names
#'@returns a list of dates for each ticker. It calls \code{getSymFromDate} to get the dates.
#'@examples getLastPriceDate("SPY")
#'@export
getLastPriceDate = function(ticker) {
  dplyr::if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
                  NA,
                  {ticker = getSymFromDate(ticker, Sys.Date() - 5)
                  ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
                  sapply(ticker, function(x) format(zoo::index(x[nrow(x)]), "%d.%m.%Y"))
                  })
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
  if (is.null(ticker) |
      ### NA shall work as a numeric for future computations like round(x,2)
      ticker %in% c("", "All", "STOCK")) return(list(last = NA, change = NA))
  tryCatch({
    ticks = getSymFromDate(ticker, Sys.Date() - 5)[[1]] ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    ##names(ticks)[length(names(ticks))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data = ticks[[nrow(ticks), 6]]
    p_last_data = ticks[[nrow(ticks) - 1, 6]]
    return(list(
      last = round(last_data, 2),
      change = scales::label_percent(accuracy = 0.01)(Tbasics::change(last_data, p_last_data))
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
#'For a given ticker this function returns the last known value from IBKR. It first looks for last value stored in prices.csv file.
#'If one exists that is no older than 1 hour, then this value is returned. Otherwise it looks for IBKR service to retrieve a new data.
#'If IBKR service is not available, it will request it from end-user on console.In the end the new price is stored in prices.csv file.
#'
#'This function is not vectorized and accepts only one ticker at a time.
#'@param sym string - IBKR style of ticker, if unknown then function returns -1
#'@param close Boolean TRUE/FALSE if true then retrieve last close price if true then retrieve active price
#'@returns a value
#'@examples
#'\dontrun{
#'getStockPrice(sym="SPY")
#'getStockPrice(sym="ESTX50")
#'getStockPrice(sym="USO",close=TRUE)
#'}
#'@export
getStockPrice = function(sym, close = FALSE) {
  #### Default value for security type is Stock
  #### Default value for exchange is SMART
  message("getStockPrice")

  if (close) return(
    data.frame(
      datetime = format(lubridate::ymd_hms(paste(Sys.Date() - 1,"22:00:00")), "%d %b %Y %Hh%M"),
      sym = sym,
      price = getLastAdjustedPrice(sym)
    )
  )

  ### Just retrieve last price from DB but no update from DB
  mydb <- DBI::dbConnect(RSQLite::SQLite(),  config::get("DB"))
  line <- DBI::dbGetQuery(mydb, "SELECT * FROM Prices
                          WHERE sym = ? ORDER BY ROWID DESC LIMIT 1;", params = list(sym))
  DBI::dbDisconnect(mydb)

  return(line)
}

#### Used by Gonet.R script and RAnalysis
###
#'getIBKRPrice
#'
#'Retrieves a price or a list of price from IBKR and
#'returns a data frame with date and time (formatted), the list of tickers and corresponding prices.
#'
#'For a given ticker or a list of tickers, this function returns prices from IBKR.
#'If IBKR service is not available, it will return an error code and display an error message: 0 if no IBKR service, -1 if contract is unknown.
#'@param list_sec string or vector of strings- IBKR security type, either \code{STK} or \code{IND} or \code{OPT} or \code{FUT}, default is \code{STK}.
#' if unknown then function returns -1
#'@param list_sym string or vector of strings- IBKR style of ticker, if unknown then function returns -1
#'@param list_currency string or vector of strings- list of currencies - either EUR, CHF or USD - default is USD
#'@param list_exchange string or vector of strings- list of exchanges SMART, EUREX, CBOE,..., if unknown then function returns -1 - default is SMART
#'@param reqType integer - should be either 2 or 4, default is 2
#'@param close Boolean TRUE/FALSE, default is FALSE. if TRUE then retrieve last close price from IBKR, if FALSE then retrieve market price from IBKR
#'@returns a data frmae with the following fields: \code{datetime} (formatted date and time), \code{sym} (ticker name), \code{price} (price as double)
#'@examples
#'\dontrun{
#'getIBKRPrice(list_sym="SPY")
#'getIBKRPrice(list_sec=c("IND","STK"), list_sym=c("ESTX50","USO"), list_currency=c("EUR","USD"), list_exchange = c("EUREX","SMART"))
#'getIBKRPrice(list_sym="USO",close=TRUE)
#'}
#'@export
getIBKRPrice <- function(list_sec="STK", list_sym, list_currency="USD", list_exchange="SMART", reqType=2, close=FALSE) {

  ### This will work even if list_sym is a vector and not the other IBKR contract components
  list_price <- reticulate::py$getValue(list_sec, list_sym, list_currency, list_exchange, reqType, close)

  ### Error case do not go further on###
  if (length(list_price) == 1) {
    display_error_message("IBKR data retrieval did not work - either no connection or contract does not exist")
    return (list_price)
  }

  ### Last close price should not be stored as date and time will be wrong (IBKR returned last day close data...)
  if (!close) {
    myconn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    DBI::dbAppendTable(myconn, "Prices", list_price)
    DBI::dbDisconnect(myconn)
  }

  ### Available readily if needed
  return(list_price)
}
####
###
#' #'getStockPriceServer
#' #'
#' #'For a given ticker this function returns the last known value from IBKR. It first looks for last value stored in prices.csv file.
#' #'If one exists that is no older than 1 hour, then this value is returned. Otherwise it looks for IBKR service to retrieve a new data.
#' #'If IBKR service is not available, it will request it from end-user on console.In the end the new price is stored in prices.csv file.
#' #'
#' #'This function is not vectorized and accepts only one ticker at a time.
#' #'@param id module id
#' #'@param sec security type, equals \code{STK} by default.
#' #'Other values are \code{IND} (for index), or \code{FUT} (for future)
#' #'@param sym string - IBKR style of ticker, if unknown then function returns -1
#' #'@param currency string with possibles values "USD", "CHF", "EUR".
#' #'@param exchange string - default value is "SMART". Can be also "EUREX", "CBOE",...
#' #'@param reqType default value for IBKR ticker request.
#' #'@param close Boolean TRUE/FALSE if true then retrieve last close price if true then retrieve active price
#' #'@returns a value
#' #'@export
#' getStockPriceServer = function(id, sec=shiny::reactive("STK"),sym,
#'                                currency,exchange=shiny::reactive("SMART"),reqType=4,close=shiny::reactive(FALSE)) {
#'
#'   shiny::moduleServer(id,
#'                       function(input,output,session) {
#'                         ############  Action to be defined #########
#'                         ns=shiny::NS(id)
#'
#'   #### Default value for security type is Stock
#'   #### Default value for exchange is SMART
#'   message("getStockPriceServer")
#'   # ### Special case for CSBGU0 stock I own in Gonet portfolio
#'   # if (sym == "CSBGU0") reqType=3 or 4
#'
#'   ### SMART works fine in many cases but not for ESTX50, SPX and XSP cases
#'   exch = switch(sym(), ESTX50= "EUREX", SPX =, XSP = "CBOE", exchange())
#'   security = switch(sym(), SPX=,XSP=,ESTX50="IND",sec())
#'
#'   ### getStockValue will either get a price from "C:/Users/aldoh/Documents/NewTrading/prices.csv" if one exists younger than 1 hour
#'   ### Or requests a price from IBKR
#'   ### Or returns null if IBKR is not available or price cannot be retrieved
#'   ### It shall return -1 if the ticker is unknown by IBKR
#'
#'   line = reticulate::py$getStockValue(sec=security,sym=sym(),currency=currency(),exchange=exch,
#'                                       reqType=reqType,close=close())
#'
#'   ### Case where no IBKR connection exists (NULL) or no value returned
#'   if (is.null(line)) {
#'     val=Tbasics::enter_numerical_data(sym())
#'     print(val)
#'
#'     #### Write data to CSV file as data input by end user
#'     line=dplyr::tibble(datetime=format(Sys.time(),"%e %b %Y %Hh%M"),sym=sym())
#'     line$price=val
#'     # utils::write.table(line,paste0(config::get("DirNewTrading"),"prices.csv"),sep=";",
#'     #                    row.names = FALSE,quote=F,col.names = FALSE,append=TRUE)
#'     conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
#'     DBI::dbAppendTable(conn, "Prices", line)
#'     DBI::dbDisconnect(conn)
#'   }
#'   else val=line[["price"]]
#'
#'   if (val== -1) return(shiny::reactive(-1))
#'   else {
#'     print(paste0("DateTime:",line[["datetime"]], " Value: ",val))
#'     return(shiny::reactive(val))
#'   }
#'   })
#' }
#'
#'
#'
#' priceApp = function() {
#'   ui = shiny::fluidPage(
#'     shiny::fluidRow(
#'       shiny::column(width=4, shiny::selectInput("sec","Security: ",choices=c("STK","IND"),selected="STK")),
#'       shiny::column(width=4, shiny::textInput("sym","Symbol: ",value="")),
#'       shiny::column(width=4, shiny::selectInput("currency","Currency: ",choices=c("USD","EUR","CHF","JPY"),selected="USD")),
#'     ),
#'     shiny::fluidRow(
#'       shiny::column(width=4, shiny::selectInput("close","Close price?: ",choices=c(TRUE,FALSE),selected=FALSE)),
#'       shiny::column(width=4, shiny::selectInput("exchange","Exchange: ",choices=c("SMART","EUREX","CBOE","LSEETF"),selected="SMART"))
#'     ),
#'     shiny::h3(shiny::textOutput("price"))
#'   )
#'
#'   server = function(input,output,session) {
#'     output$price=shiny::renderText({
#'       shiny::validate(shiny::need(input$sym != "", "Enter sym name!"))
#'       #browser()
#'       price = getStockPriceServer(id="price",sec=shiny::reactive(input$sec),sym=shiny::reactive(input$sym),
#'                           currency=shiny::reactive(input$currency),exchange=shiny::reactive(input$exchange),
#'                           close=shiny::reactive(input$close))
#'       paste0("Sym: ",input$sym," current price= ", price())
#'     })
#'   }
#'   shiny::shinyApp(ui,server)
#' }
