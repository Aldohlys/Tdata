#### Account related utilities
#'   readAccount
#'
#' This function reads the Account table.
#' It then filters data so that it matches \code{accountnr} number
#' Finally it formats account data with right Date and HMS format
#'@param accountnr is the account number (IBKR)
#'@returns a tibble with the following fields: \code{ account	date	heure
#' NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
#' FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
#'  CashFlow}
#'@examples
#'\dontrun{
#'readAccount("DU5555")
#'}
#'@export
readAccount = function(accountnr) {
  # file=paste0(config::get("DirNewTrading"),"Account",".csv")
  # account_data = suppressWarnings(read_delim(file=file,delim=";",
  #                                            locale=locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  account_data = DBI::dbReadTable(conn,"Account")

  ### account	date	heure
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  ### Starts on Oct 4th, 2022 for IBKR, on June 1st for Gonet

  ### Filter based upon account number and remove account number from result
  account_data = dplyr::filter(account_data,account==accountnr)

  #### Finalize SQL query to DB
  account_data = dplyr::collect(dplyr::select(account_data,-account))

  ### Close DB connection as no more necessary
  DBI::dbDisconnect(conn)

  ### If there is at least one line then do conversion date and heure
  if (nrow(account_data) != 0) {
    ### Convert to internal R date format from of integer date format
    account_data$date=as.Date(as.character(account_data$date),"%Y%m%d")
    account_data$heure=hms::parse_hms(account_data$heure)
  }
  else Tbasics::display_error_message(paste0("No data recorded for ", accountnr))
  return(account_data)
}



############# PORTFOLIO specific functions #############
#' readPortfolio
#'
#'
#' This function reads a portfolio table given as entry
#' and performs a bit of data wrangling before returning a data frame with all data.
#'
#'
#' Data wrangling:
#' 1. formats it with right internal R Date format and heure HMS format
#'
#'@param portfname is a string that is a name of a portfolio table into local DB.
#'local DB path is retrieved through config.yaml file
#'@returns a data frame with the following columns:
#' \code{TradeNr; date; heure; symbol; expdate; strike; pos}
#' \code{mktPrice; optPrice; mktValue; avgCost; unPnL; IV; pvDividend}
#' \code{delta; gamma; vega; theta; uPrice; multiplier; currency; type; Instrument}
#'
#'@examples
#'\dontrun{
#'readPortfolio("DU5555")
#'}
#'@export
readPortfolio = function(portfname) {
  message("readPortfolio")

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

  #### Test if requested portfolio is present in DB (e.g. Live portfolio does not exist)
  name = DBI::dbGetQuery(conn,"SELECT name FROM sqlite_master WHERE type='table' AND name=?",params=list(portfname))

  ### If portfolio exists there is one and only one portfolio name referred
  if (nrow(name)==1) {

    ### read table from DB - no collect function necessary as there is no lazy evaluation later implied (no dplyr)
    portf = DBI::dbReadTable(conn,portfname)
    DBI::dbDisconnect(conn)

    ### Convert from European date format to internal R date format
    portf$date <- as.Date(as.character(portf$date),"%Y%m%d")
    portf$heure <- hms::parse_hms(portf$heure)

    ### With DB it is not necessary to convert position into an integer (this is not a float)
    ### portf$position=as.integer(portf$position)
    ## There are no CASH positions that are virtual
    ### portf = dplyr::filter(portf, type!="CASH")

    ### Case where there are options in the portfolio - these field names come from IBKR - cannot be changed

    #### Remove special Gonet "USD_" fields if present
    ###  portf = dplyr::select(portf,!dplyr::starts_with("USD_"))
    return(portf)
  }
  else {
    DBI::dbDisconnect(conn)

    message("Portfolio doesn't exist, please check portfolio name")
    Tbasics::display_error_message("Portfolio doesn't exist, please check portfolio name")
    return(dplyr::tibble())
  }
}

############# PORTFOLIO specific functions #############
#' readLastPortfolio
#'
#'
#' This function reads the last record of a portfolio table given as entry,
#' and performs a bit of data wrangling before returning a data frame with all data.
#'
#'
#' Data wrangling:
#' 1. format with right internal R Date format and heure HMS format: date and expdate (if it exists) fields
#'
#'
#'@param portfname is a string whose value is actual portfolio table name in DB
#'@returns a data frame with the following columns:
#' \code{TradeNr; date; heure; symbol; expdate; strike; pos; }
#' \code{mktPrice; optPrice; mktValue; avgCost; unPnL; IV; pvDividend; }
#' \code{delta; gamma; vega; theta; uPrice; multiplier; currency; type; Instrument}
#'
#'@examples
#'\dontrun{
#'readLastPortfolio("DU5555")
#'}
#'@export
readLastPortfolio <- function(portfname) {
  # ### retrieve last recorded (date, time) - Other possible implementation
  # last_portf = portf[unlist(portf %>% group_rows() %>% last),]
  message("readLastPortfolio")
  mydb <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

  ### Perform the required query as needed
  #### Live/Simu come from IBKR reporting files
  #### Change them into account numbers

  last_portf <- switch(portfname,
                       "U1804173" = DBI::dbGetQuery(mydb,
                        "WITH Last_record AS(SELECT max(date) as date, heure FROM (SELECT date, MAX(heure) as heure FROM U1804173 GROUP BY date))
					SELECT * FROM U1804173 WHERE date= (SELECT date FROM Last_record) AND heure= (SELECT heure FROM Last_record)"),
                       "DU5221795" = DBI::dbGetQuery(mydb,
                        "WITH Last_record AS(SELECT max(date) as date, heure FROM (SELECT date, MAX(heure) as heure FROM DU5221795 GROUP BY date))
					SELECT * FROM DU5221795 WHERE date= (SELECT date FROM Last_record) AND heure= (SELECT heure FROM Last_record)"),
                       "Gonet" = DBI::dbGetQuery(mydb,
                                                     "WITH Last_record AS(SELECT max(date) as date, heure FROM (SELECT date, MAX(heure) as heure FROM Gonet GROUP BY date))
					SELECT * FROM Gonet WHERE date= (SELECT date FROM Last_record) AND heure= (SELECT heure FROM Last_record)"))

  DBI::dbDisconnect(mydb)

  ### Convert from European date format to internal R date format
  ### NB date is stored as integer in DB so conversion to character is really necessary - not to be fancy
  last_portf$date <- as.Date(as.character(last_portf$date),"%Y%m%d")
  last_portf$heure <- hms::parse_hms(last_portf$heure)
  if ("expdate" %in% colnames(last_portf)) last_portf$expdate <- as.Date(as.character(last_portf$expdate),"%Y%m%d")

  return(last_portf)
}


###############  TWR function
##############################
#'   twr
#'
#' This function Time Weighted Return computes TWR for
#' a given list of dates with corresponding end of day net liquidation values plus cashflows (inflows/outflows).
#'
#'  It is assumed that portfolio value begin of day (n) = portfolio value end of day (n-1)
#'  Also it is assumed that portfolio is valued every calendar day (incl. non business days)
#'  twr will be computed for all dates and then filtered so that it fit with input dates.
#'
#'@param dates list of dates when data are provided - there should be only one data point per date
#'@param e_nlv End of day net liquidation values: D1, D2, ... Dn
#'@param cashflows Begin of day cash flow inflows: D1, D2,... Dn
#'@returns a numerical vector of TWR values
#'@export
twr <- function(dates, e_nlv, cashflows) {
  message("twr")
  ### dates are dates when data are provided - there should be only one data point per dates
  if (!all(!duplicated(dates))) {
    Tbasics::display_error_message("twr:All dates must be different!")
    return(NA)
  }
  else {
    #####  e_nlv: End of day net liquidation values: D1, D2, ... Dn
    ##### cash_flows: Begin of day cash flow inflows: D1, D2,... Dn
    ### The merge takes care of missing days -ensures that all time periods are equal -i.e=1 day

    ###

    e_nlv_regular <- merge(xts::xts(e_nlv, order.by = dates),
                           seq(from = min(dates), to = max(dates), by = "day"))
    cash_flows <- merge(xts::xts(cashflows, order.by = dates),
                        seq(from = min(dates), to = max(dates), by = "day"))

    e_nlv <- as.numeric(zoo::na.approx(e_nlv_regular))
    cash_flows[is.na(cash_flows)]=0
    cash_flows=as.numeric(cash_flows)

    n <- length(e_nlv)
    ##print(paste0("n:",n," nb elts:",length(dates)))

    ### If missing cash flows then means that equals to 0
    if (missing(cash_flows)) cash_flows=rep(0,n)

    ### Error management
    if (length(cash_flows) != n)  {
      print(paste0("NLV:",length(e_nlv)))
      print(paste0("Cash flows",length(cash_flows)))
      Tbasics::display_error_message("twr:Cash flows number of elements different from Porfolio values!!!!")
      return(NA)
    }
    else {
      ### Cash flows are beginning of the day, cash_flows
      ## Special case for first day return computation
      rn=numeric()
      twr=numeric()
      rn[1]= 0
      twr[1]= 1

      ### If only one portfolio value (end of day, cash flow)
      if (n==1) return(twr-1)

      for (i in 2:n) {
        rn[i]= e_nlv[i]/(e_nlv[i-1]+cash_flows[i])
        twr[i]=twr[i-1]*rn[i]
      }

      ### After computation, extract only twr values corresponding to dates
      ##print(paste0("twr min:",min(dates)," max:",max(dates)))

      twr=xts::xts(twr,order.by=seq(from=min(dates),to=max(dates),by="day"))[dates]

      ### returns twr as a numerical vector
      ### Substract 1 to all ratios so to get returns
      return(as.numeric(twr)-1)
    }
  }
}

#############  greeksNet function ###############
##############################
#'   greeksNet
#'
#' This function computes for a portfolio the net position of each Greek, summing over all positions the Greek value of each individual position.
#'
#' Each position will be multiplied by multiplier and a Greek to obtain the Greek net value fo the position.
#' All Greek net values will be then summed up over all positions, for each Greek.
#'
#'@param portf a data frame with one line per instrument, may be grouped by date and time.
#'It should contain at least the following columns: \code{type;pos; mktPrice; delta; gamma; vega; theta; uPrice;
#'multiplier; currency} - see also readPortfolio function
#'@returns a data frame of double numbers with \code{delta, deltadollars, gamma, theta, vega} for each group. It is worth noticing that deltadollars is an amount in USD.
#'(if necessary it is converted to USD from original currency).
#'@export
greeksNet = function(portf) {
  if ("delta" %in% colnames(portf)) {

    ### First converts underlying price from symbol's currency to USD
    usd=getCurrencyPairs()
    EUR=usd$EUR
    CHF=usd$CHF
    CAD=usd$CAD
    portf=dplyr::mutate(portf,uPrice=as.numeric(purrr::map2(uPrice, currency, convert_to_usd, EUR, CHF, CAD)))

    #### portf is grouped by datetime
    #### Therefore summarize will do the computation per datetime

    dplyr::summarize(dplyr::mutate(portf,
                                       dnet=dplyr::case_when(
                                         type=="Stock" ~ 1*pos,
                                         (type=="Call" | type=="Put") ~ multiplier*delta*pos,
                                         TRUE ~ 0),
                                       ddnet=dplyr::case_when(
                                         type=="Stock" ~ 1*pos*mktPrice,
                                         (type=="Call" | type=="Put") ~ multiplier*delta*pos*uPrice,
                                         TRUE ~ 0),
                                       gnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                           multiplier*gamma*pos,
                                                           0),
                                       tnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                           multiplier*theta*pos,
                                                           0),
                                       vnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                           multiplier*vega*pos,
                                                           0)),
                     delta=sum(dnet,na.rm=FALSE),
                     deltadollars=sum(ddnet,na.rm=FALSE),
                     gamma=sum(gnet,na.rm=FALSE),
                     theta=sum(tnet,na.rm=FALSE),
                     vega=sum(vnet,na.rm=FALSE))
  }
}

##############################
#'   getIBKR
#'
#' This function retrieves account, portfolio and prices from IBKR and then store them in DB
#'
#' Account data will be stored in Account table, portfolio data in Uxxx or DUxxx table, depending upon account data.
#' Option underlying prices will be also stored as well as stock prices if any stock position is present.
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getIBKR()
#'}
getIBKR <- function() {

  # replace_date  <- function(x) {
  #   sub("(\\d{2}).(\\d{2}).(\\d{4})","\\3\\2\\1",x)
  # }

  #### Start retrieving currency pairs
  # Tbasics::display_message("Retrieving EUR/USD !")
  # EUR = try(reticulate::py$getCurrencyPairValue("EURUSD",reqType=2))
  # if (is.null(EUR)) EUR=Tbasics::enter_numerical_data("EUR/USD")
  # else if (is.na(EUR)) EUR=Tbasics::enter_numerical_data("EUR/USD")
  #
  # Tbasics::display_message("Retrieving CHF/USD !")
  # CHF = try(reticulate::py$getCurrencyPairValue("CHFUSD",reqType=2))
  # if (is.null(CHF)) CHF=Tbasics::enter_numerical_data("CHF/USD")
  # else if (is.na(CHF)) CHF=Tbasics::enter_numerical_data("CHF/USD")
  #
  # Sys.sleep(1)
  Tbasics::display_message("Call IBKR to retrieve data...")

  l = reticulate::py$getIBKRData()

  if (typeof(l) != "list") {
    Tbasics::display_error_message("No IB connection possible!")
    return()
  }

  ### Open connection to user DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

  #### 1. Process new account data #################
  account_data = l[[1]]
  DBI::dbAppendTable(conn,"Account", account_data)

  #### 2. Process new prices for underlyings part of the portfolio  ##########
  uprices_data = l[[2]]
  DBI::dbAppendTable(conn,"Prices",uprices_data)

  #### Process portfolio last position #############
  portf_data = l[[3]]

  ### Retrieve opened trades
  open_trades = getActiveTrades(account_data$account)

  ### Extract TradeNr and Instrument - some instrument may have been part of the trade but closed and still appear here
  ### currency, expdate is empty for treasury bills
  open_trades_instrument=dplyr::distinct(dplyr::select(open_trades, TradeNr, Instrument, Currency, Exp.Date))

  ### Generate type field from secType IBKR field - default case it is equal to secType
  portf_data = dplyr::mutate(portf_data, type= dplyr::case_match(secType,"STK" ~ "Stock",
                                                                 c("OPT","FOP") ~ dplyr::if_else(right=="P","Put","Call"),
                                                                 "FUT" ~ "Future", "BILL" ~ "TreasuryBill",
                                                                 .default = secType),
                                        .keep="unused")

  ### In case of stocks set multiplier to 1 and have multipliers of other types of instrument set as integer
  portf_data$multiplier = dplyr::if_else(portf_data$type == "Stock", 1, as.integer(portf_data$multiplier))
  ### For stocks set delta to 1
  portf_data$delta = dplyr::if_else(portf_data$type == "Stock", 1, portf_data$delta)


  ### In case one single instrument has been used in several trades - I choose first trade as trade number
  ### It is also possible that trades not yet recorded appear in portf_data and that closed trades are still opened in trades recorded
  ### portf_data should come first - if necessary trade_nr will be equal to NA
  portf_data = dplyr::left_join(portf_data, open_trades_instrument, multiple="first")

  ### Move TradeNr to 1st place
  portf_data = dplyr::select(portf_data, TradeNr, dplyr::everything())

  ### Verify that all portf_data have been matched by a TradeNr
  ### If it is not the case then display a warning message to end-user
  if (any(is.na(portf_data$TradeNr))) {
    unmatched_instruments = portf_data[is.na(portf_data$TradeNr),"Instrument"]
    Tbasics::display_message("One or several instrument could not be matched in DB Trades table !!")
    cat(unmatched_instruments)
  }

  ### Append to DB
  DBI::dbAppendTable(conn,account_data$account,portf_data)


  #### 3. Process new currency data ##############
  currency_pairs_data <- l[[4]]
  currencies_list = currency_pairs_data[[1]]
  currencies_values = currency_pairs_data[[2]]

  ### Retrieve last record date either from DB or from Yahoo - date is in character/integer format
  last_date <- as.Date(as.character(getLastCurrencyPairs()$date), "%Y%m%d")

  ### In case last record date is prior to today then look at the record
  ### Otherwise no reason to save it
  ### It will be saved only if all are different from NaN
  if ((last_date != Sys.Date()) && (!all(is.na(currencies_values)))) {
    usd = data.frame(date = as.integer(format(Sys.Date(), "%Y%m%d")),
                     currency = currencies_list,
                     usd_value = currencies_values)
    usd = usd[!is.na(usd$usd_value),]

    DBI::dbAppendTable(conn, "CurrencyPairs", usd)
  }


  DBI::dbDisconnect(conn)
}

##############################
#'   getGonet
#'
#' This function loads current Gonet positions, the list of all Gonet trades, and retrieves current price information from IBKR (or end-user).
#' It then computes the unrealized PnL (as sum of current market value and total cost incurred), deduce then the average cost per current position.
#' It stores result in DB "Gonet" table.
#'
#' Once Gonet trades are retrieved from GonetTrades.csv file, it computes total cost by summing all symbol-related cashflows, stores it in \code{cost}.
#'
#' It then retrieves last available prices (named \code{mktPrice}) from IBKR - or from end-user- and compute \code{mktValue = mktPrice * pos},
#' \code{unPnL = mktValue + cost}, \code{avgCost = cost / pos}
#' Finally it stores updated Gonet portfolio positions into DB "Gonet" table.
#'
#' Resulting columns in Gonet table are \code{TradeNr, date, heure, symbol,
#' pos, mktPrice, mktValue, avgCost, unPnL, currency and type}.
#'
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getGonet()
#'}
getGonet <- function() {

   gonet_pos = suppressWarnings(readr::read_delim(file="C:/Users/aldoh/Documents/NewTrading/GonetPos.csv",delim=";",
                                                    show_col_types = FALSE,
                                                    locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))

   gonet_trades = suppressWarnings(readr::read_delim(file="C:/Users/aldoh/Documents/NewTrading/GonetTrades.csv",delim=";",
                                                    show_col_types = FALSE,
                                             locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))

  ### Get the list of trades and deduce what are the remaining positions - see Gonet.R
  ### Remove all null position and verify there are no negative positions (no short on Gonet)
  ### Only position that exist (<>0) are taken into account for computations - incl. unrealized PnL

  ### This will build the portfolio current position
  portf_cashflow <- dplyr::summarize(dplyr::group_by(gonet_trades, sym_yahoo),
                                  TradeNr = dplyr::first(TradeNr),
                                  cost = sum(init_cost),
                                  currency = dplyr::first(currency))

  portf <- dplyr::filter(gonet_pos, position > 0)
  portf$date <- format(Sys.Date(),"%Y%m%d")
  portf$heure <- format(Sys.time(),"%H:%M:%S")  ### Allows for several recordings in the same day
  portf = dplyr::left_join(portf, portf_cashflow, by = c("sym_yahoo" = "sym_yahoo"))

  ### get prices from IBKR using list_sec= "STK", and otherwise values from GonetTrades
  last_price <- getIBKRPrice(sym = portf$sym_ibkr, currency = portf$currency,
                              exchange = portf$exchange)

  #### Request user to enter prices where price = NaN
  price_user <- last_price[is.nan(last_price$price),]

  ### Replace NaN values with price entered by user in price_user
  price_user$price <- Tbasics::enter_numerical_data(price_user$sym)

  ### Merge prices with value retrieved from IBKR plus prices with values entered by user
  last_price <- rbind(last_price[!is.nan(last_price$price),],
                      price_user)

  ### Use these last_price as price for portf
  portf = dplyr::left_join(portf, last_price, by = c("sym_ibkr"="sym"))

  ### Compute all necessary fields for storing in CSV/DB
  portf <- dplyr::mutate(portf, secType="STK", symbol=sym_ibkr, pos=position, type="Stock",
                    mktPrice=price, mktValue=round(pos*mktPrice,2),
                    unPnL=price*pos+cost,
                    avgCost=round(-cost/pos,2))

  portf <- dplyr::select(portf, TradeNr, date, heure, secType, symbol, pos, mktPrice, mktValue,
                         avgCost, unPnL, currency, type)

  ### store prices in .CSV / DB
  ## Make them available for other functions
  ### Open connection to user DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  DBI::dbAppendTable(conn,"Gonet", portf)
  ### Non NaN prices already been stored in DB by getIBKRPrice function - so only price_user need to be stored in DB
  DBI::dbAppendTable(conn,"Prices", price_user)
  DBI::dbDisconnect(conn)
}


############################
#'   getAccountGonet
#'
#'This function reads last portfolio from Gonet and then deduces account record similar to IBKR
#'and stores it into DB "Account" table.
#'The tricky piece is to manage Cash positions
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getAccountGonet()
#'}
getAccountGonet <- function() {

  account.var = c("account","date","heure","NetLiquidation","EquityWithLoanValue","FullAvailableFunds","FullInitMarginReq","FullMaintMarginReq","FullExcessLiquidity","OptionMarketValue","StockMarketValue","UnrealizedPnL","RealizedPnL","TotalCashBalance","CashFlow")
  portf <- readLastPortfolio("Gonet")

  #### There are "portf_lines" opened positions in the GOnet portfolio (stocks)
  #### Some may be empty (NA lines) -> in this case the whole is considered as NA and therefore not stored
  #### ### DO not take into account days where one of the exchanges (NYSE, Euronext, SMI) is closed

  acc = dplyr::summarize(portf, StockMarketValue = round(sum(convert_to_usd_date(mktValue, currency, Sys.Date()), na.rm = FALSE),2),
                UnrealizedPnL = round(sum(convert_to_usd_date(unPnL, currency, Sys.Date()), na.rm = FALSE),2))
  if (any(is.na(acc))) {
    Tbasics::display_error_message("Could not get a complete potfolio record - some prices are missing -> no account recorded")
    return()
  }

  ### Create a cash position in Gonet where 26'000 EUR from June 1st, 2022 till March 15th
  ### After March 15th, 2023 cash position is closed
  Cash_EUR = xts::xts(c(rep(26000,287),rep(0,Sys.Date()-as.Date("2022-06-01")-286)),
               order.by = seq(as.Date("2022-06-01"), length=Sys.Date() - as.Date("2022-06-01") + 1,by="days"))

  #### Create a USD Cash position - closed on March 15th, 2023
  Cash_USD = xts::xts(c(rep(33000,287), rep(0,Sys.Date()-as.Date("2022-06-01")-286)),
               order.by = seq(as.Date("2022-06-01"), length = Sys.Date() - as.Date("2022-06-01") + 1, by="days"))
  # names(Cash_USD)="Cash_USD"
  # Cash_USD = data.frame(date=as.Date(zoo::index(Cash_USD)), Cash_USD=as.numeric(Cash_USD))

  #### Add Cash positions to acc data frame using date as join
 cash_balance <- round(convert_to_usd_date(as.numeric(Cash_EUR[Sys.Date()]), "EUR", Sys.Date()) +
                  as.numeric(Cash_USD[Sys.Date()]), 2)

  ### convert to USD all Gonet positions
  acc <- dplyr::mutate(acc, account="Gonet",
                    date = format(Sys.Date(),"%Y%m%d"),
                    heure = format(Sys.time(),"%H:%M:%S"),
                    TotalCashBalance = cash_balance,
                    NetLiquidation = round(TotalCashBalance + StockMarketValue, 2),
                    EquityWithLoanValue = NetLiquidation,
                    FullAvailableFunds = TotalCashBalance,
                    CashFlow = 0,
                    FullInitMarginReq = NA,
                    FullMaintMarginReq = NA,
                    FullExcessLiquidity = NA,
                    OptionMarketValue = 0,
                    RealizedPnL = 0,
    )

    ### Remove Cash positions
    # acc=select(acc,!any_of(c("Cash_EUR","Cash_CHF","Cash_USD")))

    #### account;date;heure;NetLiquidation;EquityWithLoanValue;FullAvailableFunds;FullInitMarginReq;
    ####  FullMaintMarginReq;FullExcessLiquidity;OptionMarketValue;StockMarketValue;
    ####  UnrealizedPnL;RealizedPnL;TotalCashBalance;CashFlow
    acc = dplyr::select(acc, dplyr::all_of(account.var))
    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    DBI::dbAppendTable(conn,"Account",acc)
    DBI::dbDisconnect(conn)
}

############################
#'   getAccountLive
#'
#'This function reads last portfolio from Gonet and then deduces account record similar to IBKR
#'and stores it into DB "Account" table.
#'The tricky piece is to manage Cash positions
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getAccountLive()
#'}
getAccountLive <- function() {

  account.var=c("account","date","heure","NetLiquidation","EquityWithLoanValue","FullAvailableFunds","FullInitMarginReq","FullMaintMarginReq","FullExcessLiquidity","OptionMarketValue","StockMarketValue","UnrealizedPnL","RealizedPnL","TotalCashBalance","CashFlow")
  account.var.gonet = c(account.var[1:6],account.var[10:15])
  s_date = format(Sys.Date(),"%Y%m%d")

  #### Assumes that Gonet and Uxxx are already in this file -
  #### This is a post processing function that computes Live data from these 2 accounts

  ### Processes only data that is posterior to s_date - so it can be used on a regular basis (every day or so)

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  account_d <- DBI::dbReadTable(conn,"Account")

  ### No need to transform date format, comparison done using character comparison
  account_d <- dplyr::filter(account_d, date >= s_date)

  acc1 = dplyr::select(dplyr::filter(account_d,account=="U1804173"), dplyr::all_of(account.var))
  acc2 = dplyr::select(dplyr::filter(account_d,account=="Gonet"), dplyr::all_of(account.var.gonet))

  data = dplyr::inner_join(acc1, acc2, by="date", multiple="any", suffix=c(".1",".2"))

  #### Empty lines in Gonet account due to closed days in Europe that are not closed in US (ex: 10.04.2023 - Easter Monday)
  #### And vice-versa - in this case it is not possible to produce a Live account -> exit function
  data = data[!is.na(data$NetLiquidation.2),]

  if(!nrow(data)) {
      Tbasics::display_error_message("Not enough data to process for write_account_live function!!! Needs both Uxx and Gonet data")
      return()
  }

  #### Add all the columns that are common to Gonet and Uxxx -
  #### knowing that Gonet columns is a subset of Uxxx columns
  ### Remove fields that can't be added: account, date, heure
  sub.var.gonet = account.var.gonet[-(1:3)]
  account.var.1 = paste0(sub.var.gonet,".1")
  account.var.2 = paste0(sub.var.gonet,".2")

  ### Compute sum of fields - Live is a virtual account sum of Uxx and Gonet account
  sub_res= round(data[account.var.1] + data[account.var.2],2)
  names(sub_res) = sub.var.gonet

  #### Build Live account record from previous data
  data=cbind(account = "Live", date = data$date, heure = data["heure.1"],
             data[c("FullInitMarginReq", "FullMaintMarginReq", "FullExcessLiquidity")], sub_res)
  data = dplyr::rename(data, heure = heure.1)
  data= dplyr::select(data,dplyr::all_of(account.var))

  ### Stored in DB the Live account record
  DBI::dbAppendTable(conn, "Account", data)
  DBI::dbDisconnect(conn)
}

