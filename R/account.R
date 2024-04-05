#### Account related utilities
#'   readAccount
#'
#' This function reads the Account.csv file and formats it with right Date and HMS format
#' It then filters data so that it matches \code{accountnr} number
#'@param accountnr is the account number (IBKR)
#'@returns a tibble with the following fields: \code{ account	date	heure
#' NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
#' FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
#'  CashFlow}
#'@export
readAccount = function(accountnr) {
  # file=paste0(config::get("DirNewTrading"),"Account",".csv")
  # account_data = suppressMessages(read_delim(file=file,delim=";",
  #                                            locale=locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))
  account_data = pool::dbReadTable(.GlobalEnv$mydb,"Account")
  ### account	date	heure
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  ### Starts on Oct 4th, 2022 for IBKR, on June 1st for Gonet

  ### Use internal R date format instead of European date format
  account_data$date=account_data$rdate
  account_data$heure=hms::parse_hms(account_data$heure)
  dplyr::filter(account_data,account==accountnr)
}



############# PORTFOLIO specific functions #############
#' readPortfolio
#'
#'
#' This function reads the input file and performs a bunch of data wrangling before returning a tibble with all data.
#'
#'
#' Data wrangling:
#' 1. formats it with right Date and HMS format
#' 2. converts \code{position} to \code{integer}
#' 3. removes all CASH positions
#' 4. rename IBKR columns to more friendly names
#' 5. Remove secType and right columns and replace them by \code{type} column.
#' This assumes a right column exists in portfolio file
#' \code{type} value is either Stock, Put, Call or NA for anything else

#'
#'@param portfname is the absolute path to a portfolio file. Portfolio file is a CSV file that must follow IBKR naming rules.
#'@returns a tibble with the following columns:
#' \code{date; heure; symbol; type; expiration; strike; pos; mktPrice; optPrice}
#' \code{mktValue; avgCost; uPnL; IV; pvDividend; delta; gamma; vega; theta; uPrice; multiplier; currency}
#'@export
readPortfolio = function(portfname) {
  message("readPortfolio")

  #### Test if requested portfolio is present in DB (e.g. Live portfolio does not exist)
  name = pool::dbGetQuery(.GlobalEnv$mydb,"SELECT name FROM sqlite_master WHERE type='table' AND name=?",params=list(portfname))

  if (nrow(name)==1) {
    # portf= suppressMessages(read_delim(file=file,delim=";",
    #                                    locale=locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))
    ### portf contains all following columns: date;heure;secType;symbol;lastTradeDateOrContractMonth;strike;
    # right;position;marketPrice;optPrice;
    # marketValue;averageCost;unrealizedPnL;impliedVol;pvDividend;delta;gamma;vega;theta;undPrice;multiplier;currency
    ### Convert from European date format to internal R date format

    portf = pool::dbReadTable(.GlobalEnv$mydb,portfname)
    portf$heure=hms::parse_hms(portf$heure)
    ### With DB it is not necewssary to convert position into an integer (this is not a float)
    ### portf$position=as.integer(portf$position)
    ## There are no CASH positions that are virtual
    ### portf = dplyr::filter(portf, secType!="CASH")

    ## For nicer output on screen -makes column names shorter

    ### Case where there are options in the portfolio - these field names come from IBKR - cannot be changed
    if ("lastTradeDateOrContractMonth" %in% colnames(portf)) portf = dplyr::rename(portf, expiration=lastTradeDateOrContractMonth)
    if ("undPrice" %in% colnames(portf)) portf = dplyr::rename(portf, uPrice=undPrice)
    if ("impliedVol" %in% colnames(portf)) portf = dplyr::rename(portf,IV=impliedVol)

    #### Remove special Gonet "USD_" fields if present
    portf = dplyr::select(portf,!dplyr::starts_with("USD_"))

    portf = dplyr::rename(portf, pos=position,
                      mktPrice=marketPrice, mktValue=marketValue,
                      avgCost=averageCost,uPnL=unrealizedPnL)

    # portf = dplyr::mutate(portf, .after=symbol,
    #                       type= dplyr::case_match(secType,
    #                                               "STK" ~ "Stock",
    #                                               c("OPT","FOP") ~ dplyr::if_else(portf$right=="P","Put","Call"),
    #                                               "FUT" ~ "Future",
    #                                             .default = secType),
    #                     .keep="unused")
    # portf = dplyr::select(portf,-right)

    return(portf)
  }
  else {
    message("Portfolio doesn't exist, please check portfolio name")
    display_error_message("Portfolio doesn't exist, please check portfolio name")
    return(dplyr::tibble())
  }
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
  ### dates are dates when data are provided - there should be only one data point per dates
  if (!all(!duplicated(dates))) {
    display_error_message("twr:All dates must be different!")
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
      display_error_message("twr:Cash flows number of elements different from Porfolio values!!!!")
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
#' This function computes net position for each Greek, using as input Greek value for each individual option
#'
#' Each position will be multiplied by multiplier and a Greek to obtain the Greek net value.
#' All Greek net values will be summed over all positions, for each Greek.
#'
#'@param portf a data frame with one line per instrument, may be grouped by date and time.
#'It should contain at least the following columns: \code{type;pos; mktPrice; delta; gamma; vega; theta; uPrice;
#'multiplier; currency} - see also readPortfolio function
#'@returns a data frame with \code{delta, deltadollars, gamma, theta, vega} for each group
#'@export
greeksNet = function(portf) {
  if ("delta" %in% colnames(portf)) {

    ### First converts underlying price from symbol's currency to USD
    usd=getCurrencyPairs()
    EUR=usd$EUR
    CHF=usd$CHF
    portf=dplyr::mutate(portf,uPrice=as.numeric(purrr::map2(uPrice,currency,convert_to_usd,EUR,CHF)))

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
                     deltadollars=currency_format(sum(ddnet,na.rm=FALSE),"USD"),
                     gamma=sum(gnet,na.rm=FALSE),
                     theta=sum(tnet,na.rm=FALSE),
                     vega=sum(vnet,na.rm=FALSE))
  }
}


#############  getIBKR function ###############
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
getIBKR <- function() {

  replace_date  <- function(x) {
    sub("(\\d{2}).(\\d{2}).(\\d{4})","\\3\\2\\1",x)
  }

  l = reticulate::py$getIBKRData()

  #### Process new account data
  account_data = l[[1]]
  account_type = switch(account_data$account,"U1804173"="Live","DU5221795"="Simu")

  account = dplyr::mutate(account_data, new_date= replace_date(date), rdate=as.Date(as.character(new_date),"%Y%m%d"),.after=date)
  pool::dbAppendTable(.GlobalEnv$mydb,"Account",account)

  #### Process new prices for underlyings part of the portfolio
  uprices_data = l[[2]]
  pool::dbAppendTable(.GlobalEnv$mydb,"Prices",uprices_data)

  #### Process portfolio last positions
  portf_data = dplyr::select(l[[3]], date, heure, secType, symbol, lastTradeDateOrContractMonth, strike,
                             right, position, marketPrice, optPrice, marketValue, averageCost, unrealizedPnL=unrealizedPNL,
                             impliedVol, pvDividend, delta, gamma, vega, theta, undPrice, multiplier, currency)

  ### Retrieve opened trades
  open_trades = dplyr::filter(getAllTrades(), Statut == "Ajust\U00e9" | Statut ==  "Ouvert", Account == account_type)
  ### Extract TradeNr and Instrument - some instrument may have been part of the trade but closed and still appear here
  open_trades_instrument=dplyr::distinct(dplyr::select(open_trades,TradeNr, Instrument))

  ### Transform fresh data from IBKR
  portf_data = dplyr::mutate(portf_data, expdate=as.Date(lastTradeDateOrContractMonth,"%Y%m%d"), .before=lastTradeDateOrContractMonth)
  portf_data = dplyr::mutate(portf_data, type= dplyr::case_match(secType,"STK" ~ "Stock",
                                                                 c("OPT","FOP") ~ dplyr::if_else(right=="P","Put","Call"),
                                                                 "FUT" ~ "Future",
                                                                 .default = secType))

  ### field right not needed anymore - removed
  portf_data$right=NULL

  portf_data = dplyr::mutate(portf_data,Instrument=buildInstrumentName(symbol,expdate,strike,type))
  ### In case one single instrument has been used in several trades - I choose first trade as trade number
  ### It is also possible that trades not yet recorded appear in portf_data and that closed trades are still opened in trades recorded
  ### portf_data should come first - if necessary trade_nr will be equal to NA
  portf_data = dplyr::left_join(portf_data, open_trades_instrument,multiple="first")

  ### Move TradeNr to 1st place
  portf_data = dplyr::select(portf_data, TradeNr, everything())

  ### Add some date related stuff
  portf_data = dplyr::mutate(portf_data, new_date= replace_date(date), rdate=as.Date(as.character(new_date),"%Y%m%d"),.after=date)

  ### Append to DB
  pool::dbAppendTable(.GlobalEnv$mydb,account_data$account,portf_data)
}
