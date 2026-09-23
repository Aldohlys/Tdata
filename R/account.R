#### Account related utilities

# Indirection points so tests can swap in fixture tables (TestAccount /
# TestAccountWithConversionRate) without touching the SQL. Production code
# uses the live names; tests use with_mocked_bindings to point them at
# fixture equivalents. See test-account.R.
get_account_view_name <- function() "AccountWithConversionRate"
get_account_table_name <- function() "Account"

#'   readAccount
#'
#' This function reads the Account table.
#' It then filters data so that it matches \code{accountnr} number
#' Finally it formats account data with right Date and HMS format
#'@param account_name is the account name (IBKR)
#'@returns a tibble with the following fields: \code{ account	date	heure Currency
#' NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
#' FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
#'  CashFlow Notes}. \code{Notes} is free text (e.g. why a cash flow was
#'  recorded) and, unlike the amounts, is not converted to the base currency.
#'@examples
#'\dontrun{
#'readAccount("DU5555")
#'}
#'@export
readAccount = function(account_name) {

  #### Open database and prepare disconnection
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add= TRUE)

  ### account	date	heure
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  ### Starts on Oct 4th, 2022 for IBKR, on June 1st for Gonet

  base_currency <- getParam("BaseCurrency")

  ### Should not be needed - just in case no access to DB was possible ###
  if (!(base_currency %in% c("USD", "CHF"))) {
    logger::log_error("Could not find base currency equal to CHF or USD", namespace="Tdata")
    Tbasics::display_message("Could not find base currency equal to CHF or USD!")
    return(data.frame())
  }

  ## Then determine which base currency is and corresponding conversion rate
  conversion_column <- ifelse(base_currency == "CHF", "chf_conversion_rate", "usd_conversion_rate")

  sql_query <- paste0("SELECT
      date, heure, Currency,
      NetLiquidation * ", conversion_column, " AS NetLiquidation,
      EquityWithLoanValue * ", conversion_column, " AS EquityWithLoanValue,
      FullAvailableFunds * ", conversion_column, " AS FullAvailableFunds,
      FullInitMarginReq * ", conversion_column, " AS FullInitMarginReq,
      FullMaintMarginReq * ", conversion_column, " AS FullMaintMarginReq,
      FullExcessLiquidity * ", conversion_column, " AS FullExcessLiquidity,
      OptionMarketValue * ", conversion_column, " AS OptionMarketValue,
      StockMarketValue * ", conversion_column, " AS StockMarketValue,
      UnrealizedPnL * ", conversion_column, " AS UnrealizedPnL,
      RealizedPnL * ", conversion_column, " AS RealizedPnL,
      TotalCashBalance * ", conversion_column, " AS TotalCashBalance,
      CashBalanceCHF * ", conversion_column, " AS CashBalanceCHF,
      CashBalanceEUR * ", conversion_column, " AS CashBalanceEUR,
      CashBalanceUSD * ", conversion_column, " AS CashBalanceUSD,
      CashFlow * ", conversion_column, " AS CashFlow,
      Notes
  FROM ", get_account_view_name(), "
  WHERE account = ?")

  # Execute parameterized query
  account_data <- DBI::dbGetQuery(conn, sql_query, params = list(account_name))


  ### If there is at least one line then do conversion date and heure
  if (nrow(account_data) != 0) {
    ### Convert to internal R date format from of integer date format
    account_data$date=as.Date(as.character(account_data$date),"%Y%m%d")
    account_data$heure=hms::parse_hms(account_data$heure)
  }
  else Tbasics::display_message(paste0("No data recorded for ", accountnr))
  return(account_data)
}



############# PORTFOLIO specific functions
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
#'
#' N.B: It will first check that portfname exists as a table in DB, and returns an error if not
#'@param portfname is a string that is a name of a portfolio table into local DB.
#'local DB path is retrieved through config.yaml file
#'@returns a data frame with the following columns:
#' \code{TradeNr; date; heure; symbol; expdate; strike; pos}
#' \code{mktPrice; optPrice; mktValue; avgCost; unPnL; IV; pvDividend}
#' \code{delta; gamma; vega; theta; uPrice; multiplier; currency; type; Instrument; margin}
#'
#'@examples
#'\dontrun{
#'readPortfolio("DU5555")
#'}
#'@export
readPortfolio = function(portfname) {
  message("readPortfolio")

  conn <- safe_db_connect()

  #### Check if requested portfolio is present in DB (e.g. Live portfolio does not exist)
  name = DBI::dbGetQuery(conn,"SELECT name FROM sqlite_master WHERE type='table' AND name=?",params=list(portfname))

  ### If portfolio exists there is one and only one portfolio name referred
  if (nrow(name)==1) {

    ### read table from DB - no collect function necessary as there is no lazy evaluation later implied (no dplyr)
    portf = DBI::dbReadTable(conn, portfname)
    DBI::dbDisconnect(conn)

    ### Convert from European date format to internal R date format
    portf$date <- as.Date(as.character(portf$date),"%Y%m%d")
    portf$heure <- hms::parse_hms(portf$heure)

    ### With DB it is not necessary to convert position into an integer (this is not a float)
    ### portf$position=as.integer(portf$position)
    ## There are no CASH positions that are virtual
    ### portf = dplyr::filter(portf, type!="CASH")

    return(portf)
  }
  else {
    DBI::dbDisconnect(conn)

    Tbasics::display_message("Portfolio doesn't exist, please check portfolio name")
    return(dplyr::tibble())
  }
}

############# PORTFOLIO specific functions
#' readPortfolioDate
#'
#'
#' This function reads a portfolio table given as entry
#' and performs a bit of data wrangling before returning a data frame with all data.
#'
#'
#' Data wrangling:
#' 1. formats it with right internal R Date format and heure HMS format
#'
#'
#' N.B: It will first check that portfname exists as a table in DB, and returns an error message if not
#'@param portfname is a string that is a name of a portfolio table into local DB.
#'local DB path is retrieved through config.yaml file
#'@param date is a date to filter only records for a given date.
#'@returns a data frame with the following columns:
#' \code{TradeNr; heure; symbol; expdate; strike; pos}
#' \code{mktPrice; optPrice; mktValue; avgCost; unPnL; IV; pvDividend}
#' \code{delta; gamma; vega; theta; uPrice; multiplier; currency; type; Instrument; margin}
#'
#'@examples
#'\dontrun{
#'readPortfolio("DUxxx", Sys.Date())
#'}
#'@export
readPortfolioDate = function(portfname, date) {
  message("readPortfolioDate")

  conn <- safe_db_connect()

  #### Check if requested portfolio is present in DB (e.g. Live portfolio does not exist)
  name = DBI::dbGetQuery(conn,"SELECT name FROM sqlite_master WHERE type='table' AND name=?",params=list(portfname))

  ### If portfolio exists there is one and only one portfolio name referred
  if (nrow(name)==1) {

    ### Convert date parameter into integer
    if (!inherits(date, "Date")) Tbasics::display_error_message("date parameter to readPortfolioDate must be a date!")
    t_date = format(date, "%Y%m%d")

    ### read table from DB - no collect function necessary as there is no lazy evaluation later implied (no dplyr)
    query = paste0("SELECT * FROM ",portfname," WHERE DATE=?")
    portf = DBI::dbGetQuery(conn, query, params=list(t_date))
    DBI::dbDisconnect(conn)

    ### Convert from European date format to internal R date format
    portf$heure <- hms::parse_hms(portf$heure)

    return(portf)
  }
  else {
    DBI::dbDisconnect(conn)

    Tbasics::display_message("Portfolio doesn't exist, please check portfolio name")
    return(dplyr::tibble())
  }
}


############# PORTFOLIO specific functions
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
#' N.B: It will first check that portfname exists as a table in DB, and returns an error message if not
#'@param portfname is a string whose value is actual portfolio table name in DB
#'@returns a data frame with the following columns:
#' \code{TradeNr; date; heure; symbol; expdate; strike; pos; }
#' \code{mktPrice; optPrice; mktValue; avgCost; unPnL; IV; pvDividend; }
#' \code{delta; gamma; vega; theta; uPrice; multiplier; currency; type; Instrument; margin}
#'
#'@examples
#'\dontrun{
#'readLastPortfolio("DU5555")
#'}
#'@export
readLastPortfolio <- function(portfname) {
  # ### retrieve last recorded (date, time) - Other possible implementation
  message("readLastPortfolio")
  conn <- safe_db_connect()

  #### Check if requested portfolio is present in DB (e.g. Live portfolio does not exist)
  name = DBI::dbGetQuery(conn,"SELECT name FROM sqlite_master WHERE type='table' AND name=?",params=list(portfname))

  ### If portfolio exists there is one and only one portfolio name referred
  if (nrow(name)==1) {
      ### Build the required string query as needed
      query = paste0("WITH Last_record AS(SELECT max(date) as date, heure FROM (SELECT date, MAX(heure) as heure FROM ",
                     portfname,
                     " GROUP BY date)) SELECT * FROM ",
                     portfname,
                     " WHERE date= (SELECT date FROM Last_record) AND heure= (SELECT heure FROM Last_record)")
      last_portf = DBI::dbGetQuery(conn, query)
      DBI::dbDisconnect(conn)

      ### Convert from European date format to internal R date format
      ### NB date is stored as integer in DB so conversion to character is really necessary - not to be fancy
      last_portf$date <- as.Date(as.character(last_portf$date),"%Y%m%d")
      last_portf$heure <- hms::parse_hms(last_portf$heure)
      if ("expdate" %in% colnames(last_portf)) last_portf$expdate <- as.Date(as.character(last_portf$expdate),"%Y%m%d")

      return(last_portf)
  }

  else {
    DBI::dbDisconnect(conn)

    Tbasics::display_message("Portfolio doesn't exist, please check portfolio name")
    return(dplyr::tibble())
  }

}


###############  TWR function
#'   twr
#'
#' Time-Weighted Return for an irregular series of observed end-of-day
#' net-liquidation snapshots and cashflows.
#'
#' Each consecutive pair of OBSERVED dates is treated as one return period:
#'   rn[i] = NLV[i] / (NLV[i-1] + CF[i])
#' Cash flow CF[i] is assumed to occur at the start of period i (i.e.,
#' applied to the prior observed NLV before measuring the return).
#'
#' No interpolation across missing calendar days. Previously the function
#' linearly interpolated NLV on every absent day, which injected phantom
#' gains/losses whenever a cashflow event followed a multi-day gap.
#'
#'@param dates vector of dates (one observation per date)
#'@param e_nlv End-of-period net-liquidation values aligned with `dates`
#'@param cashflows Start-of-period cashflows aligned with `dates`
#'@returns numeric vector of cumulative TWRs aligned with input `dates`
#'@export
twr <- function(dates, e_nlv, cashflows) {
  message("twr")
  if (!all(!duplicated(dates))) {
    Tbasics::display_error_message("twr:All dates must be different!")
    return(NA_real_)
  }

  n <- length(dates)
  if (length(e_nlv) != n) {
    Tbasics::display_error_message("twr:NLV length does not match dates length")
    return(NA_real_)
  }
  if (missing(cashflows)) cashflows <- rep(0, n)
  if (length(cashflows) != n) {
    Tbasics::display_error_message("twr:Cash flows number of elements different from Portfolio values!!!!")
    return(NA_real_)
  }

  if (n == 1L) return(0)

  ### Sort by date so chaining proceeds chronologically regardless of input order
  ord <- order(dates)
  e_nlv_sorted <- e_nlv[ord]
  cf_sorted <- cashflows[ord]

  rn <- numeric(n)
  twr_acc <- numeric(n)
  twr_acc[1] <- 1   # First date is reference; cumulative TWR there is 0

  for (i in 2:n) {
    denom <- e_nlv_sorted[i - 1] + cf_sorted[i]
    rn[i] <- if (is.na(denom) || denom == 0 || is.na(e_nlv_sorted[i])) NA_real_ else e_nlv_sorted[i] / denom
    ### An undefined sub-period return (zero/NA base — e.g. the first snapshot
    ### recorded before account funding settles, leaving NLV = 0 — or a missing
    ### NLV) must not poison the cumulative chain via NA multiplication. Treat it
    ### as neutral so chaining resumes once a valid base exists again.
    if (is.na(rn[i])) rn[i] <- 1
    twr_acc[i] <- twr_acc[i - 1] * rn[i]
  }

  ### Restore original input ordering
  out <- numeric(n)
  out[ord] <- twr_acc - 1
  return(out)
}

#'   greeksNet
#'
#' This function computes for a portfolio the net position of each Greek, summing over all positions the Greek value of each individual position.
#'
#' Each position will be multiplied by multiplier and a Greek to obtain the Greek net value fo the position.
#' All Greek net values will be then summed up over all positions, for each Greek. If data is grouped, then Greeks will be computed separately for each group (summarize will do the trick).
#'
#'@param portf a data frame with one line per instrument, may be grouped by date and time.
#'Either it contains only \code{pos; mktPrice} columns and then only delta and delta notional are computed
#'or it contains \code{type; pos; multiplier; delta; gamma; vega; theta; uPrice;
#' theta; uPrice} - these are named after portfolio tables in DB, see also readPortfolio function.
#' and then all Greeks are computed. Type is necessary to have a distinction between stocks and options.
#'@returns a data frame with \code{delta, deltanotional, gamma, theta, vega} and \code{currency}
#' for each group.
#'\code{deltanotional} is expressed in the group's own (trade) currency, carried back in the
#' \code{currency} column. Each group is therefore expected to hold a single currency (e.g. one
#' symbol, one trade, or one position). Callers that aggregate across currencies must first
#' convert each group's \code{deltanotional} to a common currency (see \code{convert_to_base_date})
#' before summing.
#'@export
greeksNet = function(portf) {
  ## Manage case of Gonet portfolio - without options
  if (!all(c("type","pos", "multiplier", "delta", "uPrice", "gamma", "theta", "vega")
      %in% colnames(portf))) {
    dplyr::summarize(dplyr::mutate(portf, dnet = pos, ddnet = pos*mktPrice, gnet = 0, tnet = NA_real_, vnet = NA_real_),
                     delta=sum(dnet,na.rm=FALSE),
                     deltanotional=sum(ddnet,na.rm=FALSE),
                     gamma=0,
                     theta= NA_real_,
                     vega= NA_real_,
                     currency=dplyr::first(currency))
  }

  else {
    #### portf is grouped by datetime (or symbol / position)
    #### Therefore summarize will do the computation per group.
    #### deltanotional stays in the group's trade currency (no FX conversion here);
    #### the currency is carried back so aggregating callers can convert before summing.
    portf_extended <- dplyr::mutate(portf,
                                   dnet=dplyr::case_when(
                                     (type=="Stock"| type=="Future") ~ 1*pos,
                                     (type=="Call" | type=="Put") ~ multiplier*delta*pos,
                                     type=="CASH" ~ 1*pos,  # CASH: linear exposure to FX rate
                                     TRUE ~ 0),
                                   ## uPrice is the OPTION underlying price (IBKR undPrice from
                                   ## modelGreeks) and is 0 for non-options. A stock/future is its
                                   ## own underlying, so its notional uses mktPrice; options use uPrice.
                                   ddnet=dplyr::case_when(
                                     type=="Stock" ~ 1*pos*mktPrice,
                                     type=="Future" ~ multiplier*pos*mktPrice,
                                     (type=="Call" | type=="Put") ~ multiplier*delta*pos*uPrice,
                                     type=="CASH" ~ mktValue,  # CASH: mktValue already in the row's currency (base)
                                     type=="TreasuryBill" ~ mktValue,  # bond-like: notional = its market (dollar) value
                                     TRUE ~ 0),  # CFD (and other types) intentionally left out
                                   gnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                       multiplier*gamma*pos,
                                                       0),  # CASH: no gamma
                                   tnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                       multiplier*theta*pos,
                                                       0),  # CASH: no theta
                                   vnet=dplyr::if_else((type=="Call" | type=="Put"),
                                                       multiplier*vega*pos,
                                                       0))  # CASH: no vega

    if (any(is.na(portf_extended[,c("dnet", "ddnet", "gnet", "tnet", "vnet")]))) {
      logger::log_warn("Greeks computation returns NA because one or several positions Greeks are NA", namespace="Tdata")
    }

    dplyr::summarize(portf_extended,
              delta=sum(dnet,na.rm=FALSE),
              deltanotional=sum(ddnet,na.rm=FALSE),
              gamma=sum(gnet,na.rm=FALSE),
              theta=sum(tnet,na.rm=FALSE),
              vega=sum(vnet,na.rm=FALSE),
              currency=dplyr::first(currency)
              )
  }
}



#'   getIBKR
#'
#' This function retrieves account, portfolio data from IBKR and then store them in DB
#'
#' Account data will be stored in Account table, portfolio data in Uxxx or DUxxx table, depending upon account data.
#'
#'@returns an integer, between 0 (no value returned) and 3 (account, portfolio and margin data retrieved and stored)
#' \itemize{
#' \item{0 =  could not access to IBKR or DB}
#' \item{1 = account data retrieved from IBKR and stored in data}
#' \item{2 = portfolio and account data retrieved from IBKR and stored in data}
#' \item{3 = margin, portfolio and account data retrieved from IBKR and stored in data}
#' }
#'@examples
#'\dontrun{
#'getIBKR()
#'}
#'@export
getIBKR <- function(account = NULL) {

  exit_code = 0

  ### Test first if IB is available - no use to continue if not
  if (!isIBAvailable()) return(exit_code)

  ### Retrieve account and portfolio data in a list
  l = tdata_py$getIBKRData(account)

  if (typeof(l) != "list") {
    warning("No value returned from IB!")
    return(exit_code)
  }

  #### 1. Process new account data
  account_data = l[[1]]

  ### There should be exactly 1 line retrieved for one account
  if (nrow(account_data) != 1) return(exit_code)

  ### Open connection to user DB and prepare for exit properly
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ### Add Base Currency to the date
  account_data <- dplyr::mutate(account_data, Currency = getParam("BaseCurrency"))
  safe_db_append(conn,"Account", account_data)

  ### New account data retrieved properly, update exit code
  exit_code = 1

  #### 2. Process portfolio last position
  portf_data = l[[2]]

  ### Test if no data then exit the function
  if (nrow(portf_data) == 0) return(exit_code)

  #### 3. Process currency balances for CASH positions
  currency_balances = l[[3]]

  ### Initialize empty CASH portfolio data
  cash_portf_data = data.frame()

  ### Following Python extract, all fields are either double or character
  portf_data = dplyr::mutate(portf_data,
                             date = as.integer(date),
                      pos = as.integer(pos),
                      multiplier = as.integer(multiplier))

  ### Retrieve opened trades
  open_trades = getActiveTrades(account_data$account)

  ### Extract TradeNr and Instrument - some instrument may have been part of the trade but closed and still appear here
  ### currency, expdate is empty for treasury bills
  open_trades_instrument=dplyr::distinct(dplyr::select(open_trades, TradeNr, Strategy, Instrument, Symbol, Currency, Exp.Date))

  ### Generate type field from secType IBKR field - default case it is equal to secType
  portf_data = dplyr::mutate(portf_data, type= dplyr::case_match(secType,"STK" ~ "Stock",
                                                                 c("OPT","FOP") ~ dplyr::if_else(right=="P","Put","Call"),
                                                                 "FUT" ~ "Future", "BILL" ~ "TreasuryBill",
                                                                 .default = secType),
                                        .keep="unused")

  ### In case of stocks set multiplier to 1 and have multipliers of other types of instrument set as integer
  ### Price is 100 face value. but position is counted in 1000's so multiplier allows to have
  ### mktValue = pos * mktPrice * multiplier - just like options
  portf_data$multiplier = dplyr::if_else(portf_data$type == "Stock", as.integer(1),
                                         dplyr::if_else(portf_data$type == "TreasuryBill", as.integer(10),
                                                 portf_data$multiplier))
  ### For stocks set delta to 1
  portf_data$delta = dplyr::if_else(portf_data$type == "Stock", 1, portf_data$delta)


  ### In case one single instrument has been used in several trades - I choose first trade as trade number
  ### It is also possible that trades not yet recorded appear in portf_data and that closed trades are still opened in trades recorded
  ### portf_data should come first - if necessary trade_nr will be equal to NA

  portf_data = dplyr::mutate(portf_data,
                             Instrument = dplyr::if_else(type=="TreasuryBill",
                                                         as.character(conId),
                                                         Tbasics::buildInstrumentName(symbol,as.Date(as.character(expdate),"%Y%m%d"),
                                                                                      strike,
                                                                                      type)),
                             symbol = dplyr::if_else(type=="TreasuryBill", "US-T", symbol)
                             )

  ### Remove CASH positions from IBKR portfolio — they are handled separately
  ### by create_cash_portfolio_row() using currency_balances data, which correctly
  ### links to open trades via getCashTradeForCurrency()
  portf_data <- dplyr::filter(portf_data, type != "CASH")

  ### Stocks: join on symbol == Symbol (Instrument is IBKR company name, doesn't match ticker)
  ### Options/Futures/TreasuryBill: join on Instrument (buildInstrumentName matches trade Instrument)
  portf_stocks <- dplyr::filter(portf_data, type == "Stock")
  portf_other <- dplyr::filter(portf_data, type != "Stock")

  if (nrow(portf_stocks) > 0) {
    trades_for_stocks <- dplyr::distinct(dplyr::select(open_trades_instrument, TradeNr, Strategy, Symbol, Currency, Exp.Date))
    portf_stocks <- dplyr::left_join(portf_stocks, trades_for_stocks,
                                     by = c("symbol" = "Symbol"), multiple = "first")
  }
  if (nrow(portf_other) > 0) {
    trades_for_other <- dplyr::distinct(dplyr::select(open_trades_instrument, TradeNr, Strategy, Instrument, Currency, Exp.Date))
    portf_other <- dplyr::left_join(portf_other, trades_for_other,
                                    by = "Instrument", multiple = "first")
  }
  portf_data <- dplyr::bind_rows(portf_stocks, portf_other)

  ### No portfolio data to process further - this may happen if opened trades and portfolio are not in sync
  if (nrow(portf_data) == 0) return(exit_code)

  ### Now we got account + portfolio data
  exit_code = 2

  portf_data = dplyr::mutate(portf_data,
                             currency = dplyr::if_else(type=="TreasuryBill", Currency, currency),
                             expdate = dplyr::if_else(type=="TreasuryBill", format(as.Date(Exp.Date,format="%d.%m.%Y"),"%Y%m%d"),
                                                      expdate),
                             marginable = dplyr::if_else(Strategy %in% c("WHEEL", "OFI", "CS"), "Yes", "No"),
                             Currency = NULL,
                             Exp.Date = NULL)

  portf_data = dplyr::arrange(dplyr::group_by(portf_data, TradeNr), TradeNr, pos)

  ### Add margin data
  do_compute_margin = getParam("ComputeMargin")
  if (!is.null(do_compute_margin) &&  do_compute_margin == "Yes") {
    result = compute_margin_data(portf_data, exit_code)
    exit_code = result$exit_code
    portf_data = result$portf_data
  }

  ## Remove all data that cannot be stored in Portfolio table, used by compute_margin_data function
  portf_data = dplyr::mutate(portf_data,
                             Strategy = NULL,
                             conId = NULL,
                             marginable = NULL,
                             contracts = NULL)

  ### Move TradeNr column as first column
  portf_data = dplyr::select(portf_data, TradeNr, dplyr::everything())

  ### Verify that all portf_data have been matched by a TradeNr
  ### If it is not the case then display a warning message to end-user
  if (any(is.na(portf_data$TradeNr))) {
    unmatched_instruments = portf_data[is.na(portf_data$TradeNr),"Instrument"]
    logger::log_info("One or several instruments could not be matched in DB Trades table : {unmatched_instruments}", namespace="Tdata")
  }

  #### Process currency balances and create CASH portfolio rows
  if (!is.null(currency_balances) && nrow(currency_balances) > 0) {
    logger::log_debug("Processing {nrow(currency_balances)} currency balances for CASH positions", namespace="Tdata")

    base_currency <- getParam("BaseCurrency")

    # Get snapshot timestamp from regular portfolio (CRITICAL: must match for readLastPortfolio)
    snapshot_date <- portf_data$date[1]  # Already in YYYYMMDD integer format from Python
    snapshot_heure <- portf_data$heure[1]  # Already in HH:MM:SS format from Python

    ### Create CASH portfolio rows for non-base currencies
    cash_rows <- lapply(seq_len(nrow(currency_balances)), function(i) {
      curr <- currency_balances$currency[i]
      bal <- currency_balances$balance[i]

      # Skip base currency, BASE total, or near-zero balances
      if (curr %in% c(base_currency, "BASE") || abs(bal) < 0.01) {
        return(NULL)
      }

      create_cash_portfolio_row(
        currency = curr,
        balance = bal,
        snapshot_date = snapshot_date,
        snapshot_heure = snapshot_heure,
        account_table = account_data$account
      )
    })

    ### Combine CASH rows and append to portfolio
    cash_df <- do.call(rbind, Filter(Negate(is.null), cash_rows))

    if (!is.null(cash_df) && nrow(cash_df) > 0) {
      logger::log_info("Adding {nrow(cash_df)} CASH positions to portfolio", namespace="Tdata")

      ### Ensure type compatibility before rbind
      # Convert expdate column to match portf_data type (character from Python)
      if ("expdate" %in% colnames(portf_data) && "expdate" %in% colnames(cash_df)) {
        # Match the type from portf_data
        if (is.character(portf_data$expdate)) {
          cash_df$expdate <- as.character(cash_df$expdate)
        } else if (is.integer(portf_data$expdate)) {
          portf_data$expdate <- as.integer(portf_data$expdate)
        }
      }

      ### Append CASH rows to portfolio data
      portf_data <- rbind(portf_data, cash_df)
    }
  }

  ### Append combined portfolio (stocks + options + CASH) to DB
  safe_db_append(conn,account_data$account,portf_data)

  ### Account data, portfolio data and potentially margin data retrieved and stored
  return(exit_code)
}


## Builds the ConvertToUSD and ConvertToCHF rows from an IBKR forex fetch.
## `usd_per_unit` is what tdata_py$retrieveCurrencyPairs returns: USD per 1 unit
## of each currency (price for a direct pair, 1/price for an inverted one).
## ConvertToUSD stores the pair AS QUOTED (units per USD when `direct` is "No",
## e.g. JPY ~157), like the Yahoo path; ConvertToCHF stores CHF per 1 unit.
## Until 5.20.9 the caller inverted the inverted pairs a second time and divided
## by CHF-per-USD instead of multiplying: GBP/CHF 1.683 (2026-06-13) and 1.671
## (09-05), JPY/CHF 195.96 (08-09). The branch writes only when Yahoo has no rate
## for the day yet -- weekends mostly -- hence the sporadic errors.
ibkr_fx_rows <- function(currencies, usd_per_unit, direct, chf_per_usd, date) {
  usd <- data.frame(date = date, currency = currencies,
                    usd_value = round(ifelse(direct == "No", 1 / usd_per_unit, usd_per_unit), 4))
  chf <- data.frame(date = date, currency = currencies,
                    chf_value = round(usd_per_unit * chf_per_usd, 6))
  list(usd = usd[!is.na(usd$usd_value), , drop = FALSE],
       chf = chf[!is.na(chf$chf_value), , drop = FALSE])
}

## Drops rows whose rate moved more than `max_jump` (default 5%) from the last
## stored rate of the same currency, logging each. A daily FX move of that size
## does not happen between these currencies; a wrong pair, an inversion or a bad
## data tick does -- ConvertToCHF held GBP 0.577 (a CAD-sized value) on
## 2026-04-21 from the Yahoo path. Currencies with no stored rate pass.
fx_drop_implausible <- function(df, value_col, stored_ccy, stored_value,
                                source = "", max_jump = 0.05) {
  if (is.null(df) || nrow(df) == 0) return(df)
  last <- stored_value[match(df$currency, stored_ccy)]
  ratio <- df[[value_col]] / last
  bad <- !is.na(ratio) & is.finite(ratio) & abs(ratio - 1) > max_jump
  for (i in which(bad))
    logger::log_warn("{source}: {df$currency[i]} rate {df[[value_col]][i]} is {round(100 * (ratio[i] - 1), 1)}% from the last stored {last[i]} - not written",
                     namespace = "Tdata")
  df[!bad, , drop = FALSE]
}

#' getIBKRActiveCurrencyValues
#'
#' Retrieves current currency values from IBKR and updates both ConvertToUSD and ConvertToCHF tables
#'
#' This function retrieves active currencies pairs values from DB ActiveCurrencies table,
#' and then :
#' \itemize{
#' \item{1. Queries Yahoo service, and update DB if more recent data is obtained from Yahoo service.}
#' \item{2. Queries IBKR using Forex contracts, and update DB if more recent data is obtained from IBKR.}
#' }
#'
#' This does not request any value from end-user yet, in case IBKR does not return any value.
#'
#' @return List with USD and CHF update counts
#' @export
getIBKRActiveCurrencyValues <- function() {
  ### Open connection to user DB
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ### Skip USD, CHF and all inactive currencies for the IBKR-pair loop
  ### (USD has no IBKRPair; CHF is the base currency with ratio 1.0)
  Tbasics::display_message("Retrieve currencies from DB...")
  currency_data <- DBI::dbGetQuery(conn, "SELECT Name, IBKRPair, DirectConversion FROM Currencies
                                          WHERE Active = 'Yes' AND Name NOT IN ('USD', 'CHF')")

  ### Update with Yahoo data first - DB update will be done during calls
  Tbasics::display_message("Yahoo service to retrieve data... stored in DB if more recent than data in DB")
  currencies <- currency_data$Name

  # Get stored values for both USD and CHF
  stored_usd_values <- getLastUSDValue(currencies)
  stored_chf_values <- getLastCHFValue(currencies)

  ### Refresh USD/CHF rate separately via Yahoo CHFUSD=X (USD has no IBKRPair,
  ### so it's excluded from the main loop; getLastCHFValue handles USD as special case)
  usd_active <- DBI::dbGetQuery(conn, "SELECT 1 FROM Currencies WHERE Name='USD' AND Active='Yes'")
  if (nrow(usd_active) > 0) {
    getLastCHFValue("USD")  # side effect: appends to ConvertToCHF if newer
  }

  ### Check if data is already current for today
  today_date <- as.integer(format(Sys.Date(), "%Y%m%d"))
  usd_current <- all(stored_usd_values$date == today_date)
  chf_current <- all(stored_chf_values$date == today_date)

  if (usd_current && chf_current) {
    Tbasics::display_message("All currency data is already up to date - no need to query!")
    return(list(usd_updates = 0, chf_updates = 0))
  }

  # Create lookup vectors for efficient comparison
  stored_usd_dates <- setNames(stored_usd_values$date, stored_usd_values$currency)
  stored_chf_dates <- setNames(stored_chf_values$date, stored_chf_values$currency)

  ### Test first if IB is available - no use to continue if not
  if (!isIBAvailable()) return(list(usd_updates = 0, chf_updates = 0))

  ### Prepare to retrieve data from IBKR
  currency_pairs <- currency_data$IBKRPair
  direct_conv <- currency_data$DirectConversion

  Tbasics::display_message("Call IBKR to retrieve data...")
  currency_pairs_data <- tdata_py$retrieveCurrencyPairs(currencies, currency_pairs, direct_conv)

  #### Process new currency data
  currencies_list <- currency_pairs_data[[1]]
  currencies_values <- currency_pairs_data[[2]]

  ### If any retrieved data is different from NA then build the prices
  if (any(!is.na(currencies_values))) {

    ### Conversion and table conventions: see ibkr_fx_rows.
    direct <- currency_data$DirectConversion[match(currencies_list, currency_data$Name)]
    # CHF per 1 USD, from ConvertToCHF; fallback: ConvertToUSD CHF is USD per 1 CHF.
    chf_per_usd <- getStoredCHFValue("USD")$chf_value
    if (length(chf_per_usd) == 0 || is.na(chf_per_usd)) {
      chf_per_usd <- 1 / getStoredUSDValue("CHF")$usd_value
    }
    rates <- ibkr_fx_rows(currencies_list, currencies_values, direct, chf_per_usd, today_date)
    ibkr_usd <- rates$usd
    ibkr_chf <- rates$chf

    ### Never append a rate that jumped more than 5% from the last stored one.
    ibkr_usd <- fx_drop_implausible(ibkr_usd, "usd_value", stored_usd_values$currency,
                                    stored_usd_values$usd_value, "ConvertToUSD (IBKR)")
    ibkr_chf <- fx_drop_implausible(ibkr_chf, "chf_value", stored_chf_values$currency,
                                    stored_chf_values$chf_value, "ConvertToCHF (IBKR)")

    # Process USD updates
    usd_updates_needed <- ibkr_usd |>
      dplyr::filter(date > stored_usd_dates[currency] | is.na(stored_usd_dates[currency]))

    usd_update_count <- 0
    if (nrow(usd_updates_needed) > 0) {
      safe_db_append(conn, "ConvertToUSD", usd_updates_needed)
      logger::log_info("Updated {nrow(usd_updates_needed)} USD currency rates", namespace = "Tdata")
      usd_update_count <- nrow(usd_updates_needed)
    } else {
      logger::log_info("No USD updates needed - stored data is current", namespace = "Tdata")
    }

    # Process CHF updates
    chf_updates_needed <- ibkr_chf |>
      dplyr::filter(date > stored_chf_dates[currency] | is.na(stored_chf_dates[currency]))

    chf_update_count <- 0
    if (nrow(chf_updates_needed) > 0) {
      safe_db_append(conn, "ConvertToCHF", chf_updates_needed)
      logger::log_info("Updated {nrow(chf_updates_needed)} CHF currency rates", namespace = "Tdata")
      chf_update_count <- nrow(chf_updates_needed)
    } else {
      logger::log_info("No CHF updates needed - stored data is current", namespace = "Tdata")
    }

    return(list(usd_updates = usd_update_count, chf_updates = chf_update_count))

  } else {
    logger::log_info("No valid currency data retrieved from IBKR", namespace = "Tdata")
    return(list(usd_updates = 0, chf_updates = 0))
  }
}

## Realized FX gain/loss (base currency) on closed / partially-closed Gonet
## trades in one currency. Walks the currency's stock trades chronologically
## with average-cost lots; on each sell, realized FX =
##   closed_native_cost * (sell_rate - avg_entry_rate).
## CASH ledger rows (sym_ibkr == the currency code) are excluded, and base-
## currency trades net zero (rate == 1 throughout). Returns 0 when there are
## no closing legs. This is the FX that has actually been banked into cash via
## trading; open positions keep their unrealized FX inside their own valuation.
gonet_realized_fx <- function(gonet_trades, ccy) {
  tr <- gonet_trades[gonet_trades$currency == ccy & gonet_trades$sym_ibkr != ccy, , drop = FALSE]
  if (nrow(tr) == 0) return(0)

  dates <- as.Date(as.character(tr$orig_date), format = "%d.%m.%Y")
  ord <- order(dates)
  tr <- tr[ord, , drop = FALSE]; dates <- dates[ord]

  rfx <- 0
  for (sym in unique(tr$sym_ibkr)) {
    idx <- which(tr$sym_ibkr == sym)
    shares <- 0; nat_cost <- 0; chf_cost <- 0
    for (i in idx) {
      n <- tr$init_position[i]
      if (is.na(n) || n == 0) next
      if (n > 0) {                                    # buy: add to the lot
        bcost <- -tr$init_cost[i]                     # native cost of the buy (>= 0)
        shares   <- shares + n
        nat_cost <- nat_cost + bcost
        chf_cost <- chf_cost + bcost * convert_to_base_date(1, ccy, dates[i])
      } else if (shares > 0) {                        # sell: realize FX on the closed portion
        frac <- min(1, (-n) / shares)
        closed_nat <- nat_cost * frac
        closed_chf <- chf_cost * frac
        entry_rate <- if (closed_nat != 0) closed_chf / closed_nat else 1
        rfx <- rfx + closed_nat * (convert_to_base_date(1, ccy, dates[i]) - entry_rate)
        shares   <- shares + n
        nat_cost <- nat_cost - closed_nat
        chf_cost <- chf_cost - closed_chf
      }
    }
  }
  round(rfx, 2)
}

## Which fetched prices are unusable. IBKR does not signal an unsubscribed
## instrument with NaN -- getValue returns a plain 0 alongside "Error 354,
## Requested market data is not subscribed" -- so a test on is.nan() alone let
## the 0 through and priced the position at zero. Anything non-finite or
## non-positive means no price.
gonet_price_missing <- function(price) {
  !is.finite(price) | price <= 0
}

## The price to use for each symbol whose fetch returned nothing: the operator's
## if there is an operator to ask, otherwise the carried-forward default.
##
## Asking is only safe at an interactive prompt. Without a console
## Tbasics::enter_numerical_data falls through to readLines("stdin"), which
## BLOCKS rather than returning -- and getGonet runs unattended from
## daily_portfolio_update.R, so one missing price would hang the scheduled task
## for ever. Under Shiny the answer is the defaults either way: that is what
## enter_numerical_data's own isRunning() branch returns.
gonet_prices_or_ask <- function(syms, defaults, ask = interactive()) {
  if (!ask) return(defaults)
  Tbasics::enter_numerical_data(syms, defaults)
}

## Last price actually observed for each symbol, used when the live fetch
## returns nothing.
##
## IBKR does not signal an unsubscribed instrument with NaN -- getValue returns
## a plain 0 alongside "Error 354, Requested market data is not subscribed"
## (NUCL, DTLA, CNYA on LSEETF). A 0 priced the position at zero and reported
## the whole cost as a loss, so "no price" has to mean every non-positive or
## non-finite value, not just NaN.
##
## The Gonet snapshot is searched first and the Prices table second. Snapshots
## are written several times a day while Prices is only appended when a price is
## typed in by hand -- it held NUCL from April and DTLA from December, and
## nothing at all for CNYA. Rows priced at 0 by this very bug are skipped, so a
## bad snapshot is not carried forward.
##
## `unchanged_days` says how long that price has stood still, so a symbol IBKR
## has quietly stopped quoting does not sit at the same number for ever without
## anyone noticing.
##
## Returns one row per symbol found: sym, price, asof, source, unchanged_days.
gonet_last_known_price <- function(syms) {
  empty <- data.frame(sym = character(), price = numeric(),
                      asof = character(), source = character(),
                      unchanged_days = integer(),
                      stringsAsFactors = FALSE)
  syms <- unique(syms[!is.na(syms)])
  if (length(syms) == 0) return(empty)

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ph   <- paste(rep("?", length(syms)), collapse = ",")
  snap <- DBI::dbGetQuery(conn, paste0(
    "SELECT symbol AS sym, mktPrice AS price, date, heure FROM Gonet ",
    "WHERE symbol IN (", ph, ") AND mktPrice > 0 ",
    "ORDER BY date DESC, heure DESC"), params = as.list(syms))

  found <- empty
  if (nrow(snap) > 0) {
    ### Rows arrive newest first. Walk back while the price is unchanged and
    ### date the earliest one: a carried-forward price is written back
    ### identical, so a long run is the signature of a symbol that has stopped
    ### being quoted. A genuinely traded instrument can print the same value
    ### twice, so over a day or two this means little -- over many days it is
    ### the signal that the carried price is no longer standing in for anything.
    rows <- lapply(unique(snap$sym), function(s) {
      d    <- snap[snap$sym == s, , drop = FALSE]
      same <- abs(d$price - d$price[1]) < 1e-9
      run  <- if (all(same)) nrow(d) else which(!same)[1] - 1L
      data.frame(sym = s, price = d$price[1],
                 asof = paste(d$date[1], d$heure[1]),
                 source = "Gonet snapshot",
                 unchanged_days = as.integer(Sys.Date() - as.Date(d$date[run], "%Y%m%d")),
                 stringsAsFactors = FALSE)
    })
    found <- do.call(rbind, rows)
  }

  missing <- setdiff(syms, found$sym)
  if (length(missing) > 0) {
    stored <- getStoredMetrics(missing)
    if (nrow(stored) > 0) {
      stored <- stored[!is.na(stored$price) & stored$price > 0, , drop = FALSE]
      if (nrow(stored) > 0)
        found <- rbind(found, data.frame(
          sym = stored$sym, price = stored$price,
          asof = as.character(stored$datetime), source = "Prices table",
          ### The Prices table keeps one row per hand-entered price, so there is
          ### no run to walk: the age of that single entry is the age.
          unchanged_days = as.integer(
            Sys.Date() - as.Date(substr(as.character(stored$datetime), 1, 8), "%Y%m%d")),
          stringsAsFactors = FALSE))
    }
  }
  found
}

## Latest daily close from Yahoo for each symbol IBKR could not price.
##
## NUCL, DTLA and CNYA are not subscribed on LSEETF, so IBKR never prices them;
## TRE7 is subscribed but trades a handful of shares a day, so it often has no
## last either. Carrying the previous snapshot forward only freezes them, and a
## snapshot taken before the day's first trade freezes them at yesterday's close
## at best. Yahoo publishes a daily close for all of them (on a no-volume day it
## is the quote), which is what the bank values them at.
##
## `sym_yahoo` is the GonetPos.csv column, named by `sym` (sym_ibkr). A price is
## rejected when it is more than a factor 2 away from `reference` -- the last
## known price -- which is the signature of a pence/pound quote or of the wrong
## listing, not of a market move.
##
## Returns one row per symbol priced: sym, price, asof, source.
gonet_yahoo_price <- function(sym, sym_yahoo, reference = rep(NA_real_, length(sym)),
                              fetch = gonet_fetch_yahoo_close) {
  empty <- data.frame(sym = character(), price = numeric(), asof = character(),
                      source = character(), stringsAsFactors = FALSE)
  rows <- lapply(seq_along(sym), function(i) {
    y <- sym_yahoo[i]
    if (is.na(y) || !nzchar(y) || y == "NA") return(NULL)
    q <- tryCatch(fetch(y), error = function(e) {
      logger::log_warn("Gonet: Yahoo fetch failed for {y}: {conditionMessage(e)}", namespace = "Tdata")
      NULL
    })
    if (is.null(q) || gonet_price_missing(q$price)) return(NULL)
    ref <- reference[i]
    if (!is.na(ref) && ref > 0 && (q$price / ref > 2 || q$price / ref < 0.5)) {
      logger::log_warn("Gonet: Yahoo {y} = {q$price} is more than 2x away from the last known {ref} - ignored",
                       namespace = "Tdata")
      return(NULL)
    }
    data.frame(sym = sym[i], price = q$price, asof = q$asof,
               source = paste("Yahoo", y), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) empty else do.call(rbind, rows)
}

## Last non-missing daily close of one Yahoo symbol: list(price, asof).
gonet_fetch_yahoo_close <- function(sym_yahoo) {
  x <- suppressWarnings(quantmod::getSymbols(sym_yahoo, src = "yahoo",
                                             from = Sys.Date() - 14,
                                             auto.assign = FALSE))
  cl <- stats::na.omit(quantmod::Cl(x))
  if (NROW(cl) == 0) return(NULL)
  list(price = as.numeric(cl[NROW(cl)]),
       asof  = format(zoo::index(cl)[NROW(cl)], "%Y%m%d"))
}

## Converts IBKR quotes into the currency the Gonet position is booked in, for
## symbols whose Tickers row quotes another listing (AMRZ: USD in Tickers, CHF
## at Gonet). `quote_ccy` and `pos_ccy` are named by sym. Missing prices and
## symbols without both currencies pass through unchanged. Uses today's rate,
## the rate the snapshot is valued at.
gonet_quote_to_position_ccy <- function(last_price, quote_ccy, pos_ccy,
                                        rate = function(ccy) convert_to_base_date(1, ccy, Sys.Date())) {
  if (is.null(last_price) || nrow(last_price) == 0) return(last_price)
  for (i in seq_len(nrow(last_price))) {
    s  <- last_price$sym[i]
    qc <- unname(quote_ccy[s]); pc <- unname(pos_ccy[s])
    if (length(qc) == 0 || length(pc) == 0 || is.na(qc) || is.na(pc) || qc == pc) next
    if (gonet_price_missing(last_price$price[i])) next
    conv <- last_price$price[i] * rate(qc) / rate(pc)
    logger::log_info("Gonet: {s} quoted in {qc} ({last_price$price[i]}), position booked in {pc} - converted to {round(conv, 4)}",
                     namespace = "Tdata")
    last_price$price[i] <- conv
  }
  last_price
}

## Cash events attributed to a trade. A GonetTrades.csv row whose sym_ibkr is a
## currency code is a cash ledger row. When its TradeNr also appears on a
## non-cash leg the row is a cash flow belonging to that trade -- a dividend, a
## coupon, a tax refund -- and not part of the cash baseline; the baseline rows
## carry TradeNrs of their own (26/27/28) that match no trade.
##
## Sign convention on an attributed row: init_position is the cash balance
## delta, init_cost the amount attributable to the trade as P&L. A dividend has
## them equal (all profit, no basis relieved): +117.09 / +117.09. A baseline row
## instead carries init_cost = -init_position, cash acquired at zero gain.
##
## The date plays no part in the attribution -- the QQQ dividend of 10.07.2026
## falls on the baseline date itself and still belongs to trade 9. It is used
## only to pick the target position: Gonet TradeNrs are reused across
## instruments, so an event is booked against the leg current on its own date.
gonet_cash_events <- function(gonet_trades) {
  empty <- data.frame(TradeNr = integer(), date = as.Date(character()),
                      sym_yahoo = character(), amount = numeric(),
                      currency = character(), pos_currency = character(),
                      balance_delta = numeric(), stringsAsFactors = FALSE)
  if (nrow(gonet_trades) == 0) return(empty)

  ### A bare `==` yields NA where either side is NA, and an NA row index hands
  ### back a phantom all-NA row rather than dropping it.
  is_cash <- !is.na(gonet_trades$sym_ibkr) & !is.na(gonet_trades$currency) &
             gonet_trades$sym_ibkr == gonet_trades$currency
  cash    <- gonet_trades[is_cash, , drop = FALSE]
  stock   <- gonet_trades[!is_cash, , drop = FALSE]
  if (nrow(cash) == 0 || nrow(stock) == 0) return(empty)

  ev <- cash[cash$TradeNr %in% stock$TradeNr, , drop = FALSE]
  if (nrow(ev) == 0) return(empty)

  ev_dates    <- as.Date(as.character(ev$orig_date),    format = "%d.%m.%Y")
  stock_dates <- as.Date(as.character(stock$orig_date), format = "%d.%m.%Y")

  rows <- lapply(seq_len(nrow(ev)), function(i) {
    idx <- which(stock$TradeNr == ev$TradeNr[i])
    if (length(unique(stock$sym_yahoo[idx])) > 1)
      logger::log_warn("Gonet TradeNr {ev$TradeNr[i]} spans several instruments ({paste(unique(stock$sym_ibkr[idx]), collapse=', ')}) - cash event of {ev$orig_date[i]} booked against the one held on that date",
                       namespace = "Tdata")
    ### The leg current on the event date; fall back to the earliest leg if the
    ### event predates them all.
    prior <- idx[!is.na(stock_dates[idx]) & stock_dates[idx] <= ev_dates[i]]
    leg   <- if (length(prior)) prior[which.max(stock_dates[prior])]
             else               idx[which.min(stock_dates[idx])]
    data.frame(TradeNr       = ev$TradeNr[i],
               date          = ev_dates[i],
               sym_yahoo     = stock$sym_yahoo[leg],
               amount        = ev$init_cost[i],
               currency      = ev$currency[i],
               pos_currency  = stock$currency[leg],
               balance_delta = ev$init_position[i],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

## Cash balance per currency, rolled forward from the ledger baseline.
##
## Every cash ledger row's init_position is a balance delta, the baseline rows
## included -- theirs is the opening balance read off the bank statement. So the
## balance is that sum plus the cash moved by the stock legs booked since:
##
##   balance(ccy) = sum(init_position) over cash ledger rows in ccy
##                + sum(init_cost)     over non-cash legs in ccy dated after the
##                                     baseline (a buy takes cash out, a sale
##                                     puts proceeds back in)
##
## The baseline date is the earliest date carrying an unattributed cash row
## (10.07.2026). Legs on or before it are already inside the stated balance.
## Unattributed cash rows dated later are ordinary flows -- a deposit, a
## withdrawal, an FX conversion -- and are summed like any other. Re-baselining
## therefore means replacing the old baseline rows, not adding a second set.
##
## GonetPos.csv is not consulted: its CASH rows would have to be re-edited after
## every sale to stay right. as_of restricts the roll-forward to a past date.
## Returns a named vector, empty when no baseline row exists (the caller then
## keeps whatever GonetPos.csv declares).
gonet_cash_balances <- function(gonet_trades, as_of = NULL) {
  none <- stats::setNames(numeric(0), character(0))
  if (nrow(gonet_trades) == 0) return(none)

  dates <- as.Date(as.character(gonet_trades$orig_date), format = "%d.%m.%Y")
  if (!is.null(as_of)) {
    keep  <- !is.na(dates) & dates <= as.Date(as_of)
    gonet_trades <- gonet_trades[keep, , drop = FALSE]
    dates <- dates[keep]
  }
  if (nrow(gonet_trades) == 0) return(none)

  is_cash    <- !is.na(gonet_trades$sym_ibkr) & !is.na(gonet_trades$currency) &
                gonet_trades$sym_ibkr == gonet_trades$currency
  trade_nrs  <- unique(gonet_trades$TradeNr[!is_cash])
  baseline   <- is_cash & !(gonet_trades$TradeNr %in% trade_nrs) & !is.na(dates)
  if (!any(baseline)) return(none)

  cut_off     <- min(dates[baseline])
  stock_after <- !is_cash & !is.na(dates) & dates > cut_off

  ccys <- unique(gonet_trades$currency[is_cash | stock_after])
  ccys <- ccys[!is.na(ccys)]
  vapply(ccys, function(ccy) {
    in_ccy <- !is.na(gonet_trades$currency) & gonet_trades$currency == ccy
    round(sum(gonet_trades$init_position[in_ccy & is_cash], na.rm = TRUE) +
          sum(gonet_trades$init_cost[in_ccy & stock_after], na.rm = TRUE), 2)
  }, numeric(1))
}

## Average-cost lots for Gonet stock positions. Walks each symbol's trades
## chronologically: a buy adds shares and cost, a sell relieves the proportional
## share of the cost and banks the difference as realized P&L. Returns one row
## per symbol carrying the basis still held by the open shares.
##
## Summing init_cost across every leg instead subtracts a sale's full proceeds
## from the surviving shares' basis. That understates avgCost -- negative once
## proceeds exceed the original outlay, as on ABBN after selling 200 of 500 --
## and leaves the realized gain inside unPnL, which then reads as unrealized.
##
## Cash events attributed to a trade (gonet_cash_events) are realized income:
## a dividend adds to `realized` and to nothing else, so the surviving shares
## keep their avgCost and the gain does not read as unrealized. One paid in a
## currency other than the position's -- AMRZ is booked in CHF and pays USD --
## is converted at the event date, the rate at which the cash was received.
##
## The remaining CASH ledger rows (sym_ibkr == the currency code) are excluded;
## they are valued by gonet_realized_fx instead. as_of restricts the walk to
## legs on or before that date, so a historical snapshot can be revalued on the
## basis that applied when it was taken.
gonet_lots <- function(gonet_trades, as_of = NULL) {
  empty <- data.frame(sym_yahoo = character(), TradeNr = integer(),
                      currency = character(), shares = numeric(),
                      basis = numeric(), realized = numeric(),
                      income = numeric(), stringsAsFactors = FALSE)

  legs <- gonet_legs(gonet_trades, as_of)
  if (nrow(legs) == 0) return(empty)

  ### One row per symbol, in the order the symbols first trade -- the walk's
  ### own order. The NA key (precious metals) is matched explicitly, as below.
  syms <- unique(legs$sym_yahoo)
  rows <- lapply(syms, function(sym) {
    l <- if (is.na(sym)) legs[is.na(legs$sym_yahoo), , drop = FALSE]
         else legs[!is.na(legs$sym_yahoo) & legs$sym_yahoo == sym, , drop = FALSE]
    st <- l[!(l$action %in% gonet_income_actions), , drop = FALSE]
    income <- sum(l$realized[l$action %in% gonet_income_actions])
    data.frame(sym_yahoo = sym, TradeNr = st$TradeNr[1],
               currency = st$currency[1],
               shares = if (nrow(st)) st$shares_after[nrow(st)] else 0,
               basis  = if (nrow(st)) st$basis_after[nrow(st)]  else 0,
               realized = sum(l$realized), income = income,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

## Leg actions that move cash but no shares: a dividend or other attributed
## cash event, and the cash paid for the fractions of a free-share grant.
gonet_income_actions <- c("Income", "Grant cash")

## Leg-by-leg history of the Gonet stock positions: the same average-cost walk
## as gonet_lots, kept one row per leg instead of folded into one per symbol.
## gonet_lots is built on it, so the history and the snapshot cannot disagree.
##
## action is Buy, Sell, Grant (a zero-cost free-share attribution), Income (a
## dividend or other cash event attributed to the trade, gonet_cash_events) or
## Grant cash (such an event on a grant's date: payment for the fractions). `realized` is what that leg banked: a sale's
## proceeds less the average cost it relieved, an income row's amount, 0 for a
## buy. shares_after / basis_after are the lot after the leg. Amounts are in
## the position's currency; an income paid in another currency is converted at
## its own date. `total` is the cash moved (init_cost: negative for a buy).
##
## Gonet reuses TradeNrs across instruments, so a leg is identified by
## (TradeNr, sym_yahoo), never by TradeNr alone.
gonet_legs <- function(gonet_trades, as_of = NULL) {
  empty <- data.frame(TradeNr = integer(), sym_yahoo = character(),
                      symbol = character(), date = as.Date(character()),
                      action = character(), pos = numeric(), price = numeric(),
                      total = numeric(), realized = numeric(),
                      shares_after = numeric(), basis_after = numeric(),
                      currency = character(), stringsAsFactors = FALSE)

  tr <- gonet_trades[is.na(gonet_trades$sym_ibkr) | is.na(gonet_trades$currency) |
                       gonet_trades$sym_ibkr != gonet_trades$currency, , drop = FALSE]
  if (nrow(tr) == 0) return(empty)

  dates <- as.Date(as.character(tr$orig_date), format = "%d.%m.%Y")
  if (!is.null(as_of)) {
    keep  <- !is.na(dates) & dates <= as.Date(as_of)
    tr    <- tr[keep, , drop = FALSE]
    dates <- dates[keep]
  }
  if (nrow(tr) == 0) return(empty)

  ord <- order(dates)
  tr  <- tr[ord, , drop = FALSE]
  dates <- dates[ord]

  events <- gonet_cash_events(gonet_trades)
  if (!is.null(as_of) && nrow(events) > 0)
    events <- events[!is.na(events$date) & events$date <= as.Date(as_of), , drop = FALSE]

  rows <- lapply(unique(tr$sym_yahoo), function(sym) {
    ### The precious-metal row carries a literal "NA" sym_yahoo, and `x == NA`
    ### is NA -- which() would drop every one of its legs and hand back a zero
    ### basis. Match the NA key explicitly; the left_join downstream pairs it
    ### with the position row the same way.
    idx <- if (is.na(sym)) which(is.na(tr$sym_yahoo))
           else which(!is.na(tr$sym_yahoo) & tr$sym_yahoo == sym)
    pos_ccy <- tr$currency[idx[1]]
    shares <- 0; basis <- 0
    out <- list()
    for (i in idx) {
      n <- tr$init_position[i]
      if (is.na(n) || n == 0) next
      banked <- 0
      if (n > 0) {                                  # buy: add to the lot
        shares <- shares + n
        basis  <- basis + (-tr$init_cost[i])        # init_cost is cash out (<= 0)
      } else if (shares > 0) {                      # sell: relieve the closed fraction
        frac        <- min(1, (-n) / shares)        # clamp: never relieve more than held
        closed_cost <- basis * frac
        banked      <- tr$init_cost[i] - closed_cost
        shares      <- shares + n
        basis       <- basis - closed_cost
      } else next                                   # a sale with nothing held books nothing
      out[[length(out) + 1]] <- data.frame(
        TradeNr = tr$TradeNr[i], sym_yahoo = sym, symbol = tr$sym_ibkr[i],
        date = dates[i],
        ### A buy at zero cost is a free-share grant (Air Liquide loyalty
        ### attribution): no cash paid, the shares dilute the basis.
        action = if (n < 0) "Sell" else if (isTRUE(tr$init_cost[i] == 0)) "Grant" else "Buy",
        pos = n, price = tr$init_price[i], total = tr$init_cost[i],
        realized = banked, shares_after = shares, basis_after = basis,
        currency = pos_ccy, stringsAsFactors = FALSE)
    }

    ### Dividends and other attributed cash events, in the position's currency.
    ### They move neither shares nor basis; the lot state after them is the
    ### state of the last stock leg on or before their date.
    eidx <- if (nrow(events) == 0) integer(0)
            else if (is.na(sym)) which(is.na(events$sym_yahoo))
            else which(!is.na(events$sym_yahoo) & events$sym_yahoo == sym)
    for (j in eidx) {
      amt <- events$amount[j]
      if (!identical(events$currency[j], pos_ccy))
        amt <- convert_to_base_date(amt, events$currency[j], events$date[j]) /
               convert_to_base_date(1,   pos_ccy,            events$date[j])
      out[[length(out) + 1]] <- data.frame(
        TradeNr = events$TradeNr[j], sym_yahoo = sym, symbol = tr$sym_ibkr[idx[1]],
        date = events$date[j], action = "Income", pos = 0, price = NA_real_,
        total = amt, realized = amt, shares_after = NA_real_, basis_after = NA_real_,
        currency = pos_ccy, stringsAsFactors = FALSE)
    }
    if (length(out) == 0) return(NULL)
    l <- do.call(rbind, out)
    ### Cash booked on a grant's date is the payment for the fractional grant
    ### shares (the statement's "Indemnisation 0.53 AIR LIQUIDE"), not a dividend.
    grant_days <- l$date[l$action == "Grant"]
    l$action[l$action == "Income" & l$date %in% grant_days] <- "Grant cash"
    ### Stock legs keep their walk order (stable sort); income slots in by date.
    is_inc <- l$action %in% gonet_income_actions
    l <- l[order(l$date, is_inc), , drop = FALSE]
    is_inc <- l$action %in% gonet_income_actions
    for (k in seq_len(nrow(l))) if (is_inc[k]) {
      prev <- which(!is_inc[seq_len(k)])
      if (length(prev)) {
        l$shares_after[k] <- l$shares_after[max(prev)]
        l$basis_after[k]  <- l$basis_after[max(prev)]
      }
    }
    l
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

## Reads GonetTrades.csv from the configured Gonet directory. NULL, with a
## warning, when the file is missing.
read_gonet_trades <- function() {
  f <- file.path(config::get("gonet_dir"), "GonetTrades.csv")
  if (!file.exists(f)) {
    warning("Gonet trades file not found: ", f)
    return(NULL)
  }
  suppressWarnings(readr::read_delim(file = f, delim = ";", show_col_types = FALSE,
    locale = readr::locale(date_names = "en", decimal_mark = ".", grouping_mark = "",
                           encoding = "UTF-8")))
}

#'   getGonetLegs
#'
#' Leg-by-leg history of the Gonet positions, read from GonetTrades.csv: every
#' buy, sale and attributed dividend with the realized P&L it banked and the lot
#' (shares, basis) after it. See \code{gonet_legs}.
#'
#' @param as_of Optional date: only legs on or before it.
#' @returns data.frame with TradeNr, sym_yahoo, symbol, date, action, pos, price,
#'   total, realized, shares_after, basis_after, currency. Empty when the file is missing.
#' @export
getGonetLegs <- function(as_of = NULL) {
  tr <- read_gonet_trades()
  if (is.null(tr)) return(gonet_legs(data.frame(sym_ibkr = character(), currency = character()), as_of))
  legs <- gonet_legs(tr, as_of)

  ### Name each leg as the snapshot does. The Gonet table takes `symbol` from
  ### GonetPos.csv, and the two files can disagree (the EUR bond fund is
  ### 433080107 in the trades, IE00B67T5G21 in the positions). sym_yahoo is the
  ### key both share.
  pos_file <- file.path(config::get("gonet_dir"), "GonetPos.csv")
  if (file.exists(pos_file) && nrow(legs) > 0) {
    gp <- suppressWarnings(readr::read_delim(pos_file, delim = ";", show_col_types = FALSE))
    m  <- match(legs$sym_yahoo, gp$sym_yahoo)
    hit <- !is.na(m) & !is.na(legs$sym_yahoo)
    legs$symbol[hit] <- gp$sym_ibkr[m[hit]]
    ### Precious metals: the snapshot names them PM_<ZKB id> (see getGonet).
    pm <- which(is.na(legs$sym_yahoo))
    pm_row <- which(!is.na(gp$type) & gp$type == "Precious Metals")
    if (length(pm) && length(pm_row))
      legs$symbol[pm] <- paste0("PM_", sub(".*FI_ID_NOTATION=([0-9]+).*", "\\1", gp$exchange[pm_row[1]]))
  }
  legs
}

#'   getGonetTradeDates
#'
#' Opening date of each open Gonet position: the date of the first buy of the
#' current holding, i.e. the first buy after the position was last flat.
#' A position closed and reopened under the same symbol counts from the reopening.
#'
#' @param as_of Optional date: the state on that date.
#' @returns data.frame with TradeNr, sym_yahoo, symbol, orig_date (Date).
#' @export
getGonetTradeDates <- function(as_of = NULL) {
  legs <- getGonetLegs(as_of)
  gonet_open_dates(legs)
}

gonet_open_dates <- function(legs) {
  st <- legs[!(legs$action %in% gonet_income_actions), , drop = FALSE]
  empty <- data.frame(TradeNr = integer(), sym_yahoo = character(),
                      symbol = character(), orig_date = as.Date(character()),
                      stringsAsFactors = FALSE)
  if (nrow(st) == 0) return(empty)
  keys <- unique(st$sym_yahoo)
  rows <- lapply(keys, function(sym) {
    l <- if (is.na(sym)) st[is.na(st$sym_yahoo), , drop = FALSE]
         else st[!is.na(st$sym_yahoo) & st$sym_yahoo == sym, , drop = FALSE]
    if (l$shares_after[nrow(l)] <= 0) return(NULL)
    flat  <- which(l$shares_after <= 0)
    first <- if (length(flat)) max(flat) + 1 else 1
    data.frame(TradeNr = l$TradeNr[first], sym_yahoo = sym, symbol = l$symbol[nrow(l)],
               orig_date = l$date[first], stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) empty else do.call(rbind, rows)
}

#'   getGonet
#'
#' This function loads current Gonet positions, the list of all Gonet trades, and retrieves current price information from IBKR (or end-user).
#' It then computes the unrealized PnL (as sum of current market value and total cost incurred), deduce then the average cost per current position.
#' It stores result in DB "Gonet" table.
#'
#' Once Gonet trades are retrieved from GonetTrades.csv file, it walks each symbol's legs as
#' average-cost lots (\code{gonet_lots}) to get the \code{basis} still carried by the open shares
#' and the \code{realized} P&L banked by past sales and by cash events booked against the trade
#' (dividends and coupons, recorded as a cash ledger row carrying the trade's \code{TradeNr}).
#'
#' Cash positions are rolled forward from the ledger baseline by \code{gonet_cash_balances}:
#' the balance of each currency is its baseline row plus every cash flow recorded since, so a
#' sale's proceeds and a dividend land in cash without GonetPos.csv having to be re-edited.
#'
#' It then retrieves last available prices (named \code{mktPrice}) from IBKR - or from end-user- and compute \code{mktValue = mktPrice * pos},
#' \code{unPnL = mktValue - basis}, \code{realizedPnL = realized}, \code{avgCost = basis / pos}
#' Finally it stores updated Gonet portfolio positions into DB "Gonet" table.
#'
#' Resulting columns in Gonet table are \code{TradeNr, date, heure, symbol,
#' pos, mktPrice, mktValue, avgCost, unPnL, realizedPnL, currency and type}.
#'
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getGonet()
#'}
getGonet <- function(use_defaults = FALSE) {

  ### Test first if IB is available - no use to continue if not
  if (!isIBAvailable()) return()

  gonet_dir <- config::get("gonet_dir")
  gonet_pos_file <- file.path(gonet_dir, "GonetPos.csv")
  gonet_trades_file <- file.path(gonet_dir, "GonetTrades.csv")

  if (!file.exists(gonet_pos_file) || !file.exists(gonet_trades_file)) {
    warning("Gonet CSV files not found in ", gonet_dir)
    return(invisible())
  }

  gonet_pos = suppressWarnings(readr::read_delim(file=gonet_pos_file,delim=";",
                                                    show_col_types = FALSE,
                                                    locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))

  gonet_trades = suppressWarnings(readr::read_delim(file=gonet_trades_file,delim=";",
                                                    show_col_types = FALSE,
                                             locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))

  ### Get the list of trades and deduce what are the remaining positions - see Gonet.R
  ### Remove all null position and verify there are no negative positions (no short on Gonet)
  ### Only position that exist (<>0) are taken into account for computations - incl. unrealized PnL

  ### This will build the portfolio current position
  portf_cashflow <- gonet_lots(gonet_trades)

  portf <- dplyr::filter(gonet_pos, position > 0)
  portf$date <- format(Sys.Date(),"%Y%m%d")
  portf$heure <- format(Sys.time(),"%H:%M:%S")  ### Allows for several recordings in the same day
  portf = dplyr::left_join(portf, portf_cashflow, by = c("sym_yahoo" = "sym_yahoo"))

  ### Split CASH positions out of the price-fetch pipeline. Cash is not a
  ### tradeable IBKR symbol, so it is priced by the FX rate to base and its
  ### unPnL is the realized FX banked through closed trades (see below); it is
  ### recombined with the stock rows just before the DB append.
  cash_mask <- !is.na(portf$type) & portf$type == "CASH"
  portf_cash <- portf[cash_mask, , drop = FALSE]
  portf <- portf[!cash_mask, , drop = FALSE]

  ### Cash balances come from the ledger, not from GonetPos.csv. Every flow since
  ### the baseline is already a leg -- sale proceeds, purchases, attributed
  ### dividends -- so the CSV would have to be re-edited after each one to stay
  ### right, and while it was stale a sale simply destroyed value in the snapshot
  ### (stock market value fell, cash did not rise). Its CASH rows still declare
  ### which currency books exist; the `position` they carry is the baseline value
  ### and is no longer read.
  cash_ledger <- gonet_cash_balances(gonet_trades)
  if (nrow(portf_cash) > 0 && length(cash_ledger) > 0) {
    unvalued <- setdiff(names(cash_ledger)[abs(cash_ledger) > 0.005], portf_cash$sym_ibkr)
    if (length(unvalued) > 0)
      logger::log_warn("Gonet ledger holds cash in {paste(unvalued, collapse=', ')} with no CASH row in GonetPos.csv - that balance is not valued",
                       namespace = "Tdata")
    ### gonet_lots drops the cash ledger rows, so the left_join above left every
    ### cash position with TradeNr NA -- and the Trade tab, which filters on
    ### !is.na(TradeNr), stopped showing cash at all, taking its realized FX out
    ### of the tab's TOTAL. Take the TradeNr from the currency's baseline row.
    is_cash_row  <- !is.na(gonet_trades$sym_ibkr) & !is.na(gonet_trades$currency) &
                    gonet_trades$sym_ibkr == gonet_trades$currency
    baseline_row <- is_cash_row & !(gonet_trades$TradeNr %in% gonet_trades$TradeNr[!is_cash_row])
    for (i in seq_len(nrow(portf_cash))) {
      ccy <- portf_cash$sym_ibkr[i]
      if (is.na(ccy)) next
      if (ccy %in% names(cash_ledger)) portf_cash$position[i] <- cash_ledger[[ccy]]
      own <- which(baseline_row & gonet_trades$currency == ccy)
      if (length(own) > 0) portf_cash$TradeNr[i] <- gonet_trades$TradeNr[own[1]]
    }
  }

  ### GonetPos.csv is authoritative for the share count, but the basis comes from
  ### the GonetTrades.csv legs. If they disagree a leg is missing, and avgCost
  ### would be silently divided by a share count the basis never paid for.
  share_gap <- !is.na(portf$shares) & abs(portf$shares - portf$position) > 1e-6
  if (any(share_gap)) {
    logger::log_warn("Gonet share count differs from trade legs for {paste(portf$sym_ibkr[share_gap], collapse=', ')} - avgCost/unPnL may be wrong",
                     namespace = "Tdata")
  }

  ### Handle precious metals positions (gold coins, etc.) with web-based pricing
  ### Identify positions with type == "Precious Metals" and URL in exchange field
  ### Update sym_ibkr BEFORE IBKR price fetch to avoid fetching NA symbols
  precious_metals_mask <- !is.na(portf$type) & portf$type == "Precious Metals"

  if (any(precious_metals_mask)) {
    for (i in which(precious_metals_mask)) {
      url <- portf$exchange[i]

      ### Extract identifier from URL (FI_ID_NOTATION parameter)
      url_id <- sub(".*FI_ID_NOTATION=([0-9]+).*", "\\1", url)

      ### Create unique symbol identifier and update portf
      pm_symbol <- paste0("PM_", url_id)
      portf$sym_ibkr[i] <- pm_symbol

      logger::log_info("Found precious metal position: {pm_symbol}", namespace = "Tdata")
    }
  }

  ### get prices from IBKR - split by exchange type to use correct reqType
  ### Some exchanges (LSEETF, EBS, ALLFUNDS) require delayed data (reqType=4)
  ### while others use frozen data (reqType=2)

  delayed_exchanges <- c("LSEETF", "EBS", "ALLFUNDS")

  # Identify which symbols need delayed data
  # Exclude precious metals symbols (start with PM_) from IBKR fetch
  symbols_delayed <- character()
  symbols_regular <- character()
  quote_ccy <- character()          # currency IBKR quotes each symbol in

  for (s in portf$sym_ibkr) {
    ### Skip precious metals - they have web-based pricing
    if (!is.na(s) && grepl("^PM_", s)) {
      next
    }

    ticker <- getTicker(s)
    if (nrow(ticker) > 0 && !is.na(ticker$Currency)) quote_ccy[s] <- ticker$Currency
    if (nrow(ticker) > 0 && !is.na(ticker$Exchange) && ticker$Exchange %in% delayed_exchanges) {
      symbols_delayed <- c(symbols_delayed, s)
    } else {
      symbols_regular <- c(symbols_regular, s)
    }
  }

  # Call getValue() separately for each group
  last_price_list <- list()

  if (length(symbols_delayed) > 0) {
    last_price_list[[1]] <- tdata_py$getValue(list_sym=symbols_delayed, ib=NULL, reqType=4)
  }

  if (length(symbols_regular) > 0) {
    last_price_list[[2]] <- tdata_py$getValue(list_sym=symbols_regular, ib=NULL, reqType=2)
  }

  # Combine results
  last_price <- do.call(rbind, last_price_list)

  ### IBKR quotes the listing the Tickers row describes, which is not always
  ### the one Gonet holds. AMRZ is the US listing there (USD, used by the
  ### scanner) while the Gonet shares are the SIX line from the Holcim spin-off
  ### and are booked in CHF -- the USD quote was stored as CHF and overstated
  ### the position by ~25%. Convert each quote to the position's currency.
  last_price <- gonet_quote_to_position_ccy(
    last_price, quote_ccy,
    stats::setNames(portf$currency, portf$sym_ibkr))

  ### Fetch prices for precious metals from web sources
  if (any(precious_metals_mask)) {
    for (i in which(precious_metals_mask)) {
      url <- portf$exchange[i]
      sym_for_price <- portf$sym_ibkr[i]  # Use sym_ibkr from GonetTrades.csv (e.g., "PM_15606539")

      logger::log_info("Fetching precious metal price from {url}", namespace = "Tdata")

      price_result <- tryCatch({
        ### Fetch the webpage content
        response <- httr::GET(url, httr::timeout(10))

        if (httr::status_code(response) != 200) {
          logger::log_warn("HTTP error {httr::status_code(response)} fetching precious metal price", namespace = "Tdata")
          return(NA_real_)
        }

        content_text <- httr::content(response, as = "text", encoding = "UTF-8")

        ### Extract price - look for "Mittelkurs" followed by the price value
        ### Pattern: Find "Mittelkurs" and extract following number (e.g., "662.80")
        price_pattern <- "Mittelkurs[^0-9]*(\\d+\\.\\d+)"
        price_match <- regmatches(content_text, regexec(price_pattern, content_text))

        if (length(price_match[[1]]) >= 2) {
          price_value <- as.numeric(price_match[[1]][2])

          if (!is.na(price_value) && price_value > 0) {
            logger::log_info("Retrieved price {price_value} CHF from ZKB website", namespace = "Tdata")
            price_value
          } else {
            logger::log_warn("Invalid price value extracted: {price_value}", namespace = "Tdata")
            NA_real_
          }
        } else {
          logger::log_warn("Could not find price pattern in webpage", namespace = "Tdata")
          NA_real_
        }
      }, error = function(e) {
        logger::log_warn("Error fetching precious metal price: {e$message}", namespace = "Tdata")
        NA_real_
      })

      ### Add to last_price data frame (with NaN if fetch failed, so user can enter manually)
      pm_price_row <- data.frame(
        sym = sym_for_price,
        datetime = format(Sys.time(), "%Y%m%d %H:%M:%S"),
        price = if (is.na(price_result)) NaN else price_result,
        stringsAsFactors = FALSE
      )
      last_price <- rbind(last_price, pm_price_row)
    }
  }

  #### price_user is the subset of last_price with no usable price, i.e. the
  #### fetch returned nothing. IBKR answers an unsubscribed instrument with a
  #### plain 0 rather than NaN (Error 354), so testing is.nan() alone let a 0
  #### through: NUCL, DTLA and CNYA were priced at zero and their whole cost
  #### reported as a loss. Any non-finite or non-positive price counts.
  no_price  <- gonet_price_missing(last_price$price)
  price_user <- last_price[no_price, , drop = FALSE]
  new_price_entries <- data.frame(sym = character(), datetime = character(), price = numeric(), stringsAsFactors = FALSE)

  if (nrow(price_user) > 0) {
    ### Fall back to Yahoo's latest close, then to the last price actually
    ### observed for the symbol.
    known <- gonet_last_known_price(price_user$sym)
    yahoo <- gonet_yahoo_price(
      price_user$sym,
      portf$sym_yahoo[match(price_user$sym, portf$sym_ibkr)],
      reference = known$price[match(price_user$sym, known$sym)])

    default_values <- numeric(nrow(price_user))
    for (i in seq_len(nrow(price_user))) {
      yrow <- yahoo[yahoo$sym == price_user$sym[i], , drop = FALSE]
      row  <- known[known$sym == price_user$sym[i], , drop = FALSE]
      if (nrow(yrow) > 0) {
        default_values[i] <- yrow$price[1]
        logger::log_info("Gonet: no IBKR price for {price_user$sym[i]} - using {yrow$price[1]} from {yrow$source[1]} close of {yrow$asof[1]}",
                         namespace = "Tdata")
      } else if (nrow(row) > 0) {
        default_values[i] <- row$price[1]
        logger::log_warn("Gonet: no price for {price_user$sym[i]} - carrying forward {row$price[1]} from {row$source[1]} of {row$asof[1]}, unchanged for {row$unchanged_days[1]} day(s)",
                         namespace = "Tdata")
      } else {
        default_values[i] <- NA
        logger::log_warn("Gonet: no price for {price_user$sym[i]} and none stored - its market value will be NA",
                         namespace = "Tdata")
      }
    }

    ### Ask the operator when there is one; otherwise take the carried-forward
    ### price rather than blocking on stdin (see gonet_prices_or_ask).
    entered_prices <- gonet_prices_or_ask(price_user$sym, default_values)

    ### Update price_user with entered prices
    price_user$price <- entered_prices

    ### Save to database if user entered a valid price (not NA)
    ### Don't save if user just pressed Enter and kept the stored price (no change)
    ### A carried-forward price equals its default, so it is not recorded as a
    ### fresh observation -- only a hand-typed price is.
    changed_mask <- !is.na(entered_prices) &
                    (is.na(default_values) | abs(entered_prices - default_values) > 0.0001)

    ### Only save newly entered or updated prices to database
    if (any(changed_mask)) {
      new_price_entries <- price_user[changed_mask, ]
    }
  }

  ### Merge prices with value retrieved from IBKR plus prices with values entered by user
  last_price <- rbind(last_price[!no_price, , drop = FALSE],
                      price_user)

  ### Use these last_price as price for portf
  portf = dplyr::left_join(portf, last_price, by = c("sym_ibkr"="sym"))

  ### Compute all necessary fields for storing in CSV/DB
  portf <- dplyr::mutate(portf, symbol=sym_ibkr, pos=position,
                    mktPrice=price, mktValue=round(pos*mktPrice,2),
                    unPnL=round(mktValue-basis,2),
                    realizedPnL=round(realized,2),
                    avgCost=round(basis/pos,2))

  portf <- dplyr::select(portf, TradeNr, date, heure, symbol, pos, mktPrice, mktValue,
                         avgCost, unPnL, realizedPnL, currency, type)

  ### Build CASH position rows and recombine with the stock rows. Each cash row
  ### is valued at the FX spot rate to base; its unPnL is the realized FX banked
  ### through closed/partial trades in that currency (gonet_realized_fx). Stored
  ### currency is base, so downstream conversion is identity. avgCost is the
  ### implied cost rate (mktValue - unPnL)/pos, kept for internal consistency but
  ### not shown for cash (the Trade tab blanks it — realized FX dwarfs the small
  ### residual balance, so a per-unit cost would be meaningless).
  ###
  ### realizedPnL stays NA for cash: the banked FX is already reported in unPnL
  ### under Convention A (see reference on cash-FX in totals), so repeating it
  ### here would double-count it in any unPnL + realizedPnL total.
  if (nrow(portf_cash) > 0) {
    base_ccy <- getParam("BaseCurrency")
    cash_rows <- lapply(seq_len(nrow(portf_cash)), function(i) {
      ccy     <- portf_cash$sym_ibkr[i]
      balance <- portf_cash$position[i]
      spot    <- convert_to_base_date(1, ccy, Sys.Date())
      mkt     <- round(balance * spot, 2)
      rfx     <- gonet_realized_fx(gonet_trades, ccy)
      data.frame(
        TradeNr  = portf_cash$TradeNr[i],
        date     = portf_cash$date[i],
        heure    = portf_cash$heure[i],
        symbol   = ccy,
        pos      = balance,
        mktPrice = round(spot, 6),
        mktValue = mkt,
        avgCost  = if (balance != 0) round((mkt - rfx) / balance, 6) else NA_real_,
        unPnL    = rfx,
        realizedPnL = NA_real_,
        currency = base_ccy,
        type     = "CASH",
        stringsAsFactors = FALSE)
    })
    portf <- rbind(portf, do.call(rbind, cash_rows))
  }

  ### store prices in .CSV / DB
  ## Make them available for other functions
  ### Open connection to user DB
  conn <- safe_db_connect()
  safe_db_append(conn,"Gonet", portf)
  ### Only store newly entered prices (not stored prices retrieved from DB)
  if (nrow(new_price_entries) > 0) {
    safe_db_append(conn,"Prices", new_price_entries)
  }

  DBI::dbDisconnect(conn)

  ### Write account data. Cash is now part of the Gonet snapshot as CASH
  ### positions, so getAccountGonet() derives the cash balances from the rows
  ### just written — no interactive cash prompt (use_defaults is now unused but
  ### kept for call-site compatibility, e.g. daily_portfolio_update.R).
  getAccountGonet()

  invisible(portf)
}
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
getAccountGonet <- function(gonet_cash = NULL) {

  account.var = c("account","date","heure","Currency","NetLiquidation","EquityWithLoanValue","FullAvailableFunds","FullInitMarginReq","FullMaintMarginReq","FullExcessLiquidity","OptionMarketValue","StockMarketValue","UnrealizedPnL","RealizedPnL","TotalCashBalance","CashFlow","CashBalanceCHF","CashBalanceUSD","CashBalanceEUR")
  portf <- readLastPortfolio("Gonet")

  #### There are "portf_lines" opened positions in the GOnet portfolio (stocks)
  #### Some may be empty (NA lines) -> in this case the whole is considered as NA and therefore not stored
  #### ### DO not take into account days where one of the exchanges (NYSE, Euronext, SMI) is closed

  ### Split CASH positions from stock positions. Cash rows carry currency = base
  ### with mktValue already in base; stocks are in native currency. Cash is
  ### excluded from StockMarketValue (it becomes the cash balance) but its
  ### realized-FX unPnL IS included in the account UnrealizedPnL so the equity /
  ### P&L curve reflects currency gains banked through trading.
  is_cash <- !is.na(portf$type) & portf$type == "CASH"
  stock   <- portf[!is_cash, , drop = FALSE]
  cash    <- portf[is_cash, , drop = FALSE]

  StockMarketValue <- round(sum(convert_to_base_date(stock$mktValue, stock$currency, Sys.Date()), na.rm = FALSE), 2)
  UnrealizedPnL    <- round(sum(convert_to_base_date(portf$unPnL,  portf$currency, Sys.Date()), na.rm = FALSE), 2)
  acc <- data.frame(StockMarketValue = StockMarketValue, UnrealizedPnL = UnrealizedPnL)
  if (any(is.na(acc))) {
    warning("Could not get a complete portfolio record - some prices are missing -> no account recorded")
    return(invisible())
  }

  ### Cash balances are the native pos of each CASH position; total is the sum
  ### of their base-currency mktValue.
  cash_bal <- function(ccy) { v <- cash$pos[cash$symbol == ccy]; if (length(v)) sum(v) else 0 }
  cash_chf <- cash_bal("CHF")
  cash_usd <- cash_bal("USD")
  cash_eur <- cash_bal("EUR")
  cash_balance <- round(sum(cash$mktValue, na.rm = TRUE), 2)

  ### convert to base currency all Gonet positions
  base_currency <- getParam("BaseCurrency")
  acc <- dplyr::mutate(acc, account="Gonet",
                    date = format(Sys.Date(),"%Y%m%d"),
                    heure = format(Sys.time(),"%H:%M:%S"),
                    Currency = base_currency,
                    TotalCashBalance = cash_balance,
                    CashBalanceCHF = cash_chf,
                    CashBalanceUSD = cash_usd,
                    CashBalanceEUR = cash_eur,
                    NetLiquidation = round(TotalCashBalance + StockMarketValue, 2),
                    EquityWithLoanValue = NetLiquidation,
                    FullAvailableFunds = TotalCashBalance,
                    CashFlow = 0,
                    FullInitMarginReq = NA_real_,
                    FullMaintMarginReq = NA_real_,
                    FullExcessLiquidity = NA_real_,
                    OptionMarketValue = 0,
                    RealizedPnL = 0,
    )

    ### Remove Cash positions
    # acc=select(acc,!any_of(c("Cash_EUR","Cash_CHF","Cash_USD")))

    #### account;date;heure;NetLiquidation;EquityWithLoanValue;FullAvailableFunds;FullInitMarginReq;
    ####  FullMaintMarginReq;FullExcessLiquidity;OptionMarketValue;StockMarketValue;
    ####  UnrealizedPnL;RealizedPnL;TotalCashBalance;CashFlow
    acc = dplyr::select(acc, dplyr::all_of(account.var))
    conn <- safe_db_connect()
    safe_db_append(conn,"Account",acc)
    DBI::dbDisconnect(conn)
}

#' Get Account Choices from Config
#'
#' Returns account list from config.yml, optionally filtered by type.
#' This is the single source of truth for account dropdowns across all UIs.
#'
#' @param type One of "all" (all accounts including Live/Gonet),
#'   "ibkr" (IBKR accounts only: U.../DU...), or "trade" (tradeable IBKR accounts).
#' @return Character vector of account names
#' @export
#' @examples
#' \dontrun{
#' getAccountChoices("all")   # U1804173, U25343478, DU5221795, Gonet, Live
#' getAccountChoices("ibkr")  # U1804173, U25343478, DU5221795
#' getAccountChoices("trade") # U1804173, U25343478, DU5221795
#' }
getAccountChoices <- function(type = c("all", "ibkr", "trade")) {
  type <- match.arg(type)
  accounts <- config::get("account")
  switch(type,
    "all"   = accounts,
    "ibkr"  = accounts[grepl("^[UD]", accounts)],
    "trade" = accounts[!accounts %in% c("Live", "Gonet")]
  )
}

#'   getAccountLive
#'
#'Derives the virtual "Live" account (U1804173 + U25343478 + Gonet) for today and
#'stores it in the DB \code{Account} table. Live has no table of its own.
#'
#'Three rules make the result usable by \code{readAccount("Live")}:
#'
#'\itemize{
#'  \item \strong{The day's row is the last REAL snapshot.} A cash-flow row carries
#'    \code{NetLiquidation = 0} and is booked in its own native currency, so
#'    picking one as the day's value understated Live NLV by a whole sub-account
#'    and mixed currencies. Only rows with a non-zero NetLiquidation are eligible.
#'  \item \strong{Currency is always written.} A NULL \code{Currency} makes the
#'    \code{AccountWithConversionRate} view join produce a NULL rate, which turns
#'    every metric into NA - the whole Live Account tab. Amounts are converted to
#'    the base currency before summing, so the stored row is self-consistent with
#'    the \code{Currency} it declares.
#'  \item \strong{Cash flows are summed per day} across every row of the day, each
#'    converted from its native currency, rather than being whatever an arbitrary
#'    joined row happened to carry.
#'}
#'
#'Idempotent: it runs several times a day and replaces the dates it writes instead
#'of appending, which used to leave a date duplicated once per run.
#'
#'@returns No value
#'@export
#'@examples
#'\dontrun{
#'getAccountLive()
#'}
getAccountLive <- function() {

  account.var = c("account","date","heure","NetLiquidation","EquityWithLoanValue",
                  "FullAvailableFunds","FullInitMarginReq","FullMaintMarginReq",
                  "FullExcessLiquidity","OptionMarketValue","StockMarketValue",
                  "UnrealizedPnL","RealizedPnL","TotalCashBalance","CashFlow")
  ## Gonet has fewer columns (no margin fields)
  account.var.gonet = c(account.var[1:6], account.var[10:15])
  ## Numeric fields that get summed (exclude account, date, heure)
  sum.var = account.var[-(1:3)]
  sum.var.gonet = account.var.gonet[-(1:3)]

  s_date = format(Sys.Date(), "%Y%m%d")
  base_ccy = getParam("BaseCurrency")
  if (is.null(base_ccy) || is.na(base_ccy)) base_ccy = "CHF"

  #### Post-processing function: computes Live = U1804173 + U25343478 + Gonet
  #### Assumes all three accounts are already stored in Account table

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  account_d <- DBI::dbReadTable(conn, "Account")
  account_d <- dplyr::filter(account_d, date >= s_date)

  ## The day's snapshot for an account: the last heure among rows that carry a
  ## real NetLiquidation. Cash-flow rows have NetLiquidation = 0 and their own
  ## native currency, so including them both understates the day's value and
  ## mixes currencies into a row that can only declare one.
  daily_snapshot = function(acct, vars) {
    rows = dplyr::filter(account_d, account == acct,
                         !is.na(NetLiquidation), NetLiquidation != 0)
    if (!nrow(rows)) return(rows[, c(vars, "Currency"), drop = FALSE])
    rows = dplyr::ungroup(dplyr::slice_max(dplyr::group_by(rows, date), heure,
                                           n = 1, with_ties = FALSE))
    rows = dplyr::select(rows, dplyr::all_of(c(vars, "Currency")))
    ## Convert to base currency before any summing: sub-account snapshots have
    ## been USD historically and CHF since, and Live declares a single Currency.
    for (v in vars[-(1:3)])
      rows[[v]] = convert_to_base_date(rows[[v]], rows$Currency, rows$date)
    rows
  }

  ## The day's cash flow for an account: every flow row of that date, each
  ## converted from the currency it was booked in. Taking it from one arbitrary
  ## joined row meant a flow reached Live only if that row happened to be picked.
  daily_cashflow = function(acct) {
    rows = dplyr::filter(account_d, account == acct, !is.na(CashFlow), CashFlow != 0)
    if (!nrow(rows)) return(data.frame(date = numeric(0), flow = numeric(0)))
    rows$.flow = convert_to_base_date(rows$CashFlow, rows$Currency, rows$date)
    dplyr::summarize(dplyr::group_by(rows, date),
                     flow = sum(.flow, na.rm = TRUE), .groups = "drop")
  }

  acc_ib1 = daily_snapshot("U1804173", account.var)
  acc_ib2 = daily_snapshot("U25343478", account.var)
  acc_gon = daily_snapshot("Gonet", account.var.gonet)

  ## Join IBKR sub-accounts (same trading calendar -> inner_join). One row per
  ## date on each side now, so no arbitrary pick is involved.
  ibkr = dplyr::inner_join(acc_ib1, acc_ib2, by = "date", suffix = c(".ib1", ".ib2"))

  if (!nrow(ibkr)) {
    warning("Not enough data: need both U1804173 and U25343478 for Live account")
    return(invisible())
  }

  ## Sum IBKR numeric fields
  ibkr_sum = round(ibkr[paste0(sum.var, ".ib1")] + ibkr[paste0(sum.var, ".ib2")], 2)
  names(ibkr_sum) = sum.var

  ## Left-join with Gonet (different holiday calendar — may be missing some days)
  ibkr_base = data.frame(date = ibkr$date, heure = ibkr$heure.ib1, ibkr_sum)
  gonet_num = dplyr::select(acc_gon, date, dplyr::all_of(sum.var.gonet))

  merged = dplyr::left_join(ibkr_base, gonet_num, by = "date", suffix = c("", ".gon"))

  ## Add Gonet values where available (fill missing with 0)
  for (v in sum.var.gonet) {
    gon_col = if (paste0(v, ".gon") %in% names(merged)) paste0(v, ".gon") else v
    if (gon_col != v && gon_col %in% names(merged)) {
      gon_vals = merged[[gon_col]]
      gon_vals[is.na(gon_vals)] = 0
      merged[[v]] = merged[[v]] + gon_vals
      merged[[gon_col]] = NULL
    }
  }

  ## CashFlow comes from the per-day aggregation, not from the snapshot rows
  ## (which are all zero by construction now).
  flows = dplyr::bind_rows(daily_cashflow("U1804173"),
                           daily_cashflow("U25343478"),
                           daily_cashflow("Gonet"))
  if (nrow(flows)) {
    flows = dplyr::summarize(dplyr::group_by(flows, date),
                             flow = sum(flow, na.rm = TRUE), .groups = "drop")
    merged = dplyr::left_join(merged, flows, by = "date")
    merged$CashFlow = round(ifelse(is.na(merged$flow), 0, merged$flow), 2)
    merged$flow = NULL
  } else {
    merged$CashFlow = 0
  }

  ## Build Live account record. Currency is mandatory: the
  ## AccountWithConversionRate view joins on it, and a NULL yields a NULL rate
  ## that turns every metric into NA.
  data = cbind(account = "Live", merged, Currency = base_ccy)
  data = dplyr::select(data, dplyr::all_of(c(account.var, "Currency")))

  ### Store in DB, replacing the dates written rather than appending to them
  DBI::dbExecute(conn,
                 sprintf("DELETE FROM Account WHERE account = 'Live' AND date IN (%s)",
                         paste(unique(data$date), collapse = ",")))
  safe_db_append(conn, "Account", data)
}

#'@keywords internal
compute_margin_data <- function(portf_data, exit_code) {

  ### This assumes that negative position is always first as grouped by position
  margin_ibkr_data = dplyr::summarize(portf_data,
                                      contracts = dplyr::case_match(dplyr::first(Strategy),
                                                                    "WHEEL" ~ dplyr::first(conId),
                                                                    "OFI" ~  dplyr::first(conId),
                                                                    .default = NA),
                                      margin = 0)

  ### Retrieve margin data from IBKR -
  ### for WHEEL/OFI strategies :
  ###     margin for each contract listed (first contract in the trade)
  margin_ibkr_contracts = margin_ibkr_data$contracts

  if (!all(is.na(margin_ibkr_contracts))) {
    non_na_margin_contracts_ind = !is.na(margin_ibkr_contracts)

    ### Send to IBKR only contracts that do have margin
    ibkr_contracts = as.character(margin_ibkr_contracts[non_na_margin_contracts_ind])
    margin_data = tdata_py$retrieveAccountMarginData(ibkr_contracts)

    ### If data received then process and set exit code to 3
    if (length(margin_data) != 0) {
      margin_ibkr_data$margin[non_na_margin_contracts_ind] =  margin_data

      portf_data = dplyr::left_join(portf_data, margin_ibkr_data)
      portf_data = dplyr::mutate(portf_data,
                                 margin = dplyr::if_else(dplyr::first(marginable) == "Yes",
                                                         dplyr::case_match(dplyr::first(Strategy),
                                                                           "CS" ~ abs(sum(multiplier*pos*strike)),
                                                                           c("WHEEL", "OFI") ~ abs(dplyr::first(pos))*dplyr::first(margin),
                                                                           .default = 0
                                                         ),
                                                         0))
      exit_code = 3
    }
  }

  ### exit_code may be left unchanged if no margin data retrieved
  return(list(exit_code = exit_code, portf_data = portf_data))
}


#' getCurrencyExposure
#'
#' Computes currency exposure breakdown for a given account, including:
#' - Cash positions by currency
#' - Unrealized PnL by currency
#' - Total market value by currency
#' - All values converted to base currency
#'
#' This function tracks currency risk by showing exposure in CHF, USD, and EUR
#' across both cash holdings and investment positions. Negative exposure indicates
#' short positions (e.g., short options, sold stocks).
#'
#' @param account_name Account name (e.g., "U1804173", "Live", "Gonet")
#' @param date Optional date for historical analysis (defaults to most recent)
#'
#' @return Data frame with columns:
#' \describe{
#'   \item{Currency}{Currency code (CHF, USD, EUR)}
#'   \item{CashPosition}{Cash balance in original currency}
#'   \item{CashPositionBase}{Cash balance in base currency}
#'   \item{MarketValue}{Total market value of positions in original currency}
#'   \item{MarketValueBase}{Total market value in base currency}
#'   \item{UnrealizedPnL}{Unrealized P&L in original currency}
#'   \item{UnrealizedPnLBase}{Unrealized P&L in base currency}
#'   \item{TotalExposureBase}{Total exposure (cash + market value) in base currency}
#'   \item{PercentOfPortfolio}{Percentage of total portfolio}
#' }
#'
#' @note Future enhancement: Forex futures (e.g., MSF) will add synthetic currency exposure
#'
#' @export
#' @examples
#' \dontrun{
#' getCurrencyExposure("U1804173")
#' getCurrencyExposure("Live", date = Sys.Date() - 7)
#' }
getCurrencyExposure <- function(account_name, date = NULL) {

  base_currency <- getParam("BaseCurrency")

  ### Open database and prepare disconnection
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ### 1. Get cash positions from account table
  account_tbl <- get_account_table_name()
  if (is.null(date)) {
    # Get most recent account data
    account_query <- paste0("SELECT date, heure, CashBalanceCHF, CashBalanceUSD, CashBalanceEUR,
                      TotalCashBalance, NetLiquidation
                      FROM ", account_tbl, "
                      WHERE account = ?
                      ORDER BY date DESC, heure DESC
                      LIMIT 1")
    account_data <- DBI::dbGetQuery(conn, account_query, params = list(account_name))
  } else {
    # Get account data for specific date
    date_int <- as.integer(format(as.Date(date), "%Y%m%d"))
    account_query <- paste0("SELECT date, heure, CashBalanceCHF, CashBalanceUSD, CashBalanceEUR,
                      TotalCashBalance, NetLiquidation
                      FROM ", account_tbl, "
                      WHERE account = ? AND date = ?
                      ORDER BY heure DESC
                      LIMIT 1")
    account_data <- DBI::dbGetQuery(conn, account_query, params = list(account_name, date_int))
  }

  if (nrow(account_data) == 0) {
    logger::log_warn("No account data found for {account_name}", namespace = "Tdata")
    return(data.frame())
  }

  ### 2. Get portfolio positions grouped by currency
  ### CRITICAL: For CASH positions, group by symbol (trading currency) instead of currency field
  ### to correctly attribute USD/EUR cash to their respective currency exposures
  if (is.null(date)) {
    portf_query <- glue::glue("SELECT
                      CASE
                        WHEN type = 'CASH' THEN symbol
                        ELSE currency
                      END as currency,
                      SUM(mktValue) as market_value,
                      SUM(unPnL) as unrealized_pnl
                    FROM (
                      SELECT type, symbol, currency, mktValue, unPnL
                      FROM {`account_name`}
                      WHERE (date, heure) = (
                        SELECT date, heure FROM {`account_name`}
                        ORDER BY date DESC, heure DESC LIMIT 1
                      )
                    )
                    GROUP BY 1")
  } else {
    date_int <- as.integer(format(as.Date(date), "%Y%m%d"))
    portf_query <- glue::glue("SELECT
                      CASE
                        WHEN type = 'CASH' THEN symbol
                        ELSE currency
                      END as currency,
                      SUM(mktValue) as market_value,
                      SUM(unPnL) as unrealized_pnl
                    FROM (
                      SELECT type, symbol, currency, mktValue, unPnL
                      FROM {`account_name`}
                      WHERE (date, heure) = (
                        SELECT date, heure FROM {`account_name`}
                        WHERE date = {date_int}
                        ORDER BY heure DESC LIMIT 1
                      )
                    )
                    GROUP BY 1")
  }

  portf_data <- tryCatch({
    result <- DBI::dbGetQuery(conn, portf_query)
    ### If date-specific query returned 0 rows, fall back to latest available data
    if (nrow(result) == 0 && !is.null(date)) {
      fallback_query <- glue::glue("SELECT
                        CASE
                          WHEN type = 'CASH' THEN symbol
                          ELSE currency
                        END as currency,
                        SUM(mktValue) as market_value,
                        SUM(unPnL) as unrealized_pnl
                      FROM (
                        SELECT type, symbol, currency, mktValue, unPnL
                        FROM {`account_name`}
                        WHERE (date, heure) = (
                          SELECT date, heure FROM {`account_name`}
                          ORDER BY date DESC, heure DESC LIMIT 1
                        )
                      )
                      GROUP BY 1")
      result <- DBI::dbGetQuery(conn, fallback_query)
    }
    result
  }, error = function(e) {
    logger::log_debug("No portfolio table for {account_name}: {e$message}", namespace = "Tdata")
    data.frame(currency = character(), market_value = numeric(), unrealized_pnl = numeric())
  })

  ### Ensure currency column is character even when query returns 0 rows (DBI defaults to logical)
  if (nrow(portf_data) > 0 && !is.character(portf_data$currency)) {
    portf_data$currency <- as.character(portf_data$currency)
  } else if (nrow(portf_data) == 0) {
    portf_data <- data.frame(currency = character(), market_value = numeric(), unrealized_pnl = numeric())
  }

  ### 3. Build cash positions data frame (only non-zero positions)
  cash_data <- data.frame(
    currency = c("CHF", "USD", "EUR"),
    cash_position = c(
      account_data$CashBalanceCHF,
      account_data$CashBalanceUSD,
      account_data$CashBalanceEUR
    ),
    stringsAsFactors = FALSE
  )

  ### Remove zero cash positions for cleaner display
  cash_data <- cash_data[cash_data$cash_position != 0, ]

  ### 4. Convert all values to base currency
  conversion_date <- if (is.null(date)) Sys.Date() else as.Date(date)

  if (nrow(cash_data) > 0) {
    cash_data$cash_position_base <- purrr::pmap_dbl(
      list(cash_data$cash_position, cash_data$currency, conversion_date),
      convert_to_base_date
    )
  } else {
    cash_data$cash_position_base <- numeric(0)
  }

  if (nrow(portf_data) > 0) {
    portf_data$market_value_base <- purrr::pmap_dbl(
      list(portf_data$market_value, portf_data$currency, conversion_date),
      convert_to_base_date
    )
    portf_data$unrealized_pnl_base <- purrr::pmap_dbl(
      list(portf_data$unrealized_pnl, portf_data$currency, conversion_date),
      convert_to_base_date
    )
  }

  ### 5. Merge cash and portfolio data
  exposure <- dplyr::full_join(
    cash_data,
    portf_data,
    by = "currency"
  )

  ### If no exposure data at all, return empty
  if (nrow(exposure) == 0) {
    return(data.frame())
  }

  ### Replace NA with 0
  exposure$cash_position <- ifelse(is.na(exposure$cash_position), 0, exposure$cash_position)
  exposure$cash_position_base <- ifelse(is.na(exposure$cash_position_base), 0, exposure$cash_position_base)
  exposure$market_value <- ifelse(is.na(exposure$market_value), 0, exposure$market_value)
  exposure$market_value_base <- ifelse(is.na(exposure$market_value_base), 0, exposure$market_value_base)
  exposure$unrealized_pnl <- ifelse(is.na(exposure$unrealized_pnl), 0, exposure$unrealized_pnl)
  exposure$unrealized_pnl_base <- ifelse(is.na(exposure$unrealized_pnl_base), 0, exposure$unrealized_pnl_base)

  ### 6. Calculate total exposure and percentages
  exposure <- dplyr::mutate(exposure,
    total_exposure_base = cash_position_base + market_value_base
  )

  total_net_liquidation <- account_data$NetLiquidation
  exposure <- dplyr::mutate(exposure,
    pct_of_portfolio = round(total_exposure_base / total_net_liquidation * 100, 2)
  )

  ### 7. Sort by absolute total exposure (descending) to show largest exposures first
  exposure <- dplyr::arrange(exposure, dplyr::desc(abs(total_exposure_base)))

  ### 8. Select and rename columns for output
  exposure <- dplyr::select(exposure,
    Currency = currency,
    CashPosition = cash_position,
    CashPositionBase = cash_position_base,
    MarketValue = market_value,
    MarketValueBase = market_value_base,
    UnrealizedPnL = unrealized_pnl,
    UnrealizedPnLBase = unrealized_pnl_base,
    TotalExposureBase = total_exposure_base,
    PercentOfPortfolio = pct_of_portfolio
  )

  return(exposure)
}
