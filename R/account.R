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
#'  CashFlow}
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
      CashFlow * ", conversion_column, " AS CashFlow
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

    # Create USD data frame
    ibkr_usd <- data.frame(
      date = today_date,
      currency = currencies_list,
      usd_value = round(currencies_values, 4)
    )
    ibkr_usd <- ibkr_usd[!is.na(ibkr_usd$usd_value), ]

    # Get current CHF/USD rate for CHF conversion
    chf_usd_rate <- getStoredCHFValue("USD")$chf_value
    if (is.na(chf_usd_rate) || length(chf_usd_rate) == 0) {
      # Fallback: get from USD table
      usd_chf_rate <- getStoredUSDValue("CHF")$usd_value
      chf_usd_rate <- 1 / usd_chf_rate
    }

    # Create CHF data frame - convert USD values to CHF using DirectConversion logic
    chf_values <- numeric(length(currencies_values))

    for (i in seq_along(currencies_list)) {
      currency <- currencies_list[i]
      usd_value <- currencies_values[i]
      direct_conv <- currency_data$DirectConversion[currency_data$Name == currency]

      # Apply DirectConversion logic (same as Yahoo)
      if (direct_conv == "No") {
        # Need to invert: IBKR gives USD-base rate, invert to get foreign-currency-base
        actual_usd_rate <- 1 / usd_value
      } else {
        # Direct foreign-currency-base rate
        actual_usd_rate <- usd_value
      }

      chf_values[i] <- actual_usd_rate / chf_usd_rate
    }

    ibkr_chf <- data.frame(
      date = today_date,
      currency = currencies_list,
      chf_value = round(chf_values, 4)
    )
    ibkr_chf <- ibkr_chf[!is.na(ibkr_chf$chf_value), ]

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
  portf_cashflow <- dplyr::summarize(dplyr::group_by(gonet_trades, sym_yahoo),
                                  TradeNr = dplyr::first(TradeNr),
                                  cost = sum(init_cost),
                                  currency = dplyr::first(currency))

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

  for (s in portf$sym_ibkr) {
    ### Skip precious metals - they have web-based pricing
    if (!is.na(s) && grepl("^PM_", s)) {
      next
    }

    ticker <- getTicker(s)
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

  #### price_user is the subset of last_price where price = NaN, i.e. price could not be retrieved from IBKR
  price_user <- last_price[is.nan(last_price$price),]
  new_price_entries <- data.frame(sym = character(), datetime = character(), price = numeric(), stringsAsFactors = FALSE)

  if (nrow(price_user) > 0) {
    ### Retrieve stored prices from database to use as defaults
    stored_prices <- getStoredMetrics(price_user$sym)

    ### Create vector of default values (stored prices if available, NA otherwise)
    default_values <- numeric(nrow(price_user))
    for (i in seq_len(nrow(price_user))) {
      stored_row <- stored_prices[stored_prices$sym == price_user$sym[i], ]
      if (nrow(stored_row) > 0 && !is.na(stored_row$price[1])) {
        default_values[i] <- stored_row$price[1]
      } else {
        default_values[i] <- NA
      }
    }

    ### Prompt user for all prices, showing stored values as defaults
    ### User can press Enter to keep default, or enter new value
    entered_prices <- Tbasics::enter_numerical_data(price_user$sym, default_values)

    ### Update price_user with entered prices
    price_user$price <- entered_prices

    ### Save to database if user entered a valid price (not NA)
    ### Don't save if user just pressed Enter and kept the stored price (no change)
    changed_mask <- !is.na(entered_prices) &
                    (is.na(default_values) | abs(entered_prices - default_values) > 0.0001)

    ### Only save newly entered or updated prices to database
    if (any(changed_mask)) {
      new_price_entries <- price_user[changed_mask, ]
    }
  }

  ### Merge prices with value retrieved from IBKR plus prices with values entered by user
  last_price <- rbind(last_price[!is.nan(last_price$price),],
                      price_user)

  ### Use these last_price as price for portf
  portf = dplyr::left_join(portf, last_price, by = c("sym_ibkr"="sym"))

  ### Compute all necessary fields for storing in CSV/DB
  portf <- dplyr::mutate(portf, symbol=sym_ibkr, pos=position,
                    mktPrice=price, mktValue=round(pos*mktPrice,2),
                    unPnL=price*pos+cost,
                    avgCost=round(-cost/pos,2))

  portf <- dplyr::select(portf, TradeNr, date, heure, symbol, pos, mktPrice, mktValue,
                         avgCost, unPnL, currency, type)

  ### Build CASH position rows and recombine with the stock rows. Each cash row
  ### is valued at the FX spot rate to base; its unPnL is the realized FX banked
  ### through closed/partial trades in that currency (gonet_realized_fx). Stored
  ### currency is base, so downstream conversion is identity. avgCost is the
  ### implied cost rate (mktValue - unPnL)/pos, kept for internal consistency but
  ### not shown for cash (the Trade tab blanks it — realized FX dwarfs the small
  ### residual balance, so a per-unit cost would be meaningless).
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

  #### Post-processing function: computes Live = U1804173 + U25343478 + Gonet
  #### Assumes all three accounts are already stored in Account table

  conn <- safe_db_connect()
  account_d <- DBI::dbReadTable(conn, "Account")
  account_d <- dplyr::filter(account_d, date >= s_date)

  acc_ib1 = dplyr::select(dplyr::filter(account_d, account == "U1804173"), dplyr::all_of(account.var))
  acc_ib2 = dplyr::select(dplyr::filter(account_d, account == "U25343478"), dplyr::all_of(account.var))
  acc_gon = dplyr::select(dplyr::filter(account_d, account == "Gonet"), dplyr::all_of(account.var.gonet))

  ## Join IBKR sub-accounts (same trading calendar -> inner_join)
  ibkr = dplyr::inner_join(acc_ib1, acc_ib2, by = "date", multiple = "any", suffix = c(".ib1", ".ib2"))

  if (!nrow(ibkr)) {
    warning("Not enough data: need both U1804173 and U25343478 for Live account")
    DBI::dbDisconnect(conn)
    return(invisible())
  }

  ## Sum IBKR numeric fields
  ibkr_sum = round(ibkr[paste0(sum.var, ".ib1")] + ibkr[paste0(sum.var, ".ib2")], 2)
  names(ibkr_sum) = sum.var

  ## Left-join with Gonet (different holiday calendar — may be missing some days)
  ibkr_base = data.frame(date = ibkr$date, heure = ibkr$heure.ib1, ibkr_sum)
  gonet_num = dplyr::select(acc_gon, date, dplyr::all_of(sum.var.gonet))

  merged = dplyr::left_join(ibkr_base, gonet_num, by = "date", multiple = "any", suffix = c("", ".gon"))

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

  ## Build Live account record
  data = cbind(account = "Live", merged)
  data = dplyr::select(data, dplyr::all_of(account.var))

  ### Store in DB
  safe_db_append(conn, "Account", data)
  DBI::dbDisconnect(conn)
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
