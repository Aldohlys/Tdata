
#####  trades.R
##### All utilities related to trades

#' saveTrades
#'
#' This function is used by RReporting functions and allows to update DB with modified trades
#' It takes as parameters a trades data frame that must have the same structure as the Trades table
#'
#' To be sure that correct data types will be used, it converts field types to the target fields, i.e.
#' * Integer for \code{TradeNr} and \code{Pos}
#' * Real (double) for \code{Prix, Comm., Total, Risk, Reward, PnL}
#'@param trades data frame with the following fields:
#'\code{TradeNr, Account, TradeDate, Strategy, Instrument, Ssjacent, Pos, Prix,
#'Comm., Total, Exp.Date, Risk, Reward, PnL, Statut, Currency}
#'@return No value or Error code from dbWriteTable
#'@export
saveTrades = function(trades) {

  # file.copy(from=paste0(config::get("DirNewTrading"),"Trades.csv"),
  #           to=paste0(config::get("DirNewTrading"),"Trades-old.csv"),overwrite = T)
  #
  # utils::write.table(trades,file=paste0(config::get("DirNewTrading"),"Trades.csv"),append=F,
  #             col.names=TRUE,row.names=FALSE,sep=";",dec=".",quote=TRUE)

  conn <- safe_db_connect()
  safe_db_write(conn, "Trades", trades, #### This will overwrite table in DB
                    field.types=c("TradeNr"=	"INTEGER","TradeDate"	= "INTEGER",
                                  "Pos"	= "INTEGER",
                                  "Prix" =	"REAL",
                                  "Comm." =	"REAL",
                                  "Total"	= "REAL",
                                  "Risk"=	"REAL",
                                  "Reward"=	"REAL",
                                  "PnL"= "REAL" ))
  DBI::dbDisconnect(conn)
}

#' getAllTrades
#'
#' This function works only for IBKR accounts not for Gonet account
#' This function is used by other Tdata functions but also for RReporting directly.
#' No argument - takes its source from config::get()
#'
#' It verifies that \code{TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward, PnL} are all numeric,
#' and if not, displays an error message and converts them
#'@return All trades stored in Trades table from mydb DB. Format is the following:
#'\code{TradeNr, Account, TradeDate, Strategy, Instrument, Ssjacent, Pos, Prix,
#'Comm., Total, Exp.Date, Risk, Reward, PnL, Statut, Currency}
#'@export
getAllTrades = function() {
  ### Read all trades from DB
  conn <- safe_db_connect()
  alltrades = DBI::dbReadTable(conn, "Trades")
  DBI::dbDisconnect(conn)

  if (any(with(alltrades, !is.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL))))) {
    Tbasics::display_message("Trades input data had to be converted!")
    with(alltrades, as.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL)))
  }
  alltrades
}

### Not to be exported - for test purposes
getClosedRecentTradeQuery <- function(account, windowDate) {
  conn <- safe_db_connect()
  closedtrades = DBI::dbGetQuery(conn,
                                 "Select * from Trades WHERE Statut == 'Ferm\U00e9' AND Account = ? AND TradeDate >= ?",
                                 params=list(account, windowDate))
  DBI::dbDisconnect(conn)
  closedtrades
}


### Not to be exported - for test purposes
getActiveTradeQuery <- function(account) {
  conn <- safe_db_connect()
  activetrades = DBI::dbGetQuery(conn,
                                 "Select * from Trades WHERE Statut != 'Ferm\U00e9' AND Account = ?",
                                 params=list(account))
  DBI::dbDisconnect(conn)
  activetrades
}

#' getActiveTrades
#'
#' This function works only for IBKR accounts not for Gonet account.
#' It reads active trades from given selected account and returns all corresponding records from table Trades in DB.
#'
#' It retrieves all trades whose \code{Statut} is different from closed. It also verifies that \code{TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward, PnL} are all numeric,
#' and if not, displays an error message and converts them
#'@param account string, account name
#'@return All trades related to active position stored in Trades table from mydb DB. Format is the following:
#'\code{TradeNr, Account, TradeDate, Strategy, Instrument, Ssjacent, Pos, Prix,
#'Comm., Total, Exp.Date, Risk, Reward, PnL, Statut, Currency}
#'@examples
#'\dontrun{
#'getActiveTrades("DUxxx")
#'}
#'@export
getActiveTrades = function(account) {
  ### Convert account selection to right account name
  account <- switch(account, "U1804173"="Live", "DU5221795"="Simu")
  if (is.null(account)) Tbasics::display_error_message("No account exists !")

  activetrades = getActiveTradeQuery(account)

  if (any(with(activetrades, !is.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL))))) {
      Tbasics::display_message("Trades input data had to be converted!")
      with(activetrades, as.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL)))
  }
  activetrades
}

#' getClosedTrades
#'
#' This function works only for IBKR accounts not for Gonet account.
#' It retrieves closed trades from given selected account, after a given window date, and returns all corresponding records from table Trades in DB.
#'
#' It retrieves all trades whose \code{Statut} is equal to closed.
#' It also verifies that \code{TradeNr, TradeDate, Pos, Prix, Comm., Total, Risk, Reward, PnL} are all numeric,
#' and if not, displays an error message and converts them
#'@param account string, account name
#'@param windowDate date, only records whose original date is greater than window date will be returned. Default value is today minus 200 days.
#'@return All trades related to closed position stored in Trades table from mydb DB. Format is the following:
#'\code{TradeNr, Account, TradeDate, Strategy, Instrument, Ssjacent, Pos, Prix,
#'Comm., Total, Exp.Date, Risk, Reward, PnL, Statut, Currency}
#'@export
#'@examples
#'\dontrun{
#'getClosedTrades("DUxxx")
#'getClosedTrades("DUxxx", as.Date("2024-02-01"))
#'}
getClosedTrades = function(account, windowDate = Sys.Date()-200) {
  ### Convert account selection to right account name
  account <- switch(account, "U1804173"="Live", "DU5221795"="Simu")
  if (is.null(account)) Tbasics::display_error_message("No account exists !")
  if (!inherits(windowDate,"Date")) Tbasics::display_error_message("window date must be a date !")

  ### Transform windowDate from Date to integer
  windowDate = as.numeric(format(windowDate,"%Y%m%d"))
  closed_trades = getClosedRecentTradeQuery(account, windowDate)

  if (nrow(closed_trades) == 0) {
    logger::log_warn("No closed trades to return", namespace="Tdata")
    return(data.frame())
  }

  ### If necessary display a warning message and convert data
  if (any(with(closed_trades, !is.numeric(c(TradeNr, TradeDate, Pos, Prix, Comm., Total, Risk, Reward, PnL))))) {
      Tbasics::display_message("Trades input data had to be converted!")
      with(closed_trades, as.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL)))
  }

  window_date_info <- getTradeDates(unique(closed_trades$TradeNr))

  ### Check if any difference between both data frames
  if (nrow(window_date_info) < nrow(closed_trades)) {
    pasted_msg = paste(unique(closed_trades[!closed_trades$TradeNr %in% window_date_info$TradeNr, "TradeNr"]), collapse=", ")
    logger::log_warn("Some closed trades are not recorded correctly in trades table: {pasted_msg}", namespace = "Tdata")
    ### Keep only correct trades
    closed_trades = closed_trades[closed_trades$TradeNr %in% window_date_info$TradeNr, ]
    if (nrow(closed_trades) == 0) {
      logger::log_warn("No closed trades to return", namespace="Tdata")
      return(data.frame())
    }
  }

  ### By comparing all trade data with trade opening date (orig_date)
  ###   we make sure that all trade data belong to trades whose opening date is posterior to window date
  closed_trades <- dplyr::left_join(closed_trades, window_date_info, by = "TradeNr") |>
                   dplyr::filter(orig_date >= as.Date(as.character(windowDate),"%Y%m%d")) |>
                   dplyr::select(-c(strategy, exp_date, orig_date, last_date))
  closed_trades
}



### This function is meant as a wrapper to make test possible
#'@noRd
getToday = function() {
  Sys.Date()
}

### This function is meant as a wrapper to make test possible
#'@noRd
getInstrumentQuery <- function(conn, tradenr, instr) {
  return(DBI::dbGetQuery(conn,
                          "SELECT Prix As startPrice,`Exp.Date` as expdate, Ssjacent as symbol, min(TradeDate) AS initial_trade_date
                         FROM Trades WHERE TradeNr = ? AND Instrument = ?",
                          params=list(tradenr, instr)))

}

### This function is meant as a wrapper to make test possible
#'@noRd
getInterestRate <- function(initial_trade_date, expdate) {
  Tbasics::getInterestRate(initial_trade_date,
                           difftime(expdate, initial_trade_date, units="weeks"))
}

############################
#'   getInstrument
#'
#'This function returns initial trade date, price, implied volatility and DTE at initial trade time
#'information related to the opening trade for an instrument - trade may be open or closed.
#'
#'It queries the Trades table and gets the initial trade opening date, as well as its price (without commission though).
#'It then retrieves interest rate at the time of opening, computes DTE and then deduce its implied volatility.
#'It assumes dividend = 0 in IV computation.
#'
#'@param tradenr integer in order to filter trades with the right trade number (TradeNr)
#'@param instrument an instrument or a list of instruments as input data
#'@returns a data frame, and for each instrument the following fields: \code{initial_trade_date, startPrice, DTE, u_price} and \code{startIV}.
#'If an instrument does not exist or is closed, then NA will be returned for this instrument
#'@export
#'@examples
#'\dontrun{
#'getInstrument(tradenr=527,instrument=c("SPX 08NOV24 5870 P", "SPX 08NOV24 5900 P"))
#'}
getInstrument <- function(tradenr, instrument) {

  get_init_info <- function(tradenr, instr) {

    na_data = data.frame(initial_trade_date=NA, startPrice=NA, DTE=NA, u_price=NA, startIV=NA)
    data = getInstrumentQuery(conn, tradenr, instr)
    type= strsplit(instr, "\\s+")[[1]][4]

    if (is.na(data$initial_trade_date)) {
      Tbasics::display_message("getInstrument: instrument does not exist!")
      return(na_data)
    }
    else if (is.na(type)) {
      Tbasics::display_message("getInstrument: one instrument is not a Call or a Put!")
      return(na_data)
    }

    ### If type is not NA then it is a call or a put
    else {
      ## Rewrite option type into Call or Put
      type= switch(type, "C"="Call", "P"="Put")

      ### Retrieve strike
      strike = as.numeric(strsplit(instr, "\\s+")[[1]][3])

      data = dplyr::mutate(data, expdate = as.Date(expdate,"%d.%m.%Y"),
                           initial_trade_date = as.Date(as.character(initial_trade_date),"%Y%m%d"))
      data = dplyr::mutate(data, interest_rate = getInterestRate(initial_trade_date, expdate))

      ### Compute DTE from InitialStartDate till Exp.Date
      data = dplyr::mutate(data, DTE = Tbasics::getDTE(initial_trade_date, expdate))
      data = dplyr::mutate(data, u_price = getSymPrice(symbol, report_date=initial_trade_date))
      data = dplyr::mutate(data, startIV = Tbasics::getImpliedVolOpt(type, u_price, strike, interest_rate, DTE, div=0, startPrice))

      ### Select and return information of interest only
      data = dplyr::select(data, initial_trade_date, startPrice, DTE, u_price, startIV)
      return(data)
    }
  }


  ### Open connection with DB to query trades table in get_init_info
  conn <- safe_db_connect()

  ### Recycle account up to instrument length
  if (length(tradenr) == 1) tradenr = rep(tradenr, length(instrument))

  #### Error case handling
  if (length(tradenr) != length(instrument)) {
    Tbasics::display_message("getInstrument: Account and instrument must have the same length!")
    DBI::dbDisconnect(conn)
    return(NA)
  }

  ### Apply get_init_info to each member of instrument vector
  v_instr <- dplyr::bind_rows(purrr::map2(tradenr, instrument,  get_init_info))

  ### CLose DB connection
  DBI::dbDisconnect(conn)

  return(v_instr)
}

### This function is useful for test purposes
getTradeQuery <- function(conn) {
  return(DBI::dbGetQuery(conn,
                         "SELECT DISTINCT TradeNr from Trades WHERE Statut = 'Ouvert' OR Statut = 'Ajust\u00e9'"
                         ))
}

### This function is useful for test purposes
getTradeDataQuery <- function(conn, params) {
  return(DBI::dbGetQuery(conn,
                         "SELECT * from Trades WHERE TradeNr = ?",
                         params = list(params)
  ))
}


#' getTradeData
#'
#' This function retrieves all data related to a given trade
#'
#' It works by querying the Trades table in DB
#' If TradeNr does not exist then it will return a record with 0 row
#'
#'@param TradeNr an integer or vector of integers
#'@return The following fields are returned:
#'\itemize{
#' \item{TradeNr}
#' \item{Account: Simu/Live}
#' \item{TradeDate: YYYYMMDD}
#' \item{Strategy TBILL, OFI, WHEEL, A14,... NA also possible}
#' \item{Instrument}
#' \item{Ssjacent: a.k.a symbol}
#' \item{Pos: Integer}
#' \item{Prix: numeric}
#' \item{Comm.: idem}
#' \item{Total: ditto}
#' \item{Risk}
#' \item{Reward}
#' \item{PnL}
#' \item{Statut: Fermé, Ouvert or Ajusté}
#' \item{Currency: USD, EUR, CHF,...}
#' \item{Remarques: Text}
#'}
#'
#'@export
getTradeData = function(TradeNr) {
  ### Get the list of all trade numbers currently active
  conn <- safe_db_connect()
  ### getTradeQuery returns a list
  trade_data <- getTradeDataQuery(conn, TradeNr)
  DBI::dbDisconnect(conn)
  ### Returns matching status of TradeNr
  return(trade_data)
}

#' isTradeOpened
#'
#' This function retrieves the status of a vector of trade numers and returns a boolean vector
#'
#' It works by querying the Trades table in DB and comparing active trades in DB with argument.
#' If TradeNr does not exist yet it will return FALSE just like if it were closed.
#'
#'@param TradeNr an integer or vector of integers
#'@return a boolean or a vector of booleans
#'@export
isTradeOpened = function(TradeNr) {
  ### Get the list of all trade numbers currently active
  conn <- safe_db_connect()
  ### getTradeQuery returns a list
  active_trade_nr <- getTradeQuery(conn)[[1]]
  DBI::dbDisconnect(conn)
  ### Returns matching status of TradeNr
  return(TradeNr %in% active_trade_nr)
}



#' getTradeNr
#'
#' This function retrieves one or a vector of trade numbers, all pertaining to open/adjusted trades
#' including instruments listed in \code{v_instrument} argument.
#'
#' It works by matching all instruments with Trades table in DB,
#' If no instruments are retrieved then NA is returned and an error message is displayed.
#'
#'@param v_instrument String on IBKR format, or vector of strings. Such as "SPY 15DEC23 400 P"
#'@param account_type one of the string values ("Live", "Simu", or NA), specifies with which account type trades are to be retrieved.
#'Default value is NA, which means no filtering done on account_type
#'@param unique Boolean - if TRUE then will return only unique trade number values otherwise will return one trade number per instrument
#'@return Integer or a vector of integers
#'@export
getTradeNr = function(v_instrument,account_type=NA,unique=T) {
  if(length(v_instrument)==0) {
    Tbasics::display_message("No instrument to be searched!")
    return(NA)
  }

  if (!is.na(account_type) && !(account_type %in% c("Live","Simu")))
    stop("Trades.csv understands only Live/Simu types of account or must be equal to NA")
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  ### Only opened trades can be retrieved
  trades = getAllTrades()
  trades = dplyr::filter(trades, Statut=="Ouvert" | Statut=="Ajust\u00e9")
  if (!is.na(account_type)) trades = dplyr::filter(trades, Account == account_type)
  trades = dplyr::select(trades, Instrument,TradeNr)

  ### There may be duplicate lines in case the same instrument has been traded multiple times in the same trade
  ### In this case duplicated trades are removed as the same instrument will otherwise appear more than one time
  ### for one trade, and therefore the trade number will be listed more than one time due to the join
  trades=trades[!duplicated(trades),]

  ### Retrieve trade_nr in trades.csv corresponding to instruments of dt
  ### Suppress join message by suppresswMessages
  trade_nr=suppressMessages(dplyr::pull(
    dplyr::group_by(dplyr::left_join(as.data.frame(list(Instrument=v_instrument)),trades),
                    Instrument),TradeNr))
  ### If left join returns NA -> trade is not present - not yet recorded in Trades table
  if (all(is.na(trade_nr))) {
    Tbasics::display_message(paste0("For instruments ", do.call(paste,as.list(c(v_instrument,sep=" and "))),
                                          " no opened/adjusted trades in Trades table!"))
    return(NA)
  }

  ### Retrieve the common Trade Nr - there may be several trade_nr if requested by unique argument
  if (unique) trade_nr=unique(trade_nr)

  ### Converted to integer (vector of integer if necessary)
  return(as.integer(trade_nr))
}

#' getTradeDates
#'
#' This function retrieves one or a vector of dates, pertaining to open, adjusted or closed trades, plus trade number and strategy.
#'
#' It works by matching all trade number arguments against corresponding trade dates found in Trades.csv
#' Then the oldest date per trade is returned, plus still active expiration/active opening date (for existing position only)
#' plus strategy.
#' If the same trade number is present multiple times as input, then all output data will be repeated as many times
#' This is useful for multiple getTradeDate calls.
#'
#' * If any trade_nr element is non-numeric, it will log an error message and return NULL.
#' * If any trade_nr element does not belong to Trade table, it will be removed from trade_nr vector and an error message will be logged.
#' * If any trade has an instrument with position different from 0 (open position) and expiration date is greater than today,
#'then it will be removed from result and an error message will be logged.
#'
#' N.B: If no trade_nr element is valid, it will log an error message and return NULL.
#'@param trade_nr an integer or a vector of integers
#'@return a data frame including:
#'* `TradeNr` trade number, an integer
#'* `strategy` a string, the strategy that was used: can be "BOT", "BPT", "OFI", "CS". or even "Erreur"
#'* `exp_date` the first still active expiration date  - only for opened/adjusted trades
#'* `orig_date`  the original opening trade date (even if corresponding instrument part of the trade has been closed since)
#'* `last_date` the last trading date on the trade - will be the close date if the trade is closed
#'@export
getTradeDates = function(trade_nr) {

  if (!is.numeric(trade_nr)) {
    pasted_msg = paste(trade_nr, collapse=", ")
    logger::log_error("trade_nr must be a numeric: {pasted_msg}", trade_nr, namespace="Tdata")
    return(NULL)
  }
  logger::log_debug("getTradeDates arguments: {paste(trade_nr, collapse=', ')}", namespace = "Tdata")
  trades=getAllTrades()
  missing_trades = trade_nr[!trade_nr %in% unique(trades$TradeNr)]

  if (length(missing_trades) != 0) {
    pasted_msg <- paste(missing_trades, collapse=", ")
    logger::log_error("There are inexisting trades in argument: {pasted_msg}", namespace="Tdata")
    trade_nr = trade_nr[trade_nr %in% unique(trades$TradeNr)]
  }

  if (length(trade_nr) == 0) {
    logger::log_error("No trade to analyze", namespace="Tdata")
    return(NULL)
  }

  ### Keep only trade data related to trade_nr - we know they all exist
  ### trades is grouped by TradeNr & Instrument for summarize to use TradeNr and Instrument as grouping keys
  ### Retrieve initial opening date for each trade
  ### orig_date is the oldest date recorded for the trade -
  ### last_date is the most recent date recorded for the trade

  all_dates_data = trades[trades$TradeNr %in% trade_nr,] |>
            dplyr::mutate(TradeDate = as.Date(as.character(TradeDate),format="%Y%m%d"),
                          Exp.Date = dplyr::if_else(Exp.Date == "NA", as.Date(NA), as.Date(Exp.Date,format="%d.%m.%Y"))) |>
            dplyr::group_by(TradeNr, Instrument) |>
            #### Result will still be grouped by TradeNr - 1 line per Instrument
            dplyr::summarize(orig_date=min(TradeDate),
                     last_date=max(TradeDate),
                     Exp.Date=first(Exp.Date),
                     Pos=sum(Pos),
                     strategy=dplyr::first(Strategy))

  ### Verify that for all non 0 positions, Exp.Date is greater than today if it exists
  wrong_trades = all_dates_data[all_dates_data$Pos!=0,] |>
                 dplyr::filter(!is.na(Exp.Date), Exp.Date < getToday())

  if (nrow(wrong_trades) > 0) {
    pasted_msg <- paste(unique(wrong_trades$TradeNr), collapse=", ")
    logger::log_warn("There are active trades in DB with expiration date prior today in argument: {pasted_msg}", namespace="Tdata")
  }
  ## Remove all wrong trades
  all_dates_data <- all_dates_data |>
    dplyr::anti_join(wrong_trades, by = "TradeNr")
  if (nrow(all_dates_data) == 0) {
    logger::log_error("No valid trade data", namespace="Tdata")
    return(NULL)
  }

  ### Prepare result ###
  ### Grouping key is TradeNr - now only 1 line per trade
  result <- dplyr::summarize(all_dates_data,
                      orig_date = min(orig_date),
                      last_date = max(last_date),
                      strategy = first(strategy))

  ### TradeNr is used as grouping key for summarize
  ## only Instruments whose position is not 0 are taken into account and Exp.Date is not NA
  ### Take the first date to come in those trades
  trade_exp_date = all_dates_data[all_dates_data$Pos!=0 & !is.na(all_dates_data$Exp.Date),
                                   !names(all_dates_data) %in% c("Instrument", "Pos")]

  if (nrow(trade_exp_date) > 0) {
    trade_exp_date = dplyr::summarize(trade_exp_date, exp_date = min(Exp.Date, na.rm=TRUE))
    result <- suppressMessages(dplyr::left_join(result, trade_exp_date)) |>
      dplyr::select(TradeNr, strategy, exp_date,  orig_date, last_date)
  }

  else result = dplyr::mutate(result, exp_date=as.Date(NA)) |>
         dplyr::select(TradeNr, strategy, exp_date,  orig_date, last_date)

  ### This tibble is grouped by TradeNr for future handling
  result <- dplyr::group_by(result, TradeNr)

  ### If same TradeNr is requested multiple times then result will be repeated multiple times
  if (anyDuplicated(trade_nr) > 0) suppressMessages(dplyr::left_join(data.frame(TradeNr = trade_nr), result))
  else result
}

#' getRnR
#'
#' This function retrieves a data frame listing reward and risk for a given list of trade numbers
#'
#' It works by matching all trade numbers in Trades DB against the argument \code{trade_nr} given.
#' Then it groups by trade number and compute for each trade number the total of reward and risks.
#' Trades may be opened, adjusted or closed.
#' If the same trade number is present multiple times, then risk and reward data will be repeated
#' This is useful for mass getRnR calls
#'
#'@param trade_nr an integer or a vector of integers
#'@return a dataframe with column trade number, reward and risk.
#'Each row is associated with a trade number.
#'Reward column is the sum of rewards (non-NA) for all given instruments that belong to the trade number
#'Risk column is the sum of risks (non-NA) for all given instruments that belong to the trade number
#'@export
getRnR = function(trade_nr) {

  if (!is.numeric(trade_nr)) {
    Tbasics::display_error_message("trade_nr must be a numeric")
  }

  trades <- getAllTrades()

  ### Retrieve only trades that have TradeNr within trade_nr input data
  trades <- dplyr::filter(trades, TradeNr %in% trade_nr)

  if (nrow(trades)==0) {
    Tbasics::display_message("Trade does not exist!")
    return(NA)
  }

  ### Group by TradeNr for summarize to work properly
  trades <- dplyr::group_by(trades, TradeNr)


  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR <-  dplyr::summarize(trades,
                         risk = sum(as.double(Risk), na.rm=T),
                         reward = sum(as.double(Reward), na.rm=T))

  ### If the same trade number is requested multiple times then RnR will be returned as many times
  RnR <- suppressMessages(dplyr::left_join(data.frame(TradeNr = trade_nr), RnR))

  return(RnR)
}
