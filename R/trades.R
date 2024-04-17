
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

  file.copy(from=paste0(config::get("DirNewTrading"),"Trades.csv"),
            to=paste0(config::get("DirNewTrading"),"Trades-old.csv"),overwrite = T)

  utils::write.table(trades,file=paste0(config::get("DirNewTrading"),"Trades.csv"),append=F,
              col.names=TRUE,row.names=FALSE,sep=";",dec=".",quote=TRUE)

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  DBI::dbWriteTable(conn, "Trades", trades, overwrite = TRUE,
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
#' This function work only for IBKR accounts not for Gonet account
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
  # suppressMessages(read_delim(file=config::get("Trades"),
  #                                    delim=";",locale=locale(date_names="en",decimal_mark=".",
  #                                                            grouping_mark="",encoding="UTF-8")))
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  alltrades = DBI::dbReadTable(conn, "Trades")
  DBI::dbDisconnect(conn)

  if (any(with(alltrades, !is.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL))))) {
    Tbasics::display_error_message("Trades input data had to be converted!")
    with(alltrades, as.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL)))
  }
  alltrades
}

### This function is useful for test_that test functions as getToday will then be changed for mocking test
getToday = function() {
  Sys.Date()
}

#' getTradeNr
#'
#' This function retrieves one or a vector of trade numbers, all pertaining to open/adjusted trades
#' including instruments listed in \code{v_instrument} argument.
#'
#' It works by matching all instruments in Trades.csv file argument,
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
    Tbasics::display_error_message("No instrument to be searched!")
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
    Tbasics::display_error_message(paste0("For instruments ", do.call(paste,as.list(c(v_instrument,sep=" and "))),
                                          " no opened/adjusted trades in Trades table!"))
    return(NA)
  }

  ### Retrieve the common Trade Nr - there may be several trade_nr if requested by unique argument
  if (unique) trade_nr=unique(trade_nr)

  ### If at least one then retrieve corresponding trade nr
  # ### And get the original trade date of the trade nr
  # if (length(trade_nr) >1) {
  #   display_error_message("There is more than one trade in Instrument argument! Display oldest trade nr")
  #   return(NA)
  # }

  ### Converted to integer (vector of integer if necessary)
  return(as.integer(trade_nr))
}

#' getOpenDate
#'
#' This function retrieves one or a vector of dates, pertaining to open, adjusted or closed trades, plus trade number and strategy.
#'
#' It works by matching all trade number arguments against corresponding trade dates found in Trades.csv
#' Then the oldest date per trade is returned, plus still active expiration/active opening date (for existing position only)
#' plus strategy.
#' If the same trade number is present multiple times, then risk and reward data will be repeated
#' This is useful for mass getRnR calls
#'
#'@param trade_nr an integer or a vector of integers
#'@return a data frame including:
#'* `TradeNr` trade number, an integer
#'* `strategy` a string, the strategy that was used: can be "BOT", "BPT", "OFI", "CS". or even "Erreur"
#'* `exp_date` the first still active expiration date  - only for opened/adjusted trades
#'* `orig_date`  the original opening trade date (even if corresponding instrument part of the trade has been closed since)
#'* `last_date` the last trading date on the trade - will be the close date if the trade is closed
#'@export
getOpenDate = function(trade_nr) {

  if (!is.numeric(trade_nr)) {
    Tbasics::display_error_message("trade_nr must be a numeric")
    return(NA)
  }

  trades=getAllTrades()
  ### This will create a line for trade_nr even if trade_nr does not exist in trades data frame
  ### trades is grouped by TradeNr
  trades = suppressMessages(dplyr::group_by(dplyr::right_join(trades, data.frame(TradeNr=trade_nr), by="TradeNr"), TradeNr))

  ### Retrieve initial opening date for each trade - trades are grouped by trade_nr
  ### orig_date is the oldest date recorded for the trade -
  orig_date = dplyr::summarize(trades,orig_date=min(as.Date(as.character(TradeDate),format="%Y%m%d")))

  ### last_date is the most recent date recorded for the trade
  last_date = dplyr::summarize(trades,last_date=max(as.Date(as.character(TradeDate),format="%Y%m%d")))

  ### Retrieve strategies for each trade - trades are grouped by trade_nr
  strategy = dplyr::summarize(trades,strategy=dplyr::first(Strategy))

  non_trades = dplyr::pull(dplyr::filter(orig_date,is.na(orig_date)), TradeNr)
  if (length(non_trades)!=0) {
    stop("There are inexisting trades in argument ",non_trades)
  }

  ### Remove all instrument that have been closed - keep only active ones
  ### For any instrument within a trade, keep the last active date
  ### trades is grouped by TradeNr
  trades = dplyr::filter(
              dplyr::summarize(
                dplyr::group_by(trades,TradeNr,Instrument),
                ### Exp.Date is expiration date associated to Instrument - unique by definition
                Exp.Date=as.Date(dplyr::first(Exp.Date),format="%d.%m.%Y"),
                ### Pos gives the current position of the instrument - may be 0 if instrument has been sold in the trade (ex: roll out)
                ### Will be 0 if trade is closed in any case
                Pos=sum(Pos)),
              Pos !=0)

  #### In case expiration trades have not been recorded,
  #### keep only positions whose expiration date is posterior or equal to today
  trades = dplyr::filter(trades, Exp.Date >= getToday())

  ### Remove Instrument and Pos column
  trades = dplyr::select(trades,c(TradeNr,exp_date=Exp.Date))

  ### In case there are still open/adjusted trades
  if (nrow(trades) != 0) {
    ### Remove duplicated lines - in case several instrument were handled at the same time for the same trade
    trades = trades[!duplicated(trades),]
    ### Retrieve first expiration date to come (still active) for each trade number
    trades = dplyr::group_by(dplyr::summarize(trades, exp_date=min(exp_date)),TradeNr)
  }


  ### Include also original trade date, last_trade date and strategy for each trade
  ### In order to make sure to include also closed trades start by left join with orig_date
  trades = suppressMessages(dplyr::left_join(strategy,trades,by=TradeNr))
  trades = suppressMessages(dplyr::left_join(trades,orig_date,by=TradeNr))
  trades = suppressMessages(dplyr::left_join(trades,last_date, by=TradeNr))

  ### This tibble is grouped by TradeNr for future handling
  result <- dplyr::group_by(trades,TradeNr)

  ### If same TradeNr is requested multiple times then result will be repeated multiple times
  suppressMessages(dplyr::left_join(data.frame(TradeNr = trade_nr), result))
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
    return(NA)
  }

  trades <- getAllTrades()

  ### Retrieve only trades that have TradeNr within trade_nr input data
  trades <- dplyr::filter(trades, TradeNr %in% trade_nr)

  if (nrow(trades)==0) {
    Tbasics::display_error_message("Trade does not exist!")
    return(NA)
  }

  ### Group by TradeNr for summarize to work properly
  trades <- dplyr::group_by(trades, TradeNr)


  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR <-  dplyr::summarize(trades,
                         risk = sum(as.double(Risk), na.rm=T),
                         reward = sum(as.double(Reward), na.rm=T))
  #print(RnR)

  ###├ If the same trade number is requested multiple times then RnR will be returned as many times
  RnR <- suppressMessages(dplyr::left_join(data.frame(TradeNr = trade_nr), RnR))

  return(RnR)
}
