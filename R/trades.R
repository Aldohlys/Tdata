
###library(DescTools) ### for pgcd
###library(quantmod) ## to retrieve data from Yahoo - getSymbols
###library(stringr) ### for str_to_upper function
##library(RQuantLib) ## for isBusinessDay function
### library(lubridate) ### for today() function

#' getAllTrades function
#'
#' This function work only for IBKR accounts not for Gonet account
#' This function is used by other Tutils functions - NOT to be exported
#' No argument - takes its source from config::get()
#' @importFrom readr read_delim locale
getAllTrades = function() {
  suppressMessages(read_delim(file=config::get("Trades"),
                                     delim=";",locale=locale(date_names="en",decimal_mark=".",
                                                             grouping_mark="",encoding="UTF-8")))
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
    display_error_message("No instrument to be searched!")
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
  ### If left join returns NA -> trade is not present - not yet recorded in Trades.csv
  if (all(is.na(trade_nr))) {
    display_error_message("Trades not opened/adjusted in Trades.csv file!")
    print(v_instrument)
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
#' This function retrieves one or a vector of dates, all pertaining to open/adjusted trades
#'
#' It works by matching all trade number in Trades.csv file argument against trade dates
#' Then the oldest date per trade is returned.
#'
#'@param trade_nr an integer or a vector of integers
#'@return a data frame giving for each trade number:
#'* `active_open_date` the opening date of the instrument corresponding to `expdate`
#'* `expdate` the first still active expiration date
#'* `orig_date`  the original opening trade date (even if corresponding instrument part of the trade has been closed since)
#'@export
getOpenDate = function(trade_nr) {

  if (!is.numeric(trade_nr)) {
    display_error_message("trade_nr must be a numeric")
    return(NA)
  }

  trades=getAllTrades()
  trades = dplyr::group_by(dplyr::right_join(trades, data.frame(TradeNr=trade_nr), by="TradeNr"), TradeNr)

  if (nrow(trades)==0) {
    display_error_message("Trade do not exist !")
    return(NA)
  }
  if (all(trades$Statut %in% "Ferm\u00e9")) {
    display_error_message("All these trades are closed in Trades.csv file!")
    return(NA)
  }

  ### Remove closed trades - this makes sense for vectorized input only
  trades = dplyr::filter(trades, Statut != "Ferm\u00e9")

  ### Retrieve initial opening date for each trade - trades are grouped by trade_nr
  ### orig_date is the oldest date recorded for the trade -
  orig_date = dplyr::summarize(trades,orig_date=min(lubridate::dmy(TradeDate)))

  ### Remove all instrument that have been closed - keep only active ones
  trades = dplyr::filter(
              dplyr::summarize(
                dplyr::group_by(trades,TradeNr,Instrument),
                ### Exp.Date is expiration date associated to Instrument - unique by definition
                Exp.Date=dplyr::first(lubridate::dmy(Exp.Date)),
                ### TradeDate is the date where Instrument has been traded within the trade for the first time
                TradeDate=min(lubridate::dmy(TradeDate)),
                ### Pos gives the current position of the instrument - may be 0 if instrument has been sold in the trade (ex: roll out)
                Pos=sum(Pos)),
              Pos !=0)

  ### Retrieve first expiration date to come (still active) for each trade number
  exp_date = dplyr::summarize(trades, Exp.Date=min(Exp.Date))

  ### Keep only for each trade number the first expiration date data
  trades = dplyr::inner_join(trades,exp_date,by=c(TradeNr,Exp.Date))
  ### Include also original trade date for each trade
  trades = dplyr::left_join(trades,orig_date,by=TradeNr)

  ### This tibble is grouped by TradeNr for future handling
  dplyr::select(trades,TradeNr,active_open_date=TradeDate, expdate=Exp.Date,orig_date)
}

#' getRnR
#'
#' This function retrieves a data frame listing reward and risk for a given list of trade numbers
#'
#' It works by matching all trade number in Trades.csv file argument against the argument given.
#' Then it groups by trade number and compute for each trade numnber the total of reward and risks
#'
#'@param trade_nr an integer or a vector of integers
#'@return a dataframe with column trade number, reward and risk.
#'Each row is associated with a trade number.
#'Reward column is the sum of rewards (non-NA) for all given instruments that belong to the trade number
#'Risk column is the sum of risks (non-NA) for all given instruments that belong to the trade number
#'@export
getRnR = function(trade_nr) {
  message("getRnR - Reward and Risk")

  if (!is.numeric(trade_nr)) {
    display_error_message("trade_nr must be a numeric")
    return(NA)
  }

  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  trades = getAllTrades()
  trades = dplyr::group_by(dplyr::right_join(trades, data.frame(TradeNr=trade_nr), by="TradeNr"), TradeNr)

  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }

  if (all(trades$Statut %in% "Ferm\u00e9")) {
    display_error_message("All these trades are closed in Trades.csv file!")
    return(NA)
  }

  ### Remove closed trades - Extract only open/adjusted trades
  trades = dplyr::filter(trades, Statut != "Ferm\u00e9")

  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR=  dplyr::summarize(trades, reward= sum(as.double(Reward),na.rm=T),risk= sum(as.double(Risk),na.rm=T))
  #print(RnR)
  return(RnR)
}
