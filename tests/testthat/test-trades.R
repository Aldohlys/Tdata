
conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))

getTestTrades = function() {
  alltrades = DBI::dbReadTable(conn, "TestTrades")
  if (any(with(alltrades, !is.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL))))) {
    Tbasics::display_message("Trades input data had to be converted!")
    with(alltrades, as.numeric(c(TradeNr,TradeDate,Pos,Prix, Comm., Total, Risk, Reward,PnL)))
  }
  alltrades
}

getActiveTestTradeQuery <- function(account) {
  DBI::dbGetQuery(conn,
                                 "Select * from TestTrades WHERE Statut != 'Ferm\U00e9' AND Account = ?",
                                 params=list(account))
}

getTestInstrumentQuery <- function(conn, account, instr) {
  return(DBI::dbGetQuery(conn,
                         "SELECT Prix As startPrice,`Exp.Date` as expdate, Ssjacent as symbol, min(TradeDate) AS initial_trade_date FROM TestTrades WHERE Account = ? AND Instrument = ? AND (Statut ='Ouvert' OR Statut = 'Ajust\U00e9')",
                         params=list(account, instr)))

}

getTestTradeQuery <- function(conn) {
  return(DBI::dbGetQuery(conn,
                         "SELECT DISTINCT TradeNr from TestTrades WHERE Statut = 'Ouvert' OR Statut = 'Ajust\u00e9'"
                         ))
}




# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

test_that("It is possible to retrieve one instrument in one account", {
  with_mocked_bindings(
    getInstrumentQuery = getTestInstrumentQuery, {
      df = getInstrument(account=c("U1804173"),instrument=c("CCJ 19APR24 36 P"))
      expect_equal(df,
                   data.frame(initial_trade_date= as.Date("2024-02-23"),
                              startPrice= 0.69,  DTE=56.88, u_price =40.189999,
                              startIV=  0.38),
                   tolerance = 0.01)
    })
})

test_that("getInstrument is checked for account and instrument lengths",{
  with_mocked_bindings(
    getInstrumentQuery = getTestInstrumentQuery, {
      df = getInstrument(account=c("U1804173","U1804173"),instrument=c("SPY 21JUN24 525 P"))
      expect_true(is.na(df))
    })
})

test_that("getInstrument returns NA for instrument that are closed",{
  with_mocked_bindings(
    getInstrumentQuery = getTestInstrumentQuery, {
      df = getInstrument(account="U1804173",instrument="SLV 17MAR23 20 P")
      expect_true(all(is.na(df)))
    })
})

test_that("It is possible to retrieve multiple instruments from one account", {
  with_mocked_bindings(
    getInstrumentQuery = getTestInstrumentQuery, {
      df = getInstrument(account=c("U1804173","U1804173"),instrument=c("CCJ 19APR24 36 P",
                                                                       "SMH 19APR24 240 C"))
      expect_equal(df,
                   data.frame(initial_trade_date= c(as.Date("2024-02-23"),as.Date("2024-03-21")),
                              startPrice= c(0.69, 3.05),  DTE= c(56.88, 29.88), u_price = c(40,226),
                              startIV=  c(0.38, 0.29)),
                   tolerance = 0.01)
    })
})


test_that("Retrieve trade number status for a single trade",{
  with_mocked_bindings(
    getTradeQuery = getTestTradeQuery, {
      expect_false(isTradeOpened(397))
      expect_true(isTradeOpened(396)) ### Ajusté case
      expect_true(isTradeOpened(401)) ### Ouvert case
    }
  )
})

test_that("Retrieve trade number status for numerous trades including non existing trade",{
  with_mocked_bindings(
    getTradeQuery = getTestTradeQuery, {
      expect_true(all(isTradeOpened(c(397, 396, 401, 405)) == c(FALSE, TRUE, TRUE, FALSE)))
    }
  )
})



test_that("It is possible to retrieve a trade number for a single instrument", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("NEM 19APR24 30 P",account_type="Live")
      expect_true(trade_nr == 394)
    })
})

test_that("It is possible to retrieve a trade number for a single instrument", {
    with_mocked_bindings(
      getAllTrades = getTestTrades, {
        trade_nr=getTradeNr("TLT 15MAR24 92 P",account_type="Live")
        expect_true(trade_nr == 380)
      })
})

test_that("Error displayed if no trade for these instrument", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(c("STOCKALEX 15MAR24 92 P","YVESSTARTUP 32FEB24 100 C"),account_type="Live")
      expect_true(is.na(trade_nr))
    })
})


test_that("It is possible to retrieve a trade number for a single instrument without specifying account type", {
  with_mocked_bindings(
    getAllTrades = getTestTrades,  {
      trade_nr=getTradeNr("TLT 15MAR24 92 P")
      expect_true(trade_nr == 380)
    })
})


test_that("It is possible to retrieve a trade number for a single instrument with account type=NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("TLT 15MAR24 92 P",NA)
      expect_true(trade_nr == 380)
    })
})

test_that("It is possible to retrieve a trade number for a trade with multiple instruments", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("GLD 09FEB24 189 P",account_type="Simu")
      expect_true(trade_nr == 383)
    })
})


test_that("It is possible to retrieve a trade number for a trade with multiple instruments, using multiple inputs", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("GLD 09FEB24 189 P","GLD 16FEB24 187.5 P")), account_type="Simu")
      expect_true(trade_nr == 383)
    })
})

test_that("It is possible to retrieve trade numbers for 2 trades with multiple instruments, using one input per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("GOLD 19APR24 17 C","NEM 20SEP24 35 P")), account_type="Live")
      expect_true(all(trade_nr == c(395,394)))
    })
})

test_that("It is possible to retrieve a trade number for 2 trades with multiple instruments, using multiple inputs per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("INTC","GLD 23FEB24 184 P","AAPL Vertical Spread 04.11.2022 140/145 P/P")),
                          account_type="Simu")
      expect_true(all(compareNA(trade_nr,c(NA,383,390))))
    })
})

test_that("It is possible to retrieve a trade number for 3 trades with multiple instruments, using multiple inputs per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SMH 19APR24 240 C","ESTX50 21JUN24 4125 P","ESTX50 21JUN24 4525 P","AMGN 19APR24 290 C")),
                          account_type="Live")
      expect_true(all(trade_nr == c(400,396,398)))
    })
})

test_that("It is possible to retrieve trade numbers for 3 trades with multiple instruments, using multiple inputs per trade but with repeated trade numbers for each instrument", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SMH 19APR24 240 C","ESTX50 21JUN24 4125 P","ESTX50 21JUN24 4525 P","AMGN 19APR24 290 C")),
                          account_type="Live", unique= FALSE)
      expect_true(all(trade_nr == c(400,396,396,398)))
    })
})

test_that("It is possible to retrieve a trade number providing also data for closed trades", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("VALE SPY 21JUN24 525 P","CVS 19APR24 75 P")), account_type="Live")
      expect_true(all(trade_nr == 399,na.rm=TRUE))
    })
})

test_that("If no trade or closed trade then returns NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("VALE 19JAN24 13 P","STOCK")), account_type="Live")
      expect_true(all(trade_nr == 367,na.rm=TRUE))
    })
})

test_that("If no trade at all then returns NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")), account_type="Live")
      expect_true(is.na(trade_nr))
    })
})


test_that("If no trade at all then returns NA without account type specified", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")))
      expect_true(is.na(trade_nr))
    })
})


test_that("If no trade at all then returns NA with account type equals to NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")),NA)
      expect_true(is.na(trade_nr))
    })
})

test_that("Test a trade from Simu account with account type equals to NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24",NA)
      expect_true(trade_nr == 347)
    })
})


test_that("Test a trade from Simu account NA with account type equals to Simu", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24","Simu")
      expect_true(trade_nr == 347)
    })
})


test_that("Test a trade from Simu account NA with account type equals to Live", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24","Live")
      expect_true(is.na(trade_nr))
    })
})

test_that("getOpenDate works with a closed trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades,
    getToday = function()(as.Date("2024-04-04")), {
        expect_equal(as.data.frame(getOpenDate(300)),
             data.frame(TradeNr=300,strategy="BOT",exp_date=as.Date(NA),
                        orig_date=as.Date("2023-08-02"),last_date=as.Date("2023-08-29")))
    })
})


test_that("getOpenDate works with a closed trade without expiring closing trade recorded", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, getToday = function()(as.Date("2024-04-04")),{
      expect_equal(as.data.frame(getOpenDate(316)),
                   data.frame(TradeNr=316,strategy="OFI",exp_date=as.Date(NA),
                              orig_date=as.Date("2023-09-18"),last_date=as.Date("2023-09-28")))
    })
})


test_that("getOpenDate works with a opened trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades,getToday = function()(as.Date("2024-04-04")), {
      expect_equal(as.data.frame(getOpenDate(399)),
                   data.frame(TradeNr=399,strategy="BPT",exp_date=as.Date("2024-04-19"),
                              orig_date=as.Date("2024-03-21"),last_date=as.Date("2024-03-21")))
    })
})

test_that("getOpenDate works with opened and closed trades, opened trade with multiple expiration dates",{
  with_mocked_bindings(
    getAllTrades = getTestTrades,getToday = function()(as.Date("2024-04-04")), {
      expect_equal(as.data.frame(getOpenDate(c(367,370,392))),
                   data.frame(TradeNr=c(367,370,392),strategy=c("OFI","BPT","OFI"),
                              exp_date=c(as.Date(NA),as.Date("2025-12-19"),as.Date("2024-04-19")),
                              orig_date=c(as.Date("2023-12-14"),as.Date("2023-12-21"),as.Date("2024-02-23")),
                              last_date=c(as.Date("2024-02-08"),as.Date("2024-03-21"),as.Date("2024-02-23"))))
    })
})


test_that("getRnR works with one simple trade",{
  with_mocked_bindings(
    getAllTrades = getTestTrades,  {
      expect_equal(as.data.frame(getRnR(392)),
                   data.frame(TradeNr = 392, risk = 500, reward = 700))
  })
})

test_that("getRnR works with one simple trade with reward/risks on multiple lines",{
  with_mocked_bindings(
    getAllTrades = getTestTrades,  {
      expect_equal(as.data.frame(getRnR(395)),
                   data.frame(TradeNr = 395, risk = 215, reward = 700))
      expect_equal(as.data.frame(getRnR(c(395, 395))),
                   data.frame(TradeNr = c(395, 395), risk = c(215, 215), reward = c(700, 700)))
    })
})


test_that("getRnR works with multiple trades",{
  with_mocked_bindings(
    getAllTrades = getTestTrades,  {
      expect_equal(as.data.frame(getRnR(c(392, 380))),
                   data.frame(TradeNr = c(392, 380), risk = c(500, 800), reward = c(700, 897)))
    })
})

test_that("getActiveTrades returns correct trades", {
  with_mocked_bindings(
    getActiveTradeQuery = getActiveTestTradeQuery, {
      active_trades <- unique(getActiveTrades("U1804173")$TradeNr)
      expect_true(380 %in% active_trades)  ### Ajusté dans Uxxx
      expect_true(392 %in% active_trades) ### Ouvert dans Uxxx
      expect_false(384 %in% active_trades) ### Fermé dans Uxxx
      expect_false(383 %in% active_trades) ### Ajusté dans DUxxx
    }
  )
})

test_that("getClosedTrades does not return trades whose trade dates are across window date", {
  closed_trades <- getClosedTrades("U1804173", windowDate = as.Date("20240201", "%Y%m%d"))
  trade_nr <- unique(closed_trades$TradeNr)
  expect_false(371 %in% trade_nr)
  expect_true(380 %in% trade_nr)
})

DBI::dbDisconnect(conn)
