### ---------------------------------------------------------------------------
### ibkr_statement_dividends() / dividend_target_trade(): dividend import
### ---------------------------------------------------------------------------

.multi_statement <- function() {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "﻿Statement,Header,Field Name,Field Value",
    "Account Information,Data,Account,U1804173",
    "Dividends,Header,Currency,Date,Description,Amount",
    "Dividends,Data,GBP,2026-04-24,CRST(GB00B8VZXT93) Cash Dividend GBP 0.018 per Share (Ordinary Dividend),7.2",
    "Dividends,Data,Total,,,7.2",
    "Dividends,Data,Total in CHF,,,7.64784",
    "Account Information,Data,Account,U25343478",
    "Dividends,Data,CAD,2026-06-30,MRD(CA5854671032) Cash Dividend CAD 0.15 per Share (Ordinary Dividend),30",
    "Dividends,Data,EUR,2026-05-28,CA(FR0000120172) Cash Dividend EUR 0.97 per Share (Ordinary Dividend),97",
    "Dividends,Data,EUR,2026-07-30,CA(FR0000120172) Cash Dividend EUR 0.21 per Share (Bonus Dividend),42",
    "Withholding Tax,Header,Currency,Date,Description,Amount,Code",
    "Withholding Tax,Data,CAD,2026-06-30,MRD(CA5854671032) Cash Dividend CAD 0.15 per Share - CA Tax,-4.5,",
    "Withholding Tax,Data,EUR,2026-05-28,CA(FR0000120172) Cash Dividend EUR 0.97 per Share - FR Tax,-24.25,",
    "Withholding Tax,Data,EUR,2026-07-30,CA(FR0000120172) Cash Dividend EUR 0.21 per Share - FR Tax,-10.5,",
    "Withholding Tax,Data,Total,,,-39.25,"
  ), f, useBytes = TRUE)
  f
}

test_that("ibkr_statement_dividends pairs each dividend with its tax, per account", {
  d <- ibkr_statement_dividends(.multi_statement())
  expect_equal(nrow(d), 4)
  expect_equal(d$account, c("U1804173", "U25343478", "U25343478", "U25343478"))
  expect_equal(d$symbol, c("CRST", "CA", "MRD", "CA"))
  expect_equal(d$net, c(7.2, 72.75, 25.5, 31.5))
  expect_equal(d$tax, c(0, -24.25, -4.5, -10.5))
  expect_equal(d$rate, c(0.018, 0.97, 0.15, 0.21))
})

test_that("a dividend reversed inside the statement nets to nothing", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "Account Information,Data,Account,U25343478",
    "Dividends,Data,EUR,2026-05-28,CA(FR0000120172) Cash Dividend EUR 0.97 per Share (Ordinary Dividend),97",
    "Dividends,Data,EUR,2026-05-28,CA(FR0000120172) Cash Dividend EUR 0.97 per Share (Ordinary Dividend),-97"
  ), f)
  expect_equal(nrow(ibkr_statement_dividends(f)), 0)
})

.trades_fixture <- data.frame(
  TradeNr = c(697L, 697L, 702L, 800L, 800L),
  Account = c("U25343478", "U25343478", "U25343478", "U1804173", "U1804173"),
  TradeDate = c(20260316L, 20260526L, 20260320L, 20250101L, 20250601L),
  DateTime = c("2026-03-16 14:53:31", "2026-05-26 07:04:02", "2026-03-20 15:15:00",
               "2025-01-01 10:00:00", "2025-06-01 10:00:00"),
  Symbol = c("CA", "CA", "CRST", "CA", "CA"),
  Pos = c(100L, 100L, 400L, 50L, -50L),
  EventType = c("Open", "Adjust", "Open", "Open", "Close"),
  stringsAsFactors = FALSE)

test_that("dividend_target_trade picks the trade holding the stock on the pay date", {
  leg <- dividend_target_trade(.trades_fixture, "CA", as.Date("2026-05-28"), "U25343478")
  expect_equal(leg$TradeNr, 697L)
  ### Trade 800 was closed (net 0): never a target.
  expect_null(dividend_target_trade(.trades_fixture[4:5, ], "CA", as.Date("2026-05-28"), "U1804173"))
})

test_that("dividend_target_trade finds a stock transferred to the other account", {
  ### CRST: the dividend is paid into U1804173, the trade lives in U25343478.
  leg <- dividend_target_trade(.trades_fixture, "CRST", as.Date("2026-04-24"), "U1804173")
  expect_equal(leg$TradeNr, 702L)
  expect_equal(leg$Account, "U25343478")
})

test_that("dividend_target_trade ignores trades opened after the pay date", {
  expect_null(dividend_target_trade(.trades_fixture, "CRST", as.Date("2026-03-01"), "U25343478"))
})

test_that("planIBKRDividends books against the table it is given and skips what is there", {
  tr <- .trades_fixture
  tr$Instrument <- tr$Symbol; tr$Strategy <- "VALUE"; tr$Status <- "Ouvert"
  tr$TimeZoneSource <- "America/New_York"; tr$Currency <- "EUR"; tr$Total <- 0
  plan <- planIBKRDividends(.multi_statement(), tr)
  expect_equal(plan$payments$action, c("insert", "insert", "no trade", "insert"))
  expect_equal(plan$new_rows$TradeNr, c(702L, 697L, 697L))
  expect_true(all(plan$new_rows$EventType == "Dividend" & plan$new_rows$Pos == 0))
  ### Feed the booked rows back: nothing left to insert.
  tr2 <- dplyr::bind_rows(tr, plan$new_rows)
  again <- planIBKRDividends(.multi_statement(), tr2)
  expect_null(again$new_rows)
  expect_equal(sum(again$payments$action == "exists"), 3)
})

test_that("saveTrades refuses a snapshot that would drop imported dividend rows", {
  reset_loaded_trades()          # isolate from the freshness check
  rows <- data.frame(TradeNr = c(697L, 697L), Account = "U25343478",
                     TradeDate = c(20260316L, 20260528L), Pos = c(100L, 0L),
                     Total = c(-1568.25, 72.75), Currency = "EUR",
                     EventType = c("Open", "Dividend"), stringsAsFactors = FALSE)
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  DBI::dbWriteTable(db, "Trades", rows)
  ### A connection wrapper that survives saveTrades' own dbDisconnect.
  keep <- function() { con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
                       RSQLite::sqliteCopyDatabase(db, con); con }
  with_mocked_bindings(
    safe_db_connect = keep,
    safe_db_write = function(...) stop("must not write"),
    .package = "Tdata", {
      expect_null(suppressMessages(saveTrades(rows[1, ])))          # dividend missing -> abort
    })
  written <- FALSE
  with_mocked_bindings(
    safe_db_connect = keep,
    safe_db_write = function(...) { written <<- TRUE; invisible(TRUE) },
    .package = "Tdata", {
      saveTrades(rows)                                               # dividend kept -> writes
    })
  expect_true(written)
})

.flex_cash <- function() {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    '"Date/Time","ClientAccountID","Type","AssetClass","DividendType","Symbol","Description","Amount","CurrencyPrimary","FXRateToBase","IssuerCountryCode","ExDate"',
    '"20260929;202000","U25343478","Withholding Tax","STK","","MRD","MRD(CA5854671032) CASH DIVIDEND CAD 0.15 PER SHARE - CA TAX","-4.5","CAD","0.58768","CA",""',
    '"20260928;202000","U25343478","Withholding Tax","STK","","9273.T","9273.T(JP3283420002) CASH DIVIDEND JPY 18 PER SHARE - JP TAX","-1097","JPY","0.0052867","JP",""',
    '"20260929;202000","U25343478","Dividends","STK","Ordinary Dividend","MRD","MRD(CA5854671032) CASH DIVIDEND CAD 0.15 PER SHARE (Ordinary Dividend)","30","CAD","0.58768","CA","20260915"',
    '"20260928;202000","U25343478","Dividends","STK","Ordinary Dividend","9273.T","9273.T(JP3283420002) CASH DIVIDEND JPY 18 PER SHARE (Ordinary Dividend)","7164","JPY","0.0052867","JP","20260629"',
    '"20260928;202000","U25343478","Payment In Lieu Of Dividends","STK","Ordinary Dividend","9273.T","9273.T(JP3283420002) PAYMENT IN LIEU OF DIVIDEND (Ordinary Dividend)","36","JPY","0.0052867","JP","20260629"',
    '"20260626","U25343478","Other Fees","","","","KRW CUSTODY FEE ON STK FOR 2026-06-25 FOR JUN 2026","-1.5","KRW","0.00052752","",""',
    '"Date","ClientAccountID","AssetClass","Symbol","Description","NetAmount","GrossAmount","GrossRate","Fee","Tax","Quantity","PayDate","ExDate","IssuerCountryCode","FXRateToBase","CurrencyPrimary","ToAcct","FromAcct"',
    '"20260914","U25343478","STK","MRD","MELCOR DEVELOPMENTS LTD","25.5","30","0.15","0","4.5","200","20260929","20260915","CA","0.58821","CAD","",""'
  ), f)
  f
}

test_that("a Flex cash-transaction CSV parses like a statement; fees and accruals are ignored", {
  d <- ibkr_statement_dividends(.flex_cash())
  expect_equal(d$symbol, c("9273", "9273", "MRD"))
  expect_equal(d$date, as.Date(c("2026-09-28", "2026-09-28", "2026-09-29")))
  ### Tax pairs with the cash dividend, the payment in lieu stands alone.
  expect_equal(d$net, c(6067, 36, 25.5))
  expect_equal(sum(d$net[d$symbol == "9273"]), 6103)
  expect_equal(d$rate, c(18, NA, 0.15))
})

test_that("Flex and statement rows of the same payment are the same booking", {
  st <- tempfile(fileext = ".csv")
  writeLines(c(
    "Account Information,Data,Account,U25343478",
    "Dividends,Data,CAD,2026-09-29,MRD(CA5854671032) Cash Dividend CAD 0.15 per Share (Ordinary Dividend),30",
    "Withholding Tax,Data,CAD,2026-09-29,MRD(CA5854671032) Cash Dividend CAD 0.15 per Share - CA Tax,-4.5,"
  ), st)
  a <- ibkr_statement_dividends(st)
  b <- ibkr_statement_dividends(.flex_cash())
  b <- b[b$symbol == "MRD", ]
  expect_equal(b[, c("account", "currency", "date", "symbol", "rate", "net")],
               a[, c("account", "currency", "date", "symbol", "rate", "net")],
               ignore_attr = TRUE)
})
