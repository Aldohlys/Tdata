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
