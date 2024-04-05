test_that("readPortfolio U1804173 returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio("U1804173")
       all(colnames(portf) %in% c("TradeNr", "date", "new_date", "rdate", "heure", "secType", "symbol", "expiration", "expdate", "strike",
                                  "pos", "mktPrice", "optPrice", "mktValue", "avgCost", "uPnL", "IV", "pvDividend", "delta", "gamma",
                                  "vega", "theta", "uPrice", "multiplier", "currency", "type", "Instrument")) &&
      nrow(portf) >=1
  })
})

test_that("readPortfolio Gonet returns some data and columns are all authorized names", {
  skip("Skip Gonet")
  expect_true({
    portf=readPortfolio("Gonet")
    all(colnames(portf) %in% c("TradeNr","date", "new_date", "rdate", "heure", "symbol", "type", "expiration", "strike", "pos", "mktPrice", "optPrice",
                               "mktValue", "avgCost", "uPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                               "uPrice", "multiplier", "currency")) &&
      nrow(portf) >=1
  })
})

test_that("readAccount Simu  returns some data and columns look good", {
  expect_true({
    acc=readAccount("DU5221795")
    identical(colnames(acc),  c("account", "date","new_date", "rdate", "heure", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})


test_that("readAccount Gonet  returns some data and columns look good", {
  expect_true({
    acc=readAccount("Gonet")
    identical(colnames(acc),  c("account", "date","new_date", "rdate","heure", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})
