test_that("readPortfolio DU5221795 returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio("DU5221795")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol",   "expdate", "strike", "pos", "mktPrice", "optPrice",
                               "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                               "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readPortfolio U1804173 returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio("U1804173")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol",   "expdate", "strike", "pos", "mktPrice", "optPrice",
                                "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                                "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})



test_that("readLastPortfolio Simu returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("DU5221795")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "expdate", "strike",  "pos", "mktPrice", "optPrice",
                            "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                            "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readLastPortfolio Live returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("U1804173")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "expdate", "strike", "pos", "mktPrice", "optPrice",
                            "mktValue", "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                            "uPrice", "multiplier", "currency", "type", "Instrument", "margin")))
    expect_true(nrow(portf) >=1)
  })
})


test_that("readLastPortfolio Gonet returns some data and columns are all authorized names", {
  expect_true({
    portf=readLastPortfolio("Gonet")
    expect_true(identical(colnames(portf),
                          c("TradeNr","date", "heure", "symbol", "pos", "mktPrice",
                            "mktValue", "avgCost", "unPnL",
                            "currency", "type", "margin")))
    expect_true(nrow(portf) >=1)
  })
})

test_that("readAccount Simu  returns some data and columns look good", {
  expect_true({
    acc=readAccount("DU5221795")
    identical(colnames(acc),  c("date", "heure", "Currency", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})


test_that("readAccount Gonet  returns some data and columns look good", {
  expect_true({
    acc=readAccount("Gonet")
    identical(colnames(acc),  c("date","heure", "Currency", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})
