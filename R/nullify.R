#### Created on 31.12.2023
### This is to remove warning messages from check()
### Quote from R Packages manual:
#'The “no visible binding” note is a peculiarity of using dplyr and unquoted variable names inside a package,
#'where the use of bare variable names (english and temp) looks suspicious.
#'
### See also https://r-pkgs.org/package-within.html

### for chartServer call
x <- y1 <- y2 <- NULL

### for getBSComboPrice
pos <- type <- strike <- DTE <- mul <- NULL

### for getOpenDate
TradeNr <- Statut <- TradeDate <- Exp.Date <- Pos <- NULL

### for getTradeNr
Account <- Reward <- Risk <- Instrument <- NULL

### for readAccount
account <- NULL

## for readPortfolio
secType <- lastTradeDateOrContractMonth <- undPrice <- impliedVol <- position <- NULL
marketPrice <- marketValue <- averageCost <- unrealizedPnL <- symbol <- right <- NULL

### for stock_price
exchange <- sec <- NULL
