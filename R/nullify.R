#### Created on 31.12.2023
### This is to remove warning messages from check()
### Quote from R Packages manual:
#'The “no visible binding” note is a peculiarity of using dplyr and unquoted variable names inside a package,
#'where the use of bare variable names (english and temp) looks suspicious.
#'
### See also https://r-pkgs.org/package-within.html

### for getOpenDate
TradeNr <- Statut <- TradeDate <- Strategy <- exp_date <- Exp.Date <- Pos <- NULL

### for getTradeNr
Account <- Reward <- Risk <- Instrument <- NULL

### for readAccount
account <- NULL

## for readPortfolio
secType <- lastTradeDateOrContractMonth <- undPrice <- impliedVol <- position <- NULL
marketPrice <- marketValue <- averageCost <- unrealizedPnL <- symbol <- right <- NULL

### for stock_price
exchange <- sec <- NULL

### for greeksNet
pos <- uPrice <- currency <- multiplier <- delta <- theta <- vega <- dnet <- ddnet <- gnet <- tnet <- vnet <- NULL

### for currency
### mydb <- NULL

### for getIBKR
new_date <- heure <- strike <- optPrice <- pvDividend <- expdate <- type <- unrealizedPNL <- NULL
Currency <- conId <- marginable <- NULL

### for getGonet
sym_ibkr <- sym_yahoo <- orig_date <- price <- mktPrice <- orig_adjusted_price <- NULL
unPnL <- cost <- avgCost <- orig_adjusted_price <- mktValue <- init_cost <- NULL

## add for getAccountGonet
mktValue <- unPnL <- TotalCashBalance <- StockMarketValue <- NetLiquidation <- heure.1 <- NULL

### add for getInstrument
initial_trade_date <- Ssjacent <- u_price <- interest_rate <- DTE <- startPrice <- startIV <- NULL

### add for getClosedTrades
strategy <- last_date <- NULL

### add for c_to_usd
am <- cur <- NULL

### getLastUsdValue
usd_value <- DirectConversion <- value <- NULL

### getIBKR
Exchange <- NULL


