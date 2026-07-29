#' Global variables
#'
#' These variables are defined to avoid R CMD check "no visible binding" notes
#' when using dplyr's non-standard evaluation.
#'
#' These global definitions remove warning messages from check().
#' See also https://r-pkgs.org/package-within.html
#' @keywords internal
#' @usage NULL

#### for documentation purposes
Global <- NULL

### for .init_python_environment
path <- NULL

### for geTradeDates
TradeNr <- Status <- TradeDate <- Strategy <- exp_date <- Exp.Date <- Pos <- NULL

### for getTradeNr
Account <- Reward <- Risk <- Instrument <- NULL

### for readAccount
account <- NULL

## for readPortfolio
secType <- lastTradeDateOrContractMonth <- undPrice <- impliedVol <- position <- NULL
marketPrice <- marketValue <- averageCost <- unrealizedPnL <- symbol <- right <- NULL

### for getStockPrice
datetime.x <- datetime.y <- dt_x <- dt_y <- price.y <- price.x <- datetime <- NULL

### for greeksNet
pos <- uPrice <- currency <- multiplier <- delta <- theta <- vega <- dnet <- ddnet <- gnet <- tnet <- vnet <- NULL

### for currency
### mydb <- NULL

### for getIBKR
new_date <- heure <- strike <- optPrice <- pvDividend <- expdate <- type <- unrealizedPNL <- NULL
Currency <- conId <- marginable <- NULL

### for getGonet
tdata_py <- sym_ibkr <- sym_yahoo <- orig_date <- price <- mktPrice <- orig_adjusted_price <- NULL
unPnL <- cost <- avgCost <- orig_adjusted_price <- mktValue <- init_cost <- NULL
basis <- realized <- realizedPnL <- NULL

## add for getAccountGonet
mktValue <- unPnL <- TotalCashBalance <- StockMarketValue <- NetLiquidation <- heure.1 <- NULL

### add for getInstrument
initial_trade_date <- Symbol <- u_price <- interest_rate <- DTE <- startPrice <- startIV <- NULL

### getInterestRate
last_ir_update <- ir1week <- ir1month <- ir3months <- ir6months <- ir1year <- ir2years <- NULL

### add for getClosedTrades
strategy <- last_date <- NULL

### add for c_to_usd
am <- cur <- NULL

### getLastUsdValue
usd_value <- DirectConversion <- value <- NULL

### getIBKR
Exchange <- NULL

### getGonet
ticker <- NULL

### getAllOptionPrices
bid <- ask <- put_iv <- put_bid <- put_ask <- call_bid <- call_ask <- call_iv <- call_value <- put_value <- has_put <- has_call <- call_mid <- put_mid <- NULL

### getOptionPrices
impliedvol <- NULL

### getIBKRMetrics
IV <- Name <- current_iv <- current_price <- iv_percentile <- current_hv <- hv_percentile <- NULL

### currency
Adjusted <- date <- ticker <- currency <- usd_value <- accountnr <- chf_value <- NULL

