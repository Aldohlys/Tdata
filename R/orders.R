### Live resting orders from TWS, and their coverage of open trades.
###
### Orders have no DB persistence: they are a "right now" view, so everything
### here is fetched live and nothing is stored.

#' emptyOpenOrders
#'
#' Zero-row data frame with the exact shape \code{getOpenOrders} returns, used
#' whenever TWS cannot be asked so that callers never have to branch on NULL.
#'@returns an empty data frame with the open-order columns
#'@keywords internal
emptyOpenOrders <- function() {
  data.frame(
    account = character(), permId = numeric(), orderId = numeric(),
    parentId = numeric(), ocaGroup = character(),
    isCombo = logical(), legCount = numeric(), legIndex = numeric(),
    legRatio = numeric(), legAction = character(),
    secType = character(), symbol = character(), localSymbol = character(),
    tradingClass = character(), expiry = character(), expiryDate = as.Date(character()),
    strike = numeric(), right = character(), multiplier = numeric(),
    currency = character(), exchange = character(),
    action = character(), quantity = numeric(), orderType = character(),
    lmtPrice = numeric(), auxPrice = numeric(), trailStopPrice = numeric(),
    price = numeric(), instrument = character(),
    tif = character(), status = character(),
    filled = numeric(), remaining = numeric(),
    stringsAsFactors = FALSE)
}


### Not to be exported - for test purposes.
### Isolates the live TWS call so coverage logic can be tested without TWS.
getOpenOrderQuery <- function(account) {
  if (is.null(tdata_py)) {
    logger::log_warn("tdata_py Python module not available - cannot retrieve open orders",
                     namespace = "Tdata")
    return(NULL)
  }
  tryCatch(tdata_py$get_open_orders(account),
           error = function(e) {
             logger::log_error("get_open_orders failed: {e$message}", namespace = "Tdata")
             NULL
           })
}


#' getOpenOrders
#'
#' Retrieves every resting (open) order from TWS, across all client ids.
#'
#' A multi-leg combo (BAG) order is expanded to one row per leg, each leg's own
#' contract resolved by \code{conId}, so that a spread-closing order can be read
#' leg by leg. \code{legAction} gives the leg's effective side once the combo's
#' own BUY/SELL is applied; \code{action} stays the combo-level side.
#'
#' The result carries a \code{tws_available} attribute: FALSE means TWS could not
#' be reached, and the zero rows say nothing about whether orders exist. Callers
#' displaying coverage must distinguish the two - "no resting orders" and "could
#' not ask" look identical otherwise.
#'
#' Note \code{orderId} is 0 for orders placed from TWS itself or another API
#' client; \code{permId} is the only stable order identifier.
#'@param account string, IBKR account code. NULL (default) returns every account
#' visible to the TWS session.
#'@returns a data frame of open orders (see \code{emptyOpenOrders} for the
#' columns), with \code{price} holding the order's operative price (stop trigger
#' for STP/TRAIL, limit for LMT) and \code{instrument} the IBKR-format contract
#' name matching \code{Trades.Instrument} for options.
#'@examples
#'\dontrun{
#'getOpenOrders()
#'getOpenOrders("U1804173")
#'}
#'@export
getOpenOrders <- function(account = NULL) {
  orders <- getOpenOrderQuery(account)

  if (is.null(orders)) {
    empty <- emptyOpenOrders()
    attr(empty, "tws_available") <- FALSE
    return(empty)
  }

  if (nrow(orders) == 0) {
    empty <- emptyOpenOrders()
    attr(empty, "tws_available") <- TRUE
    return(empty)
  }

  ### Python hands back NaN for every unset numeric - normalise to NA
  for (col in c("strike", "multiplier", "lmtPrice", "auxPrice", "trailStopPrice",
                "quantity", "filled", "remaining", "legRatio", "legIndex", "legCount")) {
    orders[[col]][is.nan(orders[[col]])] <- NA_real_
  }

  orders$expiryDate <- as.Date(orders$expiry, format = "%Y%m%d")

  ### Operative price: what the order will actually trigger or fill at.
  ### STP is tested before LMT so a "STP LMT" reports its stop trigger.
  orders$price <- dplyr::case_when(
    grepl("TRAIL", orders$orderType) ~ dplyr::coalesce(orders$trailStopPrice, orders$auxPrice),
    grepl("STP", orders$orderType)   ~ orders$auxPrice,
    grepl("LMT", orders$orderType)   ~ orders$lmtPrice,
    .default = NA_real_)

  ### Contract name in IBKR format. Options rebuild the Trades.Instrument string;
  ### stocks use the root (Trades.Instrument holds the company name for those, so
  ### Symbol is the only usable key - see the type-gated join in account.R).
  orders$instrument <- ifelse(orders$secType == "STK", orders$symbol, orders$localSymbol)
  is_option <- orders$secType %in% c("OPT", "FOP") &
    !is.na(orders$expiryDate) & !is.na(orders$strike) & !is.na(orders$right)
  if (any(is_option)) {
    orders$instrument[is_option] <- Tbasics::buildInstrumentName(
      orders$symbol[is_option], orders$expiryDate[is_option],
      orders$strike[is_option], orders$right[is_option])
  }

  orders <- orders[, names(emptyOpenOrders())]
  attr(orders, "tws_available") <- TRUE
  orders
}


#' buildTradeOrderCoverage
#'
#' Pairs open trades with the resting orders that would act on them, and - the
#' point of the exercise - keeps the open trades that have no order at all.
#'
#' Matching is on \code{(Account, Symbol)}, the product root: this is the one key
#' that holds for every instrument type, including a combo order whose BAG
#' carries the root while its legs carry the individual contracts. The
#' \code{Match} column then says how tight the match is - \code{"leg"} when the
#' order's contract is one the trade actually holds, \code{"root"} when only the
#' underlying agrees (an order on a strike the trade does not hold).
#'
#' A trade whose legs net to zero is treated as closed and dropped, so a rolled
#' or fully-offset leg does not show up asking for protection.
#'
#'@param trades data frame of active trades, as \code{getActiveTrades} returns
#' (one row per event, several rows per leg).
#'@param orders data frame of open orders, as \code{getOpenOrders} returns.
#'@returns a data frame with one row per (trade, order) pair, plus one row per
#' open trade with no order and one row per order matching no open trade.
#' Columns: \code{Flag, TradeNr, Account, Symbol, Strategy, Legs, Pos, ExpDate,
#' Contract, Match, Action, Qty, OrderType, Price, TIF, OrderStatus, OCA,
#' PermId, Currency}. \code{Flag} is one of \code{""} (covered),
#' \code{"NO ORDER"}, \code{"CASH"}, \code{"AMBIGUOUS"}, \code{"UNMATCHED"}.
#'@examples
#'\dontrun{
#'buildTradeOrderCoverage(getActiveTrades("U1804173"), getOpenOrders("U1804173"))
#'}
#'@export
buildTradeOrderCoverage <- function(trades, orders) {
  if (is.null(trades)) trades <- data.frame()
  if (is.null(orders)) orders <- emptyOpenOrders()

  units <- summariseOpenTradeUnits(trades)
  orders <- prepareCoverageOrders(orders)

  units$.key  <- paste(units$Account, units$Symbol)
  orders$.key <- paste(orders$account, orders$symbol)

  ### Cross product of every order with every open trade sharing its root.
  ### An order matching two trades on the same root is genuinely ambiguous and
  ### is reported against both rather than assigned by guesswork.
  paired <- dplyr::inner_join(orders, units, by = ".key", relationship = "many-to-many")

  if (nrow(paired) != 0) {
    matches_per_order <- table(paired$.oid)
    paired$Flag <- ifelse(matches_per_order[as.character(paired$.oid)] > 1, "AMBIGUOUS", "")
    ### A stock's contract IS its root, so a root match is already exact. Its
    ### Trades.Instrument holds the company name ("CARREFOUR SA"), which no order
    ### field reproduces - comparing instruments there would always say "root".
    held_leg <- mapply(function(instr, held) isTRUE(instr %in% held),
                       paired$instrument, paired$Instruments, USE.NAMES = FALSE)
    paired$Match <- ifelse(held_leg | paired$secType == "STK", "leg", "root")
  }

  ### Orders acting on nothing we hold open: a stale GTC on a closed position, or
  ### a trade not yet recorded. Cheap to surface and always worth knowing.
  loose <- orders[!orders$.key %in% units$.key, , drop = FALSE]
  ### The reason this table exists: open trades with no resting order.
  ### Cash/FX trades are currency balances, not positions to protect.
  naked <- units[!units$.key %in% orders$.key, , drop = FALSE]

  coverage <- dplyr::bind_rows(
    coverageRows(paired, kind = "paired"),
    coverageRows(naked,  kind = "naked"),
    coverageRows(loose,  kind = "loose"))

  sortCoverage(coverage)
}


#' sortCoverage
#'
#' Puts a coverage table in reading order: uncovered trades first, since they are
#' what the table exists to surface, then the rows needing a decision, then the
#' covered ones. Exported because a caller merging several accounts' coverage has
#' to re-apply it - concatenated blocks would otherwise bury one account's
#' uncovered trades in the middle of another's covered ones.
#'@param coverage a coverage data frame from \code{buildTradeOrderCoverage}
#'@returns the same rows, reordered
#'@examples
#'\dontrun{
#'sortCoverage(dplyr::bind_rows(cov_u1, cov_u2))
#'}
#'@export
sortCoverage <- function(coverage) {
  if (is.null(coverage) || nrow(coverage) == 0) return(coverage)
  rank <- match(coverage$Flag, c("NO ORDER", "AMBIGUOUS", "", "CASH", "UNMATCHED"))
  coverage[order(rank, coverage$Symbol, coverage$TradeNr, coverage$Contract), ]
}


### Collapses the event-level Trades rows into one row per open trade root.
### Not exported - internal to buildTradeOrderCoverage, exposed for testing.
summariseOpenTradeUnits <- function(trades) {
  empty <- data.frame(TradeNr = numeric(), Account = character(), Symbol = character(),
                      Strategy = character(), Legs = integer(), Pos = numeric(),
                      ExpDate = as.Date(character()), Currency = character(),
                      Cash = logical(), stringsAsFactors = FALSE)
  empty$Instruments <- list()
  if (is.null(trades) || nrow(trades) == 0) return(empty)

  trades$.cash <- is_cash_trade(trades)
  ### Exp.Date is stored dd.mm.yyyy; a trade spanning expirations reports the
  ### nearest one, which is the leg that forces a decision first.
  trades$.exp <- as.Date(trades$`Exp.Date`, format = "%d.%m.%Y")

  legs <- dplyr::summarise(
    dplyr::group_by(trades, TradeNr, Account, Symbol, Instrument),
    Pos = sum(Pos, na.rm = TRUE),
    Strategy = dplyr::first(Strategy),
    ExpDate = minDate(.exp),
    Currency = dplyr::first(Currency),
    Cash = any(.cash),
    .groups = "drop")
  ### A leg netting to zero is closed, whatever the trade's Status says.
  legs <- dplyr::filter(legs, Pos != 0)
  if (nrow(legs) == 0) return(empty)

  units <- dplyr::summarise(
    dplyr::group_by(legs, TradeNr, Account, Symbol),
    Legs = dplyr::n(),
    ### A net position only means something for a single-leg trade; summing a
    ### vertical's legs would report a flat, harmless-looking zero.
    Pos = ifelse(dplyr::n() == 1, Pos[1], NA_real_),
    Strategy = dplyr::first(Strategy),
    ExpDate = minDate(ExpDate),
    Currency = dplyr::first(Currency),
    Cash = any(Cash),
    Instruments = list(Instrument),
    .groups = "drop")
  as.data.frame(units)
}


### min() over dates returns Inf (and drops the Date class) when every value is
### NA, which then poisons the column - return a proper NA date instead.
minDate <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) as.Date(NA) else min(x)
}


### Adds the per-row id that ambiguity counting needs.
prepareCoverageOrders <- function(orders) {
  if (nrow(orders) == 0) {
    orders$.oid <- integer()
    return(orders)
  }
  orders$.oid <- seq_len(nrow(orders))
  orders
}


### Renders one of the three row kinds into the shared coverage shape.
coverageRows <- function(df, kind) {
  blank <- data.frame(
    Flag = character(), TradeNr = numeric(), Account = character(),
    Symbol = character(), Strategy = character(), Legs = integer(), Pos = numeric(),
    ExpDate = as.Date(character()), Contract = character(), Match = character(),
    Action = character(), Qty = numeric(), OrderType = character(), Price = numeric(),
    TIF = character(), OrderStatus = character(), OCA = character(),
    PermId = numeric(), Currency = character(), stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(blank)

  na_chr <- rep(NA_character_, nrow(df))
  na_num <- rep(NA_real_, nrow(df))

  if (kind == "loose") {
    return(data.frame(
      Flag = "UNMATCHED", TradeNr = na_num, Account = df$account,
      Symbol = df$symbol, Strategy = na_chr, Legs = NA_integer_, Pos = na_num,
      ExpDate = as.Date(NA), Contract = df$instrument, Match = na_chr,
      Action = coverageAction(df), Qty = df$quantity, OrderType = df$orderType,
      Price = df$price, TIF = df$tif, OrderStatus = df$status, OCA = df$ocaGroup,
      PermId = df$permId, Currency = df$currency, stringsAsFactors = FALSE))
  }

  if (kind == "naked") {
    return(data.frame(
      Flag = ifelse(df$Cash, "CASH", "NO ORDER"), TradeNr = df$TradeNr,
      Account = df$Account, Symbol = df$Symbol, Strategy = df$Strategy,
      Legs = df$Legs, Pos = df$Pos, ExpDate = df$ExpDate,
      Contract = na_chr, Match = na_chr, Action = na_chr, Qty = na_num,
      OrderType = na_chr, Price = na_num, TIF = na_chr, OrderStatus = na_chr,
      OCA = na_chr, PermId = na_num, Currency = df$Currency,
      stringsAsFactors = FALSE))
  }

  data.frame(
    Flag = df$Flag, TradeNr = df$TradeNr, Account = df$Account, Symbol = df$Symbol,
    Strategy = df$Strategy, Legs = df$Legs, Pos = df$Pos, ExpDate = df$ExpDate,
    Contract = df$instrument, Match = df$Match, Action = coverageAction(df),
    Qty = df$quantity, OrderType = df$orderType, Price = df$price, TIF = df$tif,
    OrderStatus = df$status, OCA = df$ocaGroup, PermId = df$permId,
    Currency = df$currency, stringsAsFactors = FALSE)
}


### A combo leg's own side is what actually hits the position, so it wins over
### the combo-level action when present.
coverageAction <- function(df) {
  dplyr::coalesce(df$legAction, df$action)
}


#' getTradeOrderCoverage
#'
#' Convenience wrapper: fetches the account's active trades and its live resting
#' orders, and pairs them with \code{buildTradeOrderCoverage}.
#'
#' The result carries the \code{tws_available} attribute from
#' \code{getOpenOrders}: when FALSE every trade is reported as "NO ORDER" purely
#' because TWS could not be asked, and the caller must say so rather than let it
#' read as an absence of orders.
#'@param account string, IBKR account code
#'@returns a coverage data frame, see \code{buildTradeOrderCoverage}
#'@examples
#'\dontrun{
#'getTradeOrderCoverage("U1804173")
#'}
#'@export
getTradeOrderCoverage <- function(account) {
  orders <- getOpenOrders(account)
  coverage <- buildTradeOrderCoverage(getActiveTrades(account), orders)
  attr(coverage, "tws_available") <- attr(orders, "tws_available")
  coverage
}
