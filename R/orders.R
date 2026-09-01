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
    algoStrategy = character(),
    lmtPrice = numeric(), auxPrice = numeric(), trailStopPrice = numeric(),
    price = numeric(), instrument = character(),
    tif = character(), status = character(), whyHeld = character(),
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
#' Pairs the positions actually held with the resting orders that would act on
#' them, and - the point of the exercise - keeps the positions that have no order
#' at all.
#'
#' Open trades are taken from the **portfolio snapshot**, not from
#' \code{Trades.Status}. A trade left at "Ouvert" whose legs expired long ago has
#' no position to protect, and reporting it as uncovered is a false alarm; the
#' portfolio is also what the TWS order window is compared against. The
#' portfolio's own \code{TradeNr} is the authoritative attribution - the Trades
#' table can carry a leg recorded under the wrong trade.
#'
#' Cash/FX balances are excluded: they are not positions an order acts on.
#'
#' Matching is on \code{(Account, Symbol)}, the one key valid for every instrument
#' type (a combo order's BAG carries the root while its legs carry the
#' contracts). When an order's contract exactly matches the legs of one trade,
#' that trade wins outright; only a root-level match against several trades is
#' reported as \code{AMBIGUOUS}.
#'
#'@param positions data frame of held positions - one portfolio snapshot, as
#' \code{getLatestPositions} returns.
#'@param orders data frame of open orders, as \code{getOpenOrders} returns.
#'@param trades optional data frame of active trades, used only to label
#' \code{Strategy} and to resolve a position whose \code{TradeNr} is not set.
#'@param account string, the account these positions belong to.
#'@returns a data frame with one row per (trade, order) pair, plus one row per
#' held position with no order and one row per order matching no position.
#' Columns: \code{Flag, TradeNr, Account, Symbol, Strategy, Legs, Pos, ExpDate,
#' Contract, Match, Action, Qty, OrderType, Price, TIF, State, OCA,
#' PermId, Currency}. \code{Flag} is one of \code{""} (covered),
#' \code{"NO ORDER"}, \code{"AMBIGUOUS"}, \code{"UNMATCHED"}.
#'@examples
#'\dontrun{
#'buildTradeOrderCoverage(getLatestPositions("U1804173"), getOpenOrders("U1804173"))
#'}
#'@export
buildTradeOrderCoverage <- function(positions, orders, trades = NULL,
                                    account = NA_character_) {
  if (is.null(orders)) orders <- emptyOpenOrders()

  units <- summariseHeldUnits(positions, trades, account)
  orders <- prepareCoverageOrders(orders)
  if (nrow(units) != 0) units$.uid <- seq_len(nrow(units))

  ### Joined on the product root, then narrowed to the same account. Account is
  ### applied as a filter rather than folded into the key so an unknown (NA)
  ### account still matches instead of silently pairing with nothing.
  units$.key  <- units$Symbol
  orders$.key <- orders$symbol
  paired <- dplyr::inner_join(orders, units, by = ".key", relationship = "many-to-many")
  paired <- paired[is.na(paired$Account) | is.na(paired$account) |
                     paired$Account == paired$account, , drop = FALSE]
  paired <- resolveOrderMatches(paired)

  ### Orders acting on nothing we hold: a stale GTC on a closed position, or a
  ### position the snapshot predates. Cheap to surface and always worth knowing.
  loose <- orders[!orders$.oid %in% paired$.oid, , drop = FALSE]
  ### The reason this table exists: held positions with no resting order.
  naked <- units[!units$.uid %in% paired$.uid, , drop = FALSE]

  coverage <- dplyr::bind_rows(
    coverageRows(paired, kind = "paired"),
    coverageRows(naked,  kind = "naked"),
    coverageRows(loose,  kind = "loose"))

  sortCoverage(coverage)
}


### Decides which trade each order belongs to, and how tight the match is.
### An exact contract match beats a root match: two trades on the same
### underlying stop being "ambiguous" as soon as the order names a leg only one
### of them holds.
resolveOrderMatches <- function(paired) {
  if (nrow(paired) == 0) return(paired)

  ### A stock's contract IS its root, so a root match is already exact. Its
  ### Trades.Instrument holds the company name ("CARREFOUR SA"), which no order
  ### field reproduces - comparing instruments there would always say "root".
  exact <- mapply(function(instr, held) isTRUE(instr %in% held),
                  paired$instrument, paired$Instruments, USE.NAMES = FALSE) |
    paired$secType == "STK"

  ### Drop the root-level candidates of any order that also has an exact one.
  has_exact <- unique(paired$.oid[exact])
  keep <- exact | !paired$.oid %in% has_exact
  paired <- paired[keep, , drop = FALSE]
  exact <- exact[keep]

  matches_per_order <- table(paired$.oid)
  paired$Flag <- ifelse(matches_per_order[as.character(paired$.oid)] > 1,
                        "AMBIGUOUS", "")
  paired$Match <- ifelse(exact, "leg", "root")
  paired
}


#' sortCoverage
#'
#' Puts a coverage table in reading order: uncovered positions first, since they
#' are what the table exists to surface, then the rows needing a decision, then
#' the covered ones. Exported because a caller merging several accounts' coverage
#' has to re-apply it - concatenated blocks would otherwise bury one account's
#' uncovered positions in the middle of another's covered ones.
#'@param coverage a coverage data frame from \code{buildTradeOrderCoverage}
#'@returns the same rows, reordered
#'@examples
#'\dontrun{
#'sortCoverage(dplyr::bind_rows(cov_u1, cov_u2))
#'}
#'@export
sortCoverage <- function(coverage) {
  if (is.null(coverage) || nrow(coverage) == 0) return(coverage)
  rank <- match(coverage$Flag, c("NO ORDER", "AMBIGUOUS", "", "UNMATCHED"))
  coverage[order(rank, coverage$Symbol, coverage$TradeNr, coverage$Contract), ]
}


### Collapses a portfolio snapshot into one row per (trade, underlying).
### Not exported - internal to buildTradeOrderCoverage, exposed for testing.
summariseHeldUnits <- function(positions, trades = NULL, account = NA_character_) {
  empty <- data.frame(TradeNr = numeric(), Account = character(), Symbol = character(),
                      Strategy = character(), Legs = integer(), Pos = numeric(),
                      ExpDate = as.Date(character()), Currency = character(),
                      stringsAsFactors = FALSE)
  empty$Instruments <- list()
  if (is.null(positions) || nrow(positions) == 0) return(empty)

  ### Cash/FX balances are not positions an order acts on.
  held <- positions[!is_cash_position(positions) & !is.na(positions$pos) &
                      positions$pos != 0, , drop = FALSE]
  if (nrow(held) == 0) return(empty)

  held$TradeNr <- resolveHeldTradeNr(held, trades)
  held$Account <- account
  ### readPortfolio converts date/heure but leaves expdate as a YYYYMMDD integer.
  held$.exp <- as.Date(as.character(held$expdate), format = "%Y%m%d")

  units <- dplyr::summarise(
    dplyr::group_by(held, TradeNr, Account, Symbol = symbol),
    Legs = dplyr::n(),
    ### A net position only means something for a single-leg trade; summing a
    ### vertical's legs would report a flat, harmless zero.
    Pos = ifelse(dplyr::n() == 1, pos[1], NA_real_),
    ### A trade spanning expirations reports the nearest one - the leg that
    ### forces a decision first.
    ExpDate = minDate(.exp),
    Currency = dplyr::first(currency),
    Instruments = list(Instrument),
    .groups = "drop")

  units <- as.data.frame(units)
  units$Strategy <- tradeStrategy(units$TradeNr, trades)
  units
}


### The portfolio leaves TradeNr unset when a leg was entered after the snapshot
### was taken. Recover it from the Trades table when exactly one open trade holds
### that contract; anything less certain stays NA rather than being guessed.
resolveHeldTradeNr <- function(held, trades) {
  tradenr <- held$TradeNr
  if (is.null(trades) || nrow(trades) == 0 || !any(is.na(tradenr))) return(tradenr)

  for (i in which(is.na(tradenr))) {
    candidates <- unique(trades$TradeNr[!is.na(trades$Instrument) &
                                          trades$Instrument == held$Instrument[i]])
    if (length(candidates) == 1) tradenr[i] <- candidates
  }
  tradenr
}


### Strategy label for each trade, from the Trades table.
tradeStrategy <- function(tradenr, trades) {
  if (is.null(trades) || nrow(trades) == 0) return(rep(NA_character_, length(tradenr)))
  lookup <- trades[!duplicated(trades$TradeNr), c("TradeNr", "Strategy")]
  lookup$Strategy[match(tradenr, lookup$TradeNr)]
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
    TIF = character(), State = character(), OCA = character(),
    PermId = numeric(), Currency = character(), stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(blank)

  na_chr <- rep(NA_character_, nrow(df))
  na_num <- rep(NA_real_, nrow(df))

  if (kind == "loose") {
    return(data.frame(
      Flag = "UNMATCHED", TradeNr = na_num, Account = df$account,
      Symbol = df$symbol, Strategy = na_chr, Legs = NA_integer_, Pos = na_num,
      ExpDate = as.Date(NA), Contract = df$instrument, Match = na_chr,
      Action = coverageAction(df), Qty = df$quantity, OrderType = coverageType(df),
      Price = df$price, TIF = df$tif, State = coverageState(df), OCA = df$ocaGroup,
      PermId = df$permId, Currency = df$currency, stringsAsFactors = FALSE))
  }

  if (kind == "naked") {
    return(data.frame(
      Flag = "NO ORDER", TradeNr = df$TradeNr,
      Account = df$Account, Symbol = df$Symbol, Strategy = df$Strategy,
      Legs = df$Legs, Pos = df$Pos, ExpDate = df$ExpDate,
      Contract = na_chr, Match = na_chr, Action = na_chr, Qty = na_num,
      OrderType = na_chr, Price = na_num, TIF = na_chr, State = na_chr,
      OCA = na_chr, PermId = na_num, Currency = df$Currency,
      stringsAsFactors = FALSE))
  }

  data.frame(
    Flag = df$Flag, TradeNr = df$TradeNr, Account = df$Account, Symbol = df$Symbol,
    Strategy = df$Strategy, Legs = df$Legs, Pos = df$Pos, ExpDate = df$ExpDate,
    Contract = df$instrument, Match = df$Match, Action = coverageAction(df),
    Qty = df$quantity, OrderType = coverageType(df), Price = df$price, TIF = df$tif,
    State = coverageState(df), OCA = df$ocaGroup, PermId = df$permId,
    Currency = df$currency, stringsAsFactors = FALSE)
}


### A combo leg's own side is what actually hits the position, so it wins over
### the combo-level action when present.
coverageAction <- function(df) {
  dplyr::coalesce(df$legAction, df$action)
}


### "PreSubmitted" is the normal resting state of a stop: a stop is a simulated
### order type that IBKR holds and only routes to the exchange when its trigger
### is touched. Reporting the raw status makes a perfectly healthy stop look
### half-finished, so say what the state means instead.
coverageState <- function(df) {
  held <- ifelse(is.na(df$whyHeld), "Held", paste0("Held: ", df$whyHeld))
  dplyr::case_when(
    is.na(df$status) ~ NA_character_,
    df$status == "Submitted" ~ "Working",
    df$status %in% c("PreSubmitted", "PendingSubmit") ~ held,
    .default = df$status)
}


### TWS labels an algo order "Adaptive LMT (IBKR)" while orderType stays "LMT",
### so the algo has to be appended or the table disagrees with the order window.
coverageType <- function(df) {
  ifelse(is.na(df$algoStrategy), df$orderType,
         paste0(df$orderType, " (", df$algoStrategy, ")"))
}


### Not to be exported - isolates the DB read so coverage can be tested without it.
getPositionsQuery <- function(account) {
  readPortfolio(account)
}


#' getLatestPositions
#'
#' The most recent portfolio snapshot for an account - the positions actually
#' held, which is what a resting order acts on.
#'
#' A snapshot is a (date, heure) pair, so the latest time of the latest date is
#' taken; earlier times of the same day are prior snapshots, not extra positions.
#'@param account string, IBKR account code
#'@returns a data frame of portfolio rows, empty when the account has no snapshot
#'@examples
#'\dontrun{
#'getLatestPositions("U1804173")
#'}
#'@export
getLatestPositions <- function(account) {
  portf <- getPositionsQuery(account)
  if (is.null(portf) || nrow(portf) == 0) return(portf)

  portf <- portf[portf$date == max(portf$date, na.rm = TRUE), , drop = FALSE]
  portf[portf$heure == max(portf$heure, na.rm = TRUE), , drop = FALSE]
}


#' getTradeOrderCoverage
#'
#' Convenience wrapper: fetches the account's held positions, its active trades
#' and its live resting orders, and pairs them with
#' \code{buildTradeOrderCoverage}.
#'
#' The result carries the \code{tws_available} attribute from
#' \code{getOpenOrders}: when FALSE every position is reported as "NO ORDER"
#' purely because TWS could not be asked, and the caller must say so rather than
#' let it read as an absence of orders.
#'@param account string, IBKR account code
#'@returns a coverage data frame, see \code{buildTradeOrderCoverage}
#'@examples
#'\dontrun{
#'getTradeOrderCoverage("U1804173")
#'}
#'@export
getTradeOrderCoverage <- function(account) {
  orders <- getOpenOrders(account)
  coverage <- buildTradeOrderCoverage(
    getLatestPositions(account), orders, getActiveTrades(account), account)
  attr(coverage, "tws_available") <- attr(orders, "tws_available")
  coverage
}
