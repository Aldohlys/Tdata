### order_entry.R - quote, preview and stage stock orders in TWS
###
### R side of tdata_py/order_entry.py. Orders are STAGED (transmit = FALSE):
### they appear in TWS, under the TWS client itself, and the user transmits
### them there. Nothing here sends an order to the market.

### Not to be exported - isolates the Python call so the wrappers can be tested
### without TWS. Returns NULL when the Python module is missing or fails.
orderEntryQuery <- function(fn, args) {
  if (is.null(tdata_py)) {
    logger::log_warn("tdata_py Python module not available - cannot call {fn}", namespace = "Tdata")
    return(NULL)
  }
  tryCatch(do.call(tdata_py[[fn]], args),
           error = function(e) {
             logger::log_error("{fn} failed: {e$message}", namespace = "Tdata")
             NULL
           })
}

### NA / empty arguments go to Python as None, never as NaN
py_arg <- function(x) if (length(x) != 1 || is.na(x)) NULL else x

order_args <- function(account, sym, action, quantity, order_type, limit, take_profit,
                       stop_loss, currency, exchange) {
  stopifnot(is.character(account), length(account) == 1, is.character(sym), length(sym) == 1)
  list(account = account, sym = sym, action = toupper(action),
       quantity = py_arg(quantity), order_type = order_type,
       limit = py_arg(limit), take_profit = py_arg(take_profit), stop_loss = py_arg(stop_loss),
       currency = py_arg(currency), exchange = py_arg(exchange))
}


#' quoteStock
#'
#' Bid, ask, last and close of a stock from TWS (live, IBKR falls back to
#' frozen outside trading hours).
#'
#'@param sym IBKR symbol
#'@param currency currency, NULL takes it from the Tickers table (else USD)
#'@param exchange exchange, NULL means SMART
#'@returns NULL when TWS or the contract is unavailable; otherwise a list with
#' bid, ask, last, close, mid (NA when missing), mktdata_type (1 live, 2 frozen,
#' 3 delayed, 4 delayed-frozen), conId, currency
#'@export
quoteStock <- function(sym, currency = NULL, exchange = NULL) {
  q <- orderEntryQuery("quote_stock", list(sym = sym, currency = py_arg(currency),
                                           exchange = py_arg(exchange)))
  if (is.null(q)) return(NULL)
  na <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  list(bid = na(q$bid), ask = na(q$ask), last = na(q$last), close = na(q$close),
       mid = na(q$mid), mktdata_type = as.integer(q$mktdata_type),
       conId = as.integer(q$conId), currency = q$currency)
}


#' previewStockOrder
#'
#' What TWS would do with a stock order (IBKR whatIfOrder): margin change,
#' commission, warning. Nothing is placed. For a bracket the entry order is
#' previewed.
#'
#'@param account IBKR account code (must be managed by the TWS session)
#'@param sym IBKR symbol
#'@param action "BUY" or "SELL"
#'@param quantity whole number of shares
#'@param order_type "market", "limit" or "bracket"
#'@param limit limit price (limit and bracket)
#'@param take_profit take-profit price (bracket)
#'@param stop_loss stop-loss price (bracket, STP)
#'@param currency,exchange as in \code{quoteStock}
#'@returns list(ok, message, init_margin_change, maint_margin_change,
#' equity_with_loan_after, commission, commission_currency, warning)
#'@export
previewStockOrder <- function(account, sym, action, quantity, order_type, limit = NA,
                              take_profit = NA, stop_loss = NA, currency = NULL, exchange = NULL) {
  # Built before the call: its contract checks must not be caught as a TWS failure
  args <- order_args(account, sym, action, quantity, order_type, limit,
                     take_profit, stop_loss, currency, exchange)
  r <- orderEntryQuery("preview_stock_order", args)
  if (is.null(r)) return(list(ok = FALSE, message = "Preview not available (TWS or Python module)."))
  r
}


#' stageStockOrder
#'
#' Places a stock order in TWS with transmit = FALSE: it is staged there,
#' not sent to the market, under the TWS client (client id 0) so it can be
#' transmitted, modified or cancelled from TWS. All orders are GTC. Market and
#' limit orders use the Adaptive algo; a bracket is a limit entry with a
#' take-profit limit and a stop-loss STP.
#'
#'@inheritParams previewStockOrder
#'@returns list(ok, message, order_ids, statuses)
#'@export
stageStockOrder <- function(account, sym, action, quantity, order_type, limit = NA,
                            take_profit = NA, stop_loss = NA, currency = NULL, exchange = NULL) {
  # Built before the call: its contract checks must not be caught as a TWS failure
  args <- order_args(account, sym, action, quantity, order_type, limit,
                     take_profit, stop_loss, currency, exchange)
  r <- orderEntryQuery("stage_stock_order", args)
  if (is.null(r)) return(list(ok = FALSE, message = "Staging not available (TWS or Python module).",
                              order_ids = integer(), statuses = character()))
  r$order_ids <- as.integer(unlist(r$order_ids))
  r$statuses <- as.character(unlist(r$statuses))
  r
}
