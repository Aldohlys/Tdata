#' CASH Position Management Functions
#'
#' Functions for managing currency (CASH) positions as tradeable assets
#'
#' @name cash
NULL

#' Identify CASH trades in trades table
#'
#' CASH (currency/FX) trades are identified by their Instrument being a bare
#' currency code present in the Currencies table. Their Symbol holds the foreign
#' (non-base) currency of the pair, not the literal "CASH".
#'
#' @param trades Data frame with trade records from Trades table
#' @return Logical vector indicating CASH trades
#' @export
#'
#' @examples
#' \dontrun{
#' trades <- data.frame(
#'   Instrument = c("SPY", "CHF", "USD"),
#'   Symbol = c("SPY", "JPY", "USD")
#' )
#' is_cash_trade(trades)
#' }
is_cash_trade <- function(trades) {
  currencies <- getActiveCurrencies()
  !is.na(trades$Instrument) & trades$Instrument %in% currencies
}

#' Identify CASH positions in portfolio table
#'
#' @param portfolio Data frame with portfolio positions from account tables
#' @return Logical vector indicating CASH positions
#' @export
#'
#' @examples
#' portfolio <- data.frame(
#'   symbol = c("SPY", "CHF", "USD"),
#'   type = c("Stock", "CASH", "CASH")
#' )
#' is_cash_position(portfolio)
is_cash_position <- function(portfolio) {
  # CASH positions identified by type = "CASH"
  !is.na(portfolio$type) & portfolio$type == "CASH"
}

#' Get exchange rate for currency pair on specific date
#'
#' @param from Base currency code (e.g., "CHF", "USD", "EUR")
#' @param to Quote currency code (e.g., "USD", "CHF")
#' @param date Date for rate lookup (default today)
#' @return Exchange rate (numeric) - how many units of 'to' per 1 unit of 'from'
#' @export
#'
#' @details
#' Returns exchange rate showing how many units of 'to' currency you get
#' for 1 unit of 'from' currency.
#'
#' Example: get_exchange_rate("CHF", "USD") returns USD per 1 CHF
#'
#' This function leverages existing currency conversion utilities from currency.R:
#' - Uses convert_to_usd_date() and convert_to_chf_date() for conversions
#' - Handles any currency pair by using USD as intermediary
#' - Supports vectorized operations
#'
#' @examples
#' \dontrun{
#' # How many USD per 1 CHF?
#' rate <- get_exchange_rate("CHF", "USD", Sys.Date())
#' }
get_exchange_rate <- function(from, to, date = Sys.Date()) {
  # Handle same currency
  if (from == to) return(1.0)

  # Optimize when either currency is USD - only one conversion needed
  if (to == "USD") {
    # Direct conversion: FROM -> USD
    return(convert_to_usd_date(amount = 1, currency = from, convert_date = date))
  }

  if (from == "USD") {
    # Inverse: USD -> TO = 1 / (TO -> USD)
    to_usd_rate <- convert_to_usd_date(amount = 1, currency = to, convert_date = date)
    return(1 / to_usd_rate)
  }

  # Use existing currency conversion functions from currency.R
  # Strategy for cross rates: Convert 1 unit of 'from' currency to 'to' currency
  # from -> to = (from -> USD) / (to -> USD)

  # Get FROM/USD rate: how many USD per 1 unit of 'from'
  from_to_usd <- convert_to_usd_date(amount = 1, currency = from, convert_date = date)

  # Get TO/USD rate: how many USD per 1 unit of 'to'
  to_to_usd <- convert_to_usd_date(amount = 1, currency = to, convert_date = date)

  # Calculate cross rate: FROM/TO = (FROM/USD) / (TO/USD)
  rate <- from_to_usd / to_to_usd

  return(rate)
}

#' Calculate unrealized P&L for CASH positions
#'
#' @param trades Data frame with open CASH trades
#' @return Data frame with UnrealizedPnL column added
#' @export
#'
#' @details
#' Calculates P&L from exchange rate movements. Cost basis includes commission
#' (Total field includes commission as part of cash outflow).
#'
#' @examples
#' \dontrun{
#' trades <- getActiveTrades("U1804173")
#' trades_with_pnl <- calculate_cash_unrealized_pnl(trades)
#' }
calculate_cash_unrealized_pnl <- function(trades) {
  # Filter for CASH trades only
  cash_trades <- trades[is_cash_trade(trades), ]

  if (nrow(cash_trades) == 0) return(trades)

  # For each CASH trade, calculate P&L
  cash_trades$UnrealizedPnL <- vapply(seq_len(nrow(cash_trades)), function(i) {
    row <- cash_trades[i, ]

    # Get current exchange rate (Instrument currency to transaction Currency)
    # Instrument contains the currency code (e.g., "CHF")
    # Currency is what we paid with (e.g., "USD")
    current_rate <- get_exchange_rate(row$Instrument, row$Currency, Sys.Date())

    # Current value in transaction currency
    # Example: 30,000 CHF × 1.2561 USD/CHF = 37,683 USD
    current_value <- row$Pos * current_rate

    # Cost basis INCLUDING commission
    # Total field is negative for purchases (cash outflow) and includes commission
    cost_basis <- abs(row$Total)

    # P&L in transaction currency
    # Example: 37,683 - 37,501.60 = 181.4 USD
    pnl_transaction_currency <- current_value - cost_basis

    # Convert to base currency
    # CRITICAL: Use the same exchange rate source for consistency
    # We already have Instrument->Currency rate (current_rate)
    # So Currency->Instrument rate is 1/current_rate
    base_currency <- getParam("BaseCurrency")

    if (row$Currency == base_currency) {
      # Transaction currency IS base currency, no conversion needed
      pnl_base <- pnl_transaction_currency
    } else if (row$Instrument == base_currency) {
      # Instrument IS base currency
      # P&L is in transaction currency, convert using inverse of current_rate
      # Example: 181.4 USD / 1.2561 (CHF->USD) = 144.42 CHF
      pnl_base <- pnl_transaction_currency / current_rate
    } else {
      # Neither is base currency - need cross conversion
      # Fall back to convert_to_base_date (less common case)
      pnl_base <- convert_to_base_date(pnl_transaction_currency, row$Currency, Sys.Date())
    }

    return(pnl_base)
  }, FUN.VALUE = numeric(1))

  # Merge back with original trades
  trades$UnrealizedPnL[is_cash_trade(trades)] <- cash_trades$UnrealizedPnL

  return(trades)
}

#' Get CASH positions from portfolio
#'
#' @param account Account code
#' @return Data frame with CASH positions including unrealized P&L
#' @export
#'
#' @examples
#' \dontrun{
#' cash_positions <- get_cash_positions("U1804173")
#' }
get_cash_positions <- function(account) {
  # Get active trades
  active <- getActiveTrades(account)

  # Filter for CASH trades
  cash_positions <- active[is_cash_trade(active), ]

  # Calculate unrealized P&L
  cash_positions <- calculate_cash_unrealized_pnl(cash_positions)

  return(cash_positions)
}

#' Look up the most recent open CASH trade for a currency
#'
#' @param account_table Account code (e.g., "U1804173")
#' @param currency Currency code to search for (e.g., "JPY")
#' @return Data frame with TradeNr, Price, Instrument, Currency columns (0 rows if no match)
#' @keywords internal
getCashTradeForCurrency <- function(account_table, currency) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  account_db <- account_table

  # CASH trades store the foreign (non-base) currency in Symbol, so the balance
  # currency matches Symbol regardless of which leg sits in Instrument/Currency.
  query <- "SELECT TradeNr, Price, Instrument, Currency FROM Trades
            WHERE Account = ? AND Symbol = ? AND Status != 'Fermé'
            ORDER BY TradeDate DESC LIMIT 1"
  DBI::dbGetQuery(conn, query, params = list(account_db, currency))
}

#' Get all open trades denominated in a currency (for FX cost basis)
#'
#' Returns the TradeDate and local-currency Total of every open trade whose
#' Currency is the given code — both explicit FX conversions and foreign-currency
#' stock purchases. Extracted so tests can mock it.
#'
#' @param account_table Account code (e.g., "U25343478")
#' @param currency Currency code (e.g., "JPY")
#' @return Data frame with TradeDate, Total columns (0 rows if none)
#' @keywords internal
getCurrencyTradesForBasis <- function(account_table, currency) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  query <- "SELECT TradeDate, Total FROM Trades
            WHERE Account = ? AND Currency = ? AND Status != 'Fermé'"
  DBI::dbGetQuery(conn, query, params = list(account_table, currency))
}

#' Weighted-average FX cost basis for a foreign-cash balance
#'
#' Cost basis = amount-weighted FX rate across all open trades in the currency
#' (Convention A). Each trade's local-currency Total (absolute) weights the base
#' rate on its TradeDate, so the result is the rate at which the currency book
#' was actually acquired. This captures the full FX exposure of a borrowed/short
#' foreign-cash balance — not just the single explicit FX trade — which is what
#' the single-trade cost basis missed (it applied one trade's rate to the whole
#' balance even when stock purchases had borrowed the rest at other rates).
#'
#' @param account_table Account code
#' @param currency Currency code
#' @param convert_date Unused placeholder kept for signature symmetry
#' @return Base-currency-per-unit rate, or NA_real_ when no open trades exist
#' @keywords internal
weighted_cash_cost_basis <- function(account_table, currency, convert_date = Sys.Date()) {
  trades <- getCurrencyTradesForBasis(account_table, currency)
  if (nrow(trades) == 0) return(NA_real_)

  trades <- trades[!is.na(trades$Total) & trades$Total != 0, , drop = FALSE]
  if (nrow(trades) == 0) return(NA_real_)

  dates <- as.Date(as.character(trades$TradeDate), "%Y%m%d")
  rates <- convert_to_base_date(1, currency, dates)
  weights <- abs(trades$Total)
  sum(weights * rates) / sum(weights)
}

#' Resolve cost basis from a CASH trade query result
#'
#' Handles the quoting convention: when trade matches on Instrument, Price is
#' already in base/foreign units. When trade matches on Currency, Price is in
#' foreign/base units and must be inverted.
#'
#' @param trade_result Data frame row from getCashTradeForCurrency (must have Price, Instrument, Currency)
#' @param position_currency The currency of the CASH balance being valued
#' @return Cost basis in base_currency per position_currency
#' @keywords internal
resolve_cash_cost_basis <- function(trade_result, position_currency) {
  prix <- trade_result$Price[1]

  if (trade_result$Instrument[1] == position_currency) {
    # Trade Instrument matches — Price is in Currency/Instrument
    # e.g., Instrument=JPY, Currency=CHF, Price = CHF per JPY
    return(prix)
  } else {
    # Trade matched on Currency — Price is in position_currency per Instrument
    # e.g., Instrument=CHF, Currency=JPY, Price=201.665 JPY/CHF
    # Need CHF/JPY, so invert
    return(1 / prix)
  }
}

#' Create CASH portfolio row from currency balance
#'
#' @param currency Currency code (e.g., "CHF", "USD", "EUR")
#' @param balance Currency balance (units)
#' @param snapshot_date Portfolio snapshot date (integer YYYYMMDD format)
#' @param snapshot_heure Portfolio snapshot time (HH:MM:SS format)
#' @param account_table Account code for linking (optional)
#' @return Data frame with single portfolio row, or NULL if base currency/zero balance
#'
#' @details
#' Creates a portfolio row for non-base currency balances. Skips base currency
#' and zero balances. CRITICAL: Uses the same snapshot_date and snapshot_heure
#' as the regular portfolio to ensure readLastPortfolio() returns all positions.
#'
#' Uses existing currency conversion functions from currency.R for exchange rates.
#'
#' @keywords internal
create_cash_portfolio_row <- function(currency, balance, snapshot_date, snapshot_heure, account_table = NULL) {
  base_currency <- getParam("BaseCurrency")  # e.g., "CHF"

  # Skip if base currency (CHF) or zero balance
  if (currency == base_currency || abs(balance) < 0.01) {
    return(NULL)
  }

  # Convert snapshot_date to Date for exchange rate lookup
  rate_date <- as.Date(as.character(snapshot_date), "%Y%m%d")

  # Get current exchange rate using existing function from currency.R
  # Convert 1 unit of currency to base currency
  exchange_rate <- convert_to_base_date(amount = 1, currency = currency, convert_date = rate_date)

  # Calculate market value in base currency
  mkt_value <- balance * exchange_rate

  # Cost basis: amount-weighted FX rate across ALL open trades in this currency
  # (Convention A). Defaults to the current spot rate — i.e. zero FX P&L — only
  # when no trade history exists to anchor the acquired rate.
  trade_nr <- NA_integer_
  trade_cost_basis <- exchange_rate

  if (!is.null(account_table)) {
    wbasis <- tryCatch(
      weighted_cash_cost_basis(account_table, currency, rate_date),
      error = function(e) {
        logger::log_warn("Weighted cash cost basis failed for {currency}: {e$message}", namespace = "Tdata")
        NA_real_
      })
    if (!is.na(wbasis)) trade_cost_basis <- wbasis

    # Link to the most recent explicit FX/CASH trade for display continuity
    # (leaves TradeNr = NA for cash borrowed purely via stock purchases).
    tryCatch({
      result <- getCashTradeForCurrency(account_table, currency)
      if (nrow(result) > 0) {
        trade_nr <- result$TradeNr[1]
        logger::log_debug("Linked CASH position {currency} to TradeNr {trade_nr}, cost basis: {trade_cost_basis}",
                         namespace = "Tdata")
      }
    }, error = function(e) {
      logger::log_warn("Could not link CASH position to trade: {e$message}", namespace = "Tdata")
    })
  }

  # Calculate unrealized P&L using trade cost basis
  unrealized_pnl <- (exchange_rate - trade_cost_basis) * balance

  # Create portfolio row matching table structure
  # CRITICAL: Use snapshot_date and snapshot_heure from regular portfolio
  row <- data.frame(
    TradeNr = trade_nr,              # Link to trade if found
    date = snapshot_date,            # Same as regular portfolio (YYYYMMDD integer)
    heure = snapshot_heure,          # Same as regular portfolio (HH:MM:SS)
    symbol = currency,               # Currency code (join key with Instrument)
    expdate = NA_integer_,
    strike = NA_real_,
    pos = as.numeric(balance),       # Currency units held (ensure numeric, not integer)
    mktPrice = exchange_rate,        # Current rate in base currency
    optPrice = NA_real_,
    mktValue = mkt_value,            # Value in base currency
    avgCost = trade_cost_basis,      # Cost basis from linked trade (or current rate if no trade)
    unPnL = unrealized_pnl,          # P&L from exchange rate movement
    IV = NA_real_,
    pvDividend = NA_real_,
    delta = 1.0,                     # Linear exposure to FX rate
    gamma = 0,
    vega = 0,
    theta = 0,
    uPrice = NA_real_,
    multiplier = 1L,
    currency = base_currency,        # Base currency for P&L
    type = "CASH",                   # Identifier for CASH positions
    Instrument = currency,           # Same as symbol (join key)
    margin = 0,
    stringsAsFactors = FALSE
  )

  return(row)
}

#' Reconcile CASH trades with portfolio positions
#'
#' @param account Account code (e.g., "U1804173")
#' @return Data frame with matched trades and positions, flagging discrepancies
#' @export
#'
#' @details
#' Joins CASH trades with portfolio positions on the foreign currency
#' (trades.Symbol = portfolio.symbol). Flags position mismatches which may
#' indicate currency movements from dividends, interest, fees, or other
#' non-trade transactions.
#'
#' @examples
#' \dontrun{
#' reconciliation <- reconcile_cash_positions("U1804173")
#' # Check for discrepancies
#' mismatches <- reconciliation[!reconciliation$position_match, ]
#' }
reconcile_cash_positions <- function(account) {
  # Get CASH trades
  all_trades <- getActiveTrades(account)
  trades <- dplyr::filter(all_trades, is_cash_trade(all_trades))

  # Get CASH portfolio positions
  all_portfolio <- readLastPortfolio(account)
  portfolio <- dplyr::filter(all_portfolio, is_cash_position(all_portfolio))

  # Join on the foreign currency: Symbol (trades) = symbol (portfolio)
  reconciled <- dplyr::left_join(
    trades,
    portfolio |> dplyr::select(symbol, pos, mktPrice, mktValue, unPnL),
    by = c("Symbol" = "symbol")
  )

  # Flag discrepancies (position mismatch)
  reconciled <- reconciled |>
    dplyr::mutate(
      position_match = abs(Pos - pos) < 0.01,
      discrepancy = dplyr::if_else(!position_match, Pos - pos, 0)
    )

  return(reconciled)
}
