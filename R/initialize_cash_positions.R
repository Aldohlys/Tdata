#' Initialize CASH Positions with Baseline Date
#'
#' Creates initial CASH trade records for existing currency balances as of
#' September 10, 2025 (when base currency changed from USD to CHF).
#'
#' This function should be run ONCE to establish a baseline for CASH position
#' tracking. It creates opening trades for all non-base currency balances as of
#' the baseline date, allowing proper P&L tracking going forward.
#'
#' @section Prerequisites:
#' You must run \code{getIBKR()} first to populate the portfolio table with
#' current CASH positions from IBKR. This function reads those CASH positions
#' from the portfolio table to create corresponding trade records.
#'
#' @param baseline_date Date when CASH position tracking begins (default: 2025-09-10)
#' @param account Account code (default: reads from config or prompts user)
#' @param dry_run If TRUE, shows what would be created without writing to DB
#' @return Data frame of created CASH trades, or NULL if dry_run
#' @export
#'
#' @examples
#' \dontrun{
#' # Step 0: REQUIRED - Get current portfolio from IBKR
#' getIBKR()
#'
#' # Step 1: Preview what would be created
#' initialize_cash_positions(dry_run = TRUE)
#'
#' # Step 2: Actually create the initial trades
#' initialize_cash_positions(dry_run = FALSE)
#' }
initialize_cash_positions <- function(baseline_date = as.Date("2025-09-10"),
                                      account = NULL,
                                      dry_run = TRUE) {

  logger::log_info("Initializing CASH positions with baseline date: {baseline_date}",
                   namespace = "Tdata")

  # Determine account if not provided
  if (is.null(account)) {
    account <- getParam("DefaultAccount")
    if (is.null(account)) {
      stop("Account must be provided or set in config as DefaultAccount")
    }
  }

  logger::log_info("Account: {account}", namespace = "Tdata")

  # Get current portfolio to see what currency balances exist
  current_portfolio <- readLastPortfolio(account)

  if (nrow(current_portfolio) == 0) {
    logger::log_warn("No portfolio data found for account {account}", namespace = "Tdata")
    return(NULL)
  }

  # Filter for CASH positions
  cash_positions <- current_portfolio[is_cash_position(current_portfolio), ]

  if (nrow(cash_positions) == 0) {
    logger::log_info("No CASH positions found in current portfolio", namespace = "Tdata")
    return(NULL)
  }

  # Filter out base currency positions (we don't create trades for holding base currency)
  base_currency <- getParam("BaseCurrency")
  cash_positions <- cash_positions[!(cash_positions$symbol %in% c(base_currency, "BASE")), ]

  if (nrow(cash_positions) == 0) {
    logger::log_info("No non-base CASH positions found to initialize", namespace = "Tdata")
    return(NULL)
  }

  logger::log_info("Found {nrow(cash_positions)} non-base CASH positions to initialize",
                   namespace = "Tdata")

  # Get next available TradeNr
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  max_trade_nr_query <- "SELECT COALESCE(MAX(TradeNr), 0) as max_nr FROM Trades"
  max_trade_nr <- DBI::dbGetQuery(conn, max_trade_nr_query)$max_nr
  next_trade_nr <- max_trade_nr + 1

  logger::log_debug("Next available TradeNr: {next_trade_nr}", namespace = "Tdata")

  # Get exchange rates for baseline date
  baseline_date_int <- as.integer(format(baseline_date, "%Y%m%d"))

  # Create initial CASH trades for each currency
  cash_trades <- lapply(seq_len(nrow(cash_positions)), function(i) {
    pos_row <- cash_positions[i, ]
    currency <- pos_row$symbol  # Currency code (e.g., "USD", "EUR")
    balance <- pos_row$pos
    current_rate <- pos_row$mktPrice

    # Get historical exchange rate for baseline date
    baseline_rate <- get_exchange_rate(
      from = currency,
      to = getParam("BaseCurrency"),
      date = baseline_date
    )

    if (is.na(baseline_rate)) {
      logger::log_warn("Cannot get exchange rate for {currency} on {baseline_date}, using current rate",
                       namespace = "Tdata")
      baseline_rate <- current_rate
    }

    # Calculate total cost in base currency
    # For initialization, we assume currency was "purchased" at baseline rate
    total_base <- -abs(balance * baseline_rate)

    # Create trade record
    trade <- data.frame(
      TradeNr = next_trade_nr + i - 1,
      Account = account,
      TradeDate = baseline_date_int,
      Strategy = "Perso",  # Personal/administrative trade
      Instrument = currency,  # Currency code (join key with portfolio)
      Ssjacent = "CASH",     # Asset type identifier
      Pos = balance,
      Prix = baseline_rate,  # Baseline exchange rate
      Strike = NA_real_,
      Right = "",
      Commission = 0,  # No commission for initialization
      Total = total_base,  # Cost in base currency (negative = outflow)
      Exp.Date = NA_integer_,
      Risk = 0,
      Reward = 0,
      PnL = 0,
      Status = "Ouvert",
      Currency = getParam("BaseCurrency"),  # Transaction currency
      Notes = paste0(currency, " CASH position initialization as of ", baseline_date),
      stringsAsFactors = FALSE
    )

    return(trade)
  })

  # Combine all trades
  cash_trades_df <- do.call(rbind, cash_trades)

  # Display summary
  logger::log_info("Created {nrow(cash_trades_df)} initial CASH trades:", namespace = "Tdata")
  print(cash_trades_df[, c("TradeNr", "Instrument", "Pos", "Prix", "Total", "Currency")])

  if (dry_run) {
    logger::log_info("DRY RUN: No data written to database", namespace = "Tdata")
    Tbasics::display_message(paste0(
      "Preview of ", nrow(cash_trades_df), " CASH trades to be created.\n",
      "Run with dry_run=FALSE to write to database."
    ))
    return(cash_trades_df)
  }

  # Write to database
  logger::log_info("Writing {nrow(cash_trades_df)} CASH trades to Trades table",
                   namespace = "Tdata")

  safe_db_append(conn, "Trades", cash_trades_df)

  logger::log_info("CASH positions initialized successfully", namespace = "Tdata")
  Tbasics::display_message(paste0(
    "Successfully initialized ", nrow(cash_trades_df), " CASH positions.\n",
    "Baseline date: ", baseline_date, "\n",
    "Trade numbers: ", min(cash_trades_df$TradeNr), " - ", max(cash_trades_df$TradeNr)
  ))

  return(cash_trades_df)
}


#' Verify CASH Position Initialization
#'
#' Checks that CASH positions have been properly initialized and shows
#' current P&L relative to baseline.
#'
#' @param account Account code
#' @return Summary data frame with position details and P&L
#' @export
#'
#' @examples
#' \dontrun{
#' verify_cash_initialization("U1804173")
#' }
verify_cash_initialization <- function(account = NULL) {

  if (is.null(account)) {
    account <- getParam("DefaultAccount")
    if (is.null(account)) {
      stop("Account must be provided or set in config as DefaultAccount")
    }
  }

  logger::log_info("Verifying CASH position initialization for {account}",
                   namespace = "Tdata")

  # Get CASH trades
  cash_trades <- get_cash_positions(account)

  if (nrow(cash_trades) == 0) {
    logger::log_warn("No CASH trades found for account {account}", namespace = "Tdata")
    Tbasics::display_message("No CASH trades found. Run initialize_cash_positions() first.")
    return(NULL)
  }

  # Get current portfolio
  current_portfolio <- readLastPortfolio(account)
  cash_portfolio <- current_portfolio[is_cash_position(current_portfolio), ]

  # Join trades with portfolio
  verification <- dplyr::left_join(
    cash_trades[, c("TradeNr", "Instrument", "Pos", "Prix", "Total", "TradeDate", "UnrealizedPnL")],
    cash_portfolio[, c("symbol", "pos", "mktPrice", "mktValue", "unPnL")],
    by = c("Instrument" = "symbol")
  )

  # Calculate P&L details
  verification <- dplyr::mutate(verification,
    InitialValue = abs(Total),
    CurrentValue = mktValue,
    PnL_Pct = (CurrentValue - InitialValue) / InitialValue * 100,
    DaysSinceInit = as.numeric(Sys.Date() - as.Date(as.character(TradeDate), "%Y%m%d"))
  )

  logger::log_info("CASH Position Summary:", namespace = "Tdata")
  print(verification)

  Tbasics::display_message(paste0(
    "CASH Position Verification Complete\n",
    "Positions: ", nrow(verification), "\n",
    "Total Initial Value: ", round(sum(verification$InitialValue, na.rm = TRUE), 2), " ", getParam("BaseCurrency"), "\n",
    "Total Current Value: ", round(sum(verification$CurrentValue, na.rm = TRUE), 2), " ", getParam("BaseCurrency"), "\n",
    "Total Unrealized P&L: ", round(sum(verification$UnrealizedPnL, na.rm = TRUE), 2), " ", getParam("BaseCurrency")
  ))

  return(verification)
}
