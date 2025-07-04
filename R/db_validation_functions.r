#' Database Type Validation Functions
#'
#' These functions ensure consistent data types before database insertion
#' to prevent RSQLite mixed-type warnings.

#' Validate and standardize data before database insertion
#'
#' This function ensures consistent data types for different database tables
#' to prevent RSQLite mixed-type warnings.
#'
#' @param data Data frame to validate
#' @param table_name Target table name
#' @return Validated data frame with consistent types
#' @export
validate_db_types <- function(data, table_name) {

  switch(table_name,
         "Account" = validate_account_data(data),
         "Tickers" = validate_tickers_data(data),
         "TestTrades" =,
         "Trades" = validate_trades_data(data),
         "ConvertToUSD" = validate_currency_data(data),
         "Journal" = validate_journal_data(data),
         "TestPortf" =,
         "Gonet" = validate_portfolio_data(data),  # Gonet is portfolio-like
         "Prices" = validate_prices_data(data),
         # For portfolio tables (pattern Uxxx ou DUxxx)
         {
           if (grepl("^(U|DU)\\d+$", table_name)) {
             validate_portfolio_data(data)
           } else {
             # Default validation for unknown tables
             data
           }
         }
  )
}

#' Validate Account table data
#' @noRd
validate_account_data <- function(data) {
   return(data)

  # Ensure CashFlow is numeric
  if ("CashFlow" %in% names(data)) data$CashFlow <- standardize_numeric(data$CashFlow)

  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
      data$date <- sapply(data$date, standardize_date_integer)
  }

  # Ensure numeric columns are properly typed
  numeric_cols <- c("NetLiquidation", "EquityWithLoanValue", "FullAvailableFunds",
                    "FullInitMarginReq", "FullMaintMarginReq", "FullExcessLiquidity",
                    "OptionMarketValue", "StockMarketValue", "UnrealizedPnL",
                    "RealizedPnL", "TotalCashBalance")

  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- standardize_numeric(data[[col]])
    }
  }

  return(data)
}

#' Validate Tickers table data
#' @noRd
validate_tickers_data <- function(data) {

  # Ensure date is integer YYYYMMDD
  if ("Expiration" %in% names(data)) {
       data$Expiration <- sapply(data$Expiration, standardize_date_integer)
  }

  ### Multiplier must be an integer, not a real
  if ("Multiplier" %in% names(data)) data$Multiplier <- as.integer(data$Multiplier)

  # Ensure other numeric columns
  numeric_cols <- c("Beta_3m", "Beta_6m", "Beta_1y", "Beta_3y", "Div_yield")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    }
  }

  return(data)
}

#' Validate portfolio table data (Uxxx tables)
#' @noRd
validate_portfolio_data <- function(data) {

  # Ensure date is integer YYYYMMDD
  date_cols <- c("expdate","date")
  for (col in date_cols)
    if (col %in% names(data))
      data[[col]] <- sapply(data[[col]], standardize_date_integer)

  # Ensure integer columns
  integer_columns <- c("TradeNr",  "pos","multiplier" )
  for (col in integer_columns) {
    if (col %in% names(data)) {
        # Integer columns
        data[[col]] <- suppressWarnings(as.integer(data[[col]]))
    }
  }

    # Ensure numeric columns
  numeric_cols <- c("strike","mktPrice", "optPrice", "mktValue",
                    "avgCost", "unPnL", "IV", "pvDividend", "delta", "gamma",
                    "vega", "theta", "uPrice",  "margin")

  for (col in numeric_cols) {
    if (col %in% names(data)) {
        # Numeric columns
        data[[col]] <- standardize_numeric(data[[col]])
    }
  }

  return(data)
}

#' Validate Trades table data
#' @noRd
validate_trades_data <- function(data) {

  # Ensure date is integer
  if ("TradeDate" %in% names(data)) {
    data$TradeDate <- sapply(data$TradeDate, standardize_date_integer)
  }

  # Other Integer columns
  int_cols <- c("TradeNr", "Pos")
  for (col in int_cols) {
    if (col %in% names(data)) {
      data[[col]] <- suppressWarnings(as.integer(data[[col]]))
    }
  }

  # Numeric columns
  num_cols <- c("Prix", "Comm.", "Total", "Risk", "Reward", "PnL")
  for (col in num_cols) {
    if (col %in% names(data)) {
      data[[col]] <- standardize_numeric(data[[col]])
    }
  }

  return(data)
}

#' Validate currency conversion data
#' @noRd
validate_currency_data <- function(data) {

  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
    data$date <- sapply(data$date, standardize_date_integer)
  }

  if ("usd_value" %in% names(data)) {
    data$usd_value <- standardize_numeric(data$usd_value)
  }

  return(data)
}

#' Validate Journal table data
#' @noRd
validate_journal_data <- function(data) {

  # Ensure entryId is integer
  if ("entryId" %in% names(data)) {
    data$entryId <- suppressWarnings(as.integer(data$entryId))
  }

  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
    data$date <- sapply(data$date, standardize_date_integer)
  }

  # Ensure numeric columns
  numeric_cols <- c("close", "mkt_price")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- sapply(data[[col]], standardize_numeric)
    }
  }

  return(data)
}

#' Validate Prices table data
#' @noRd
validate_prices_data <- function(data) {

  # Ensure numeric columns for price/volatility data
  numeric_cols <- c("price", "iv30", "ivp", "rv30", "rvp", "iv180", "iv_percentile",
                    "current_iv", "hv_percentile", "current_hv", "current_price")

  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- sapply(data[[col]], standardize_numeric)
    }
  }

  # Ensure datetime is character (timestamp format)
  if ("datetime" %in% names(data)) {
    data$datetime <- as.character(data$datetime)
  }

  return(data)
}

#' Safe database write with type validation
#'
#' This function validates data types before writing to database to prevent
#' mixed-type columns that cause RSQLite warnings.
#'
#' @param conn Database connection
#' @param table_name Table name
#' @param data Data to insert
#' @param append Logical, whether to append (TRUE) or overwrite (FALSE)
#' @param temporary Logical, whether table is temporary (skips validation)
#' @param ... Additional arguments passed to dbWriteTable/dbAppendTable
#' @return Result from database write operation
#' @export
safe_db_write <- function(conn, table_name, data, append = TRUE, temporary = FALSE, ...) {

  # Skip validation for temporary tables
  if (temporary) {
    if (append) {
      result <- DBI::dbAppendTable(conn, table_name, data, ...)
    } else {
      result <- DBI::dbWriteTable(conn, table_name, data, temporary = TRUE, ...)
    }
    return(result)
  }

  # Validate data types before writing
  validated_data <- validate_db_types(data, table_name)

  # Write to database
  if (append) {
    result <- DBI::dbAppendTable(conn, table_name, validated_data, ...)
  } else {
    result <- DBI::dbWriteTable(conn, table_name, validated_data, ...)
  }

  return(result)
}

#' Safe database append with validation
#'
#' Wrapper for dbAppendTable with automatic type validation to prevent
#' mixed-type columns.
#'
#' @param conn Database connection
#' @param table_name Table name
#' @param data Data to append
#' @param ... Additional arguments passed to dbAppendTable
#' @return Result from database append operation
#' @export
safe_db_append <- function(conn, table_name, data, ...) {
  safe_db_write(conn, table_name, data, append = TRUE, ...)
}

# Validation for specific data conversion scenarios
# Internal helper functions - not exported

# Standardize date values to integer format
standardize_date_integer <- function(date_value) {

  # Convert various date formats to YYYYMMDD integer
  if (is.na(date_value) || isTRUE(date_value == 0) || isTRUE(date_value == "")) {
    return(NA_integer_)
  }

  ### Tries first to test if Date format
  if (inherits(date_value, "Date")) date_value <- suppressWarnings(as.integer(format(date_value, "%Y%m%d")))

  ## Test if is character type, but integer - if not the case will then be returned as NA_integer_
  if (is.character(date_value)) date_value <- suppressWarnings(as.integer(date_value))

  ### Finally look at value itself, excluding NA case
  if (is.numeric(date_value) && !is.na(date_value)) {
    if (date_value >= 19000101 && date_value <= 21001231)
      return(suppressWarnings(as.integer(date_value)))
  }

  ### Anything else is NA
  return(NA_integer_)
}

# Standardize numeric values safely
standardize_numeric <- function(numeric_value) {
  # Safe numeric conversion that handles edge cases
  if (is.na(numeric_value) || is.null(numeric_value)) {
    return(NA_real_)
  }

  if (is.numeric(numeric_value)) {
    return(suppressWarnings(as.numeric(numeric_value)))
  }

  if (is.character(numeric_value)) {
    # Handle empty strings and special values
    if (numeric_value == "" || tolower(numeric_value) %in% c("na", "null", "n/a")) {
      return(NA_real_)
    }

    # Remove currency symbols and convert
    # Enlever symboles d'abord
    cleaned <- gsub("[€$%]", "", trimws(numeric_value))

    # Détecter format: si virgule suivie de 1-2 chiffres à la fin = décimales
    if (grepl(",\\d{1,2}$", cleaned)) {
      # Format européen: 100,50 ou 1.234,50
      cleaned <- gsub("\\.", "", cleaned)  # Enlever points (milliers)
      cleaned <- gsub(",", ".", cleaned)   # Virgule → point décimal
    } else {
      # Format anglo: enlever virgules (milliers)
      cleaned <- gsub(",", "", cleaned)
    }

    result <- suppressWarnings(as.numeric(cleaned))
    return(ifelse(is.na(result), NA_real_, result))
  }

  return(NA_real_)
}
