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
         "ConvertToCHF" = validate_currency_chf_data(data),  # Add CHF validation
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

  # Ensure CashFlow is numeric
  if ("CashFlow" %in% names(data)) data$CashFlow <- standardize_numeric(data$CashFlow)

  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
      data$date <- standardize_date_integer(data$date)
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
       data$Expiration <- standardize_date_integer(data$Expiration)
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
      data[[col]] <- standardize_date_integer(data[[col]])

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
    data$TradeDate <-  standardize_date_integer(data$TradeDate)
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

#' Validate currency conversion data (USD)
#' @noRd
validate_currency_data <- function(data) {
  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
    data$date <- standardize_date_integer(data$date)
  }
  # Ensure usd_value is numeric
  if ("usd_value" %in% names(data)) {
    data$usd_value <- standardize_numeric(data$usd_value)
  }
  return(data)
}

#' Validate currency conversion data (CHF)
#' @noRd
validate_currency_chf_data <- function(data) {
  # Ensure date is integer YYYYMMDD
  if ("date" %in% names(data)) {
    data$date <- standardize_date_integer(data$date)
  }
  # Ensure chf_value is numeric
  if ("chf_value" %in% names(data)) {
    data$chf_value <- standardize_numeric(data$chf_value)
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
    data$date <- standardize_date_integer(data$date)
  }

  # Ensure numeric columns
  numeric_cols <- c("close", "mkt_price")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- standardize_numeric(data[[col]])
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
      data[[col]] <- standardize_numeric(data[[col]])
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
safe_db_write <- function(conn, table_name, data, append = FALSE, temporary = FALSE, ...) {

  # Skip validation for temporary tables
  if (temporary) {
    if (append) {
      result <- DBI::dbAppendTable(conn, table_name, data, ...)
    } else {
      # For temporary tables, always overwrite
      result <- DBI::dbWriteTable(conn, table_name, data, temporary = TRUE, overwrite = TRUE, append = FALSE, ...)
    }
    return(result)
  }

  # Validate data types before writing
  validated_data <- validate_db_types(data, table_name)

  # Write to database
  if (append) {
    result <- DBI::dbAppendTable(conn, table_name, validated_data, ...)
  } else {
    # Explicitly set both overwrite and append to avoid ambiguity
    result <- DBI::dbWriteTable(conn, table_name, validated_data,
                                overwrite = TRUE, append = FALSE, ...)
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
  # Handle empty/null inputs upfront
  if (is.null(date_value) || length(date_value) == 0) {
    return(NA_integer_)
  }

  # Initialize result vector with NAs
  result <- rep(NA_integer_, length(date_value))

  # Handle Date objects first - vectorized format conversion
  if (inherits(date_value, "Date")) {
    return(suppressWarnings(as.integer(format(date_value, "%Y%m%d"))))
  }

  # Convert character vectors to numeric - vectorized operation
  if (is.character(date_value)) {
    date_value <- suppressWarnings(as.integer(date_value))
  }

  # Check for valid numeric values in valid date range
  if (is.numeric(date_value)) {
    # Vectorized logical operations replace individual checks
    valid_mask <- !is.na(date_value) &
      date_value >= 19000101 &
      date_value <= 21001231

    # Apply conversion only to valid entries
    result[valid_mask] <- suppressWarnings(as.integer(date_value[valid_mask]))
  }

  return(result)
}

# Standardize numeric values safely
standardize_numeric <- function(numeric_value) {
  # Handle empty/null inputs upfront
  if (is.null(numeric_value) || length(numeric_value) == 0) {
    return(NA_real_)
  }

  # Initialize result vector with NAs
  result <- rep(NA_real_, length(numeric_value))

  # Handle numeric vectors - direct conversion
  if (is.numeric(numeric_value)) {
    return(suppressWarnings(as.numeric(numeric_value)))
  }

  # Handle character vectors with vectorized operations
  if (is.character(numeric_value)) {
    # Vectorized empty/special value detection
    empty_mask <- numeric_value == "" |
      tolower(trimws(numeric_value)) %in% c("na", "null", "n/a")

    # Process non-empty values
    non_empty_mask <- !is.na(numeric_value) & !empty_mask

    if (any(non_empty_mask)) {
      # Vectorized cleaning operations
      cleaned <- gsub("[\U20AC$%]", "", trimws(numeric_value[non_empty_mask]))

      # Detect European format (comma followed by 1-2 digits at end)
      european_format <- grepl(",\\d{1,2}$", cleaned)

      # Handle European format - vectorized operations
      if (any(european_format)) {
        cleaned[european_format] <- gsub("\\.", "", cleaned[european_format])  # Remove thousand separators
        cleaned[european_format] <- gsub(",", ".", cleaned[european_format])   # Comma becomes decimal point
      }

      # Handle Anglo format - vectorized comma removal
      if (any(!european_format)) {
        cleaned[!european_format] <- gsub(",", "", cleaned[!european_format])
      }

      # Convert to numeric and assign to result
      converted <- suppressWarnings(as.numeric(cleaned))
      result[non_empty_mask] <- ifelse(is.na(converted), NA_real_, converted)
    }
  }

  return(result)
}