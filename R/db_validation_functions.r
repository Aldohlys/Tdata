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
  integer_columns <- c("TradeNr", "multiplier" )
  for (col in integer_columns) {
    if (col %in% names(data)) {
        # Integer columns
        data[[col]] <- suppressWarnings(as.integer(data[[col]]))
    }
  }

  # CRITICAL: pos should be numeric, not integer, to preserve decimal currency amounts
  # (e.g., 12154.52 EUR not truncated to 12154)
  if ("pos" %in% names(data)) {
    data$pos <- standardize_numeric(data$pos)
  }

    # Ensure numeric columns
  numeric_cols <- c("strike","mktPrice", "optPrice", "mktValue",
                    "avgCost", "unPnL", "realizedPnL", "IV", "pvDividend", "delta", "gamma",
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
  int_cols <- c("TradeNr")
  for (col in int_cols) {
    if (col %in% names(data)) {
      data[[col]] <- suppressWarnings(as.integer(data[[col]]))
    }
  }

  # CRITICAL: Pos should be numeric, not integer, to preserve decimal currency amounts
  # For CASH positions, Pos represents currency units which can be fractional
  # (e.g., 12154.52 EUR not truncated to 12154)
  if ("Pos" %in% names(data)) {
    data$Pos <- standardize_numeric(data$Pos)
  }

  # Numeric columns
  num_cols <- c("Price", "Commission", "Total", "Risk", "Reward", "PnL")
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

#' Safe Database Connection
#'
#' Creates a secure database connection with comprehensive error handling and
#' package check protection. This function centralizes database connection logic
#' and prevents connection attempts during package installation or checking.
#'
#' @details
#' This function performs several validation checks before establishing a database connection:
#' \itemize{
#'   \item Skips connection during R CMD check to prevent installation warnings
#'   \item Validates configuration file existence
#'   \item Verifies database file accessibility
#'   \item Provides informative error messages for troubleshooting
#' }
#'
#' The function uses the database path specified in the configuration file via
#' \code{config::get("DB")}. Configuration files are searched in the current
#' working directory and package installation directory.
#'
#' @return A DBI connection object to the SQLite database
#'
#' Error if:
#' \itemize{
#'   \item Called during package check (returns specific error message)
#'   \item Configuration file is missing
#'   \item Database file doesn't exist at specified path
#'   \item Database connection fails
#' }
#'
#' @examples
#' \dontrun{
#' # Establish database connection
#' conn <- safe_db_connect()
#'
#' # Use connection for queries
#' result <- DBI::dbGetQuery(conn, "SELECT * FROM currencies LIMIT 5")
#'
#' # Always close connection when done
#' DBI::dbDisconnect(conn)
#'
#' # Better practice - use on.exit for cleanup
#' conn <- safe_db_connect()
#' on.exit(DBI::dbDisconnect(conn), add = TRUE)
#' data <- DBI::dbReadTable(conn, "currencies")
#' }
#'
#' @seealso
#' \code{\link[DBI]{dbConnect}} for direct database connections
#' \code{\link[config]{get}} for configuration management
#'
#' @export
safe_db_connect <- function(check_lock = TRUE, max_retries = 2) {
  if (Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != "" &&
      !identical(Sys.getenv("TESTTHAT"), "true")) {
    stop("Database operations not available during package check", call. = FALSE)
  }
  # ALSO skip during package installation
  if (Sys.getenv("R_PACKAGE_NAME", "") != "") {
    stop("Database operations not available during package installation", call. = FALSE)
  }

  # Let config package handle finding the config file - it knows about R_CONFIG_FILE
  # No need for manual file.exists() check

  # Retrieve database path from configuration
  db_path <- config::get("DB")

  # Validate database file exists at specified path
  if (!file.exists(db_path)) {
    stop("Database not found at ", db_path,
         ". Please initialize database with setup_database().",
         call. = FALSE)
  }

  # Helper to check if error is DB lock
  is_db_locked <- function(error) {
    error_msg <- tolower(as.character(error))
    grepl("database is locked", error_msg) || grepl("database locked", error_msg)
  }

  # Attempt connection with optional lock detection
  for (attempt in 1:max_retries) {
    conn <- tryCatch({
      conn_obj <- DBI::dbConnect(RSQLite::SQLite(), db_path)

      # Wait out momentary locks instead of erroring instantly. Without this, a
      # sub-second write held by another process (e.g. DailyPortfolioUpdate's
      # currency refresh right before its FX-freshness check) makes the SELECT 1
      # probe below fail with SQLITE_BUSY (exit 5), crashing the caller before it
      # does any real work. 5s is a hard ceiling; the retry loop below still
      # backstops genuinely long locks.
      DBI::dbExecute(conn_obj, "PRAGMA busy_timeout = 5000")

      # Optional: Test if DB is actually accessible (not just connected)
      if (check_lock) {
        # Quick test query to detect lock early
        tryCatch({
          DBI::dbGetQuery(conn_obj, "SELECT 1")
        }, error = function(e) {
          DBI::dbDisconnect(conn_obj)
          stop(e)
        })
      }

      return(conn_obj)

    }, error = function(e) {
      if (is_db_locked(e)) {
        logger::log_warn("Database locked on connection attempt {attempt}/{max_retries}",
                        namespace = "Tdata")

        if (attempt < max_retries) {
          Sys.sleep(1)
          return(NULL)  # Signal to retry
        } else {
          # Final attempt - notify user
          error_msg <- paste0(
            "Database is locked and cannot be accessed.\n\n",
            "The database file is currently in use by another application.\n",
            "Please close any other R sessions or applications accessing the database.\n\n",
            "Database: ", db_path, "\n\n",
            "Click OK after closing other applications."
          )

          Tbasics::display_error_message(error_msg)
          logger::log_error("Database locked: {e$message}", namespace = "Tdata")
          stop("Database is locked. Please close other applications accessing the database.",
               call. = FALSE)
        }
      } else {
        # Different error
        logger::log_error("Failed to connect to database: {e$message}", namespace = "Tdata")
        stop("Failed to connect to database: ", e$message, call. = FALSE)
      }
    })

    # If we got a connection, return it
    if (!is.null(conn)) {
      return(conn)
    }
  }

  # Should not reach here
  stop("Failed to connect to database after all retries", call. = FALSE)
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
safe_db_write <- function(conn, table_name, data, append = FALSE, temporary = FALSE,
                          max_retries = 3, retry_delay = 2, ...) {

  # Helper function to check if error is DB lock
  is_db_locked <- function(error) {
    error_msg <- tolower(as.character(error))
    grepl("database is locked", error_msg) || grepl("database locked", error_msg)
  }

  # Helper function to attempt write
  attempt_write <- function() {
    # Skip validation for temporary tables
    if (temporary) {
      if (append) {
        return(DBI::dbAppendTable(conn, table_name, data, ...))
      } else {
        # For temporary tables, always overwrite
        return(DBI::dbWriteTable(conn, table_name, data, temporary = TRUE,
                                overwrite = TRUE, append = FALSE, ...))
      }
    }

    # Validate data types before writing
    validated_data <- validate_db_types(data, table_name)

    # Write to database
    if (append) {
      return(DBI::dbAppendTable(conn, table_name, validated_data, ...))
    } else {
      # Explicitly set both overwrite and append to avoid ambiguity
      return(DBI::dbWriteTable(conn, table_name, validated_data,
                              overwrite = TRUE, append = FALSE, ...))
    }
  }

  # Attempt write with retry logic
  for (attempt in 1:max_retries) {
    result <- tryCatch(
      {
        attempt_write()
      },
      error = function(e) {
        if (is_db_locked(e)) {
          # Database is locked
          logger::log_warn("Database locked on attempt {attempt}/{max_retries} for table '{table_name}'",
                          namespace = "Tdata")

          if (attempt < max_retries) {
            # Retry after delay
            Sys.sleep(retry_delay)
            return(NULL)  # Signal to retry
          } else {
            # Final attempt failed - notify user
            error_msg <- paste0(
              "Database is locked and cannot be accessed.\n\n",
              "The database file is currently in use by another application.\n",
              "Please close any other R sessions or applications that may be accessing the database.\n\n",
              "Table: ", table_name, "\n",
              "Operation: ", if(append) "APPEND" else "WRITE", "\n\n",
              "Click OK after closing other applications to retry, or Cancel to abort."
            )

            # Display message to user
            Tbasics::display_error_message(error_msg)

            # Give user time to close other apps, then retry once more
            logger::log_info("Waiting for user to resolve DB lock...", namespace = "Tdata")
            Sys.sleep(3)

            # Final retry
            return(tryCatch(
              attempt_write(),
              error = function(e2) {
                # Still locked - give up
                logger::log_error("Database still locked after user intervention: {e2$message}",
                                namespace = "Tdata")
                stop(paste0("Database remains locked. Please ensure no other applications are using the database.\n",
                           "Error: ", e2$message))
              }
            ))
          }
        } else {
          # Different error - propagate immediately
          logger::log_error("Database write error for table '{table_name}': {e$message}",
                          namespace = "Tdata")
          stop(e)
        }
      }
    )

    # Check if we got a result (not NULL from retry signal)
    if (!is.null(result)) {
      return(result)
    }
  }

  # Should not reach here, but safety fallback
  stop("Database write failed after all retries")
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
