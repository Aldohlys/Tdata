#' Migrate TestTrades Table to DateTime Precision
#'
#' Adds DateTime and TimeZoneSource columns to the TestTrades table, positioned
#' right after TradeDate, and migrates existing TradeDate data.
#'
#' This migration is IDEMPOTENT - safe to run multiple times. It will:
#' 1. Create new table structure with DateTime/TimeZoneSource after TradeDate
#' 2. Copy all existing data
#' 3. Migrate TradeDate to DateTime (as 00:00:00 UTC)
#' 4. Replace old table with new table
#'
#' @param backup Logical. If TRUE, creates a backup before migration. Default TRUE.
#' @return List with migration results: success, rows_migrated, backup_table
#' @export
#'
#' @examples
#' \dontrun{
#' # Run migration with backup
#' result <- migrate_test_trades_to_datetime()
#' }
migrate_test_trades_to_datetime <- function(backup = TRUE) {

  logger::log_info("Starting TestTrades table DateTime migration with column reordering",
                  namespace = "Tdata")

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))

  # Check if table exists
  if (!DBI::dbExistsTable(conn, "TestTrades")) {
    logger::log_error("TestTrades table does not exist", namespace = "Tdata")
    stop("TestTrades table not found")
  }

  # Create backup if requested
  backup_table <- NULL
  if (backup) {
    backup_table <- sprintf("TestTrades_backup_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
    logger::log_info("Creating backup table: {backup_table}", namespace = "Tdata")

    DBI::dbExecute(conn, sprintf("CREATE TABLE %s AS SELECT * FROM TestTrades", backup_table))
    backup_count <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) as n FROM %s", backup_table))$n
    logger::log_info("Backup created: {backup_count} rows", namespace = "Tdata")
  }

  # Get current schema
  schema <- DBI::dbGetQuery(conn, "PRAGMA table_info(TestTrades)")
  columns <- schema$name

  # Check if already migrated
  has_datetime <- "DateTime" %in% columns
  has_timezone <- "TimeZoneSource" %in% columns

  if (has_datetime && has_timezone) {
    logger::log_info("TestTrades already has DateTime columns", namespace = "Tdata")
    return(list(
      success = TRUE,
      total_records = DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM TestTrades")$n,
      message = "Already migrated",
      backup_table = NULL
    ))
  }

  # Define new column order with DateTime and TimeZoneSource after TradeDate
  other_cols <- setdiff(columns, c("TradeNr", "Account", "TradeDate"))
  new_order <- c("TradeNr", "Account", "TradeDate", "DateTime", "TimeZoneSource", other_cols)

  # Create new table with desired column order
  logger::log_info("Creating new table structure with reordered columns", namespace = "Tdata")

  # Build CREATE TABLE statement (copy structure, no data)
  create_sql <- sprintf("CREATE TABLE TestTrades_new AS SELECT %s FROM TestTrades WHERE 0",
                       paste(sprintf("`%s`", columns), collapse = ", "))
  DBI::dbExecute(conn, create_sql)

  # Add DateTime and TimeZoneSource columns
  DBI::dbExecute(conn, "ALTER TABLE TestTrades_new ADD COLUMN DateTime TEXT")
  DBI::dbExecute(conn, "ALTER TABLE TestTrades_new ADD COLUMN TimeZoneSource TEXT")

  # Copy data from old table with DateTime migration
  insert_sql <- sprintf("
    INSERT INTO TestTrades_new (%s, DateTime, TimeZoneSource)
    SELECT %s,
           CASE
             WHEN TradeDate IS NOT NULL THEN
               substr(CAST(TradeDate AS TEXT), 1, 4) || '-' ||
               substr(CAST(TradeDate AS TEXT), 5, 2) || '-' ||
               substr(CAST(TradeDate AS TEXT), 7, 2) || ' 00:00:00'
             ELSE NULL
           END as DateTime,
           CASE
             WHEN TradeDate IS NOT NULL THEN 'UTC_migrated'
             ELSE NULL
           END as TimeZoneSource
    FROM TestTrades
  ", paste(sprintf("`%s`", columns), collapse = ", "),
     paste(sprintf("`%s`", columns), collapse = ", "))

  logger::log_info("Copying data with DateTime migration", namespace = "Tdata")
  DBI::dbExecute(conn, insert_sql)

  # Verify copy
  old_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM TestTrades")$n
  new_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM TestTrades_new")$n

  if (old_count != new_count) {
    logger::log_error("Row count mismatch: old={old_count}, new={new_count}", namespace = "Tdata")
    stop("Migration failed: row count mismatch")
  }

  # Drop old table and rename new table
  logger::log_info("Replacing old table with new structure", namespace = "Tdata")
  DBI::dbExecute(conn, "DROP TABLE TestTrades")
  DBI::dbExecute(conn, "ALTER TABLE TestTrades_new RENAME TO TestTrades")

  # Get final schema to verify
  final_schema <- DBI::dbGetQuery(conn, "PRAGMA table_info(TestTrades)")
  logger::log_info("Migration complete. New column order: {paste(final_schema$name, collapse=', ')}",
                  namespace = "Tdata")

  result <- list(
    success = TRUE,
    total_records = new_count,
    migrated_records = new_count,
    backup_table = backup_table,
    column_order = final_schema$name
  )

  return(result)
}


#' Reorder Trades Table Columns
#'
#' Rearranges existing Trades table columns to put DateTime and TimeZoneSource
#' right after TradeDate. Only runs if columns are not already in correct order.
#'
#' @param backup Logical. If TRUE, creates a backup before reordering. Default TRUE.
#' @return List with results: success, reordered (TRUE if columns were reordered)
#' @export
#'
#' @examples
#' \dontrun{
#' result <- reorder_trades_columns()
#' }
reorder_trades_columns <- function(backup = TRUE) {

  logger::log_info("Checking Trades table column order", namespace = "Tdata")

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))

  if (!DBI::dbExistsTable(conn, "Trades")) {
    logger::log_error("Trades table does not exist", namespace = "Tdata")
    stop("Trades table not found")
  }

  # Get current schema
  schema <- DBI::dbGetQuery(conn, "PRAGMA table_info(Trades)")
  columns <- schema$name

  # Check if DateTime and TimeZoneSource exist
  if (!"DateTime" %in% columns || !"TimeZoneSource" %in% columns) {
    logger::log_error("DateTime or TimeZoneSource columns missing. Run migrate_trades_to_datetime() first",
                     namespace = "Tdata")
    stop("Migration required before reordering")
  }

  # Check if already in correct order
  tradedate_idx <- which(columns == "TradeDate")
  datetime_idx <- which(columns == "DateTime")
  timezone_idx <- which(columns == "TimeZoneSource")

  if (datetime_idx == tradedate_idx + 1 && timezone_idx == tradedate_idx + 2) {
    logger::log_info("Columns already in correct order", namespace = "Tdata")
    return(list(success = TRUE, reordered = FALSE, message = "Already in correct order"))
  }

  # Create backup if requested
  backup_table <- NULL
  if (backup) {
    backup_table <- sprintf("Trades_backup_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
    logger::log_info("Creating backup table: {backup_table}", namespace = "Tdata")
    DBI::dbExecute(conn, sprintf("CREATE TABLE %s AS SELECT * FROM Trades", backup_table))
  }

  # Define desired column order
  other_cols <- setdiff(columns, c("TradeNr", "Account", "TradeDate", "DateTime", "TimeZoneSource"))
  new_order <- c("TradeNr", "Account", "TradeDate", "DateTime", "TimeZoneSource", other_cols)

  logger::log_info("Reordering columns to: {paste(new_order[1:7], collapse=', ')}...",
                  namespace = "Tdata")

  # Create new table with correct order
  select_cols <- paste(sprintf("`%s`", new_order), collapse = ", ")
  DBI::dbExecute(conn, sprintf("CREATE TABLE Trades_new AS SELECT %s FROM Trades", select_cols))

  # Verify
  old_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Trades")$n
  new_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Trades_new")$n

  if (old_count != new_count) {
    logger::log_error("Row count mismatch during reorder", namespace = "Tdata")
    stop("Reordering failed: row count mismatch")
  }

  # Replace old table
  DBI::dbExecute(conn, "DROP TABLE Trades")
  DBI::dbExecute(conn, "ALTER TABLE Trades_new RENAME TO Trades")

  logger::log_info("Column reordering complete", namespace = "Tdata")

  return(list(
    success = TRUE,
    reordered = TRUE,
    backup_table = backup_table,
    new_order = new_order
  ))
}
