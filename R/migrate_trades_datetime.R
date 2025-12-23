#' Migrate Trades Table to DateTime Precision
#'
#' Adds DateTime and TimeZoneSource columns to the Trades table and migrates
#' existing TradeDate (YYYYMMDD integer) data to DateTime (UTC timestamp).
#'
#' This migration is IDEMPOTENT - safe to run multiple times. It will:
#' 1. Add DateTime column (TEXT) if it doesn't exist
#' 2. Add TimeZoneSource column (TEXT) if it doesn't exist
#' 3. Migrate existing TradeDate data to DateTime (as 00:00:00 UTC)
#' 4. Keep TradeDate column for backward compatibility
#'
#' @param backup Logical. If TRUE, creates a backup of Trades table before migration. Default TRUE.
#' @return List with migration results: success, rows_migrated, backup_table
#' @export
#'
#' @examples
#' \dontrun{
#' # Run migration with backup
#' result <- migrate_trades_to_datetime()
#'
#' # Run without backup (not recommended)
#' result <- migrate_trades_to_datetime(backup = FALSE)
#' }
migrate_trades_to_datetime <- function(backup = TRUE) {

  logger::log_info("Starting Trades table DateTime migration", namespace = "Tdata")

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))

  # Check if table exists
  if (!DBI::dbExistsTable(conn, "Trades")) {
    logger::log_error("Trades table does not exist", namespace = "Tdata")
    stop("Trades table not found")
  }

  # Create backup if requested
  backup_table <- NULL
  if (backup) {
    backup_table <- sprintf("Trades_backup_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
    logger::log_info("Creating backup table: {backup_table}", namespace = "Tdata")

    DBI::dbExecute(conn, sprintf("CREATE TABLE %s AS SELECT * FROM Trades", backup_table))

    backup_count <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) as n FROM %s", backup_table))$n
    logger::log_info("Backup created: {backup_count} rows", namespace = "Tdata")
  }

  # Get current schema
  schema <- DBI::dbGetQuery(conn, "PRAGMA table_info(Trades)")

  # Check if DateTime column already exists
  has_datetime <- "DateTime" %in% schema$name
  has_timezone <- "TimeZoneSource" %in% schema$name

  if (has_datetime && has_timezone) {
    logger::log_info("DateTime columns already exist, checking for unmigrated records",
                    namespace = "Tdata")
  }

  # Add DateTime column if it doesn't exist
  if (!has_datetime) {
    logger::log_info("Adding DateTime column to Trades table", namespace = "Tdata")
    DBI::dbExecute(conn, "ALTER TABLE Trades ADD COLUMN DateTime TEXT")
  }

  # Add TimeZoneSource column if it doesn't exist
  if (!has_timezone) {
    logger::log_info("Adding TimeZoneSource column to Trades table", namespace = "Tdata")
    DBI::dbExecute(conn, "ALTER TABLE Trades ADD COLUMN TimeZoneSource TEXT")
  }

  # Migrate TradeDate to DateTime for records where DateTime is NULL
  logger::log_info("Migrating TradeDate data to DateTime format", namespace = "Tdata")

  # Count records to migrate
  unmigrated <- DBI::dbGetQuery(conn,
    "SELECT COUNT(*) as n FROM Trades WHERE DateTime IS NULL AND TradeDate IS NOT NULL")$n

  if (unmigrated > 0) {
    logger::log_info("Migrating {unmigrated} records from TradeDate to DateTime",
                    namespace = "Tdata")

    # Update DateTime from TradeDate (convert YYYYMMDD to YYYY-MM-DD 00:00:00)
    # SQLite substr function: substr(string, start, length) - 1-indexed
    DBI::dbExecute(conn, "
      UPDATE Trades
      SET DateTime = substr(CAST(TradeDate AS TEXT), 1, 4) || '-' ||
                     substr(CAST(TradeDate AS TEXT), 5, 2) || '-' ||
                     substr(CAST(TradeDate AS TEXT), 7, 2) || ' 00:00:00',
          TimeZoneSource = 'UTC_migrated'
      WHERE DateTime IS NULL AND TradeDate IS NOT NULL
    ")

    # Verify migration
    migrated <- DBI::dbGetQuery(conn,
      "SELECT COUNT(*) as n FROM Trades WHERE DateTime IS NOT NULL")$n

    logger::log_info("Migration complete: {migrated} records now have DateTime",
                    namespace = "Tdata")
  } else {
    logger::log_info("No records to migrate (all have DateTime or TradeDate is NULL)",
                    namespace = "Tdata")
  }

  # Get final counts
  total <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Trades")$n
  with_datetime <- DBI::dbGetQuery(conn,
    "SELECT COUNT(*) as n FROM Trades WHERE DateTime IS NOT NULL")$n

  result <- list(
    success = TRUE,
    total_records = total,
    migrated_records = with_datetime,
    backup_table = backup_table,
    datetime_column_added = !has_datetime,
    timezone_column_added = !has_timezone
  )

  logger::log_info("Migration summary: {total} total, {with_datetime} with DateTime",
                  namespace = "Tdata")

  if (!is.null(backup_table)) {
    logger::log_info("Backup table: {backup_table}", namespace = "Tdata")
  }

  return(result)
}


#' Rollback Trades DateTime Migration
#'
#' Restores Trades table from a backup created during migration.
#'
#' @param backup_table String. Name of backup table to restore from.
#' @return Logical. TRUE if rollback successful.
#' @export
#'
#' @examples
#' \dontrun{
#' # Rollback to specific backup
#' rollback_trades_datetime_migration("Trades_backup_20251219_123456")
#' }
rollback_trades_datetime_migration <- function(backup_table) {

  logger::log_warn("Rolling back Trades table from backup: {backup_table}",
                  namespace = "Tdata")

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn))

  # Verify backup exists
  if (!DBI::dbExistsTable(conn, backup_table)) {
    logger::log_error("Backup table does not exist: {backup_table}", namespace = "Tdata")
    stop(sprintf("Backup table not found: %s", backup_table))
  }

  # Drop current Trades table
  logger::log_warn("Dropping current Trades table", namespace = "Tdata")
  DBI::dbExecute(conn, "DROP TABLE Trades")

  # Restore from backup
  logger::log_info("Restoring from backup", namespace = "Tdata")
  DBI::dbExecute(conn, sprintf("CREATE TABLE Trades AS SELECT * FROM %s", backup_table))

  # Verify restoration
  restored_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Trades")$n
  logger::log_info("Rollback complete: {restored_count} records restored",
                  namespace = "Tdata")

  return(TRUE)
}
