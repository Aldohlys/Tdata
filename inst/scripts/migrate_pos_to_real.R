#!/usr/bin/env Rscript
#' Database Schema Migration: Change Pos/pos from INTEGER to REAL
#'
#' This script migrates the database schema to support fractional positions:
#' - Trades.Pos: INTEGER -> REAL
#' - Portfolio tables (U*, DU*).pos: INTEGER -> REAL
#'
#' CRITICAL: Creates backup before migration
#'
#' @usage
#' Rscript migrate_pos_to_real.R [--dry-run] [--db-path=/path/to/db]

library(DBI)
library(RSQLite)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
db_path <- grep("^--db-path=", args, value = TRUE)

if (length(db_path) > 0) {
  db_path <- sub("^--db-path=", "", db_path)
} else {
  db_path <- Sys.getenv("R_DB_PATH")
  if (db_path == "") {
    stop("Database path not provided. Set R_DB_PATH or use --db-path=")
  }
}

cat("Database Schema Migration: Pos/pos INTEGER -> REAL\n")
cat("===============================================\n\n")
cat("Database:", db_path, "\n")
cat("Dry run:", dry_run, "\n\n")

# Verify database exists
if (!file.exists(db_path)) {
  stop("Database not found: ", db_path)
}

#' Get all portfolio table names (U*, DU* pattern)
get_portfolio_tables <- function(conn) {
  tables <- DBI::dbListTables(conn)
  portfolio_tables <- grep("^(U|DU)\\d+$", tables, value = TRUE)
  return(portfolio_tables)
}

#' Migrate a single table's position column
#'
#' SQLite doesn't support ALTER COLUMN TYPE directly, so we need to:
#' 1. Create new table with correct schema
#' 2. Copy data
#' 3. Drop old table
#' 4. Rename new table
migrate_table <- function(conn, table_name, pos_column) {
  cat("\n--- Migrating table:", table_name, "---\n")

  # Get current schema
  schema_query <- sprintf("SELECT sql FROM sqlite_master WHERE type='table' AND name='%s'", table_name)
  original_schema <- DBI::dbGetQuery(conn, schema_query)$sql

  if (length(original_schema) == 0) {
    cat("⚠️  Table not found:", table_name, "\n")
    return(FALSE)
  }

  cat("Original schema:\n", original_schema, "\n\n")

  # Check if column is already REAL
  if (grepl(sprintf("%s\\s+(REAL|NUMERIC|FLOAT|DOUBLE)", pos_column), original_schema, ignore.case = TRUE)) {
    cat("✅ Column", pos_column, "already REAL/NUMERIC, skipping\n")
    return(TRUE)
  }

  # Create new schema with REAL type
  # Handle both quoted and unquoted column names, various whitespace
  new_schema <- gsub(
    sprintf("([`\"]?%s[`\"]?)\\s+INTEGER", pos_column),
    "\\1 REAL",
    original_schema,
    ignore.case = TRUE
  )

  # Replace table name for temporary table
  # Handle both quoted and unquoted table names
  new_schema <- gsub(
    sprintf("CREATE TABLE ([`\"]?)%s([`\"]?)", table_name),
    sprintf("CREATE TABLE \\1%s_new\\2", table_name),
    new_schema
  )

  cat("New schema:\n", new_schema, "\n\n")

  if (!dry_run) {
    # Begin transaction
    DBI::dbExecute(conn, "BEGIN TRANSACTION")

    tryCatch({
      # Create new table
      cat("Creating new table with REAL type...\n")
      DBI::dbExecute(conn, new_schema)

      # Copy data (REAL conversion happens automatically)
      copy_query <- sprintf(
        "INSERT INTO %s_new SELECT * FROM %s",
        table_name, table_name
      )
      cat("Copying data...\n")
      rows_copied <- DBI::dbExecute(conn, copy_query)
      cat("Copied", rows_copied, "rows\n")

      # Verify row count matches
      old_count <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) as n FROM %s", table_name))$n
      new_count <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) as n FROM %s_new", table_name))$n

      if (old_count != new_count) {
        stop("Row count mismatch! Old: ", old_count, ", New: ", new_count)
      }

      # Drop old table
      cat("Dropping old table...\n")
      DBI::dbExecute(conn, sprintf("DROP TABLE %s", table_name))

      # Rename new table
      cat("Renaming new table...\n")
      DBI::dbExecute(conn, sprintf("ALTER TABLE %s_new RENAME TO %s", table_name, table_name))

      # Commit transaction
      DBI::dbExecute(conn, "COMMIT")
      cat("✅ Migration successful\n")
      return(TRUE)

    }, error = function(e) {
      # Rollback on error
      DBI::dbExecute(conn, "ROLLBACK")
      cat("❌ Migration failed:", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("DRY RUN: Would execute migration\n")
    return(TRUE)
  }
}

#' Main migration function
main <- function() {
  # Create backup
  if (!dry_run) {
    backup_path <- paste0(db_path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    cat("Creating backup:", backup_path, "\n")
    file.copy(db_path, backup_path)
    cat("✅ Backup created\n\n")
  }

  # Connect to database
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Disable foreign keys during migration
  if (!dry_run) {
    DBI::dbExecute(conn, "PRAGMA foreign_keys = OFF")
  }

  success <- TRUE

  # Migrate Trades table (Pos column)
  cat("\n=== Migrating Trades Table ===\n")
  if (DBI::dbExistsTable(conn, "Trades")) {
    success <- migrate_table(conn, "Trades", "Pos") && success
  } else {
    cat("⚠️  Trades table not found\n")
  }

  # Migrate portfolio tables (pos column)
  cat("\n=== Migrating Portfolio Tables ===\n")
  portfolio_tables <- get_portfolio_tables(conn)
  cat("Found", length(portfolio_tables), "portfolio tables:", paste(portfolio_tables, collapse=", "), "\n")

  for (table in portfolio_tables) {
    success <- migrate_table(conn, table, "pos") && success
  }

  # Re-enable foreign keys
  if (!dry_run) {
    DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  }

  # Summary
  cat("\n===============================================\n")
  cat("Migration Summary:\n")
  cat("Status:", if (success) "✅ SUCCESS" else "❌ FAILED", "\n")

  if (!dry_run) {
    # Verify schema changes
    cat("\n--- Verification ---\n")

    # Check Trades.Pos
    trades_schema <- DBI::dbGetQuery(conn, "SELECT sql FROM sqlite_master WHERE name='Trades'")$sql
    if (grepl("Pos\\s+REAL", trades_schema, ignore.case = TRUE)) {
      cat("✅ Trades.Pos is now REAL\n")
    } else {
      cat("❌ Trades.Pos migration may have failed\n")
    }

    # Check first portfolio table
    if (length(portfolio_tables) > 0) {
      portfolio_schema <- DBI::dbGetQuery(conn,
        sprintf("SELECT sql FROM sqlite_master WHERE name='%s'", portfolio_tables[1]))$sql
      if (grepl("pos\\s+REAL", portfolio_schema, ignore.case = TRUE)) {
        cat("✅ Portfolio pos is now REAL\n")
      } else {
        cat("❌ Portfolio pos migration may have failed\n")
      }
    }
  } else {
    cat("\nDRY RUN: No changes made to database\n")
    cat("Run without --dry-run to execute migration\n")
  }

  return(success)
}

# Execute migration
result <- main()
quit(status = if (result) 0 else 1)
