
#####  alert.R
##### All utilities related to alert management

#' createAlertTable
#'
#' Creates the Alerts table in the database if it does not already exist.
#' The table stores themed alerts with asset, date, description, and active status.
#'
#'@return NULL (invisible). Side effect: creates the Alerts table in DB.
#'@examples
#'\dontrun{
#'createAlertTable()
#'}
#'@export
createAlertTable <- function() {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Alerts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      Theme TEXT NOT NULL,
      Asset TEXT NOT NULL,
      AlertDate TEXT NOT NULL,
      Description TEXT,
      Active INTEGER DEFAULT 1,
      CreatedAt TEXT NOT NULL
    )
  ")

  print("Alerts table created (or already exists)")
  invisible(NULL)
}

#' getAllAlerts
#'
#' Retrieves all alerts from the database, optionally filtered to active only.
#'
#'@param active_only Logical. If TRUE (default), returns only active alerts (Active = 1).
#'  If FALSE, returns all alerts regardless of status.
#'@return A data frame with columns: id, Theme, Asset, AlertDate, Description, Active, CreatedAt,
#'  sorted by AlertDate ascending.
#'@examples
#'\dontrun{
#'getAllAlerts()
#'getAllAlerts(active_only = FALSE)
#'}
#'@export
getAllAlerts <- function(active_only = TRUE) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (active_only) {
    alerts <- DBI::dbGetQuery(conn,
      "SELECT * FROM Alerts WHERE Active = 1 ORDER BY AlertDate ASC")
  } else {
    alerts <- DBI::dbGetQuery(conn,
      "SELECT * FROM Alerts ORDER BY AlertDate ASC")
  }

  return(alerts)
}

#' addAlert
#'
#' Adds a new alert to the Alerts table.
#'
#'@param theme Character. One of: "Earnings", "Macro", "Expiration", "Technical".
#'@param asset Character. The asset symbol or name. Can be empty for non-asset alerts (e.g. FOMC).
#'@param alert_date Character or Date. The alert date in ISO format (YYYY-MM-DD).
#'@param description Character. Optional description text. Default is empty string.
#'@return The data frame that was appended (one row).
#'@examples
#'\dontrun{
#'addAlert("Earnings", "AAPL", "2025-01-15", "Q1 earnings release")
#'addAlert("Technical", "SPY", Sys.Date() + 7, "RSI oversold level")
#'}
#'@export
addAlert <- function(theme, asset = "", alert_date, description = "") {
  valid_themes <- c("Earnings", "Macro", "Expiration", "Technical")
  if (!theme %in% valid_themes) {
    stop(paste("Invalid theme:", theme, "- must be one of:", paste(valid_themes, collapse = ", ")))
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  df <- data.frame(
    Theme = theme,
    Asset = asset,
    AlertDate = as.character(alert_date),
    Description = description,
    Active = 1L,
    CreatedAt = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  result <- safe_db_append(conn, "Alerts", df)
  print(paste("Alert rows added:", result))
  return(df)
}

#' removeAlert
#'
#' Permanently deletes an alert from the database by id.
#'
#'@param id Integer. The alert id to delete.
#'@return The number of rows deleted (should be 1 on success, 0 if not found).
#'@examples
#'\dontrun{
#'removeAlert(5)
#'}
#'@export
removeAlert <- function(id) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- DBI::dbExecute(conn,
    "DELETE FROM Alerts WHERE id = ?",
    params = list(id))

  print(paste("Alert rows deleted:", result))
  return(result)
}

#' dismissAlert
#'
#' Dismisses an alert by setting its Active status to 0. The alert remains in the database
#' but will no longer appear in active queries.
#'
#'@param id Integer. The alert id to dismiss.
#'@return The number of rows updated (should be 1 on success, 0 if not found).
#'@examples
#'\dontrun{
#'dismissAlert(3)
#'}
#'@export
dismissAlert <- function(id) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  query <- DBI::dbSendStatement(conn,
    "UPDATE Alerts SET Active = 0 WHERE id = ?")
  DBI::dbBind(query, list(id))
  rows_affected <- DBI::dbGetRowsAffected(query)
  DBI::dbClearResult(query)

  print(paste("Alert rows dismissed:", rows_affected))
  return(rows_affected)
}

#' getUpcomingAlerts
#'
#' Retrieves active alerts that fall within a specified number of days from today.
#' Useful for email notification scripts.
#'
#'@param days_ahead Integer. Number of days ahead to look for alerts. Default is 1.
#'@return A data frame of upcoming active alerts sorted by AlertDate ASC.
#'@examples
#'\dontrun{
#'getUpcomingAlerts()           # alerts for today and tomorrow
#'getUpcomingAlerts(days_ahead = 7)  # alerts within the next week
#'}
#'@export
getUpcomingAlerts <- function(days_ahead = 1) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  today <- as.character(Sys.Date())
  future <- as.character(Sys.Date() + days_ahead)

  alerts <- DBI::dbGetQuery(conn,
    "SELECT * FROM Alerts WHERE Active = 1 AND AlertDate >= ? AND AlertDate <= ? ORDER BY AlertDate ASC",
    params = list(today, future))

  return(alerts)
}
