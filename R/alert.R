
#####  alert.R
##### All utilities related to alert management

## Themes accepted by addAlert: the original four plus every theme the table
## already held when the whitelist was last reviewed (2026-10-26).
ALERT_THEMES <- c("Earnings", "Macro", "Expiration", "Technical", "Breakout", "Covenant",
                  "Event", "Momentum", "Pyramid", "Roll", "Stop", "Thesis", "Volatility",
                  "Allocation")

## How an alert fires. Date: when AlertDate is near (getUpcomingAlerts).
## Condition: when its Condition is met (getTriggeredAlerts), AlertDate is only a
## reference. Both: either way.
ALERT_TRIGGER_TYPES <- c("Date", "Condition", "Both")

## Adds the TriggerType and Condition columns to an Alerts table created before
## condition alerts existed. Rows already there read as date alerts.
alert_ensure_columns <- function(conn) {
  cols <- DBI::dbListFields(conn, "Alerts")
  if (!"TriggerType" %in% cols)
    DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN TriggerType TEXT DEFAULT 'Date'")
  if (!"Condition" %in% cols)
    DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN Condition TEXT")
  invisible(NULL)
}

#' createAlertTable
#'
#' Creates the Alerts table in the database if it does not already exist.
#' The table stores themed alerts with asset, date, description, and active status,
#' plus how the alert fires (\code{TriggerType}) and, for condition alerts, the
#' condition itself as JSON (\code{Condition}). An existing table without these
#' two columns gets them added.
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
      CreatedAt TEXT NOT NULL,
      TriggerType TEXT DEFAULT 'Date',
      Condition TEXT
    )
  ")
  alert_ensure_columns(conn)

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
#' A condition alert carries a JSON condition evaluated each morning by
#' \code{getTriggeredAlerts} against the last Yahoo daily close. Grammar:
#' \itemize{
#'   \item price: \code{{"yahoo": "NUCL.L", "close_lte": 41.9}} and/or \code{"close_gte"};
#'     both keys in one node must both hold.
#'   \item date: \code{{"date_gte": "2026-07-21"}}, met from that date on.
#'   \item Gonet weight: \code{{"gonet_weight": "PM_15606539", "weight_lte": 0.09}} and/or
#'     \code{"weight_gte"}: the symbol's share of the Gonet NLV in the last Gonet snapshot,
#'     as a fraction.
#'   \item composition: \code{{"all_of": [...]}} or \code{{"any_of": [...]}} over any of the above.
#' }
#'
#'@param theme Character. One of \code{ALERT_THEMES}: "Earnings", "Macro", "Expiration",
#'  "Technical", "Breakout", "Covenant", "Event", "Momentum", "Pyramid", "Roll", "Stop",
#'  "Thesis", "Volatility".
#'@param asset Character. The asset symbol or name. Can be empty for non-asset alerts (e.g. FOMC).
#'@param alert_date Character or Date. The alert date in ISO format (YYYY-MM-DD). For a
#'  "Condition" alert it is a reference date only.
#'@param description Character. Optional description text. Default is empty string.
#'@param trigger_type Character. "Date" (default), "Condition" or "Both".
#'@param condition List or JSON string, required for "Condition" and "Both", refused for "Date".
#'@return The data frame that was appended (one row).
#'@examples
#'\dontrun{
#'addAlert("Earnings", "AAPL", "2025-01-15", "Q1 earnings release")
#'addAlert("Technical", "SPY", Sys.Date() + 7, "RSI oversold level")
#'addAlert("Stop", "NUCL", Sys.Date(), "Close below 41.90", trigger_type = "Condition",
#'         condition = list(yahoo = "NUCL.L", close_lte = 41.9))
#'}
#'@export
addAlert <- function(theme, asset = "", alert_date, description = "",
                     trigger_type = "Date", condition = NULL) {
  if (!theme %in% ALERT_THEMES) {
    stop(paste("Invalid theme:", theme, "- must be one of:", paste(ALERT_THEMES, collapse = ", ")))
  }
  if (!trigger_type %in% ALERT_TRIGGER_TYPES) {
    stop(paste("Invalid trigger_type:", trigger_type, "- must be one of:",
               paste(ALERT_TRIGGER_TYPES, collapse = ", ")))
  }
  if (trigger_type == "Date" && !is.null(condition))
    stop("A condition was given for a Date alert - use trigger_type 'Condition' or 'Both'")
  if (trigger_type != "Date" && is.null(condition))
    stop(paste0("trigger_type '", trigger_type, "' needs a condition"))

  condition_json <- NA_character_
  if (!is.null(condition)) {
    condition_json <- as.character(jsonlite::toJSON(alert_condition_parse(condition),
                                                    auto_unbox = TRUE, digits = NA))
  }

  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  alert_ensure_columns(conn)

  df <- data.frame(
    Theme = theme,
    Asset = asset,
    AlertDate = as.character(alert_date),
    Description = description,
    Active = 1L,
    CreatedAt = as.character(Sys.time()),
    TriggerType = trigger_type,
    Condition = condition_json,
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
#' Useful for email notification scripts. Alerts with TriggerType "Condition" are left
#' out: their AlertDate is a reference, they fire through \code{getTriggeredAlerts}.
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
  alert_ensure_columns(conn)

  today <- as.character(Sys.Date())
  future <- as.character(Sys.Date() + days_ahead)

  alerts <- DBI::dbGetQuery(conn,
    "SELECT * FROM Alerts WHERE Active = 1 AND AlertDate >= ? AND AlertDate <= ?
       AND (TriggerType IS NULL OR TriggerType <> 'Condition') ORDER BY AlertDate ASC",
    params = list(today, future))

  return(alerts)
}

#' getTriggeredAlerts
#'
#' Evaluates the condition of every active "Condition" or "Both" alert and returns
#' those whose condition is met now. Prices are the last daily close from Yahoo,
#' fetched once per symbol, so the check needs no TWS connection; at the 07:30 run
#' this is the previous session's close. Gonet weights come from the last Gonet
#' snapshot, read once and only when an alert uses them.
#'
#' A condition that cannot be evaluated (Yahoo returned nothing, invalid JSON) is
#' neither met nor dropped silently: those alerts are returned in the
#' \code{"unevaluated"} attribute, with the reason in \code{ConditionDetail}.
#' In \code{all_of}, one leaf that is false makes the node false even when another
#' could not be evaluated; in \code{any_of}, one true leaf makes it true.
#'
#'@param fetch Function taking a Yahoo symbol and returning \code{list(price, asof)}
#'  or NULL. Default: last non-missing Yahoo close of the past 14 days.
#'@param today Date used by \code{date_gte} conditions. Default \code{Sys.Date()}.
#'@param weights Function returning \code{list(w, asof)}, \code{w} the NLV share of each
#'  symbol of the last Gonet snapshot, or NULL. Default \code{alert_gonet_weights}.
#'@return A data frame of the triggered alerts with an added \code{ConditionDetail}
#'  column (one clause per condition leaf, e.g. "NUCL.L close 41.5 (20261027) <= 41.9: TRUE"),
#'  and attribute \code{"unevaluated"} holding the alerts that could not be evaluated.
#'@examples
#'\dontrun{
#'getTriggeredAlerts()
#'attr(getTriggeredAlerts(), "unevaluated")
#'}
#'@export
getTriggeredAlerts <- function(fetch = gonet_fetch_yahoo_close, today = Sys.Date(),
                               weights = alert_gonet_weights) {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  alert_ensure_columns(conn)

  alerts <- DBI::dbGetQuery(conn,
    "SELECT * FROM Alerts WHERE Active = 1 AND TriggerType IN ('Condition', 'Both')
       AND Condition IS NOT NULL ORDER BY id ASC")
  alerts$ConditionDetail <- rep(NA_character_, nrow(alerts))
  if (nrow(alerts) == 0) {
    attr(alerts, "unevaluated") <- alerts
    return(alerts)
  }

  conds <- lapply(seq_len(nrow(alerts)), function(i) tryCatch(
    alert_condition_parse(alerts$Condition[i]),
    error = function(e) {
      logger::log_warn("Alert {alerts$id[i]}: invalid Condition - {conditionMessage(e)}", namespace = "Tdata")
      NULL
    }))

  syms <- unique(unlist(lapply(Filter(Negate(is.null), conds), alert_condition_symbols)))
  closes <- stats::setNames(lapply(syms, function(s) tryCatch(fetch(s), error = function(e) {
    logger::log_warn("Alert condition: Yahoo fetch failed for {s}: {conditionMessage(e)}", namespace = "Tdata")
    NULL
  })), syms)

  gonet <- NULL
  if (any(vapply(Filter(Negate(is.null), conds), alert_condition_uses_gonet, logical(1)))) {
    gonet <- tryCatch(weights(), error = function(e) {
      logger::log_warn("Alert condition: Gonet weights not read: {conditionMessage(e)}", namespace = "Tdata")
      NULL
    })
  }

  met <- rep(NA, nrow(alerts))
  for (i in seq_len(nrow(alerts))) {
    if (is.null(conds[[i]])) {
      alerts$ConditionDetail[i] <- "invalid Condition JSON"
      next
    }
    ev <- alert_condition_eval(conds[[i]], closes, today, gonet)
    met[i] <- ev$met
    alerts$ConditionDetail[i] <- paste(ev$detail, collapse = "; ")
  }

  unevaluated <- alerts[is.na(met), , drop = FALSE]
  for (i in seq_len(nrow(unevaluated)))
    logger::log_warn("Alert {unevaluated$id[i]} ({unevaluated$Asset[i]}) not evaluated: {unevaluated$ConditionDetail[i]}",
                     namespace = "Tdata")

  out <- alerts[met %in% TRUE, , drop = FALSE]
  attr(out, "unevaluated") <- unevaluated
  out
}

## Turns a condition given as a JSON string or an R list into a list and checks
## it against the grammar documented in addAlert. Stops on anything else, so a
## typo is refused when the alert is written rather than ignored every morning.
alert_condition_parse <- function(condition) {
  node <- if (is.character(condition)) {
    if (length(condition) != 1 || is.na(condition)) stop("Alert condition: expected one JSON string")
    jsonlite::fromJSON(condition, simplifyVector = FALSE)
  } else condition
  alert_condition_check(node)
  node
}

alert_condition_check <- function(node) {
  keys <- names(node)
  if (!is.list(node) || length(node) == 0 || is.null(keys) || any(!nzchar(keys)))
    stop("Alert condition: each condition must be a non-empty named object")

  comp <- intersect(keys, c("all_of", "any_of"))
  if (length(comp) > 0) {
    if (length(keys) != 1) stop("Alert condition: all_of / any_of must be the only key of its object")
    kids <- node[[comp]]
    if (!is.list(kids) || length(kids) == 0 || !is.null(names(kids)))
      stop(paste0("Alert condition: ", comp, " needs a non-empty list of conditions"))
    for (k in kids) alert_condition_check(k)
    return(invisible(TRUE))
  }

  if ("date_gte" %in% keys) {
    if (length(keys) != 1) stop("Alert condition: date_gte must be the only key of its object")
    d <- tryCatch(as.Date(node$date_gte), error = function(e) NA)
    if (length(d) != 1 || is.na(d)) stop(paste("Alert condition: invalid date_gte", node$date_gte))
    return(invisible(TRUE))
  }

  if ("gonet_weight" %in% keys) {
    if (!is.character(node$gonet_weight) || length(node$gonet_weight) != 1 || !nzchar(node$gonet_weight))
      stop("Alert condition: gonet_weight must be the Gonet symbol")
    tests <- setdiff(keys, "gonet_weight")
    unknown <- setdiff(tests, c("weight_lte", "weight_gte"))
    if (length(unknown) > 0)
      stop(paste("Alert condition: unknown key(s)", paste(unknown, collapse = ", ")))
    if (length(tests) == 0)
      stop("Alert condition: a weight condition needs weight_lte and/or weight_gte")
    for (k in tests) {
      v <- node[[k]]
      if (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v < 0 || v > 1)
        stop(paste("Alert condition:", k, "must be one fraction between 0 and 1"))
    }
    return(invisible(TRUE))
  }

  if (!"yahoo" %in% keys || !is.character(node$yahoo) || length(node$yahoo) != 1 || !nzchar(node$yahoo))
    stop("Alert condition: a price condition needs 'yahoo', the Yahoo symbol")
  tests <- setdiff(keys, "yahoo")
  unknown <- setdiff(tests, c("close_lte", "close_gte"))
  if (length(unknown) > 0)
    stop(paste("Alert condition: unknown key(s)", paste(unknown, collapse = ", ")))
  if (length(tests) == 0)
    stop("Alert condition: a price condition needs close_lte and/or close_gte")
  for (k in tests) {
    v <- node[[k]]
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v))
      stop(paste("Alert condition:", k, "must be one number"))
  }
  invisible(TRUE)
}

## Yahoo symbols read by the price conditions of a parsed condition.
alert_condition_symbols <- function(node) {
  kids <- if (!is.null(node$all_of)) node$all_of else node$any_of
  if (!is.null(kids)) return(unique(unlist(lapply(kids, alert_condition_symbols))))
  if (!is.null(node$yahoo)) return(node$yahoo)
  character()
}

## TRUE when a parsed condition has a Gonet-weight leaf.
alert_condition_uses_gonet <- function(node) {
  kids <- if (!is.null(node$all_of)) node$all_of else node$any_of
  if (!is.null(kids)) return(any(vapply(kids, alert_condition_uses_gonet, logical(1))))
  !is.null(node$gonet_weight)
}

## Share of the Gonet NLV held by each symbol in the last Gonet snapshot:
## list(w = named fractions, asof = "date heure"), or NULL when there is no
## snapshot or a line has no value. mktValue is in each line's own currency
## (cash rows already in base), so every line is converted before summing.
alert_gonet_weights <- function() {
  conn <- safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  last <- DBI::dbGetQuery(conn, "SELECT date, heure FROM Gonet ORDER BY date DESC, heure DESC LIMIT 1")
  if (nrow(last) == 0) return(NULL)
  rows <- DBI::dbGetQuery(conn, "SELECT symbol, mktValue, currency FROM Gonet WHERE date = ? AND heure = ?",
                          params = list(last$date, last$heure))
  base <- convert_to_base_date(rows$mktValue, rows$currency, Sys.Date())
  if (any(is.na(base)) || sum(base) <= 0) return(NULL)
  list(w = stats::setNames(base / sum(base), rows$symbol), asof = paste(last$date, last$heure))
}

## Evaluates a parsed condition. `closes` is a list named by Yahoo symbol of
## list(price, asof), NULL where the fetch failed; `gonet` is the result of
## alert_gonet_weights (NULL when not read). Returns list(met, detail): met is
## TRUE, FALSE or NA (not evaluable), detail one string per leaf.
alert_condition_eval <- function(node, closes, today = Sys.Date(), gonet = NULL) {
  kids <- if (!is.null(node$all_of)) node$all_of else node$any_of
  if (!is.null(kids)) {
    ev  <- lapply(kids, alert_condition_eval, closes = closes, today = today, gonet = gonet)
    met <- vapply(ev, function(e) e$met, logical(1))
    ## all()/any() on TRUE/FALSE/NA give the three-valued result wanted here.
    res <- if (!is.null(node$all_of)) all(met) else any(met)
    return(list(met = res, detail = unlist(lapply(ev, function(e) e$detail))))
  }

  if (!is.null(node$date_gte)) {
    d   <- as.Date(node$date_gte)
    res <- as.Date(today) >= d
    return(list(met = res, detail = sprintf("date %s >= %s: %s", as.Date(today), d, res)))
  }

  if (!is.null(node$gonet_weight)) {
    s <- node$gonet_weight
    w <- if (is.null(gonet)) NA_real_ else unname(gonet$w[s])
    if (length(w) != 1 || is.na(w))
      return(list(met = NA, detail = sprintf("Gonet weight %s: not in the last snapshot", s)))
    checks <- character(); res <- TRUE
    if (!is.null(node$weight_lte)) {
      checks <- c(checks, sprintf("<= %.1f%%", 100 * node$weight_lte)); res <- res && w <= node$weight_lte
    }
    if (!is.null(node$weight_gte)) {
      checks <- c(checks, sprintf(">= %.1f%%", 100 * node$weight_gte)); res <- res && w >= node$weight_gte
    }
    return(list(met = res, detail = sprintf("Gonet weight %s %.1f%% (%s) %s: %s", s, 100 * w, gonet$asof,
                                            paste(checks, collapse = " and "), res)))
  }

  q <- closes[[node$yahoo]]
  if (is.null(q) || length(q$price) != 1 || !is.finite(q$price))
    return(list(met = NA, detail = sprintf("%s: no Yahoo close", node$yahoo)))
  checks <- character(); res <- TRUE
  if (!is.null(node$close_lte)) {
    checks <- c(checks, sprintf("<= %s", node$close_lte)); res <- res && q$price <= node$close_lte
  }
  if (!is.null(node$close_gte)) {
    checks <- c(checks, sprintf(">= %s", node$close_gte)); res <- res && q$price >= node$close_gte
  }
  list(met = res, detail = sprintf("%s close %s (%s) %s: %s", node$yahoo, q$price, q$asof,
                                   paste(checks, collapse = " and "), res))
}
