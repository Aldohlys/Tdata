# Tests for alert.R — Alerts table CRUD
#
# Strategy: temp SQLite DB + mocked safe_db_connect / safe_db_append.
# Each test gets its own temp DB path; helper local_alert_db() returns the path
# and registers cleanup on the parent frame.

local_alert_db <- function(envir = parent.frame()) {
  tmp <- tempfile(fileext = ".db")
  conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(conn, "
    CREATE TABLE Alerts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      Theme TEXT NOT NULL,
      Asset TEXT NOT NULL,
      AlertDate TEXT NOT NULL,
      Description TEXT,
      Active INTEGER DEFAULT 1,
      CreatedAt TEXT NOT NULL
    )")
  DBI::dbDisconnect(conn)
  withr::defer(unlink(tmp), envir = envir)
  tmp
}

# Conn factory bound to a specific path
mock_connect <- function(db_path) {
  function(...) DBI::dbConnect(RSQLite::SQLite(), db_path)
}

# Bypass validate_db_types (no Alerts validator registered)
mock_append <- function(conn, table_name, data, ...) {
  DBI::dbAppendTable(conn, table_name, data)
}

# ---------------------------------------------------------------------------
# createAlertTable
# ---------------------------------------------------------------------------
test_that("createAlertTable creates the table when it does not exist", {
  tmp <- tempfile(fileext = ".db")
  withr::defer(unlink(tmp))

  with_mocked_bindings(
    safe_db_connect = mock_connect(tmp), {
      expect_invisible(createAlertTable())
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(conn))
  expect_true("Alerts" %in% DBI::dbListTables(conn))
  expect_setequal(
    DBI::dbListFields(conn, "Alerts"),
    c("id", "Theme", "Asset", "AlertDate", "Description", "Active", "CreatedAt",
      "TriggerType", "Condition"))
})

test_that("createAlertTable adds TriggerType and Condition to an older table", {
  db_path <- local_alert_db()   # the 7-column table written before condition alerts
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, "INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt)
                        VALUES ('Earnings', 'AAPL', '2026-06-15', 1, '2026-05-01')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      createAlertTable()
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  row <- DBI::dbGetQuery(conn, "SELECT TriggerType, Condition FROM Alerts")
  expect_equal(row$TriggerType, "Date")   # existing rows read as date alerts
  expect_true(is.na(row$Condition))
})

test_that("createAlertTable is idempotent", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      expect_no_error(createAlertTable())
      expect_no_error(createAlertTable())
    })
})

# ---------------------------------------------------------------------------
# addAlert
# ---------------------------------------------------------------------------
test_that("addAlert inserts a row with valid theme and returns the data frame", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      df <- addAlert("Earnings", "AAPL", "2026-06-15", "Q2 release")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$Theme, "Earnings")
      expect_equal(df$Asset, "AAPL")
      expect_equal(df$AlertDate, "2026-06-15")
      expect_equal(df$Description, "Q2 release")
      expect_equal(df$Active, 1L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  rows <- DBI::dbGetQuery(conn, "SELECT Theme, Asset, AlertDate FROM Alerts")
  expect_equal(nrow(rows), 1)
  expect_equal(rows$Theme, "Earnings")
  expect_equal(rows$Asset, "AAPL")
})

test_that("addAlert accepts every theme of ALERT_THEMES", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      for (theme in ALERT_THEMES) {
        expect_no_error(addAlert(theme, "X", "2026-06-01", "test"))
      }
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  themes <- DBI::dbGetQuery(conn, "SELECT Theme FROM Alerts ORDER BY Theme")$Theme
  expect_setequal(themes, ALERT_THEMES)
  expect_true(all(c("Stop", "Momentum", "Volatility") %in% ALERT_THEMES))
})

test_that("addAlert stores a condition alert with its JSON condition", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      df <- addAlert("Stop", "NUCL", "2026-10-26", "close below 41.90",
                     trigger_type = "Condition",
                     condition = list(yahoo = "NUCL.L", close_lte = 41.9))
      expect_equal(df$TriggerType, "Condition")
      expect_equal(df$Condition, '{"yahoo":"NUCL.L","close_lte":41.9}')
      ## A JSON string is accepted as well and stored the same way.
      df2 <- addAlert("Stop", "NUCL", "2026-10-26", trigger_type = "Both",
                      condition = '{"yahoo": "NUCL.L", "close_lte": 41.9}')
      expect_equal(df2$Condition, df$Condition)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  rows <- DBI::dbGetQuery(conn, "SELECT TriggerType, Condition FROM Alerts ORDER BY id")
  expect_equal(rows$TriggerType, c("Condition", "Both"))
})

test_that("addAlert refuses inconsistent trigger_type / condition and bad conditions", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      expect_error(addAlert("Stop", "X", "2026-06-01", trigger_type = "Price"), "Invalid trigger_type")
      expect_error(addAlert("Stop", "X", "2026-06-01", trigger_type = "Condition"), "needs a condition")
      expect_error(addAlert("Stop", "X", "2026-06-01",
                            condition = list(yahoo = "X", close_lte = 1)), "Date alert")
      bad <- function(cond) addAlert("Stop", "X", "2026-06-01", trigger_type = "Condition", condition = cond)
      expect_error(bad(list(yahoo = "X", close_below = 1)), "unknown key")
      expect_error(bad(list(yahoo = "X")), "close_lte and/or close_gte")
      expect_error(bad(list(close_lte = 1)), "needs 'yahoo'")
      expect_error(bad(list(yahoo = "X", close_lte = "41.9")), "must be one number")
      expect_error(bad(list(any_of = list())), "non-empty list")
      expect_error(bad(list(date_gte = "not a date")), "invalid date_gte")
      expect_error(bad("{not json"))
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  expect_equal(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM Alerts")$n, 0L)
})

test_that("addAlert rejects invalid theme", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      expect_error(addAlert("Bogus", "AAPL", "2026-06-15"), "Invalid theme")
    })
})

test_that("addAlert accepts Date object for alert_date and converts to string", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      df <- addAlert("Macro", "FOMC", as.Date("2026-09-17"), "rate decision")
      expect_equal(df$AlertDate, "2026-09-17")
    })
})

test_that("addAlert allows empty asset (e.g. macro alerts)", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      df <- addAlert("Macro", asset = "", alert_date = "2026-09-17")
      expect_equal(df$Asset, "")
    })
})

# ---------------------------------------------------------------------------
# getAllAlerts
# ---------------------------------------------------------------------------
test_that("getAllAlerts default returns only Active=1 rows, sorted by AlertDate", {
  db_path <- local_alert_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Alerts (Theme, Asset, AlertDate, Description, Active, CreatedAt)
     VALUES
       ('Earnings', 'MSFT', '2026-07-25', 'Q2',     1, '2026-05-01'),
       ('Earnings', 'AAPL', '2026-06-15', 'Q1',     1, '2026-05-01'),
       ('Earnings', 'NVDA', '2026-05-20', 'old',    0, '2026-04-01')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      df <- getAllAlerts()
      expect_equal(nrow(df), 2)
      expect_true(all(df$Active == 1))
      expect_equal(df$Asset, c("AAPL", "MSFT"))  # ASC by AlertDate
    })
})

test_that("getAllAlerts(active_only=FALSE) returns all rows including dismissed", {
  db_path <- local_alert_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Alerts (Theme, Asset, AlertDate, Description, Active, CreatedAt)
     VALUES
       ('Earnings', 'AAPL', '2026-06-15', '', 1, '2026-05-01'),
       ('Earnings', 'NVDA', '2026-05-20', '', 0, '2026-04-01')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      df <- getAllAlerts(active_only = FALSE)
      expect_equal(nrow(df), 2)
    })
})

test_that("getAllAlerts on empty table returns 0-row data frame", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      df <- getAllAlerts()
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
    })
})

# ---------------------------------------------------------------------------
# removeAlert
# ---------------------------------------------------------------------------
test_that("removeAlert deletes the row and returns 1", {
  db_path <- local_alert_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt)
     VALUES ('Earnings', 'AAPL', '2026-06-15', 1, '2026-05-01')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      result <- removeAlert(1L)
      expect_equal(result, 1L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  expect_equal(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM Alerts")$n, 0L)
})

test_that("removeAlert returns 0 for non-existent id", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      expect_equal(removeAlert(999L), 0L)
    })
})

# ---------------------------------------------------------------------------
# dismissAlert
# ---------------------------------------------------------------------------
test_that("dismissAlert sets Active=0 and returns 1", {
  db_path <- local_alert_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt)
     VALUES ('Earnings', 'AAPL', '2026-06-15', 1, '2026-05-01')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      result <- dismissAlert(1L)
      expect_equal(result, 1L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  active <- DBI::dbGetQuery(conn, "SELECT Active FROM Alerts WHERE id = 1")$Active
  expect_equal(active, 0L)
})

test_that("dismissAlert returns 0 for non-existent id", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      expect_equal(dismissAlert(999L), 0L)
    })
})

# ---------------------------------------------------------------------------
# getUpcomingAlerts
# ---------------------------------------------------------------------------
test_that("getUpcomingAlerts returns alerts within window, sorted ASC", {
  db_path <- local_alert_db()
  today <- as.character(Sys.Date())
  tomorrow <- as.character(Sys.Date() + 1)
  next_week <- as.character(Sys.Date() + 7)
  next_month <- as.character(Sys.Date() + 30)
  yesterday <- as.character(Sys.Date() - 1)

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf("
    INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt) VALUES
      ('Earnings', 'A',    '%s', 1, '2026-05-01'),
      ('Earnings', 'B',    '%s', 1, '2026-05-01'),
      ('Earnings', 'C',    '%s', 1, '2026-05-01'),
      ('Earnings', 'D',    '%s', 1, '2026-05-01'),
      ('Earnings', 'E',    '%s', 0, '2026-05-01')",
    yesterday, today, tomorrow, next_month, tomorrow))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      # default days_ahead = 1: today + tomorrow
      df1 <- getUpcomingAlerts()
      expect_equal(nrow(df1), 2)
      expect_setequal(df1$Asset, c("B", "C"))
      expect_equal(df1$AlertDate, c(today, tomorrow))  # ASC

      df7 <- getUpcomingAlerts(days_ahead = 7)
      expect_setequal(df7$Asset, c("B", "C"))  # next_week not seeded in this test
    })
})

test_that("getUpcomingAlerts excludes inactive (Active=0) alerts", {
  db_path <- local_alert_db()
  today <- as.character(Sys.Date())
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf("
    INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt) VALUES
      ('Earnings', 'X', '%s', 0, '2026-05-01')", today))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      df <- getUpcomingAlerts()
      expect_equal(nrow(df), 0)
    })
})

test_that("getUpcomingAlerts leaves out Condition alerts but keeps Both", {
  db_path <- local_alert_db()
  today <- as.character(Sys.Date())
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN TriggerType TEXT DEFAULT 'Date'")
  DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN Condition TEXT")
  DBI::dbExecute(conn, sprintf("
    INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt, TriggerType, Condition) VALUES
      ('Earnings', 'D', '%s', 1, '2026-05-01', 'Date', NULL),
      ('Stop',     'C', '%s', 1, '2026-05-01', 'Condition', '{\"yahoo\":\"C\",\"close_lte\":1}'),
      ('Stop',     'B', '%s', 1, '2026-05-01', 'Both', '{\"yahoo\":\"B\",\"close_lte\":1}'),
      ('Earnings', 'N', '%s', 1, '2026-05-01', NULL, NULL)",
    today, today, today, today))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      expect_setequal(getUpcomingAlerts()$Asset, c("D", "B", "N"))
    })
})

# ---------------------------------------------------------------------------
# getTriggeredAlerts
# ---------------------------------------------------------------------------
seed_condition_alerts <- function(db_path, rows) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN TriggerType TEXT DEFAULT 'Date'")
  DBI::dbExecute(conn, "ALTER TABLE Alerts ADD COLUMN Condition TEXT")
  for (r in rows)
    DBI::dbExecute(conn,
      "INSERT INTO Alerts (Theme, Asset, AlertDate, Active, CreatedAt, TriggerType, Condition)
       VALUES ('Stop', ?, '2026-10-26', ?, '2026-10-26', ?, ?)",
      params = list(r$asset, if (is.null(r$active)) 1L else r$active, r$type, r$cond))
}

fake_closes <- function(prices) {
  calls <- new.env(); calls$n <- 0
  f <- function(sym) {
    calls$n <- calls$n + 1
    if (is.na(prices[sym])) NULL else list(price = unname(prices[sym]), asof = "20261026")
  }
  attr(f, "calls") <- calls
  f
}

test_that("getTriggeredAlerts returns the alerts whose price condition is met", {
  db_path <- local_alert_db()
  seed_condition_alerts(db_path, list(
    list(asset = "NUCL", type = "Condition", cond = '{"yahoo":"NUCL.L","close_lte":41.9}'),
    list(asset = "QQQ",  type = "Condition", cond = '{"yahoo":"QQQ","close_gte":800}'),
    list(asset = "SPY",  type = "Both",      cond = '{"yahoo":"SPY","close_lte":705}'),
    list(asset = "OLD",  type = "Condition", cond = '{"yahoo":"NUCL.L","close_lte":99}', active = 0L),
    list(asset = "DATE", type = "Date",      cond = NA)))

  fetch <- fake_closes(c(NUCL.L = 41.5, QQQ = 713.4, SPY = 700))
  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      out <- getTriggeredAlerts(fetch = fetch)
    })

  expect_equal(out$Asset, c("NUCL", "SPY"))
  expect_equal(out$ConditionDetail[1], "NUCL.L close 41.5 (20261026) <= 41.9: TRUE")
  expect_equal(nrow(attr(out, "unevaluated")), 0)
  expect_equal(attr(fetch, "calls")$n, 3)   # one fetch per symbol, inactive rows not read
})

test_that("getTriggeredAlerts reports conditions it cannot evaluate instead of dropping them", {
  db_path <- local_alert_db()
  seed_condition_alerts(db_path, list(
    list(asset = "NUCL", type = "Condition", cond = '{"yahoo":"NUCL.L","close_lte":41.9}'),
    list(asset = "BAD",  type = "Condition", cond = '{"yahoo":"X","close_under":1}')))

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      out <- getTriggeredAlerts(fetch = function(sym) stop("Yahoo down"))
    })

  expect_equal(nrow(out), 0)
  unev <- attr(out, "unevaluated")
  expect_equal(unev$Asset, c("NUCL", "BAD"))
  expect_equal(unev$ConditionDetail, c("NUCL.L: no Yahoo close", "invalid Condition JSON"))
})

test_that("getTriggeredAlerts on a table without condition alerts returns an empty frame", {
  db_path <- local_alert_db()   # old schema: the columns are added on the fly

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      out <- getTriggeredAlerts(fetch = function(sym) stop("not called"))
    })
  expect_equal(nrow(out), 0)
  expect_equal(nrow(attr(out, "unevaluated")), 0)
})

test_that("Gonet weight conditions are validated and evaluated against the snapshot weights", {
  gonet <- list(w = c(PM_15606539 = 0.105, QQQ = 0.092), asof = "20261026 16:52:29")
  ev <- function(json, g = gonet) alert_condition_eval(alert_condition_parse(json), list(), Sys.Date(), g)

  expect_false(ev('{"gonet_weight":"PM_15606539","weight_lte":0.09}')$met)
  expect_true(ev('{"gonet_weight":"PM_15606539","weight_lte":0.11}')$met)
  expect_equal(ev('{"gonet_weight":"PM_15606539","weight_lte":0.09}')$detail,
               "Gonet weight PM_15606539 10.5% (20261026 16:52:29) <= 9.0%: FALSE")
  expect_true(ev('{"gonet_weight":"QQQ","weight_gte":0.09,"weight_lte":0.12}')$met)
  ## symbol missing from the snapshot, or weights not read -> not evaluable
  expect_true(is.na(ev('{"gonet_weight":"XXX","weight_lte":0.09}')$met))
  expect_true(is.na(ev('{"gonet_weight":"QQQ","weight_lte":0.09}', NULL)$met))

  expect_error(alert_condition_parse('{"gonet_weight":"PM_15606539","weight_lte":9}'), "fraction")
  expect_error(alert_condition_parse('{"gonet_weight":"PM_15606539"}'), "weight_lte and/or weight_gte")
  expect_error(alert_condition_parse('{"gonet_weight":"PM_15606539","close_lte":1}'), "unknown key")
  expect_true(alert_condition_uses_gonet(alert_condition_parse(
    '{"any_of":[{"yahoo":"X","close_lte":1},{"gonet_weight":"PM_15606539","weight_lte":0.09}]}')))
  expect_false(alert_condition_uses_gonet(alert_condition_parse('{"yahoo":"X","close_lte":1}')))
})

test_that("getTriggeredAlerts reads Gonet weights only when a condition needs them", {
  db_path <- local_alert_db()
  seed_condition_alerts(db_path, list(
    list(asset = "GOLD", type = "Condition", cond = '{"gonet_weight":"PM_15606539","weight_lte":0.09}'),
    list(asset = "NUCL", type = "Condition", cond = '{"yahoo":"NUCL.L","close_lte":41.9}')))
  calls <- new.env(); calls$n <- 0
  w <- function() { calls$n <- calls$n + 1; list(w = c(PM_15606539 = 0.085), asof = "20261101 17:00:00") }

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path), {
      out <- getTriggeredAlerts(fetch = fake_closes(c(NUCL.L = 43)), weights = w)
    })
  expect_equal(out$Asset, "GOLD")
  expect_equal(calls$n, 1)

  db2 <- local_alert_db()
  seed_condition_alerts(db2, list(
    list(asset = "NUCL", type = "Condition", cond = '{"yahoo":"NUCL.L","close_lte":41.9}')))
  with_mocked_bindings(
    safe_db_connect = mock_connect(db2), {
      getTriggeredAlerts(fetch = fake_closes(c(NUCL.L = 43)), weights = function() stop("not needed"))
    })
  succeed()
})

test_that("alert_gonet_weights converts each line to base and divides by the snapshot total", {
  tmp <- tempfile(fileext = ".db"); withr::defer(unlink(tmp))
  conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbWriteTable(conn, "Gonet", data.frame(
    date = c(20261025, 20261026, 20261026, 20261026), heure = c("17:00:00", "16:52:29", "16:52:29", "16:52:29"),
    symbol = c("PM_15606539", "PM_15606539", "QQQ", "CHF"), mktValue = c(1, 600, 500, 100),
    currency = c("CHF", "CHF", "USD", "CHF")))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(tmp),
    convert_to_base_date = function(x, ccy, d) x * ifelse(ccy == "USD", 0.8, 1), {
      g <- alert_gonet_weights()
    })
  expect_equal(g$asof, "20261026 16:52:29")
  expect_equal(unname(g$w["PM_15606539"]), 600 / 1100)   # 600 + 500*0.8 + 100
  expect_equal(sum(g$w), 1)
})

test_that("alert_condition_eval composes all_of / any_of with dates and missing prices", {
  closes <- list(CA.PA = list(price = 13.5, asof = "20261026"), VIX = NULL)
  today <- as.Date("2026-07-01")
  ev <- function(json) alert_condition_eval(alert_condition_parse(json), closes, today)

  ## id=63 shape: earnings date OR price break
  expect_true(ev('{"any_of":[{"date_gte":"2026-07-21"},{"yahoo":"CA.PA","close_lte":14}]}')$met)
  expect_false(ev('{"any_of":[{"date_gte":"2026-07-21"},{"yahoo":"CA.PA","close_lte":13}]}')$met)
  expect_true(ev('{"date_gte":"2026-07-01"}')$met)

  ## both bounds in one leaf must hold
  expect_true(ev('{"yahoo":"CA.PA","close_gte":13,"close_lte":14}')$met)
  expect_false(ev('{"yahoo":"CA.PA","close_gte":13.6,"close_lte":14}')$met)

  ## missing price: all_of is FALSE if another leaf is FALSE, NA otherwise;
  ## any_of is TRUE if another leaf is TRUE
  expect_false(ev('{"all_of":[{"yahoo":"CA.PA","close_lte":13},{"yahoo":"VIX","close_gte":30}]}')$met)
  expect_true(is.na(ev('{"all_of":[{"yahoo":"CA.PA","close_lte":14},{"yahoo":"VIX","close_gte":30}]}')$met))
  expect_true(ev('{"any_of":[{"yahoo":"CA.PA","close_lte":14},{"yahoo":"VIX","close_gte":30}]}')$met)

  expect_setequal(alert_condition_symbols(alert_condition_parse(
    '{"all_of":[{"yahoo":"SPY","close_lte":625},{"any_of":[{"yahoo":"^VIX","close_gte":30},{"date_gte":"2026-09-18"}]}]}')),
    c("SPY", "^VIX"))
})
