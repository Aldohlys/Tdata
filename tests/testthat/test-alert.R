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
    c("id", "Theme", "Asset", "AlertDate", "Description", "Active", "CreatedAt"))
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

test_that("addAlert accepts all four valid themes", {
  db_path <- local_alert_db()

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    safe_db_append = mock_append, {
      for (theme in c("Earnings", "Macro", "Expiration", "Technical")) {
        expect_no_error(addAlert(theme, "X", "2026-06-01", "test"))
      }
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  themes <- DBI::dbGetQuery(conn, "SELECT Theme FROM Alerts ORDER BY Theme")$Theme
  expect_setequal(themes, c("Earnings", "Macro", "Expiration", "Technical"))
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
