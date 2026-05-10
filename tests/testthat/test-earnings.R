# Tests for earnings.R — getNextEarningsDate / updateEarnings / updateStaleEarnings
#
# Strategy: temp SQLite DB with a Tickers table + mocked safe_db_connect.
# tdata_py is mocked via fake list with getNextEarningsDate slot, OR
# getNextEarningsDate (R wrapper) is mocked directly when testing the higher-level fns.

local_tickers_db <- function(envir = parent.frame()) {
  tmp <- tempfile(fileext = ".db")
  conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(conn, "
    CREATE TABLE Tickers (
      Name TEXT PRIMARY KEY,
      Type TEXT,
      IV   TEXT,
      NextEarnings TEXT,
      EarningsLastUpdate TEXT
    )")
  DBI::dbDisconnect(conn)
  withr::defer(unlink(tmp), envir = envir)
  tmp
}

mock_connect <- function(db_path) {
  function(...) DBI::dbConnect(RSQLite::SQLite(), db_path)
}

# tdata_py is an active binding (zzz.R) backed by .tdata_state — `with_mocked_bindings`
# cannot replace active bindings cleanly, so swap the underlying state instead.
local_mock_tdata_py <- function(fake_module, envir = parent.frame()) {
  state <- get(".tdata_state", envir = asNamespace("Tdata"))
  old_value <- state$value
  old_initialized <- state$initialized
  state$value <- fake_module
  state$initialized <- TRUE
  withr::defer({
    state$value <- old_value
    state$initialized <- old_initialized
  }, envir = envir)
}

# ---------------------------------------------------------------------------
# getNextEarningsDate — Python wrapper
# ---------------------------------------------------------------------------
test_that("getNextEarningsDate returns string when Python returns a date", {
  local_mock_tdata_py(list(getNextEarningsDate = function(symbol) "20260720"))
  expect_equal(getNextEarningsDate("AAPL"), "20260720")
})

test_that("getNextEarningsDate returns NA when Python returns NULL", {
  local_mock_tdata_py(list(getNextEarningsDate = function(symbol) NULL))
  expect_true(is.na(getNextEarningsDate("AAPL")))
})

test_that("getNextEarningsDate returns NA when Python returns empty", {
  local_mock_tdata_py(list(getNextEarningsDate = function(symbol) character(0)))
  expect_true(is.na(getNextEarningsDate("AAPL")))
})

test_that("getNextEarningsDate catches Python errors and returns NA", {
  local_mock_tdata_py(list(
    getNextEarningsDate = function(symbol) stop("yfinance ConnectionError")))
  expect_true(is.na(getNextEarningsDate("AAPL")))
})

test_that("getNextEarningsDate returns NA when tdata_py is NULL", {
  local_mock_tdata_py(NULL)
  expect_true(is.na(getNextEarningsDate("AAPL")))
})

# ---------------------------------------------------------------------------
# updateEarnings — DB writes via mocked safe_db_connect
# ---------------------------------------------------------------------------
test_that("updateEarnings returns 0 when given empty input", {
  expect_equal(updateEarnings(character(0)), 0L)
})

test_that("updateEarnings updates NextEarnings + EarningsLastUpdate for known tickers", {
  db_path <- local_tickers_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Tickers (Name, Type, IV) VALUES ('AAPL', 'STK', 'YES'), ('MSFT', 'STK', 'YES')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) {
      switch(name, AAPL = "20260720", MSFT = "20260815")
    }, {
      n <- updateEarnings(c("AAPL", "MSFT"))
      expect_equal(n, 2L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  rows <- DBI::dbGetQuery(conn,
    "SELECT Name, NextEarnings, EarningsLastUpdate FROM Tickers ORDER BY Name")
  expect_equal(rows$NextEarnings, c("20260720", "20260815"))
  expect_true(all(rows$EarningsLastUpdate == format(Sys.Date(), "%Y%m%d")))
})

test_that("updateEarnings stores NA when Python returns no date", {
  db_path <- local_tickers_db()
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn,
    "INSERT INTO Tickers (Name, Type, IV, NextEarnings) VALUES ('SPY', 'STK', 'YES', '20251101')")
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) NA_character_, {
      n <- updateEarnings("SPY")
      expect_equal(n, 1L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  row <- DBI::dbGetQuery(conn, "SELECT NextEarnings FROM Tickers WHERE Name = 'SPY'")
  expect_true(is.na(row$NextEarnings))
})

test_that("updateEarnings does not count SKIPPED tickers (not in Tickers table)", {
  db_path <- local_tickers_db()
  # No tickers inserted

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) "20260720", {
      n <- updateEarnings("UNKNOWN")
      expect_equal(n, 0L)
    })
})

# ---------------------------------------------------------------------------
# updateStaleEarnings — selects stale rows and delegates to updateEarnings
# ---------------------------------------------------------------------------
test_that("updateStaleEarnings returns 0 when nothing is stale", {
  db_path <- local_tickers_db()
  today <- format(Sys.Date(), "%Y%m%d")
  three_days_ago <- format(Sys.Date() - 3, "%Y%m%d")
  far_future <- format(Sys.Date() + 60, "%Y%m%d")

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO Tickers (Name, Type, IV, NextEarnings, EarningsLastUpdate) VALUES
       ('AAPL', 'STK', 'YES', '%s', '%s'),
       ('MSFT', 'STK', 'YES', NULL,  '%s')",
    far_future, today, three_days_ago))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) {
      stop("should not be called when nothing is stale")
    }, {
      expect_equal(updateStaleEarnings(), 0L)
    })
})

test_that("updateStaleEarnings refreshes rows whose NextEarnings is in the past", {
  db_path <- local_tickers_db()
  past <- format(Sys.Date() - 30, "%Y%m%d")
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO Tickers (Name, Type, IV, NextEarnings, EarningsLastUpdate) VALUES
       ('AAPL', 'STK', 'YES', '%s', '%s')", past, past))
  DBI::dbDisconnect(conn)

  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) "20260920", {
      result <- updateStaleEarnings()
      expect_equal(result, 1L)
    })

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  row <- DBI::dbGetQuery(conn, "SELECT NextEarnings FROM Tickers WHERE Name = 'AAPL'")
  expect_equal(row$NextEarnings, "20260920")
})

test_that("updateStaleEarnings retries NULL NextEarnings only after 7 days", {
  db_path <- local_tickers_db()
  three_days_ago <- format(Sys.Date() - 3, "%Y%m%d")
  ten_days_ago <- format(Sys.Date() - 10, "%Y%m%d")

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO Tickers (Name, Type, IV, NextEarnings, EarningsLastUpdate) VALUES
       ('FRESH', 'STK', 'YES', NULL, '%s'),
       ('STALE', 'STK', 'YES', NULL, '%s')",
    three_days_ago, ten_days_ago))
  DBI::dbDisconnect(conn)

  fetched <- character()
  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) {
      fetched <<- c(fetched, name)
      NA_character_
    }, {
      updateStaleEarnings()
    })

  expect_equal(fetched, "STALE")  # FRESH not refreshed (under 7-day threshold)
})

test_that("updateStaleEarnings ignores non-STK or IV!=YES tickers", {
  db_path <- local_tickers_db()
  past <- format(Sys.Date() - 30, "%Y%m%d")
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO Tickers (Name, Type, IV, NextEarnings, EarningsLastUpdate) VALUES
       ('STKY', 'STK', 'YES', '%s', '%s'),
       ('STKN', 'STK', 'NO',  '%s', '%s'),
       ('IDX',  'IND', 'YES', '%s', '%s')",
    past, past, past, past, past, past))
  DBI::dbDisconnect(conn)

  fetched <- character()
  with_mocked_bindings(
    safe_db_connect = mock_connect(db_path),
    getNextEarningsDate = function(name) { fetched <<- c(fetched, name); "20260920" }, {
      updateStaleEarnings()
    })

  expect_equal(fetched, "STKY")
})
