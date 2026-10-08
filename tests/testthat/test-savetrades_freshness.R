### ---------------------------------------------------------------------------
### saveTrades(): refuses to overwrite a DB that changed after getAllTrades()
### ---------------------------------------------------------------------------

.fresh_db <- function() {
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "Trades", data.frame(
    TradeNr = c(757L, 758L, 758L), Account = "U1804173",
    TradeDate = 20261001L, Pos = c(1L, -1L, 1L),
    Price = c(0.1, 2.51, 3.35), Commission = 0.69,
    Total = c(-10, 250.31, -335.69), Risk = c(10, 585.99, 0),
    Reward = 0, PnL = 0,
    Currency = "USD", EventType = "Open", stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)
  path
}

.with_db <- function(path, code) {
  with_mocked_bindings(
    safe_db_connect = function(...) DBI::dbConnect(RSQLite::SQLite(), path),
    safe_db_write = function(conn, table, data, ...)
      DBI::dbWriteTable(conn, table, data, overwrite = TRUE),
    .package = "Tdata",
    code)
}

.sql <- function(path, q) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, q)
}

test_that("a change made in the DB after Load blocks the save and names the trade", {
  reset_loaded_trades()
  path <- .fresh_db()
  .with_db(path, {
    tr <- getAllTrades()
    .sql(path, "UPDATE Trades SET Risk = 85.38 WHERE TradeNr = 758 AND Pos = -1")
    tr$Strategy <- "Perso"                                    # an in-app edit
    expect_null(suppressMessages(saveTrades(tr)))
    expect_equal(sort(changed_tradenrs(tr[, -which(names(tr) == "Strategy")],
                                       readTradesTable())), 758L)
    ### The SQL fix survived.
    expect_equal(max(readTradesTable()$Risk), 85.38)
  })
})

test_that("an unchanged DB saves, and a second save from the same session works", {
  reset_loaded_trades()
  path <- .fresh_db()
  .with_db(path, {
    tr <- getAllTrades()
    tr$Risk[tr$TradeNr == 758 & tr$Pos == -1] <- 85.38
    expect_true(saveTrades(tr))
    tr$Risk[tr$TradeNr == 757] <- 12
    expect_true(saveTrades(tr))                               # copy was refreshed
    expect_equal(readTradesTable()$Risk, c(12, 85.38, 0))
  })
})

test_that("Tdata's internal reads do not refresh the copy", {
  reset_loaded_trades()
  path <- .fresh_db()
  .with_db(path, {
    tr <- getAllTrades()
    .sql(path, "UPDATE Trades SET Risk = 85.38 WHERE TradeNr = 758 AND Pos = -1")
    invisible(readTradesTable())                              # e.g. getTradeDates()
    expect_null(suppressMessages(saveTrades(tr)))
  })
})

test_that("force = TRUE overwrites, and no Load in the session skips the check", {
  reset_loaded_trades()
  path <- .fresh_db()
  .with_db(path, {
    tr <- getAllTrades()
    .sql(path, "UPDATE Trades SET Risk = 85.38 WHERE TradeNr = 758 AND Pos = -1")
    expect_true(saveTrades(tr, force = TRUE))
    expect_equal(max(readTradesTable()$Risk), 585.99)

    reset_loaded_trades()
    expect_true(saveTrades(readTradesTable()))
  })
})
