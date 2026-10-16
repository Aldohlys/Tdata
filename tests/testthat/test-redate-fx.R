############  Test redate_fx_london (Yahoo FX bars stamped at London midnight) ###########

fx_xts <- function(dates, close) {
  m <- cbind(Open = close, High = close, Low = close, Close = close, Volume = 0, Adjusted = close)
  xts::xts(m, order.by = as.Date(dates))
}

test_that("Summer-time FX bars move to their London date and today's empty bar is merged with the live quote", {
  # As quantmod returned EURUSD=X on 2026-10-16 17:13 UTC: Sunday row, the 10-16 session
  # dated 10-15 with an empty close, live quote dated 10-16
  x <- fx_xts(c("2026-10-11", "2026-10-12", "2026-10-13", "2026-10-14", "2026-10-15", "2026-10-16"),
              c(1.1205, 1.1160, 1.1204, 1.1195, NA, 1.1225))
  y <- Tdata:::redate_fx_london(x, now = as.POSIXct("2026-10-16 17:13:27", tz = "UTC"))
  expect_equal(as.character(zoo::index(y)),
               c("2026-10-12", "2026-10-13", "2026-10-14", "2026-10-15", "2026-10-16"))
  expect_equal(as.numeric(y[, "Close"]), c(1.1205, 1.1160, 1.1204, 1.1195, 1.1225))
})

test_that("Winter-time FX bars keep their dates", {
  x <- fx_xts(c("2026-12-14", "2026-12-15", "2026-12-16"), c(1.10, 1.11, 1.12))
  y <- Tdata:::redate_fx_london(x, now = as.POSIXct("2026-12-16 15:00:00", tz = "UTC"))
  expect_equal(as.character(zoo::index(y)), c("2026-12-14", "2026-12-15", "2026-12-16"))
  expect_equal(as.numeric(y[, "Close"]), c(1.10, 1.11, 1.12))
})

test_that("After London midnight in summer time the new bar takes London's date", {
  # 23:30 UTC on 10-16 = 00:30 BST on 10-17: the 10-17 bar is stamped 10-16 23:00 UTC
  x <- fx_xts(c("2026-10-15", "2026-10-16"), c(1.1195, NA))
  y <- Tdata:::redate_fx_london(x, now = as.POSIXct("2026-10-16 23:30:00", tz = "UTC"))
  expect_equal(as.character(zoo::index(y)), c("2026-10-16", "2026-10-17"))
})

test_that("The clock change on 2026-10-25 leaves no gap and no duplicate", {
  # Fri 10-23 bar stamped Thu 10-22 23:00 UTC (BST); Mon 10-26 bar stamped Mon 00:00 UTC (GMT)
  x <- fx_xts(c("2026-10-22", "2026-10-26"), c(1.12, 1.13))
  y <- Tdata:::redate_fx_london(x, now = as.POSIXct("2026-10-27 12:00:00", tz = "UTC"))
  expect_equal(as.character(zoo::index(y)), c("2026-10-23", "2026-10-26"))
})
