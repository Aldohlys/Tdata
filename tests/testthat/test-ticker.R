test_that("getAllTickers works", {
  data <- getAllTickers()
  expect_true(nrow(data) > 0)
})

test_that("getTicker works", {
  data <- getTickers("SPX")
  expect_true(nrow(data) == 1)
})


test_that("getTicker works with a character vector", {
  data <- getTickers(c("SPX", "ESTX50"))
  expect_true(nrow(data) == 2)
})

test_that("addTicker returns 0 fi ticker laready exists", {
  data <- addTicker("SPY")
  expect_true(data == 0)
})

test_that("addTicker works and returns 1 record with 17 columns", {
  data <- addTicker("XOM")
  expect_equal(nrow(data), 1)
  expect_equal(ncol(data) , 17)
})

test_that("removeTicker works", {
  data <- removeTicker("XOM")
  expect_equal(data , 1)
})

test_that("removeTicker works", {
  data <- removeTicker("gergergeer")
  expect_equal(data , 0)
})

test_that("setTicker updates single parameter correctly", {
  # Store original value to restore later
  original_data <- getTicker("SPY")
  original_sector <- original_data$Sector

  # Update sector
  result <- setTicker("SPY", Sector = "Technology")
  expect_equal(result$rows_affected, 1)

  # Verify update
  updated_data <- getTicker("SPY")
  expect_equal(updated_data$Sector, "Technology")
  expect_true(nchar(updated_data$LastUpdate) > 0)

  # Restore original value
  setTicker("SPY", Sector = original_sector)
})


test_that("setTicker updates multiple parameters correctly", {
  # Store original values
  original_data <- getTicker("SPX")

  # Update multiple fields
  result <- setTicker("SPX", Sector = "Energy", Beta_3m = -0.5, Currency = "EUR")
  expect_equal(result$rows_affected, 1)

  # Verify updates
  updated_data <- getTicker("SPX")
  expect_equal(updated_data$Sector, "Energy")
  expect_equal(updated_data$Beta_3m, -0.5)
  expect_equal(updated_data$Currency, "EUR")

  # Restore original values
  setTicker("SPX",
            Sector = original_data$Sector,
            Beta_3m = original_data$Beta_3m,
            Currency = original_data$Currency
  )
})
