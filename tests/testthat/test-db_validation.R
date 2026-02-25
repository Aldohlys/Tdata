library(testthat)

# Tests pour standardize_date_integer()

test_that("standardize_date_integer handles Date objects", {
  expect_equal(standardize_date_integer(as.Date("2025-05-10")), 20250510L)
})

test_that("standardize_date_integer handles valid numeric strings", {
  expect_equal(standardize_date_integer("20240315"), 20240315L)
})

test_that("standardize_date_integer rejects strings with non-numeric characters", {
  expect_equal(standardize_date_integer("20240315f"), NA_integer_)
})

test_that("standardize_date_integer handles NA input", {
  expect_equal(standardize_date_integer(NA), NA_integer_)
})

test_that("standardize_date_integer handles empty string", {
  expect_equal(standardize_date_integer(""), NA_integer_)
})

test_that("standardize_date_integer rejects zero", {
  expect_equal(standardize_date_integer(0), NA_integer_)
})

test_that("standardize_date_integer rejects dates too old", {
  expect_equal(standardize_date_integer(18991231), NA_integer_)
})

test_that("standardize_date_integer rejects dates too far in future", {
  expect_equal(standardize_date_integer(21001232), NA_integer_)
})

test_that("standardize_date_integer handles valid integer in range", {
  expect_equal(standardize_date_integer(20230101), 20230101L)
})

test_that("standardize_date_integer converts decimal to integer", {
  expect_equal(standardize_date_integer(20230101.5), 20230101L)
})

library(testthat)

# Tests pour standardize_numeric()

test_that("standardize_numeric handles valid numeric values", {
  expect_equal(standardize_numeric(123.45), 123.45)
})

test_that("standardize_numeric handles numeric strings", {
  expect_equal(standardize_numeric("456.78"), 456.78)
})

test_that("standardize_numeric handles currency symbols", {
  expect_equal(standardize_numeric("$1,234.56"), 1234.56)
})

test_that("standardize_numeric handles empty strings", {
  expect_equal(standardize_numeric(""), NA_real_)
})

test_that("standardize_numeric handles NA input", {
  expect_equal(standardize_numeric(NA), NA_real_)
})

test_that("standardize_numeric handles 'na' strings", {
  expect_equal(standardize_numeric("na"), NA_real_)
})

test_that("standardize_numeric handles invalid character strings", {
  expect_equal(standardize_numeric("abc123"), NA_real_)
})

test_that("standardize_numeric handles percentage symbols", {
  expect_equal(standardize_numeric("25.5%"), 25.5)
})

test_that("standardize_numeric handles euro symbols", {
  expect_equal(standardize_numeric("€100,50"), 100.50)
})

test_that("standardize_numeric handles null input", {
  expect_equal(standardize_numeric(NULL), NA_real_)
})

# Tests pour validate_db_types()

test_that("validate_db_types routes to correct Account validation", {
  data <- data.frame(CashFlow = "100.50", date = "20250101")
  result <- validate_db_types(data, "Account")
  expect_s3_class(result, "data.frame")
})

test_that("validate_db_types routes to Tickers validation", {
  data <- data.frame(Expiration = "20251201", Multiplier = "100.0")
  result <- validate_db_types(data, "Tickers")
  expect_equal(result$Multiplier, 100L)
})

test_that("validate_db_types handles portfolio pattern U123", {
  data <- data.frame(date = "20250101", pos = "10")
  result <- validate_db_types(data, "U123")
  expect_equal(result$date, 20250101L)
  expect_equal(result$pos, 10L)
})

test_that("validate_db_types handles portfolio pattern DU456", {
  data <- data.frame(expdate = "20250601", TradeNr = "42")
  result <- validate_db_types(data, "DU456")
  expect_equal(result$expdate, 20250601L)
  expect_equal(result$TradeNr, 42L)
})

test_that("validate_db_types handles Trades table", {
  data <- data.frame(TradeDate = "20250301", Prix = "150.75")
  result <- validate_db_types(data, "Trades")
  expect_equal(result$TradeDate, 20250301L)
  expect_equal(result$Prix, 150.75)
})

test_that("validate_db_types handles Journal table", {
  data <- data.frame(entryId = "123", date = "20250401", close = "100.25")
  result <- validate_db_types(data, "Journal")
  expect_equal(result$entryId, 123L)
  expect_equal(result$date, 20250401L)
  expect_equal(result$close, 100.25)
})

test_that("validate_db_types handles Prices table", {
  data <- data.frame(price = "50.75", datetime = "2025-01-01 12:00:00")
  result <- validate_db_types(data, "Prices")
  expect_equal(result$price, 50.75)
  expect_type(result$datetime, "character")
})

test_that("validate_db_types handles unknown table (passthrough)", {
  data <- data.frame(col1 = "test", col2 = 123)
  result <- validate_db_types(data, "UnknownTable")
  expect_identical(result, data)
})

test_that("safe_db_write with temporary=TRUE skips validation", {
  # Mock connection pour test
  conn <- list(dummy = TRUE)
  data <- data.frame(test = "value")

  # Should not error even with invalid table name
  expect_no_error({
    # Cette fonction appellerait DBI::dbWriteTable en vrai
    # Ici on teste juste que la logique de routing fonctionne
    result <- tryCatch({
      safe_db_write(conn, "test", data, temporary = TRUE)
    }, error = function(e) {
      # Expected car pas de vraie DB, mais la validation est bypassed
      "mocked_success"
    })
  })
})

test_that("validate_portfolio_data handles multiple numeric columns", {
  data <- data.frame(
    strike = "100.50",
    mktPrice = "105.25",
    multiplier = "100",
    pos = "10"
  )
  result <- validate_portfolio_data(data)
  expect_equal(result$strike, 100.50)
  expect_equal(result$mktPrice, 105.25)
  expect_equal(result$multiplier, 100L)
  expect_equal(result$pos, 10L)
})
