
############  Test prices ###########
test_that("It is possible to retrieve correctly list of prices for a ticker and an interval date", {
  expect_true({
    p=getSymIntervalDate("GOOG",as.Date("2023-11-01"),as.Date("2023-11-05"))
    round(as.numeric(p[3,"Close"]))== 130
  })
})

test_that("It is possible to retrieve correctly a last price for a ticker without date", {
  expect_type(getLastAdjustedPrice("ESTX50"), "double")
})

test_that("It is possible to retrieve correctly a price for a ticker and a given date", {
  expect_equal(
    trunc(getSymPrice("GOOG",as.Date("2023-11-03"))), ## This makes it resistant when Yahoo does not send decimals back
    129
  )
})

test_that("If I try to retrieve a price for a date in the future then it returns NA and display error message", {
  expect_true(
    is.na(getSymPrice(c("SPY","USO"),c(as.Date("2099-04-26"))))
  )
})

test_that("It is possible to retrieve correctly prices for a list of tickers for a given date", {
  expect_equal(
    as.integer(getSymPrice(c("GOOG","SIE","ESTX50"),
                      as.Date("2023-11-03"), metric = "Close")),
    c(130, 127, 4174)
  )
})

test_that("Trying to return one ticker for a set of dates will work",{
  expect_equal(
    as.integer(getSymPrice(c("SPY"),
                           c(as.Date("2024-01-10"), as.Date("2024-02-15"), as.Date("2024-02-18")),
                           metric = "Close")),
    c(476, 502, 499)
  )
})

test_that("It is possible to retrieve a vector of prices corresponding to a vector of sym for a vector of dates",{
  expect_equal(
    as.integer(getSymPrice(c("USO","SLV","GLD"),
                           c(as.Date("2024-01-10"), as.Date("2024-02-15"), as.Date("2024-02-18")))),
    c(66 , 20, 186)
  )
})

test_that("Retrieve a tibble with all last prices from a vector of tickers",{
  df <- getLastSymPrice(c("SPY","SPX"))
  expect_true(is.data.frame(df))
  expect_true(all(df$sym == c("SPY","SPX")))
  expect_true(all(is.numeric(df$value)))
  expect_true(all(class(df$date) == "Date"))
})


test_that("getYahooData is able to work with tickers that have special names: ^XSP, 1810.HK, U-UN.TO", {
 data <- getYahooData(c("^XSP", "1810.HK", "U-UN.TO", "XAU.TO"), as.Date("2025-06-25"))

 # Test that data is returned (not NULL)
 expect_false(is.null(data))

 # Test that data is a data.frame
 expect_true(is.data.frame(data))

 # Test that we have data for all tickers
 expect_equal(length(unique(data$ticker)), 4)
 expect_true(all(c("^XSP", "1810.HK", "U-UN.TO", "XAU.TO") %in% data$ticker))

 # Test expected columns exist
 expected_cols <- c("date", "ticker", "Open", "High", "Low", "Close", "Adjusted", "Volume")
 expect_true(all(expected_cols %in% colnames(data)))

 # Test no NA values in essential columns (except Volume which can be 0/NA for some markets)
 essential_cols <- c("Open", "High", "Low", "Close", "Adjusted")
 for (col in essential_cols) {
   expect_false(all(is.na(data[[col]])),
                info = paste("Column", col, "should not be all NA"))
 }

 # Test that dates are proper Date objects
 expect_true(inherits(data$date, "Date"))

 # Test that all dates are >= the requested from_date
 expect_true(all(data$date >= as.Date("2025-06-25")))

 # Test that numeric columns are actually numeric
 numeric_cols <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
 for (col in numeric_cols) {
   expect_true(is.numeric(data[[col]]),
               info = paste("Column", col, "should be numeric"))
 }

 # Test that High >= Low for all rows (basic data validation)
 valid_rows <- !is.na(data$High) & !is.na(data$Low)
 expect_true(all(data$High[valid_rows] >= data$Low[valid_rows]),
             info = "High prices should be >= Low prices")
})

test_that("getYahooData retrieves data for reliable tickers correctly", {
  skip_if_offline()  # Skip test if internet connection is unavailable

  # Explicitly use xts namespace without loading the whole package
  library(xts)  # This is needed but we'll handle the warning

  # Test with a reliable ticker
  test_ticker <- "AAPL"
  start_date <- Sys.Date() - 30  # Last 30 days
  end_date <- Sys.Date()

  result <- tryCatch({
    getYahooData(test_ticker, start_date, end_date,
                       max_retries = 3,
                       timeout = 5)  # Increase timeout for CI environments
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  # Skip if we couldn't get data
  skip_if(is.null(result), "Failed to retrieve test data from Yahoo Finance")

  # Verify structure and content
  expect_true(!is.null(result))
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 8)
  expect_equal(unique(result$ticker), test_ticker)

  # Verify date range (allowing for weekends/holidays)
  expect_true(start_date <= min(result$date))
  expect_true(end_date >= max(result$date))

  # Verify we have reasonable data values - be slightly more lenient
  # as occasionally Yahoo returns 0 values
  expect_true(any(result$Adjusted > 0))  # At least some prices should be positive
  expect_true(nrow(result) >= 10) # Should have at least 10 trading days in a month
})

test_that("getYahooData handles non-existent tickers gracefully", {
  skip_if_offline()  # Skip test if internet connection is unavailable

  # Test with a mix of valid and invalid tickers
  test_tickers <- c("AAPL", "NONEXISTENTTICKER123")
  start_date <- Sys.Date() - 5

  result <- tryCatch({
    suppressWarnings(
      getYahooData(test_tickers, start_date, chunk_size = 3,
                         timeout = 1)
    )
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  # Skip if we couldn't get any data
  skip_if(is.null(result), "Failed to retrieve test data from Yahoo Finance")

  # Should return data despite the bad ticker
  expect_true(!is.null(result))

  # Check that the fake ticker is not in the results
  expect_false("NONEXISTENTTICKER123" %in% colnames(result))

  # Check that at least one of the valid tickers was retrieved
  expect_true("AAPL" %in% result$ticker)
})

test_that("getYahooData works with mock ticker data", {
  skip_if_offline()  # Skip test if internet connection is unavailable

  # Create a mock getTickers output - just use one reliable ticker for speed
  mock_tickers <- data.frame(
    Name = "AAPL",
    YahooName = "AAPL",
    stringsAsFactors = FALSE
  )

  # Test the function with the mock data frame
  result <- tryCatch({
    suppressWarnings(
      getYahooData(mock_tickers, Sys.Date() - 5, timeout = 5)
    )
  }, error = function(e) {
    skip(paste("Yahoo Finance API unavailable:", e$message))
    NULL
  })

  # Skip if we couldn't get any data
  skip_if(is.null(result), "Failed to retrieve test data from Yahoo Finance")

  # Should return some data
  expect_true(!is.null(result))
  expect_true("AAPL" %in% result$ticker)
})


test_that("getStockPrice works with symbol SPY -1",{
  spy <- getStockPrice("SPY", close=FALSE)
  expect_true(all(is.character(c(spy$datetime, spy$sym))))
  expect_true(is.numeric(spy$price))
})

test_that("getStockPrice works with symbol SPY -2",{
  spy <- getStockPrice("SPY", close=TRUE)
  expect_true(all(is.character(c(spy$datetime, spy$sym))))
  expect_true(is.numeric(spy$price))
})

test_that("getStockPrice works with symbol SPY - 3",{
  spy <- getStockPrice("SPY")
  expect_true(all(is.character(c(spy$datetime, spy$sym))))
  expect_true(is.numeric(spy$price))
})

test_that("getStockPrice works with 2 symbols SPY, SPX", {
  res <- getStockPrice(c("SPY", "SPX"))
  expect_true(all(is.character(c(res$datetime, res$sym))))
  expect_true(is.numeric(res$price))
  expect_length(res$price, 2)
})

test_that("If close data requesed, getStockPrice returns NA for non-Yahoo syms - US-T, SOFR3", {
  res <- getStockPrice(c("US-T", "SR3Z6"), close=TRUE)
  expect_true(all(is.character(c(res$datetime, res$sym))))
  expect_true(is.numeric(res$price))
  expect_length(res$price, 2)
  expect_true(all(is.na(res$price)))
})

test_that("getSymIntervalDate works with SPY and USO", {
  res <- getSymMetricIntervalDate(sym = c("SPY","USO"), as.Date("2025-05-20"))
  expect_true(inherits(res$date, "Date"))
  expect_true(is.numeric(res$SPY))
  expect_true(is.numeric(res$USO))
  expect_true(ncol(res) == 3)
})

test_that("getStockPrice works for future -with IBKR", {
  res <- getStockPrice("SR3Z6")
  expect_true(all(is.character(c(res$datetime, res$sym))))
  expect_true(is.numeric(res$price))
  expect_length(res$price, 1)
})

test_that("getSymIntervalDate works with US-T, GVGV and USO", {
  res <- getSymMetricIntervalDate(sym = c("US-T", "GVGV" , "USO"), from=as.Date("2025-05-20"), to=as.Date("2025-05-31"))
  expect_true(inherits(res$date, "Date"))
  expect_true(is.numeric(res$USO))
  expect_true(ncol(res) == 4)
  expect_true(all(is.na(res$GVGV)))
})
