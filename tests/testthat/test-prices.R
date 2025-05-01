

test_that("It is possible to retrieve correctly list of closing prices for a ticker and an interval date", {
  expect_equal(
    getSymMetricIntervalDate("GOOG", as.Date("2023-11-01"),as.Date("2023-11-05"), "Close")$GOOG,
    c(127.57, 128.58, 130.37),
    tolerance = 0.01
  )
})


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
    as.integer(getSymPrice(c("GOOG","AAPL","ESTX50"),
                      as.Date("2023-11-03"), metric = "Close")),
    c(130, 176, 4174)
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

test_that("getYahoo retrieves data for reliable tickers correctly", {
  skip_if_offline()  # Skip test if internet connection is unavailable

  # Explicitly use xts namespace without loading the whole package
  library(xts)  # This is needed but we'll handle the warning

  # Test with a reliable ticker
  test_ticker <- "AAPL"
  start_date <- Sys.Date() - 30  # Last 30 days
  end_date <- Sys.Date()

  # Run the function with verbose=FALSE to avoid cluttering test output
  result <- tryCatch({
    getYahooData(test_ticker, start_date, end_date,
                       verbose = FALSE,
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
  test_tickers <- c("AAPL", "NONEXISTENTTICKER123", "MSFT")
  start_date <- Sys.Date() - 10

  # Run with verbose=FALSE to avoid cluttering test output and
  # catch errors to make test more robust
  result <- tryCatch({
    suppressWarnings(
      getYahooData(test_tickers, start_date, chunk_size = 3,
                         verbose = FALSE, timeout = 1)
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
  expect_true(any(c("AAPL", "MSFT") %in% result$ticker))
})

test_that("getYahoo works with mock ticker data", {
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
      getYahooData(mock_tickers, Sys.Date() - 5, verbose = FALSE, timeout = 5)
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
