

test_that("It is possible to retrieve correctly list of closing prices for a ticker and an interval date", {
  expect_equal(
    as.numeric(getSymPriceIntervalDate("GOOG", as.Date("2023-11-01"),as.Date("2023-11-05"), "Close")),
    c(127.57, 128.58, 130.37),
    tolerance = 0.01
  )
})


test_that("It is possible to retrieve correctly list of prices for a ticker and an interval date", {
  expect_true({
    p=getSymIntervalDate("GOOG",as.Date("2023-11-01"),as.Date("2023-11-05"))[[1]]
    round(as.numeric(p[3,6]))== 130
  })
})

test_that("It is possible to retrieve correctly a price for a ticker without date", {
  expect_type(getSymPrice("ESTX50"), "double")
})

test_that("It is possible to retrieve correctly a price for a ticker and a given date", {
  expect_equal(
    round(getSymPrice("GOOG",as.Date("2023-11-03"))),
    130
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
                      as.Date("2023-11-03"), OHLCVA = "Close")),
    c(130, 176, 4174)
  )
})

test_that("Trying to return one ticker for a set of dates will work",{
  expect_equal(
    as.integer(getSymPrice(c("SPY"),
                           c(as.Date("2024-01-10"), as.Date("2024-02-15"), as.Date("2024-02-18")),
                           OHLCVA = "Close")),
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
  df <- getLastSymPrice(c("SPY","XSP"))
  expect_true(is.data.frame(df))
  expect_true(all(df$sym == c("SPY","XSP")))
  expect_true(all(is.numeric(df$value)))
  expect_true(all(class(df$date) == "Date"))
})
