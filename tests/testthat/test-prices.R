test_that("It is possible to retrieve correctly list of prices for a ticker and an interval date", {
  expect_true({
    p=getSymIntervalDate("GOOG",as.Date("2023-11-01"),as.Date("2023-11-05"))[[1]]
    round(as.numeric(p[3,6]))== 130
  })
})

test_that("It is possible to retrieve correctly a price for a ticker and a given date", {
  expect_equal(
    round(getSymPrice("GOOG",as.Date("2023-11-03"))),
    130
  )
})

test_that("It is possible to retrieve correctly prices for a list of tickers for a given date", {
  expect_equal(
    round(getSymPrice(c("GOOG","AAPL","ESTX50"),as.Date("2023-11-03"))),
    c(130, 176, 4175)
  )
})
