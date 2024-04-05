test_that("It is possible to retrieve correctly prices for a ticker and a given date", {
  expect_true({
    p_10=head(getSymFromDate("GOOG",as.Date("2023-12-01")))[[1]][1]
    round(as.numeric(p_10[,6]))== 133
  })
})
