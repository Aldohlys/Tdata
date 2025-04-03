test_that("getTickers works", {
  data <- getTickers()
  expect_true(nrow(data) > 0)
})

test_that("getTicker works", {
  data <- getTicker("SPX")
  expect_true(nrow(data) > 0)
})


test_that("addTicker works", {
  data <- addTicker("SPY")
  expect_equal(data , 0)
})

test_that("addTicker works", {
  data <- addTicker("USO")
  expect_equal(data , 1)
})

test_that("removeTicker works", {
  data <- removeTicker("USO")
  expect_equal(data , 1)
})

test_that("removeTicker works", {
  data <- removeTicker("gergergeer")
  expect_equal(data , 0)
})
