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
