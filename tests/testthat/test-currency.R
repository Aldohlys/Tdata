#### Test getAllCurrencyPairs ########
## mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB"))


test_that("It is possible to retrieve CurrencyPairs table, and it contains more than one record", {
  expect_true({
    tmp= getAllCurrencyPairs()
    is.data.frame(tmp) && (nrow(tmp) >0)
    })
})

#### Test getCurrencyPairs ########
## This one does not work well if one has to look up to IBKR
test_that("Up to date currency pairs can be retrieved either from IBKR or from end-user.", {
  expect_true({
    skip("getCurrencyPairs")
    tmp=getCurrencyPairs()
    dplyr::is_tibble(tmp) && (nrow(tmp == 1))
  })
})

#### Test currency_convert #########


#### Test convert_to_usd_date #########
## 2023-11-20	EUR/USD 1.09253799915314	CHF/USD 1.1310042142868
test_that("Convert to USD a given amount in CHF and EUR", {
  expect_equal(round(convert_to_usd_date(100.45,"EUR",as.Date("2023-11-20")),2),
               round(100.45*1.0925,2))
})

### 2021-01-08	EUR/USD 1.22714447975159	CHF/USD 1.12975203990936
test_that("Convert to USD a vector of CHF and EUR as of 9.01.2023 - Value from 8.01.2023 is taken as 9.01 was not recorded",{
  expect_equal(round(convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09")),2),
               round(c(10000*1.1298,500*1.2271),2)
  )
})


