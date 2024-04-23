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
  last_currency_pairs = getCurrencyPairs()
  expect_true(is.data.frame(last_currency_pairs))
  expect_true(nrow(last_currency_pairs) == 1)
  expect_true(is.character(last_currency_pairs$date))
  expect_true(is.numeric(c(last_currency_pairs$EUR, last_currency_pairs$CHF)))
})

#### Test currency_convert #########


#### Test convert_to_usd_date #########
## 2023-11-20	EUR/USD 1.09253799915314	CHF/USD 1.1310042142868
test_that("Convert to USD a given amount in CHF and EUR", {
  expect_equal(round(convert_to_usd_date(100.45,"EUR",as.Date("2023-11-20")),2),
               round(100.45*1.0925,2))
  expect_equal(convert_to_usd_date(c(1000,2000),"EUR",as.Date("2023-12-03")),
               c(1088.3, 2176.6))
})

### 2021-01-08	EUR/USD 1.22714447975159	CHF/USD 1.12975203990936
test_that("Convert to USD a vector of CHF and EUR as of 9.01.2023 - Value from 8.01.2023 is taken as 9.01 was not recorded",{
  expect_equal(round(convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09")),2),
               round(c(10000*1.1298,500*1.2271),2)
  )
})

test_that("Convert to USD works with only one date", {
  expect_error(
    convert_to_usd_date(c(1000, 2000), c("EUR", "EUR"), c(as.Date("2023-12-03"),as.Date("2023-12-04")))
  )
})

test_that("Conversion format work for currency", {
  expect_equal(currency_format(100.45,"EUR"), "100.45 \U20AC")
  expect_equal(currency_format(10000,"CHF"), "10000.00 CHF")
  expect_equal(currency_format(758.458,"USD"), "758.46 $")
  expect_equal(currency_format(100000.455,"EUR"), "100000.46 \U20AC")
  expect_equal(currency_format(c(100.45,758.458),c("EUR","USD")), c("100.45 \U20AC", "758.46 $"))
  expect_equal(currency_format(c(100000.455,758.458,265.43) ,"USD"), c("100000.46 $","758.46 $","265.43 $"))
})
