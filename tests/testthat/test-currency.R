#### Test getAllCurrencyPairs ########
## mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB"))


test_that("It is possible to retrieve CurrencyPairs table, and it contains more than one record", {
  expect_true({
    tmp= getAllCurrenciesUSDValues()
    is.data.frame(tmp) && (nrow(tmp) >0)
    })
})

#### Test getCurrencyPairs ########
## This one does not work well if one has to look up to IBKR
test_that("Up to date currency pairs can be retrieved either from IBKR or from end-user.", {
  last_currency_pairs = getStoredUSDValue("USD")
  expect_true(is.data.frame(last_currency_pairs))
  expect_true(nrow(last_currency_pairs) == 1)
  expect_true(is.numeric(last_currency_pairs$date))
})


#### Test convert_to_usd_date #########
## 2023-11-20	EUR/USD 1.09253799915314	CHF/USD 1.1310042142868
test_that("Convert to USD a given amount in CHF and EUR", {
  expect_equal(round(convert_to_usd_date(100.45,"EUR",as.Date("2023-11-20")),2),
               round(100.45*1.0907,2))
  expect_equal(convert_to_usd_date(c(1000,2000),"EUR",as.Date("2023-12-03")),
               c(1088.8, 2177.6))
  expect_equal(convert_to_usd_date(c(1000,2000),c("EUR","CHF"),as.Date("2023-12-03")),
               c(1088.8, 2304.6))
})

###┼ Test currency convert using different date formats
test_that("Convert to USD a given amount in CHF and EUR using different date formats", {
  expect_equal(convert_to_usd_date(2000, "EUR", 20240421),
              2131.6)
  expect_equal(convert_to_usd_date(2000, "CHF", "20240421"),
               2195.4)
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

test_that("Able to retrieve currency sign for currency", {
  expect_equal(currency_sign("USD"), "$")
  expect_equal(currency_sign("CHF"), "CHF")
  expect_equal(currency_sign("EUR"), "€")
  expect_equal(currency_sign(c("EUR","EUR")), c("€", "€"))
  expect_equal(currency_sign(c("EUR", "USD")), c("€", "$"))
})


test_that("Conversion format work for currency", {
  expect_equal(currency_format(100.45,"EUR"), "100.45 \U20AC")
  expect_equal(currency_format(10000,"CHF"), "10 000.00 CHF")
  expect_equal(currency_format(758.458,"USD"), "758.46 $")
  expect_equal(currency_format(100000.455,"EUR"), "100 000.46 \U20AC")
  expect_equal(currency_format(c(100.45,758.458),c("EUR","USD")), c("100.45 \U20AC", "758.46 $"))
  expect_equal(currency_format(c(100000.455,758.458,265.43) ,"USD"), c("100 000.46 $","758.46 $","265.43 $"))
  expect_equal(currency_format("100", c("CHF", "USD")), c("100.00 CHF", "100.00 $"))
  expect_equal(currency_format(100.00, c("CHF", "USD")), c("100.00 CHF", "100.00 $"))
})

test_that("Conversion format detects correctly non numeric issue", {
  expect_true(is.na(currency_format(c("10,15,5", 100), c("CHF", "USD"))))
})

test_that("Conversion format detects correctly length issues", {
  expect_true(is.na(currency_format(c(200, 100, 50), c("CHF", "USD"))))
})
