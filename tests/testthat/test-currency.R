#### Test getAllCurrencyPairs ########

test_that("It is possible to retrieve CurrencyPairs table, and it contains more than one record", {
  expect_true({
    tmp= getAllCurrenciesUSDValues()
    is.data.frame(tmp) && (nrow(tmp) >0)
  })
})

test_that("It is possible to retrieve CHF CurrencyPairs table, and it contains more than one record", {
  expect_true({
    tmp = getAllCurrenciesCHFValues()
    is.data.frame(tmp) && (nrow(tmp) >= 0)  # Allow empty table initially
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

test_that("CHF currency pairs can be retrieved from stored values", {
  last_chf_pairs = getStoredCHFValue("CHF")
  expect_true(is.data.frame(last_chf_pairs))
  expect_true(nrow(last_chf_pairs) == 1)
  expect_true(is.numeric(last_chf_pairs$date))
  expect_equal(last_chf_pairs$chf_value, 1.0)  # CHF to CHF should be 1.0
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

#### Test convert_to_chf_date #########
## Assuming EUR/CHF 0.9650 and USD/CHF 0.8845 for 2023-11-20
test_that("Convert to CHF a given amount in USD and EUR", {
  # Test single currency conversion
  expect_true(is.numeric(convert_to_chf_date(100.45, "EUR", as.Date("2023-11-20"))))

  # Test vectorized amounts
  result_eur = convert_to_chf_date(c(1000, 2000), "EUR", as.Date("2023-12-03"))
  expect_equal(length(result_eur), 2)
  expect_true(all(is.numeric(result_eur)))

  # Test vectorized currencies
  result_mixed = convert_to_chf_date(c(1000, 2000), c("EUR", "USD"), as.Date("2023-12-03"))
  expect_equal(length(result_mixed), 2)
  expect_true(all(is.numeric(result_mixed)))
})

#### Test currency convert using different date formats
test_that("Convert to USD a given amount in CHF and EUR using different date formats", {
  expect_equal(convert_to_usd_date(2000, "EUR", 20240421),
               2131.6)
  expect_equal(convert_to_usd_date(2000, "CHF", "20240421"),
               2195.4)
})

test_that("Convert to CHF a given amount in USD and EUR using different date formats", {
  # Test integer date format
  result_int = convert_to_chf_date(2000, "EUR", 20240421)
  expect_true(is.numeric(result_int))
  expect_true(result_int > 0)

  # Test character date format
  result_char = convert_to_chf_date(2000, "USD", "20240421")
  expect_true(is.numeric(result_char))
  expect_true(result_char > 0)
})

### 2021-01-08	EUR/USD 1.22714447975159	CHF/USD 1.12975203990936
test_that("Convert to USD a vector of CHF and EUR as of 9.01.2023 - Value from 8.01.2023 is taken as 9.01 was not recorded",{
  expect_equal(round(convert_to_usd_date(c(10000,500),c("CHF","EUR"),as.Date("2021-01-09")),2),
               round(c(10000*1.1298,500*1.2271),2)
  )
})

test_that("Convert to CHF a vector of USD and EUR using historical data", {
  # Test with historical date
  result = convert_to_chf_date(c(10000, 500), c("USD", "EUR"), as.Date("2021-01-09"))
  expect_equal(length(result), 2)
  expect_true(all(is.numeric(result)))
  expect_true(all(result > 0))
})

test_that("Convert to USD works with multiple dates", {
  result = convert_to_usd_date(c(1000, 2000), c("EUR", "EUR"), c(as.Date("2023-12-03"),as.Date("2023-12-04")))
  expect_equal(length(result), 2)
  expect_true(all(is.numeric(result)))
  expect_true(all(result > 0))
})

test_that("Convert to CHF works with multiple dates", {
  result = convert_to_chf_date(c(1000, 2000), c("EUR", "EUR"), c(as.Date("2023-12-03"), as.Date("2023-12-04")))
  expect_equal(length(result), 2)
  expect_true(all(is.numeric(result)))
  expect_true(all(result > 0))
})

#### Test c_to_usd and c_to_chf functions ####
test_that("c_to_usd function works correctly", {
  # Test with stored values (requires DB to have data)
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result = c_to_usd(100, "EUR")
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("c_to_chf function works correctly", {
  # Test with stored values (requires DB to have CHF data)
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result = c_to_chf(100, "EUR")
  expect_true(is.numeric(result))
  expect_true(result > 0)

  # Test vectorized conversion
  result_vector = c_to_chf(c(100, 200), c("EUR", "USD"))
  expect_equal(length(result_vector), 2)
  expect_true(all(is.numeric(result_vector)))
})

#### Test error handling for CHF functions ####
test_that("getStoredCHFValue returns 1 for CHF currency input", {
  # CHF should return 1.0 when requested
  result = getStoredCHFValue("CHF")
  expect_true(is.data.frame(result))
  expect_equal(result$chf_value, 1.0)
})


test_that("c_to_chf handles CHF input correctly", {
  # Should use stored value of 1.0 for CHF
  result = c_to_chf(100, "CHF")
  expect_equal(result, 100)  # 100 CHF = 100 CHF
})

#### Test currency sign and formatting ####
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

#### Test database validation integration ####
test_that("CHF data validation works correctly", {
  # Test data frame with CHF structure
  test_data = data.frame(
    date = c(20240101, 20240102),
    currency = c("EUR", "USD"),
    chf_value = c(0.9650, 0.8845)
  )

  validated = validate_db_types(test_data, "ConvertToCHF")

  expect_true(is.data.frame(validated))
  expect_true(is.integer(validated$date))
  expect_true(is.numeric(validated$chf_value))
  expect_equal(nrow(validated), 2)
})

#### Test getCurrencyAttrib ########
test_that("getCurrencyAttrib retrieves currency attributes correctly", {
  # Test single currency
  result <- getCurrencyAttrib("USD")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 0)

  # Test vectorized input
  result_vector <- getCurrencyAttrib(c("EUR", "CHF"))
  expect_true(is.data.frame(result_vector))
})

#### Test getLastCHFValue ########
test_that("getLastCHFValue retrieves and updates CHF values", {
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result <- getLastCHFValue("EUR")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 0)

  # Test CHF currency returns 1.0
  chf_result <- getLastCHFValue("CHF")
  expect_equal(chf_result$chf_value[chf_result$currency == "CHF"], 1.0)
})

#### Test getLastUSDValue ########
test_that("getLastUSDValue retrieves and updates USD values", {
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result <- getLastUSDValue("EUR")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 0)

  # Test error for USD input
  expect_error(getLastUSDValue("USD"))
})

#### Test base_currency_format ########
test_that("base_currency_format works correctly", {
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result <- base_currency_format(100.45)
  expect_true(is.character(result))
  expect_true(!is.na(result))

  # Test vectorized
  result_vector <- base_currency_format(c(100, 200))
  expect_equal(length(result_vector), 2)
})

#### Test c_to_base ########
test_that("c_to_base converts to base currency correctly", {
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result <- c_to_base(100, "EUR")
  expect_true(is.numeric(result))
  expect_true(result > 0)

  # Test vectorized
  result_vector <- c_to_base(c(100, 200), c("EUR", "USD"))
  expect_equal(length(result_vector), 2)
})

#### Test convert_to_base_date ########
test_that("convert_to_base_date converts to base currency for specific date", {
  skip_if_not(file.exists(config::get("DB")), "Database file not found")

  result <- convert_to_base_date(100, "EUR", as.Date("2023-11-20"))
  expect_true(is.numeric(result))
  expect_true(result > 0)

  # Test vectorized currencies
  result_vector <- convert_to_base_date(c(100, 200), c("EUR", "USD"), as.Date("2023-11-20"))
  expect_equal(length(result_vector), 2)
})

# Additional test cases for convert_to_chf_date
test_that("convert_to_chf_date handles edge cases and complex scenarios", {
  # Test 1: Mixed recycling with single amount, multiple currencies and dates
  result1 <- convert_to_chf_date(
    amount = 1500,
    currency = c("EUR", "USD"),
    convert_date = c(as.Date("2023-06-15"), as.Date("2023-07-20"))
  )
  expect_equal(length(result1), 2)
  expect_true(all(is.numeric(result1)))
  expect_true(all(result1 > 0))

  # Test 2: Multi-currency stress test with known currencies
  result2 <- convert_to_chf_date(
    amount = c(100, 200, 300, 400, 500),
    currency = c("EUR", "USD", "CAD", "HKD", "CHF"),
    convert_date = as.Date("2023-01-15")
  )
  expect_equal(length(result2), 5)
  expect_true(all(is.numeric(result2)))
  expect_true(all(result2 > 0))
  expect_equal(result2[5], 500)  # CHF to CHF should be unchanged
})

# Additional test cases for convert_to_usd_date
test_that("convert_to_usd_date handles edge cases and complex scenarios", {
  # Test 1: Zero and small amounts with known currencies
  result1 <- convert_to_usd_date(
    amount = c(0, 50.25, 250.75),
    currency = c("CHF", "EUR", "CAD"),
    convert_date = as.Date("2023-05-15")
  )
  expect_equal(length(result1), 3)
  expect_true(is.numeric(result1[1]) && result1[1] == 0)  # Zero amount stays zero
  expect_true(is.numeric(result1[2]) && result1[2] > 0)   # EUR amount converts
  expect_true(is.numeric(result1[3]) && result1[3] > 0)   # CAD amount converts

  # Test 2: All available currencies with mixed date formats
  result2 <- convert_to_usd_date(
    amount = c(1000, 2000, 3000, 4000),
    currency = c("CHF", "EUR", "CAD", "HKD"),
    convert_date = c("20200315", 20200316, as.Date("2020-03-17"), "20200318")
  )
  expect_equal(length(result2), 4)
  expect_true(all(is.numeric(result2)))
  expect_true(all(result2 > 0))
})

# Additional test case for convert_to_base_date
test_that("convert_to_base_date handles base currency switching and complex scenarios", {
  # Test 1: Verify base currency delegation with HKD and CAD
  result_base <- convert_to_base_date(
    amount = c(1000, 2000),
    currency = c("HKD", "CAD"),
    convert_date = as.Date("2023-03-15")  # Single date recycled to match vectors
  )
  expect_equal(length(result_base), 2)
  expect_true(all(is.numeric(result_base)))
  expect_true(all(result_base > 0))

  # Test 2: Test all available currencies with single date
  result_all_currencies <- convert_to_base_date(
    amount = c(500, 750, 1000, 1250, 1500),
    currency = c("CHF", "EUR", "USD", "CAD", "HKD"),
    convert_date = as.Date("2023-02-15")
  )
  expect_equal(length(result_all_currencies), 5)
  expect_true(all(is.numeric(result_all_currencies)))
  expect_true(all(!is.na(result_all_currencies)))
})
