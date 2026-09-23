### ---------------------------------------------------------------------------
### ibkr_fx_rows() / fx_drop_implausible(): FX table writes
### ---------------------------------------------------------------------------

test_that("ibkr_fx_rows reproduces the 2026-06-13 and 08-09 fetches correctly", {
  ### Values logged by tdata_py$retrieveCurrencyPairs (USD per 1 unit).
  r <- ibkr_fx_rows(c("GBP", "JPY", "CAD"), c(1.3404, 0.0063, 0.7149),
                    c("Yes", "No", "No"), chf_per_usd = 0.7964, date = 20260613L)
  ### CHF per unit = USD per unit x CHF per USD (was divided -> GBP 1.683, JPY 195.96).
  expect_equal(r$chf$chf_value, round(c(1.3404, 0.0063, 0.7149) * 0.7964, 6))
  expect_true(abs(r$chf$chf_value[1] - 1.0675) < 0.001)
  expect_true(r$chf$chf_value[2] < 0.01)
  ### ConvertToUSD keeps the pair as quoted: JPY/CAD per USD.
  expect_equal(r$usd$usd_value, round(c(1.3404, 1 / 0.0063, 1 / 0.7149), 4))
})

test_that("ibkr_fx_rows drops currencies IBKR returned no price for", {
  r <- ibkr_fx_rows(c("EUR", "GBP"), c(NaN, 1.35), c("Yes", "Yes"), 0.8, 20260905L)
  expect_equal(r$chf$currency, "GBP")
  expect_equal(r$usd$currency, "GBP")
})

test_that("fx_drop_implausible keeps normal moves and drops >5% jumps", {
  df <- data.frame(date = 20260421L, currency = c("GBP", "EUR", "KRW"),
                   chf_value = c(0.577018, 0.9170, 0.0006), stringsAsFactors = FALSE)
  out <- fx_drop_implausible(df, "chf_value", c("GBP", "EUR"), c(1.0562, 0.9195))
  ### GBP 0.577 vs 1.056 dropped; EUR -0.3% kept; KRW has no stored rate: kept.
  expect_equal(out$currency, c("EUR", "KRW"))
})
