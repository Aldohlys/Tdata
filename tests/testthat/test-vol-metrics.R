# Vol-of-vol percentile mapping.
#
# The estimator's absolute level is not interpretable (a 10d rolling window
# returns ~1.94 even when true vol-of-vol is zero), so /analyze reports a
# percentile against stored breakpoints. These tests pin the mapping and its
# degradation path; they do not hit the network or the live Tickers table.

fake_bp <- data.frame(
  Quantile = c(1, 5, 10, 25, 50, 75, 90, 95, 99),
  Value    = c(1.98, 2.12, 2.22, 2.35, 2.49, 2.74, 2.98, 3.22, 3.53),
  NObs     = 208L,
  stringsAsFactors = FALSE
)

test_that("vov_to_percentile interpolates between stored breakpoints", {
  expect_equal(vov_to_percentile(2.49, fake_bp), 50L)
  expect_equal(vov_to_percentile(2.35, fake_bp), 25L)
  ### Midway between the 50th (2.49) and 75th (2.74) cut-points: 62.5, and
  ### round() is half-to-even, so 62 rather than 63.
  expect_equal(vov_to_percentile(2.615, fake_bp), 62L)
})

test_that("vov_to_percentile is monotone in the reading", {
  vals <- c(1.5, 2.0, 2.3, 2.6, 3.0, 3.4, 4.0)
  pcts <- vapply(vals, vov_to_percentile, integer(1), breakpoints = fake_bp)
  expect_false(is.unsorted(pcts))
  expect_true(all(pcts >= 0 & pcts <= 100))
})

test_that("vov_to_percentile clamps outside the stored range", {
  ### rule = 2 pins to the end quantiles rather than extrapolating off the grid
  expect_equal(vov_to_percentile(0.5, fake_bp), 1L)
  expect_equal(vov_to_percentile(9.9, fake_bp), 99L)
})

test_that("vov_to_percentile returns NA when breakpoints are unavailable", {
  expect_true(is.na(vov_to_percentile(2.5, NULL)))
  expect_true(is.na(vov_to_percentile(NA_real_, fake_bp)))
  expect_true(is.na(vov_to_percentile(Inf, fake_bp)))
})

test_that("compute_vol_of_vol degrades to a stated reason without breakpoints", {
  core <- list(vol_of_vol = 2.5, recent_vol_of_vol = 2.2, current_rv = 20,
               rv_percentile = 44, observations = 480)

  with_mocked_bindings(
    .vov_core = function(...) core,
    get_vov_breakpoints = function() NULL, {
      r <- compute_vol_of_vol("ANY")
      expect_true(is.na(r$vov_percentile))
      expect_match(r$interpretation, "percentile unavailable")
    })
})

test_that("compute_vol_of_vol reports the percentile when breakpoints exist", {
  core <- list(vol_of_vol = 2.49, recent_vol_of_vol = 2.2, current_rv = 20,
               rv_percentile = 44, observations = 480)

  with_mocked_bindings(
    .vov_core = function(...) core,
    get_vov_breakpoints = function() fake_bp, {
      r <- compute_vol_of_vol("ANY")
      expect_equal(r$vov_percentile, 50L)
      expect_match(r$interpretation, "percentile 50 of the 208-name")
      ### The raw level is retained as provenance for the percentile
      expect_equal(r$vol_of_vol, 2.49)
    })
})
