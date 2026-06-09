# Tests for volatility.R — pure-math / pure-logic functions only.
# IBKR / Yahoo / DB-touching paths (get30dIV, get180dIV, getVolMetrics,
# getIV_DTE, getOptionPrices, getIVPercentileLevels) are skipped because
# they need network/DB.

# Most helpers are not exported (@noRd); access via Tdata::: in tests.
walk_apply              <- Tdata:::walk_apply
signif_na               <- Tdata:::signif_na
check_na_ivs            <- Tdata:::check_na_ivs
calculate_forward_index <- Tdata:::calculate_forward_index
fit_volatility_parabola <- Tdata:::fit_volatility_parabola
predict_from_parabola   <- Tdata:::predict_from_parabola
calculate_target_vol    <- Tdata:::calculate_target_vol

# ---------------------------------------------------------------------------
# walk_apply / signif_na — element-wise apply with NA passthrough
# ---------------------------------------------------------------------------
test_that("signif_na rounds numeric vector to specified digits", {
  expect_equal(signif_na(c(1.23456, 9.87654), digits = 3), c(1.23, 9.88))
})

test_that("signif_na preserves NA values", {
  x <- c(1.234, NA_real_, 5.678)
  out <- signif_na(x, digits = 3)
  expect_equal(out[1], 1.23)
  expect_true(is.na(out[2]))
  expect_equal(out[3], 5.68)
})

test_that("walk_apply on a list applies fn to numeric elements only", {
  lst <- list(1.234, NA_real_, "5.678", "not_numeric", NULL)
  out <- walk_apply(lst, signif, digits = 3)
  expect_equal(out[[1]], 1.23)
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], 5.68)             # numeric string converted
  expect_equal(out[[4]], "not_numeric")    # non-numeric passthrough
  expect_null(out[[5]])
})

test_that("walk_apply on character vector with numeric strings converts and applies", {
  # Output stays character because walk_apply pre-allocates `result <- x`,
  # so numeric assignments are coerced back to character. Documents current
  # behaviour — callers feed character vectors only when they want the
  # rounded values back as strings.
  out <- walk_apply(c("1.234", "9.876"), signif, digits = 3)
  expect_equal(out, c("1.23", "9.88"))
})

# ---------------------------------------------------------------------------
# check_na_ivs — helper used by IV pipeline
# ---------------------------------------------------------------------------
test_that("check_na_ivs returns FALSE when matched_pairs has no NAs", {
  mp <- data.frame(strike = c(100, 105),
                   call_iv = c(0.20, 0.22),
                   put_iv  = c(0.21, 0.23))
  result <- check_na_ivs(list(matched_pairs = mp), "call", "SPY", "20260620")
  expect_false(result)
})

test_that("check_na_ivs returns TRUE when call_iv contains NA", {
  mp <- data.frame(strike = c(100, 105),
                   call_iv = c(0.20, NA_real_),
                   put_iv  = c(0.21, 0.23))
  expect_true(check_na_ivs(list(matched_pairs = mp), "call", "SPY", "20260620"))
})

# ---------------------------------------------------------------------------
# calculate_forward_index — put-call parity ATM extraction
# ---------------------------------------------------------------------------
test_that("forward index equals ATM strike when call_mid==put_mid at that strike", {
  options <- data.frame(
    strike   = c(95, 100, 105),
    call_mid = c(7.0, 4.0, 2.0),
    put_mid  = c(2.0, 4.0, 7.0)
  )
  # ATM strike (call==put) is 100. exp((r-q)*T) * (call - put) = 0 → F = 100
  F <- calculate_forward_index(options, interest_rate = 0.05,
                               time_to_expiry = 0.25, dividend_yield = 0.02)
  expect_equal(F, 100)
})

test_that("forward index uses put-call parity formula correctly", {
  options <- data.frame(
    strike   = c(100, 110),
    call_mid = c(5.0, 1.0),
    put_mid  = c(3.0, 9.0)
  )
  # ATM = strike with smallest |call - put|: at K=100, diff=2; at K=110, diff=8 → ATM=100
  # F = 100 + exp((0.05 - 0.02) * 0.5) * (5 - 3)
  expected <- 100 + exp((0.05 - 0.02) * 0.5) * 2
  F <- calculate_forward_index(options, 0.05, 0.5, 0.02)
  expect_equal(F, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# fit_volatility_parabola / predict_from_parabola — quadratic regression
# ---------------------------------------------------------------------------
test_that("fit_volatility_parabola recovers known quadratic coefficients", {
  strikes <- 80:120
  # IV = 0.0001*K^2 - 0.02*K + 1.5  (synthetic vol smile)
  ivs <- 0.0001 * strikes^2 - 0.02 * strikes + 1.5
  coefs <- fit_volatility_parabola(strikes, ivs)
  expect_length(coefs, 3)
  # Coefs returned as (intercept, b, a)
  expect_equal(coefs[1], 1.5,    tolerance = 1e-6)
  expect_equal(coefs[2], -0.02,  tolerance = 1e-8)
  expect_equal(coefs[3], 0.0001, tolerance = 1e-10)
})

test_that("fit_volatility_parabola returns NA triple when fewer than 3 valid points", {
  expect_equal(fit_volatility_parabola(c(100, 110), c(0.20, 0.22)),
               rep(NA_real_, 3))
  expect_equal(fit_volatility_parabola(c(100, 110, 120), c(0.20, NA, NA)),
               rep(NA_real_, 3))
})

test_that("predict_from_parabola evaluates a*K^2 + b*K + c", {
  coefs <- c(1.5, -0.02, 0.0001)  # (intercept, b, a)
  K <- 100
  expected <- 0.0001 * K^2 + (-0.02) * K + 1.5
  expect_equal(predict_from_parabola(coefs, K), expected)
})

test_that("predict_from_parabola returns NA when coefs include NA", {
  expect_true(is.na(predict_from_parabola(rep(NA_real_, 3), 100)))
})

# ---------------------------------------------------------------------------
# calculate_target_vol — full pipeline (synthetic options)
# ---------------------------------------------------------------------------
make_chain <- function(dte, atm = 100, vol = 0.20) {
  strikes <- seq(atm - 10, atm + 10, by = 5)
  # Symmetric prices around ATM (toy values, not real BS)
  call_mid <- pmax(0, atm + vol * 100 * sqrt(dte/365) - strikes * 0.5)
  put_mid  <- pmax(0, strikes * 0.5 - atm + vol * 100 * sqrt(dte/365))
  data.frame(
    strike          = strikes,
    days_to_expiry  = dte,
    call_mid        = call_mid,
    put_mid         = put_mid,
    call_iv         = vol + 0.0001 * (strikes - atm)^2,  # symmetric smile
    put_iv          = vol + 0.0001 * (strikes - atm)^2
  )
}

test_that("calculate_target_vol returns full structure with interpolation case", {
  near <- make_chain(dte = 21)
  next_ <- make_chain(dte = 49)
  result <- calculate_target_vol(
    near_options = near, next_options = next_,
    dividend_yield = 0.02,
    near_interest_rate = 0.05, next_interest_rate = 0.05,
    target_days = 30
  )
  expect_setequal(names(result), c(
    "near_forward","next_forward","near_atm_vol","next_atm_vol",
    "near_variance","next_variance","weights","variance_day","v"))
  expect_true(is.numeric(result$v))
  expect_length(result$weights, 2)
  # Interpolation: weights sum to 1 and both positive
  expect_equal(sum(result$weights), 1)
  expect_true(all(result$weights >= 0))
})

test_that("calculate_target_vol extrapolates towards present (target < near): w1>1, w2<0", {
  near <- make_chain(dte = 30)
  next_ <- make_chain(dte = 60)
  result <- calculate_target_vol(near, next_, 0.02, 0.05, 0.05, target_days = 15)
  # alpha = (15 - 30) / (60 - 30) = -0.5  → w1 = 1.5, w2 = -0.5
  expect_equal(result$weights[1], 1.5, tolerance = 1e-10)
  expect_equal(result$weights[2], -0.5, tolerance = 1e-10)
  expect_equal(sum(result$weights), 1)
})

test_that("calculate_target_vol extrapolates towards future (target > next): w1<0, w2>1", {
  near <- make_chain(dte = 30)
  next_ <- make_chain(dte = 60)
  result <- calculate_target_vol(near, next_, 0.02, 0.05, 0.05, target_days = 90)
  # alpha = (90 - 30) / (60 - 30) = 2.0  → w1 = -1, w2 = 2
  expect_equal(result$weights[1], -1, tolerance = 1e-10)
  expect_equal(result$weights[2],  2, tolerance = 1e-10)
  expect_equal(sum(result$weights), 1)
})

test_that("calculate_target_vol weights vary with target (no longer fixed by near/next)", {
  near <- make_chain(dte = 30)
  next_ <- make_chain(dte = 60)
  r45 <- calculate_target_vol(near, next_, 0.02, 0.05, 0.05, target_days = 45)
  r50 <- calculate_target_vol(near, next_, 0.02, 0.05, 0.05, target_days = 50)
  expect_false(isTRUE(all.equal(r45$weights, r50$weights)))
})

# ---------------------------------------------------------------------------
# getForwardPrice — exported BS forward formula
# ---------------------------------------------------------------------------
test_that("getForwardPrice matches F = S * exp((r-q)*T/365)", {
  expect_equal(getForwardPrice(100, 0.05, 0.02, 100),
               round(100 * exp((0.05 - 0.02) * 100/365), 2))
  expect_equal(getForwardPrice(50, 0.03, 0, 50),
               round(50 * exp(0.03 * 50/365), 2))
})

test_that("getForwardPrice with zero rates returns spot rounded to 2 decimals", {
  expect_equal(getForwardPrice(100, 0, 0, 30), 100)
})

test_that("getForwardPrice with r < q (negative cost of carry) gives F < S", {
  F <- getForwardPrice(100, 0.01, 0.05, 365)  # 1y forward
  expect_lt(F, 100)
})
