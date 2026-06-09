# Tests for atr_move.R — pure-logic parts only.
# The fetch (atr_pct / atr_move_distribution / atr_expected_move) needs
# Yahoo and is skipped offline; we test the Wilder ATR and the quantile/
# band projection on a synthetic distribution.

wilder_atr <- Tdata:::.wilder_atr

# ---------------------------------------------------------------------------
# .wilder_atr — recursive EMA (alpha = 1/n), warm-up NAs
# ---------------------------------------------------------------------------
test_that(".wilder_atr returns NA during warm-up then finite values", {
  set.seed(7)
  n <- 60
  close <- 100 + cumsum(rnorm(n, 0, 1))
  high  <- close + runif(n, 0.2, 1)
  low   <- close - runif(n, 0.2, 1)
  atr <- wilder_atr(high, low, close, n = 14)
  expect_length(atr, n)
  expect_true(all(is.na(atr[1:13])))      # first n-1 are warm-up
  expect_true(all(is.finite(atr[14:n])))
  expect_true(all(atr[14:n] > 0))
})

test_that(".wilder_atr tracks a constant true range to that range", {
  # Constant daily range of 2 (high-low=2, no gaps) → ATR converges to 2
  close <- rep(100, 100)
  high  <- rep(101, 100)
  low   <- rep(99, 100)
  atr <- wilder_atr(high, low, close, n = 14)
  expect_equal(utils::tail(atr, 1), 2, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# atr_expected_move_from_dist — quantile → band, asymmetry, flags
# ---------------------------------------------------------------------------
make_dist <- function(standardized, atr_pct = 2, N = 10, spot = 100, sym = "TEST") {
  list(sym = sym, spot = spot, horizon_sessions = N,
       atr_pct = atr_pct, n_obs = length(standardized),
       standardized = standardized)
}

test_that("symmetric distribution yields a near-symmetric band (asymmetry ~ 1)", {
  set.seed(1)
  d <- make_dist(rnorm(5000, 0, 1))      # symmetric, no drift
  r <- atr_expected_move_from_dist(d, conf = 0.80)
  expect_equal(r$coef_median, 0, tolerance = 0.05)
  expect_equal(r$asymmetry, 1, tolerance = 0.12)
  # lower band below spot, upper above
  expect_lt(r$price_lower, r$spot)
  expect_gt(r$price_upper, r$spot)
})

test_that("right-skewed distribution gives asymmetry > 1 (upside reach larger)", {
  set.seed(2)
  d <- make_dist(c(rnorm(4000, 0.1, 1), rexp(1000, 1)))  # positive skew
  r <- atr_expected_move_from_dist(d, conf = 0.80)
  expect_gt(r$asymmetry, 1)
})

test_that("wider confidence widens the band", {
  set.seed(3)
  d <- make_dist(rnorm(5000, 0, 1))
  r80 <- atr_expected_move_from_dist(d, conf = 0.80)
  r90 <- atr_expected_move_from_dist(d, conf = 0.90)
  expect_gt(r90$price_upper, r80$price_upper)
  expect_lt(r90$price_lower, r80$price_lower)
})

test_that("band scales with current ATR% and sqrt(N)", {
  set.seed(4)
  s <- rnorm(5000, 0, 1)
  r1 <- atr_expected_move_from_dist(make_dist(s, atr_pct = 2, N = 10), 0.80)
  r2 <- atr_expected_move_from_dist(make_dist(s, atr_pct = 4, N = 10), 0.80)
  # doubling ATR% ~ doubles the move magnitude
  expect_equal(r2$move_upper_pct / r1$move_upper_pct, 2, tolerance = 0.02)
})

test_that("validity flags fire outside the envelope", {
  set.seed(5)
  s <- rnorm(5000, 0, 1)
  long  <- atr_expected_move_from_dist(make_dist(s, atr_pct = 2, N = 40), 0.80)
  lowv  <- atr_expected_move_from_dist(make_dist(s, atr_pct = 0.5, N = 10), 0.80)
  highv <- atr_expected_move_from_dist(make_dist(s, atr_pct = 6, N = 10), 0.80)
  expect_true(any(grepl("horizon>25", long$flags)))
  expect_true(any(grepl("ATR<1", lowv$flags)))
  expect_true(any(grepl("ATR>5", highv$flags)))
})

test_that("returns NULL on NULL or too-thin distribution", {
  expect_null(atr_expected_move_from_dist(NULL))
  expect_null(atr_expected_move_from_dist(make_dist(rnorm(30))))  # < 60 obs
})
