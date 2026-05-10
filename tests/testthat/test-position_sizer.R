# Tests for position_sizer.R — sizePosition wrapper
#
# Strategy: tdata_py is an active binding (zzz.R) backed by .tdata_state.
# Swap the underlying state to a fake module exposing a size_position method
# whose return value can be controlled per test.

local_mock_tdata_py <- function(fake_module, envir = parent.frame()) {
  state <- get(".tdata_state", envir = asNamespace("Tdata"))
  old_value <- state$value
  old_initialized <- state$initialized
  state$value <- fake_module
  state$initialized <- TRUE
  withr::defer({
    state$value <- old_value
    state$initialized <- old_initialized
  }, envir = envir)
}

# Minimal "vertical credit spread" leg set — typical caller shape
sample_legs <- function() {
  data.frame(
    strike = c(95,  90),
    right  = c("P", "P"),
    pos    = c(-1,  1),
    dte    = c(35,  35),
    mul    = c(100, 100),
    sig    = c(0.30, 0.30),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Successful path
# ---------------------------------------------------------------------------
test_that("sizePosition returns Python result on success", {
  fake_result <- list(
    lots = 3L,
    regime = "low_iv",
    scenarios = list(),
    recommendation = "size 3 lots",
    inputs = list()
  )
  fake_py <- list(size_position = function(...) fake_result)
  local_mock_tdata_py(fake_py)

  out <- sizePosition(sample_legs(), spot = 100, credit = 1.20,
                      iv = 0.30, hv = 0.25, ivp = 40, hvp = 50)
  expect_equal(out$lots, 3L)
  expect_equal(out$regime, "low_iv")
})

# ---------------------------------------------------------------------------
# legs -> list-of-dicts conversion is well-formed
# ---------------------------------------------------------------------------
test_that("sizePosition converts legs data.frame to list of per-leg dicts", {
  captured <- NULL
  fake_py <- list(size_position = function(legs, ...) {
    captured <<- legs
    list(lots = 1L)
  })
  local_mock_tdata_py(fake_py)

  sizePosition(sample_legs(), spot = 100, credit = 1.20,
               iv = 0.30, hv = 0.25, ivp = 40, hvp = 50)

  expect_type(captured, "list")
  expect_length(captured, 2)
  expect_setequal(names(captured[[1]]),
                  c("strike", "right", "pos", "dte", "mul", "sig"))
  expect_equal(captured[[1]]$strike, 95)
  expect_equal(captured[[1]]$right, "P")
  expect_equal(captured[[1]]$pos, -1L)
  expect_type(captured[[1]]$pos, "integer")
  expect_equal(captured[[1]]$dte, 35L)
  expect_type(captured[[1]]$dte, "integer")
})

test_that("sizePosition forwards scalar arguments to Python", {
  captured_args <- list()
  fake_py <- list(size_position = function(...) {
    captured_args <<- list(...)
    list(lots = 1L)
  })
  local_mock_tdata_py(fake_py)

  sizePosition(sample_legs(), spot = 100, credit = 1.20,
               iv = 0.30, hv = 0.25, ivp = 40, hvp = 50,
               max_loss = 1200, n_sims = 5000L)

  expect_equal(captured_args$spot, 100)
  expect_equal(captured_args$credit, 1.20)
  expect_equal(captured_args$max_loss, 1200)
  expect_equal(captured_args$n_sims, 5000L)
  expect_type(captured_args$n_sims, "integer")
  expect_equal(captured_args$dte_cut, 21L)  # default
})

# ---------------------------------------------------------------------------
# Error paths
# ---------------------------------------------------------------------------
test_that("sizePosition returns NULL when Python returns result with $error", {
  fake_py <- list(size_position = function(...) {
    list(error = "credit must be > 0")
  })
  local_mock_tdata_py(fake_py)

  out <- sizePosition(sample_legs(), spot = 100, credit = -1,
                      iv = 0.30, hv = 0.25, ivp = 40, hvp = 50)
  expect_null(out)
})

test_that("sizePosition returns NULL when Python returns NULL outright", {
  fake_py <- list(size_position = function(...) NULL)
  local_mock_tdata_py(fake_py)

  out <- sizePosition(sample_legs(), spot = 100, credit = 1.20,
                      iv = 0.30, hv = 0.25, ivp = 40, hvp = 50)
  expect_null(out)
})

test_that("sizePosition tolerates result with empty list error (R partial-matching guard)", {
  # Per CLAUDE.md: result[["error"]] is exact-match; if Python returns
  # `errors = list()` (plural, empty), we must NOT treat it as an error.
  fake_py <- list(size_position = function(...) {
    list(lots = 2L, errors = list())   # plural, intentionally empty
  })
  local_mock_tdata_py(fake_py)

  out <- sizePosition(sample_legs(), spot = 100, credit = 1.20,
                      iv = 0.30, hv = 0.25, ivp = 40, hvp = 50)
  expect_false(is.null(out))
  expect_equal(out$lots, 2L)
})
