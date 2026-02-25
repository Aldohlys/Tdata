# Tests for cache TTL staleness detection (parquet_storage + chains_manager)
# Covers: get_file_age_days, get_cache_ttl_days, check_and_remove_stale_chains,
#          check_and_remove_stale_strikes, get_stale_warnings, clear_stale_warnings

library(testthat)
library(reticulate)

skip_if_no_python <- function() {
  skip_if_not(reticulate::py_available(), "Python not available")
}

has_tdata_py <- function() {
  tryCatch({
    tdata_py <- reticulate::import("tdata_py")
    !is.null(tdata_py)
  }, error = function(e) FALSE)
}

# ---------------------------------------------------------------------------
# get_file_age_days
# ---------------------------------------------------------------------------
test_that("get_file_age_days returns -1 for non-existent file", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")
  age <- tdata_py$get_file_age_days("/nonexistent/path/fake.parquet")
  expect_equal(age, -1)
})

test_that("get_file_age_days returns non-negative for existing file", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("test", tmp)

  age <- as.numeric(tdata_py$get_file_age_days(tmp))
  expect_true(is.numeric(age))
  # Allow tiny negative due to Windows clock granularity
  expect_true(age >= -0.01)
  # A file just created should be less than 1 day old
  expect_true(age < 1)
})

test_that("get_file_age_days detects old files correctly", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")
  py <- reticulate::import_builtins()
  os_mod <- reticulate::import("os")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("test", tmp)

  # Backdate mtime by 10 days
  now <- as.numeric(Sys.time())
  old_time <- now - 10 * 86400
  os_mod$utime(tmp, reticulate::tuple(old_time, old_time))

  age <- tdata_py$get_file_age_days(tmp)
  expect_true(age >= 9.9 && age <= 10.5)
})

# ---------------------------------------------------------------------------
# get_cache_ttl_days
# ---------------------------------------------------------------------------
test_that("get_cache_ttl_days returns a positive integer", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")
  ttl <- tdata_py$get_cache_ttl_days()
  expect_true(is.numeric(ttl))
  expect_true(ttl > 0)
})

# ---------------------------------------------------------------------------
# check_and_remove_stale_chains
# ---------------------------------------------------------------------------
test_that("check_and_remove_stale_chains returns empty for non-existent symbol dir", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  storage <- ps$ParquetChainsStorage()

  result <- storage$check_and_remove_stale_chains("ZZZZZ_NONEXISTENT")
  expect_equal(length(result), 0L)
})

test_that("check_and_remove_stale_chains deletes old chain files", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  os_mod <- reticulate::import("os")
  pa <- reticulate::import("pyarrow")
  pq <- reticulate::import("pyarrow.parquet")
  pathlib <- reticulate::import("pathlib")

  # Create a temp directory to act as storage base_dir
  test_dir <- tempfile("chains_test_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  storage <- ps$ParquetChainsStorage()
  original_base <- storage$base_dir
  storage$base_dir <- pathlib$Path(test_dir)
  on.exit({storage$base_dir <- original_base}, add = TRUE)

  # Create a fake symbol dir with a chain parquet file
  sym_dir <- file.path(test_dir, "TESTSYM")
  dir.create(sym_dir)
  chain_file <- file.path(sym_dir, "TC1_chain.parquet")

  # Write a minimal parquet file
  df <- reticulate::import("pandas")$DataFrame(list(
    exchange = "SMART",
    underlying_contract_id = 12345L,
    trading_class = "TC1",
    multiplier = "100",
    expiration = "20260601",
    strike = 100.0
  ), index = list(0L))
  pq$write_table(pa$Table$from_pandas(df), chain_file)

  expect_true(file.exists(chain_file))

  # Backdate the file by 20 days (well beyond default TTL of 7)
  now <- as.numeric(Sys.time())
  old_time <- now - 20 * 86400
  os_mod$utime(chain_file, reticulate::tuple(old_time, old_time))

  # Should detect and delete the stale file
  deleted <- storage$check_and_remove_stale_chains("TESTSYM")
  expect_true("TC1" %in% deleted)
  expect_false(file.exists(chain_file))
})

test_that("check_and_remove_stale_chains keeps fresh files", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  pa <- reticulate::import("pyarrow")
  pq <- reticulate::import("pyarrow.parquet")
  pathlib <- reticulate::import("pathlib")

  test_dir <- tempfile("chains_fresh_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  storage <- ps$ParquetChainsStorage()
  original_base <- storage$base_dir
  storage$base_dir <- pathlib$Path(test_dir)
  on.exit({storage$base_dir <- original_base}, add = TRUE)

  sym_dir <- file.path(test_dir, "FRESHSYM")
  dir.create(sym_dir)
  chain_file <- file.path(sym_dir, "TC2_chain.parquet")

  df <- reticulate::import("pandas")$DataFrame(list(
    exchange = "SMART",
    underlying_contract_id = 99999L,
    trading_class = "TC2",
    multiplier = "100",
    expiration = "20260601",
    strike = 100.0
  ), index = list(0L))
  pq$write_table(pa$Table$from_pandas(df), chain_file)

  # File is fresh (just created) — should NOT be deleted
  deleted <- storage$check_and_remove_stale_chains("FRESHSYM")
  expect_equal(length(deleted), 0L)
  expect_true(file.exists(chain_file))
})

# ---------------------------------------------------------------------------
# check_and_remove_stale_strikes
# ---------------------------------------------------------------------------
test_that("check_and_remove_stale_strikes returns NULL for non-existent file", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  storage <- ps$ParquetStrikesStorage()

  result <- storage$check_and_remove_stale_strikes("NOSYM", "NOTC", "20260601")
  expect_null(result)
})

test_that("check_and_remove_stale_strikes deletes expired contract cache", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  pa <- reticulate::import("pyarrow")
  pq <- reticulate::import("pyarrow.parquet")
  pathlib <- reticulate::import("pathlib")

  test_dir <- tempfile("strikes_exp_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  storage <- ps$ParquetStrikesStorage()
  original_base <- storage$base_dir
  storage$base_dir <- pathlib$Path(test_dir)
  on.exit({storage$base_dir <- original_base}, add = TRUE)

  # Create a strike file with a past expiration date
  strike_dir <- file.path(test_dir, "EXPSYM", "TC1")
  dir.create(strike_dir, recursive = TRUE)
  strike_file <- file.path(strike_dir, "20200101_strikes.parquet")

  df <- reticulate::import("pandas")$DataFrame(list(
    strike = c(100.0, 105.0),
    state = c("qualified", "out_of_scope")
  ))
  pq$write_table(pa$Table$from_pandas(df), strike_file)

  expect_true(file.exists(strike_file))

  # Expired contract (20200101 < today) — should be deleted
  msg <- storage$check_and_remove_stale_strikes("EXPSYM", "TC1", "20200101")
  expect_true(!is.null(msg))
  expect_true(grepl("expired", msg, ignore.case = TRUE))
  expect_false(file.exists(strike_file))
})

test_that("check_and_remove_stale_strikes deletes stale (old) cache for future expiration", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  pa <- reticulate::import("pyarrow")
  pq <- reticulate::import("pyarrow.parquet")
  pathlib <- reticulate::import("pathlib")
  os_mod <- reticulate::import("os")

  test_dir <- tempfile("strikes_stale_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  storage <- ps$ParquetStrikesStorage()
  original_base <- storage$base_dir
  storage$base_dir <- pathlib$Path(test_dir)
  on.exit({storage$base_dir <- original_base}, add = TRUE)

  # Future expiration but old file
  strike_dir <- file.path(test_dir, "STALESYM", "TC1")
  dir.create(strike_dir, recursive = TRUE)
  strike_file <- file.path(strike_dir, "20270601_strikes.parquet")

  df <- reticulate::import("pandas")$DataFrame(list(
    strike = c(100.0, 105.0),
    state = c("qualified", "out_of_scope")
  ))
  pq$write_table(pa$Table$from_pandas(df), strike_file)

  # Backdate by 15 days
  now <- as.numeric(Sys.time())
  old_time <- now - 15 * 86400
  os_mod$utime(strike_file, reticulate::tuple(old_time, old_time))

  msg <- storage$check_and_remove_stale_strikes("STALESYM", "TC1", "20270601")
  expect_true(!is.null(msg))
  expect_true(grepl("Stale", msg))
  expect_false(file.exists(strike_file))
})

test_that("check_and_remove_stale_strikes keeps fresh cache for future expiration", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  ps <- reticulate::import("tdata_py.parquet_storage")
  pa <- reticulate::import("pyarrow")
  pq <- reticulate::import("pyarrow.parquet")
  pathlib <- reticulate::import("pathlib")

  test_dir <- tempfile("strikes_fresh_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  storage <- ps$ParquetStrikesStorage()
  original_base <- storage$base_dir
  storage$base_dir <- pathlib$Path(test_dir)
  on.exit({storage$base_dir <- original_base}, add = TRUE)

  strike_dir <- file.path(test_dir, "FRESHSYM", "TC1")
  dir.create(strike_dir, recursive = TRUE)
  strike_file <- file.path(strike_dir, "20270601_strikes.parquet")

  df <- reticulate::import("pandas")$DataFrame(list(
    strike = c(100.0, 105.0),
    state = c("qualified", "out_of_scope")
  ))
  pq$write_table(pa$Table$from_pandas(df), strike_file)

  # File is fresh — should be kept
  msg <- storage$check_and_remove_stale_strikes("FRESHSYM", "TC1", "20270601")
  expect_null(msg)
  expect_true(file.exists(strike_file))
})

# ---------------------------------------------------------------------------
# Warning accumulator: get_stale_warnings / clear_stale_warnings
# ---------------------------------------------------------------------------
test_that("get_stale_warnings and clear_stale_warnings work correctly", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")

  # Clear any existing warnings
  tdata_py$clear_stale_warnings()
  warnings <- tdata_py$get_stale_warnings()
  expect_equal(length(warnings), 0L)

  # Manually push warnings via py_run_string (can't access _private attrs via $)
  reticulate::py_run_string("from tdata_py.chains_manager import _stale_warnings")
  reticulate::py_run_string("_stale_warnings.append('test warning 1')")
  reticulate::py_run_string("_stale_warnings.append('test warning 2')")

  warnings <- tdata_py$get_stale_warnings()
  expect_equal(length(warnings), 2L)
  expect_equal(warnings[[1]], "test warning 1")
  expect_equal(warnings[[2]], "test warning 2")

  # Clear and verify
  tdata_py$clear_stale_warnings()
  warnings <- tdata_py$get_stale_warnings()
  expect_equal(length(warnings), 0L)
})

test_that("exported functions exist in tdata_py namespace", {
  skip_if_no_python()
  skip_if_not(has_tdata_py(), "tdata_py module not available")

  tdata_py <- reticulate::import("tdata_py")

  expect_true(!is.null(tdata_py$get_file_age_days))
  expect_true(!is.null(tdata_py$get_cache_ttl_days))
  expect_true(!is.null(tdata_py$get_stale_warnings))
  expect_true(!is.null(tdata_py$clear_stale_warnings))
})
