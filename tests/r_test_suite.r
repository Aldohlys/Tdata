# test_parquet_chains.R
# Comprehensive test suite for Parquet chain discovery functionality
# Tests both legacy compatibility and new systematic caching features

library(reticulate)
library(testthat)
parquet_file <- tdata_py$view_parquet("C:/Users/aldoh/Documents/NewTrading/chains/SPX/SPXW_chain.parquet")


#' Setup test environment
setup_parquet_tests <- function() {
  cat("Setting up Parquet chain discovery tests...\n")

  devtools::load_all()

  # Test symbols - use liquid options symbols
  test_symbols <<- c("SPY", "QQQ", "SLV")

  cat("Test environment ready\n")
}

#' Test 1: Basic Chain Discovery
test_basic_chain_discovery <- function() {
  cat("\n=== Test 1: Basic Chain Discovery ===\n")

  for (symbol in test_symbols) {
    cat(sprintf("Testing getChains for %s...\n", symbol))

    # Test getChains
    chains <- tdata_py$getChains(symbol)

    # Validate results
    if (is.null(chains)) {
      cat(sprintf("  ❌ %s: getChains returned NULL (connection issue)\n", symbol))
      next
    }

    if (is.numeric(chains) && is.nan(chains)) {
      cat(sprintf("  ⚠️  %s: getChains returned NaN (symbol not found)\n", symbol))
      next
    }

    if (is.list(chains) && length(chains) > 0) {
      cat(sprintf("  ✅ %s: Found %d chains\n", symbol, length(chains)))

      # Examine first chain structure
      first_chain <- chains[[1]]
      if (length(first_chain) >= 6) {
        exchange <- first_chain[[1]]
        underlying_id <- first_chain[[2]]
        trading_class <- first_chain[[3]]
        multiplier <- first_chain[[4]]
        expirations <- first_chain[[5]]
        strikes <- first_chain[[6]]

        cat(sprintf("    Exchange: %s, Trading Class: %s\n", exchange, trading_class))
        cat(sprintf("    Expirations: %d, Strikes: %d\n", length(expirations), length(strikes)))
        cat(sprintf("    Sample expirations: %s\n", paste(head(expirations, 3), collapse=", ")))
        cat(sprintf("    Strike range: %.2f to %.2f\n", min(strikes), max(strikes)))
      }
    } else {
      cat(sprintf("  ❌ %s: Unexpected chains format: %s\n", symbol, class(chains)))
    }

    Sys.sleep(1)  # Rate limiting
  }
}

#' Test 2: Specific Chain Retrieval
test_specific_chain_retrieval <- function() {
  cat("\n=== Test 2: Specific Chain Retrieval ===\n")

  for (symbol in test_symbols) {
    cat(sprintf("Testing getChain for %s...\n", symbol))

    # Test getChain with default trading class (usually same as symbol)
    chain <- tdata_py$getChain(symbol, tradingClass = symbol)

    if (is.null(chain)) {
      cat(sprintf("  ❌ %s: getChain returned NULL\n", symbol))
      next
    }

    if (is.numeric(chain) && is.nan(chain)) {
      cat(sprintf("  ⚠️  %s: getChain returned NaN (trading class not found)\n", symbol))
      next
    }

    if (is.list(chain) && length(chain) >= 6) {
      trading_class <- chain[[3]]
      expirations <- chain[[5]]
      strikes <- chain[[6]]

      cat(sprintf("  ✅ %s: Found chain for trading class %s\n", symbol, trading_class))
      cat(sprintf("    %d expirations, %d strikes available\n", length(expirations), length(strikes)))

      # Store for strike testing
      if (length(expirations) > 0) {
        assign(paste0(symbol, "_expiration"), expirations[[1]], envir = .GlobalEnv)
        assign(paste0(symbol, "_trading_class"), trading_class, envir = .GlobalEnv)
      }
    } else {
      cat(sprintf("  ❌ %s: Unexpected chain format\n", symbol))
    }

    Sys.sleep(1)
  }
}

#' Test 3: Strike Qualification
test_strike_qualification <- function() {
  cat("\n=== Test 3: Strike Qualification ===\n")

  for (symbol in test_symbols) {
    # Check if we have expiration data from previous test
    exp_var <- paste0(symbol, "_expiration")
    tc_var <- paste0(symbol, "_trading_class")

    if (!exists(exp_var) || !exists(tc_var)) {
      cat(sprintf("  ⚠️  %s: Skipping strike test (no expiration data)\n", symbol))
      next
    }

    expiration <- get(exp_var)
    trading_class <- get(tc_var)

    cat(sprintf("Testing getOptionStrikes for %s %s %s...\n", symbol, trading_class, expiration))

    # Test strike qualification with a reasonable range
    strike_ranges <- list(
      "SPY" = c(600, 700),
      "QQQ" = c(500, 600),
      "SLV" = c(31, 36)
    )

    if (symbol %in% names(strike_ranges)) {
      range_limits <- strike_ranges[[symbol]]
      strike_min <- range_limits[1]
      strike_max <- range_limits[2]

      cat(sprintf("  Qualifying strikes in range [%.0f, %.0f]...\n", strike_min, strike_max))

      strikes <- tdata_py$getOptionStrikes(
        symbol,
        trading_class,
        expiration,
        strike_min = strike_min,
        strike_max = strike_max
      )

      if (is.null(strikes)) {
        cat(sprintf("  ❌ %s: getOptionStrikes returned NULL\n", symbol))
      } else if (length(strikes) == 0) {
        cat(sprintf("  ⚠️  %s: No qualified strikes in range\n", symbol))
      } else {
        cat(sprintf("  ✅ %s: Qualified %d strikes\n", symbol, length(strikes)))
        cat(sprintf("    Strike range: %.2f to %.2f\n", min(strikes), max(strikes)))
        cat(sprintf("    Sample strikes: %s\n", paste(head(strikes, 5), collapse=", ")))
      }
    } else {
      cat(sprintf("  ⚠️  %s: No predefined strike range, skipping\n", symbol))
    }

    Sys.sleep(2)  # Longer sleep for strike qualification
  }
}

#' Test 4: Caching Behavior
test_caching_behavior <- function() {
  cat("\n=== Test 4: Caching Behavior ===\n")

  symbol <- "QQQ"  # Use SPY for caching test

  cat("Testing systematic caching behavior...\n")

  # First call - should fetch from IBKR (or use existing cache)
  cat("  First getChains call...\n")
  start_time <- Sys.time()
  chains1 <- tdata_py$getChains(symbol)
  time1 <- as.numeric(Sys.time() - start_time)

  if (!is.null(chains1) && is.list(chains1)) {
    cat(sprintf("  ✅ First call completed in %.2f seconds\n", time1))

    # Second call - should use cache (much faster)
    cat("  Second getChains call (testing cache)...\n")
    start_time <- Sys.time()
    chains2 <- tdata_py$getChains(symbol)
    time2 <- as.numeric(Sys.time() - start_time)

    cat(sprintf("  ✅ Second call completed in %.2f seconds\n", time2))

    # Compare results
    if (identical(chains1, chains2)) {
      cat("  ✅ Both calls returned identical results\n")
    } else {
      cat("  ⚠️  Results differ between calls\n")
    }

    # Speed comparison
    if (time2 < time1 * 0.5) {  # Second call should be at least 50% faster
      cat("  ✅ Caching appears to be working (second call much faster)\n")
    } else {
      cat("  ⚠️  Second call not significantly faster (caching may not be working)\n")
    }
  } else {
    cat("  ❌ First call failed, cannot test caching\n")
  }
}

#' Test 5: Utility Functions (if available)
test_utility_functions <- function() {
  cat("\n=== Test 5: Utility Functions ===\n")


  # Test file listing
  cat("Testing list_parquet_files...\n")
  tryCatch({
    tdata_py$list_parquet_files()
    cat("  ✅ list_parquet_files executed successfully\n")
  }, error = function(e) {
    cat(sprintf("  ❌ list_parquet_files failed: %s\n", e$message))
  })

  # Test cleanup (dry run)
  cat("Testing cleanup_all (dry run)...\n")
  tryCatch({
    result <- tdata_py$cleanup_all(dry_run = TRUE)
    if (is.list(result)) {
      cat(sprintf("  ✅ Cleanup dry run: %d files would be processed\n",
                  result$total_files %||% 0))
    } else {
      cat("  ✅ Cleanup dry run completed\n")
    }
  }, error = function(e) {
    cat(sprintf("  ❌ cleanup_all failed: %s\n", e$message))
  })
}

#' Test 6: Batch Initialization (if available)
test_batch_initialization <- function() {
  cat("\n=== Test 6: Batch Initialization ===\n")

  # Check if batch initialization function is available
  tryCatch({
    # Try to access the batch initialization function
    if ("initialize_chains_for_symbols" %in% names(tdata_py)) {
      cat("Testing batch initialization...\n")

      test_symbols_subset <- c("SPY", "SLV")

      cat(sprintf("Initializing chains for: %s\n", paste(test_symbols_subset, collapse=", ")))

      results <- tdata_py$initialize_chains_for_symbols(test_symbols_subset)

      if (is.list(results)) {
        cat("  ✅ Batch initialization completed\n")
        for (symbol in names(results)) {
          status <- results[[symbol]]
          cat(sprintf("    %s: %s\n", symbol, status))
        }
      } else {
        cat("  ⚠️  Batch initialization returned unexpected format\n")
      }
    } else {
      cat("  ⚠️  Batch initialization not available (legacy implementation)\n")
    }
  }, error = function(e) {
    cat(sprintf("  ❌ Batch initialization failed: %s\n", e$message))
  })
}

#' Test 7: Data Structure Validation
test_data_structure <- function() {
  cat("\n=== Test 7: Data Structure Validation ===\n")

  symbol <- "SPY"
  chains <- tdata_py$getChains(symbol)

  if (is.null(chains) || !is.list(chains) || length(chains) == 0) {
    cat("  ❌ Cannot validate structure - no valid chains data\n")
    return()
  }

  cat("Validating chain data structure...\n")

  first_chain <- chains[[1]]

  # Expected structure: [exchange, underlying_id, trading_class, multiplier, expirations, strikes]
  expected_length <- 6

  if (length(first_chain) == expected_length) {
    cat("  ✅ Chain has correct number of elements (6)\n")

    # Validate each element type
    exchange <- first_chain[[1]]
    underlying_id <- first_chain[[2]]
    trading_class <- first_chain[[3]]
    multiplier <- first_chain[[4]]
    expirations <- first_chain[[5]]
    strikes <- first_chain[[6]]

    # Type checks
    checks <- list(
      "Exchange is character" = is.character(exchange),
      "Underlying ID is numeric" = is.numeric(underlying_id),
      "Trading class is character" = is.character(trading_class),
      "Multiplier is character" = is.character(multiplier),
      "Expirations is vector" = is.vector(expirations),
      "Strikes is numeric vector" = is.numeric(strikes)
    )

    for (check_name in names(checks)) {
      if (checks[[check_name]]) {
        cat(sprintf("  ✅ %s\n", check_name))
      } else {
        cat(sprintf("  ❌ %s\n", check_name))
      }
    }

    # Data quality checks
    if (length(expirations) > 0) {
      cat(sprintf("  ✅ Has %d expirations\n", length(expirations)))

      # Check expiration date format (should be YYYYMMDD)
      first_exp <- expirations[[1]]
      if (is.character(first_exp) && nchar(first_exp) == 8 && grepl("^[0-9]{8}$", first_exp)) {
        cat("  ✅ Expiration format looks correct (YYYYMMDD)\n")
      } else {
        cat(sprintf("  ⚠️  Expiration format may be incorrect: %s\n", first_exp))
      }
    }

    if (length(strikes) > 0) {
      cat(sprintf("  ✅ Has %d strikes\n", length(strikes)))
      cat(sprintf("  ✅ Strike range: %.2f to %.2f\n", min(strikes), max(strikes)))
    }

  } else {
    cat(sprintf("  ❌ Chain has incorrect structure (length %d, expected %d)\n",
                length(first_chain), expected_length))
  }
}

#' Main test runner
run_all_parquet_tests <- function() {
  cat("Starting comprehensive Parquet chain discovery tests...\n")
  cat("================================================\n")

  # Setup
  setup_parquet_tests()

  # Run all tests
  test_basic_chain_discovery()
  test_specific_chain_retrieval()
  test_strike_qualification()
  test_caching_behavior()
  test_utility_functions()
  test_batch_initialization()
  test_data_structure()

  cat("\n================================================\n")
  cat("All tests completed. Review results above.\n")
  cat("\nIf tests show ❌ errors, check:\n")
  cat("1. IBKR TWS/Gateway is running and connected\n")
  cat("2. Python tdata_py module is properly installed\n")
  cat("3. Network connectivity to IBKR servers\n")
  cat("4. API permissions are configured correctly\n")
}

#' Quick test for basic functionality
quick_test <- function(symbol = "SPY") {
  cat(sprintf("Quick test for %s...\n", symbol))

  chains <- tdata_py$getChains(symbol)

  if (is.null(chains)) {
    cat("❌ Failed: getChains returned NULL\n")
    return(FALSE)
  }

  if (is.numeric(chains) && is.nan(chains)) {
    cat("❌ Failed: Symbol not found\n")
    return(FALSE)
  }

  if (is.list(chains) && length(chains) > 0) {
    cat(sprintf("✅ Success: Found %d chains for %s\n", length(chains), symbol))
    return(TRUE)
  }

  cat("❌ Failed: Unexpected result format\n")
  return(FALSE)
}

# Safe null operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Usage examples:
# source("test_parquet_chains.R")
run_all_parquet_tests()  # Full test suite
# quick_test("SPY")        # Quick validation
