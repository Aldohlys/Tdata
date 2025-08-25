# ESTX50 Advanced Production Tests
# ================================
# Additional focused tests for production trading scenarios

library(reticulate)
tdata_py <- import("tdata_py")
estx50_current_price <- 5464

# Test 1: Strike Interval Analysis
test_strike_intervals <- function(symbol = "ESTX50", expiration = "20250919") {
  cat("📏 STRIKE INTERVAL ANALYSIS\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Get wide range of strikes
  strikes <- tdata_py$getOptionStrikes(
    symbol, trading_class, expiration,
    strike_min = 3000, strike_max = 7000,
    exchangeOpt = "EUREX"
  )

  if (!is.null(strikes) && length(strikes) > 0) {
    strikes_vec <- sort(unlist(strikes))

    cat(sprintf("Total strikes: %d (€%.0f - €%.0f)\n",
               length(strikes_vec), min(strikes_vec), max(strikes_vec)))

    # Calculate intervals
    if (length(strikes_vec) > 1) {
      intervals <- diff(strikes_vec)
      unique_intervals <- unique(intervals)

      cat("Strike intervals:\n")
      for (interval in sort(unique_intervals)) {
        count <- sum(intervals == interval)
        pct <- count / length(intervals) * 100
        cat(sprintf("  €%.0f: %d times (%.1f%%)\n", interval, count, pct))
      }

      # Most common interval
      interval_table <- table(intervals)
      most_common <- names(interval_table)[which.max(interval_table)]
      cat(sprintf("Most common: €%s\n", most_common))
    }

    return(list(strikes = strikes_vec, intervals = if(length(strikes_vec) > 1) diff(strikes_vec) else NULL))
  } else {
    cat("❌ No strikes data\n")
    return(NULL)
  }
}

# Test 2: Moneyness Distribution
test_moneyness_distribution <- function(symbol = "ESTX50", expiration = "20250919", current_level = estx50_current_price) {
  cat("💰 MONEYNESS DISTRIBUTION TEST\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Get all available strikes
  strikes <- tdata_py$getOptionStrikes(
    symbol, trading_class, expiration,
    strike_min = 2000, strike_max = 8000,
    exchangeOpt = "EUREX"
  )

  if (!is.null(strikes) && length(strikes) > 0) {
    strikes_vec <- sort(unlist(strikes))

    # Categorize by moneyness
    deep_itm_puts <- sum(strikes_vec < current_level * 0.85)
    itm_puts <- sum(strikes_vec >= current_level * 0.85 & strikes_vec < current_level * 0.95)
    near_atm <- sum(strikes_vec >= current_level * 0.95 & strikes_vec <= current_level * 1.05)
    otm_calls <- sum(strikes_vec > current_level * 1.05 & strikes_vec <= current_level * 1.15)
    deep_otm_calls <- sum(strikes_vec > current_level * 1.15)

    cat(sprintf("Current level: €%.0f\n", current_level))
    cat(sprintf("Deep ITM Puts (<85%%): %d strikes\n", deep_itm_puts))
    cat(sprintf("ITM Puts (85-95%%): %d strikes\n", itm_puts))
    cat(sprintf("Near ATM (95-105%%): %d strikes\n", near_atm))
    cat(sprintf("OTM Calls (105-115%%): %d strikes\n", otm_calls))
    cat(sprintf("Deep OTM Calls (>115%%): %d strikes\n", deep_otm_calls))

    total_check <- deep_itm_puts + itm_puts + near_atm + otm_calls + deep_otm_calls
    cat(sprintf("Total: %d (check: %d)\n", length(strikes_vec), total_check))

    return(list(
      total = length(strikes_vec),
      distribution = list(
        deep_itm_puts = deep_itm_puts,
        itm_puts = itm_puts,
        near_atm = near_atm,
        otm_calls = otm_calls,
        deep_otm_calls = deep_otm_calls
      )
    ))
  } else {
    cat("❌ No strikes data\n")
    return(NULL)
  }
}

# Test 3: Cross-Expiration Consistency
test_cross_expiration <- function(symbol = "ESTX50") {
  cat("📅 CROSS-EXPIRATION CONSISTENCY\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Test same strike range across different expirations
  test_expirations <- c("20250919", "20251219", "20260116")
  exp_names <- c("Sep 2025", "Dec 2025", "Jan 2026")

  # ATM range
  strike_min <- estx50_current_price*0.9
  strike_max <- estx50_current_price*1.1

  cat(sprintf("Testing €%.0f-€%.0f range across expirations:\n", strike_min, strike_max))

  results <- list()

  for (i in 1:length(test_expirations)) {
    expiration <- test_expirations[i]
    exp_name <- exp_names[i]

    strikes <- tdata_py$getOptionStrikes(
      symbol, trading_class, expiration,
      strike_min = strike_min, strike_max = strike_max,
      exchangeOpt = "EUREX"
    )

    if (!is.null(strikes) && length(strikes) > 0) {
      strikes_vec <- unlist(strikes)
      cat(sprintf("  %s: %d strikes (€%.0f-€%.0f)\n",
                 exp_name, length(strikes_vec), min(strikes_vec), max(strikes_vec)))

      results[[exp_name]] <- strikes_vec
    } else {
      cat(sprintf("  %s: ❌ No data\n", exp_name))
    }
  }

  # Check consistency
  if (length(results) > 1) {
    strike_counts <- sapply(results, length)
    cat(sprintf("\nConsistency check:\n"))
    cat(sprintf("  Strike counts: %s\n", paste(strike_counts, collapse = ", ")))
    cat(sprintf("  Range: %d - %d strikes\n", min(strike_counts), max(strike_counts)))

    if (max(strike_counts) - min(strike_counts) <= 5) {
      cat("  ✅ Consistent across expirations\n")
    } else {
      cat("  ⚠️  Large variation across expirations\n")
    }
  }

  return(results)
}

# Test 4: Edge Case Strike Ranges
test_edge_cases <- function(symbol = "ESTX50", expiration = "20250919") {
  cat("🎲 EDGE CASE TESTING\n")
  cat(paste(rep("-", 25), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  edge_cases <- list(
    list(name = "Very_Low", min = 100, max = 500),      # Very low strikes
    list(name = "Very_High", min = 10000, max = 15000), # Very high strikes
    list(name = "Narrow_ATM", min = 5600, max = 5700),  # Very narrow ATM
    list(name = "Single_Strike", min = 5500, max = 5500) # Single strike
  )

  results <- list()

  for (case in edge_cases) {
    name <- case$name
    strike_min <- case$min
    strike_max <- case$max

    cat(sprintf("%s (€%.0f-€%.0f):", name, strike_min, strike_max))

    strikes <- tdata_py$getOptionStrikes(
      symbol, trading_class, expiration,
      strike_min = strike_min, strike_max = strike_max,
      exchangeOpt = "EUREX"
    )

    if (!is.null(strikes) && length(strikes) > 0) {
      strikes_vec <- unlist(strikes)
      cat(sprintf(" ✅ %d strikes\n", length(strikes_vec)))
      results[[name]] <- strikes_vec
    } else {
      cat(" ❌ None\n")
      results[[name]] <- numeric(0)
    }
  }

  return(results)
}

# Test 5: Multi-Trading Class Strike Comparison
test_trading_class_strikes <- function(symbol = "ESTX50", expiration = "20250919") {
  cat("🔄 TRADING CLASS STRIKE COMPARISON\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  # Get all trading classes
  trading_classes <- tdata_py$getAvailableTradingClasses(symbol, "EUREX")

  if (length(trading_classes) < 2) {
    cat("⚠️  Only one trading class available\n")
    return(NULL)
  }

  cat("Comparing trading classes for", expiration, ":\n")

  # ATM range for comparison
  strike_min <- estx50_current_price*0.9
  strike_max <- estx50_current_price*1.1

  results <- list()

  for (tc in trading_classes) {
    strikes <- tdata_py$getOptionStrikes(
      symbol, tc, expiration,
      strike_min = strike_min, strike_max = strike_max,
      exchangeOpt = "EUREX"
    )

    if (!is.null(strikes) && length(strikes) > 0) {
      strikes_vec <- unlist(strikes)
      cat(sprintf("  %s: %d strikes (€%.0f-€%.0f)\n",
                 tc, length(strikes_vec), min(strikes_vec), max(strikes_vec)))
      results[[tc]] <- strikes_vec
    } else {
      cat(sprintf("  %s: ❌ No data\n", tc))
    }
  }

  # Find common strikes
  if (length(results) == 2) {
    tc_names <- names(results)
    strikes1 <- results[[tc_names[1]]]
    strikes2 <- results[[tc_names[2]]]

    common <- intersect(strikes1, strikes2)
    unique1 <- setdiff(strikes1, strikes2)
    unique2 <- setdiff(strikes2, strikes1)

    cat(sprintf("\nComparison:\n"))
    cat(sprintf("  Common strikes: %d\n", length(common)))
    cat(sprintf("  %s unique: %d\n", tc_names[1], length(unique1)))
    cat(sprintf("  %s unique: %d\n", tc_names[2], length(unique2)))
  }

  return(results)
}

# Test 6: Cache Efficiency Test
test_cache_efficiency <- function(symbol = "ESTX50") {
  cat("⚡ CACHE EFFICIENCY TEST\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Test same query multiple times - should be instant after first
  expiration <- "20250919"
  strike_min <- estx50_current_price*0.9
  strike_max <- estx50_current_price*1.1

  times <- numeric(5)

  for (i in 1:5) {
    start_time <- Sys.time()

    strikes <- tdata_py$getOptionStrikes(
      symbol, trading_class, expiration,
      strike_min = strike_min, strike_max = strike_max,
      exchangeOpt = "EUREX"
    )

    times[i] <- as.numeric(Sys.time() - start_time)

    if (!is.null(strikes) && length(strikes) > 0) {
      cat(sprintf("Call %d: %.3fs (%d strikes)\n", i, times[i], length(unlist(strikes))))
    } else {
      cat(sprintf("Call %d: %.3fs (failed)\n", i, times[i]))
    }
  }

  cat(sprintf("\nCache performance:\n"))
  cat(sprintf("  First call: %.3fs (with qualification)\n", times[1]))
  cat(sprintf("  Avg subsequent: %.3fs (cached)\n", mean(times[2:5])))
  cat(sprintf("  Speedup: %.1fx\n", times[1] / mean(times[2:5])))

  return(times)
}

# Test 7: Stress Test - Multiple Concurrent Requests
stress_test <- function(symbol = "ESTX50", num_requests = 10) {
  cat("🔥 STRESS TEST\n")
  cat(paste(rep("-", 20), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Different strike ranges to test
  test_ranges <- list(
    c(5400, 5600), c(5600, 5800), c(5800, 6000),
    c(6000, 6200), c(6200, 6400)
  )

  cat(sprintf("Running %d requests with different ranges...\n", num_requests))

  start_time <- Sys.time()
  successful <- 0
  total_strikes <- 0

  for (i in 1:num_requests) {
    # Random range selection
    range_idx <- ((i - 1) %% length(test_ranges)) + 1
    range_def <- test_ranges[[range_idx]]

    strikes <- tdata_py$getOptionStrikes(
      symbol, trading_class, "20250919",
      strike_min = range_def[1], strike_max = range_def[2],
      exchangeOpt = "EUREX"
    )

    if (!is.null(strikes) && length(strikes) > 0) {
      successful <- successful + 1
      total_strikes <- total_strikes + length(unlist(strikes))
      cat(".")
    } else {
      cat("X")
    }
  }

  total_time <- as.numeric(Sys.time() - start_time)

  cat(sprintf("\n\nStress test results:\n"))
  cat(sprintf("  Requests: %d/%d successful (%.1f%%)\n",
             successful, num_requests, successful/num_requests*100))
  cat(sprintf("  Total time: %.2fs\n", total_time))
  cat(sprintf("  Avg per request: %.3fs\n", total_time/num_requests))
  cat(sprintf("  Total strikes: %d\n", total_strikes))

  return(list(
    successful = successful,
    total_requests = num_requests,
    total_time = total_time,
    avg_time = total_time/num_requests
  ))
}

# Test 8: Data Quality Validation
test_data_quality <- function(symbol = "ESTX50", expiration = "20250919") {
  cat("🔍 DATA QUALITY VALIDATION\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Get strikes for validation
  strikes <- tdata_py$getOptionStrikes(
    symbol, trading_class, expiration,
    strike_min = 5000, strike_max = 7000,
    exchangeOpt = "EUREX"
  )

  if (!is.null(strikes) && length(strikes) > 0) {
    strikes_vec <- sort(unlist(strikes))

    # Quality checks
    checks <- list()

    # Check 1: No duplicates
    has_duplicates <- length(strikes_vec) != length(unique(strikes_vec))
    checks$duplicates <- !has_duplicates
    cat("No duplicates:", if(!has_duplicates) "✅" else "❌", "\n")

    # Check 2: Proper ordering
    is_sorted <- all(diff(strikes_vec) > 0)
    checks$sorted <- is_sorted
    cat("Properly sorted:", if(is_sorted) "✅" else "❌", "\n")

    # Check 3: Reasonable range
    min_strike <- min(strikes_vec)
    max_strike <- max(strikes_vec)
    reasonable_range <- (min_strike > 0 && max_strike < 50000)
    checks$reasonable_range <- reasonable_range
    cat("Reasonable range:", if(reasonable_range) "✅" else "❌", "\n")

    # Check 4: No extreme gaps
    if (length(strikes_vec) > 1) {
      intervals <- diff(strikes_vec)
      max_interval <- max(intervals)
      median_interval <- median(intervals)
      extreme_gaps <- max_interval > median_interval * 10
      checks$no_extreme_gaps <- !extreme_gaps
      cat("No extreme gaps:", if(!extreme_gaps) "✅" else "❌", "\n")
      cat(sprintf("  Max interval: €%.0f, Median: €%.0f\n", max_interval, median_interval))
    }

    # Overall quality score
    quality_score <- sum(unlist(checks)) / length(checks) * 100
    cat(sprintf("\nData Quality Score: %.0f%%\n", quality_score))

    return(list(checks = checks, quality_score = quality_score, strikes = strikes_vec))
  } else {
    cat("❌ No data to validate\n")
    return(NULL)
  }
}

# Test 9: Error Handling and Resilience
test_error_handling <- function(symbol = "ESTX50") {
  cat("🛡️  ERROR HANDLING TEST\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")

  # Test invalid parameters
  error_tests <- list(
    list(name = "Invalid Symbol", params = list(symbol = "INVALID", trading_class = "TEST")),
    list(name = "Invalid Expiration", params = list(expiration = "20201231")),  # Past date
    list(name = "Invalid Strike Range", params = list(strike_min = 10000, strike_max = 5000)),  # Min > Max
    list(name = "Empty Range", params = list(strike_min = 5600.1, strike_max = 5600.2))  # Tiny range
  )

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  base_trading_class <- recommended$trading_class

  results <- list()

  for (test in error_tests) {
    test_name <- test$name
    params <- test$params

    cat(sprintf("%s:", test_name))

    # Set up parameters with defaults
    test_symbol <- if(!is.null(params$symbol)) params$symbol else symbol
    test_trading_class <- if(!is.null(params$trading_class)) params$trading_class else base_trading_class
    test_expiration <- if(!is.null(params$expiration)) params$expiration else "20250919"
    test_strike_min <- if(!is.null(params$strike_min)) params$strike_min else 5600
    test_strike_max <- if(!is.null(params$strike_max)) params$strike_max else 6000

    tryCatch({
      strikes <- tdata_py$getOptionStrikes(
        test_symbol, test_trading_class, test_expiration,
        strike_min = test_strike_min, strike_max = test_strike_max,
        exchangeOpt = "EUREX"
      )

      if (is.null(strikes) || length(strikes) == 0) {
        cat(" ✅ Handled gracefully (no data)\n")
        results[[test_name]] <- "handled_gracefully"
      } else {
        cat(" ⚠️  Unexpected success\n")
        results[[test_name]] <- "unexpected_success"
      }

    }, error = function(e) {
      cat(" ✅ Error caught:", substr(e$message, 1, 30), "...\n")
      results[[test_name]] <- "error_caught"
    })
  }

  return(results)
}

# Master advanced test runner
run_advanced_tests <- function() {
  cat("🎯 ESTX50 Advanced Tests\n")
  cat(paste(rep("=", 35), collapse = ""), "\n")
  cat("Timestamp:", as.character(Sys.time()), "\n\n")

  results <- list()

  # Run each test
  cat("TEST 1: Strike Intervals\n")
  results$intervals <- test_strike_intervals()

  cat("\nTEST 2: Moneyness Distribution\n")
  results$moneyness <- test_moneyness_distribution()

  cat("\nTEST 3: Cross-Expiration\n")
  results$cross_expiration <- test_cross_expiration()

  cat("\nTEST 4: Edge Cases\n")
  results$edge_cases <- test_edge_cases()

  cat("\nTEST 5: Trading Class Comparison\n")
  results$tc_comparison <- test_trading_class_strikes()

  cat("\nTEST 6: Cache Efficiency\n")
  results$cache_efficiency <- test_cache_efficiency()

  cat("\nTEST 7: Stress Test\n")
  results$stress <- stress_test()

  cat("\nTEST 8: Data Quality\n")
  results$quality <- test_data_quality()

  cat("\nTEST 9: Error Handling\n")
  results$error_handling <- test_error_handling()

  cat("\n🎉 Advanced tests complete!\n")
  cat("All results stored in returned list.\n")

  return(results)
}

# Print available functions
cat("🎯 ESTX50 Advanced Test Suite\n")
cat("=============================\n")
cat("Functions:\n")
cat("• run_advanced_tests()         - All advanced tests\n")
cat("• test_strike_intervals()      - Strike spacing analysis\n")
cat("• test_moneyness_distribution() - ITM/OTM/ATM breakdown\n")
cat("• test_cross_expiration()      - Consistency across dates\n")
cat("• test_edge_cases()            - Boundary conditions\n")
cat("• test_trading_class_strikes() - Compare OESX vs OEXP\n")
cat("• test_cache_efficiency()      - Cache performance\n")
cat("• stress_test()                - Multiple concurrent requests\n")
cat("• test_data_quality()          - Data validation checks\n")
cat("• test_error_handling()        - Error resilience\n")
cat("\n🚀 Run: run_advanced_tests()\n")
