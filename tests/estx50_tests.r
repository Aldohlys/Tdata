# ESTX50 EUREX Compact Test Suite
# ===============================
# Focused testing for ESTX50 chains and strikes on EUREX
# Target expirations: Sep 2025, Dec 2025, Jan 2026

library(reticulate)
tdata_py <- import("tdata_py")

# Main comprehensive test function
test_estx50_comprehensive <- function() {
  cat("🔍 ESTX50 EUREX Test Suite\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")

  symbol <- "ESTX50"
  exchange_opt <- "EUREX"

  # Target expirations (3rd Friday of each month)
  target_exps <- list(
    "Sep_2025" = "20250919",
    "Dec_2025" = "20251219",
    "Jan_2026" = "20260116"
  )

  cat("Symbol:", symbol, "| Exchange:", exchange_opt, "\n")
  cat("Targets:", paste(names(target_exps), collapse = ", "), "\n\n")

  # Step 1: Explore trading classes
  cat("📊 STEP 1: Trading Classes\n")
  exploration <- tdata_py$exploreSymbol(symbol)

  if (exploration$status != "success") {
    cat("❌ Failed to explore", symbol, "\n")
    return(NULL)
  }

  cat("✅ Found", exploration$summary$unique_trading_classes, "trading classes\n")

  # Get recommended trading class
  recommended <- tdata_py$findBestTradingClass(symbol, exchange_opt, "most_strikes")
  trading_class <- recommended$trading_class

  cat("💡 Recommended:", trading_class, "\n")
  cat(sprintf("   %d expirations, %d strikes (€%.0f-€%.0f)\n\n",
              recommended$expiration_count, recommended$strike_count,
              recommended$strike_range$min, recommended$strike_range$max))

  # Step 2: Test each expiration
  cat("📅 STEP 2: Expiration Testing\n")

  results <- list()

  for (exp_name in names(target_exps)) {
    exp_date <- target_exps[[exp_name]]
    cat("🔍", exp_name, "(", exp_date, "):\n")

    # Get chain to check expiration availability
    chain <- tdata_py$getChain(symbol, tradingClass = trading_class, exchangeOpt = exchange_opt)

    if (is.null(chain) || (is.numeric(chain) && is.na(chain))) {
      cat("  ❌ Chain not available\n")
      next
    }

    available_exps <- unlist(chain[[5]])  # Expirations
    actual_exp <- if (exp_date %in% available_exps) exp_date else find_closest_exp(exp_date, available_exps)

    if (exp_date == actual_exp) {
      cat("  ✅ Target available\n")
    } else {
      cat("  ⚠️  Using closest:", actual_exp, "\n")
    }

    # Test strike ranges
    strike_tests <- test_strike_ranges(symbol, trading_class, actual_exp, exchange_opt)

    results[[exp_name]] <- list(
      expiration = actual_exp,
      target_matched = (exp_date == actual_exp),
      strike_results = strike_tests
    )
  }

  # Step 3: Summary
  cat("\n📋 SUMMARY\n")
  generate_summary(symbol, trading_class, results)

  return(results)
}

# Test strike ranges for an expiration
test_strike_ranges <- function(symbol, trading_class, expiration, exchange_opt) {
  # Strike test scenarios around €4800 level
  scenarios <- list(
    list(name = "ATM", center = 5460, range_pct = 0.10),      # ±10% ATM
    list(name = "OTM_Calls", center = 6000, range_pct = 0.08), # OTM calls
    list(name = "OTM_Puts", center = 5000, range_pct = 0.08)   # OTM puts
  )

  results <- list()

  for (scenario in scenarios) {
    name <- scenario$name
    center <- scenario$center
    range_pct <- scenario$range_pct

    # Calculate bounds
    range_val <- center * range_pct
    strike_min <- center - range_val
    strike_max <- center + range_val

    cat(sprintf("    🎯 %s (€%.0f ±%.0f%%):", name, center, range_pct*100))

    # Get strikes
    strikes <- tdata_py$getOptionStrikes(
      symbol, trading_class, expiration,
      strike_min = strike_min, strike_max = strike_max,
      exchangeOpt = exchange_opt
    )

    if (is.null(strikes)) {
      cat(" ❌ Error\n")
      results[[name]] <- list(status = "error")
    } else if (length(strikes) == 0 || (is.numeric(strikes) && length(strikes) == 1 && is.na(strikes))) {
      cat(" ❌ N/A\n")
      results[[name]] <- list(status = "not_available")
    } else {
      strikes_vec <- unlist(strikes)
      cat(sprintf(" ✅ %d strikes\n", length(strikes_vec)))

results[[name]] <- list(
  status = "success",
  count = length(strikes_vec),
  range = list(min = min(strikes_vec), max = max(strikes_vec)),
  strikes = strikes_vec
)
    }
  }

  return(results)
}

# Find closest expiration
find_closest_exp <- function(target, available) {
  target_int <- as.numeric(target)
  valid_exps <- available[nchar(available) == 8 & !is.na(as.numeric(available))]

  if (length(valid_exps) == 0) return(available[1])

  valid_ints <- as.numeric(valid_exps)
  closest_idx <- which.min(abs(valid_ints - target_int))
  return(valid_exps[closest_idx])
}

# Generate summary report
generate_summary <- function(symbol, trading_class, results) {
  total_exps <- length(results)
  successful_exps <- 0
  total_strikes <- 0

  cat("Symbol:", symbol, "| Trading Class:", trading_class, "\n")

  for (exp_name in names(results)) {
    exp_data <- results[[exp_name]]
    exp_strikes <- 0
    exp_success <- FALSE

    if (!is.null(exp_data$strike_results)) {
      for (range_name in names(exp_data$strike_results)) {
        range_data <- exp_data$strike_results[[range_name]]
        if (range_data$status == "success") {
          exp_strikes <- exp_strikes + range_data$count
          exp_success <- TRUE
        }
      }
    }

    if (exp_success) {
      successful_exps <- successful_exps + 1
      total_strikes <- total_strikes + exp_strikes
    }

    target_status <- if (exp_data$target_matched) "✅" else "⚠️"
    cat(sprintf("  %s %s: %d strikes\n", target_status, exp_name, exp_strikes))
  }

  success_rate <- if (total_exps > 0) (successful_exps / total_exps * 100) else 0
  cat(sprintf("\n🎯 Results: %d/%d expirations (%.0f%%), %d total strikes\n",
              successful_exps, total_exps, success_rate, total_strikes))

  if (success_rate >= 80) {
    cat("✅ EXCELLENT - Ready for production\n")
  } else if (success_rate >= 60) {
    cat("⚠️  GOOD - Minor issues\n")
  } else {
    cat("❌ POOR - Needs attention\n")
  }
}

# Quick validation test
quick_test <- function() {
  cat("⚡ Quick ESTX50 Test\n")
  cat(paste(rep("-", 25), collapse = ""), "\n")

  symbol <- "ESTX50"

  # Test trading classes
  cat("1. Trading classes...")
  trading_classes <- tdata_py$getAvailableTradingClasses(symbol, "EUREX")

  if (!is.null(trading_classes) && length(trading_classes) > 0) {
    cat(" ✅", paste(trading_classes, collapse = ", "), "\n")
  } else {
    cat(" ❌ None found\n")
    return(NULL)
  }

  # Get recommended
  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  # Test one expiration
  cat("2. Testing Sep 2025...")
  strikes <- tdata_py$getOptionStrikes(
    symbol, trading_class, "20250919",
    strike_min = 4320, strike_max = 5280,  # ±10% around 4800
    exchangeOpt = "EUREX"
  )

  if (!is.null(strikes) && length(strikes) > 0) {
    strikes_vec <- unlist(strikes)
    cat(" ✅", length(strikes_vec), "strikes\n")
  } else {
    cat(" ❌ No strikes\n")
  }

  cat("3. Cache check...")
  # Use print_storage_summary instead of accessing storage stats directly
  tryCatch({
    tdata_py$print_storage_summary()
    cat(" ✅ Cache accessible\n")
  }, error = function(e) {
    cat(" ❌ Cache issue\n")
  })

  cat("\n✅ Quick test complete!\n")
}

# Test parquet utilities
test_parquet_utils <- function() {
  cat("🧪 Testing Parquet Utilities\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  # Test 1: print_storage_summary
  cat("1. print_storage_summary()...")
  tryCatch({
    tdata_py$print_storage_summary()
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })

  # Test 2: list_parquet_files
  cat("2. list_parquet_files()...")
  tryCatch({
    tdata_py$list_parquet_files()
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })

  # Test 3: validate_parquet_structure
  cat("3. validate_parquet_structure()...")
  tryCatch({
    validation <- tdata_py$validate_parquet_structure()
    if (!is.null(validation)) {
      cat(sprintf(" ✅ %d files checked, %d issues\n",
                  validation$files_checked, validation$issues_found))
    } else {
      cat(" ❌ NULL result\n")
    }
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })
}

# Initialize cache if empty
initialize_cache <- function() {
  cat("🔄 Initializing ESTX50 Cache\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")

  symbol <- "ESTX50"

  cat("Fetching chains from IBKR...")
  chains <- tdata_py$getChains(symbol, force_refresh = TRUE)

  if (!is.null(chains) && is.list(chains)) {
    cat(" ✅", length(chains), "chains fetched\n")

    for (i in 1:length(chains)) {
      chain <- chains[[i]]
      if (length(chain) >= 6) {
        trading_class <- chain[[3]]
        exp_count <- length(chain[[5]])
        strike_count <- length(chain[[6]])
        cat(sprintf("   %s: %d expirations, %d strikes\n",
                    trading_class, exp_count, strike_count))
      }
    }
  } else {
    cat(" ❌ Failed\n")
  }
}

# Test specific expiration in detail
test_single_expiration <- function(symbol = "ESTX50", expiration = "20250919") {
  cat("🎯 Single Expiration Test:", expiration, "\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  # Get recommended trading class
  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  cat("Trading Class:", trading_class, "\n")

  # Test ATM strikes
  strikes <- tdata_py$getOptionStrikes(
    symbol, trading_class, expiration,
    strike_min = 4320, strike_max = 5280,  # ±10% around 4800
    exchangeOpt = "EUREX"
  )

  if (!is.null(strikes) && length(strikes) > 0) {
    strikes_vec <- unlist(strikes)
    cat(sprintf("✅ %d strikes qualified\n", length(strikes_vec)))
    cat("Range: €", min(strikes_vec), " - €", max(strikes_vec), "\n")
    cat("Sample:", paste(head(strikes_vec, 8), collapse = ", "), "\n")
    return(strikes_vec)
  } else {
    cat("❌ No strikes found\n")
    return(NULL)
  }
}

# Compare trading classes
compare_trading_classes <- function(symbol = "ESTX50") {
  cat("🔄 Trading Class Comparison\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")

  all_chains <- tdata_py$getAllChains(symbol, "EUREX")

  if (is.null(all_chains) || length(all_chains) == 0) {
    cat("❌ No chains available\n")
    return(NULL)
  }

  cat("Available trading classes on EUREX:\n")

  for (i in 1:length(all_chains)) {
    chain <- all_chains[[i]]
    tc <- chain$trading_class
    exp_count <- chain$expiration_count
    strike_count <- chain$strike_count
    strike_range <- chain$strike_range

    cat(sprintf("  • %s: %d exp, %d strikes (€%.0f-€%.0f)\n",
                tc, exp_count, strike_count, strike_range$min, strike_range$max))
  }

  return(all_chains)
}

# Test cache performance
benchmark_cache <- function(symbol = "ESTX50", iterations = 2) {
  cat("⚡ Cache Performance Test\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")

  cat("Testing", iterations, "iterations...\n")

  # Fresh fetch timing
  fresh_times <- numeric(iterations)
  for (i in 1:iterations) {
    start_time <- Sys.time()
    chains <- tdata_py$getChains(symbol, force_refresh = TRUE)
    fresh_times[i] <- as.numeric(Sys.time() - start_time)
    cat(sprintf("Fresh %d: %.2fs\n", i, fresh_times[i]))
  }

  # Cached fetch timing
  cached_times <- numeric(iterations)
  for (i in 1:iterations) {
    start_time <- Sys.time()
    chains <- tdata_py$getChains(symbol, force_refresh = FALSE)
    cached_times[i] <- as.numeric(Sys.time() - start_time)
    cat(sprintf("Cache %d: %.3fs\n", i, cached_times[i]))
  }

  speedup <- mean(fresh_times) / mean(cached_times)
  cat(sprintf("\n📊 Cache %.1fx faster (%.2fs vs %.3fs)\n",
              speedup, mean(fresh_times), mean(cached_times)))

  return(list(fresh = fresh_times, cached = cached_times, speedup = speedup))
}

# Test production scenarios
test_production_scenarios <- function(symbol = "ESTX50") {
  cat("🏭 Production Scenarios\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")

  recommended <- tdata_py$getRecommendedTradingClass(symbol, "most_strikes", "EUREX")
  trading_class <- recommended$trading_class

  scenarios <- list(
    list(name = "Iron_Condor", exp = "20250919", ranges = list(c(4600, 0.02), c(5000, 0.02))),
    list(name = "Straddle", exp = "20251219", ranges = list(c(4800, 0.01))),
    list(name = "Call_Spread", exp = "20260116", ranges = list(c(4900, 0.03)))
  )

  results <- list()

  for (scenario in scenarios) {
    name <- scenario$name
    expiration <- scenario$exp

    cat(sprintf("🎯 %s (%s):", name, expiration))

    success <- TRUE
    scenario_strikes <- 0

    for (range_def in scenario$ranges) {
      center <- range_def[1]
      range_pct <- range_def[2]

      range_val <- center * range_pct
      strikes <- tdata_py$getOptionStrikes(
        symbol, trading_class, expiration,
        strike_min = center - range_val, strike_max = center + range_val,
        exchangeOpt = "EUREX"
      )

      if (!is.null(strikes) && length(strikes) > 0) {
        scenario_strikes <- scenario_strikes + length(unlist(strikes))
      } else {
        success <- FALSE
      }
    }

    if (success) {
      cat(" ✅", scenario_strikes, "strikes\n")
    } else {
      cat(" ❌ Failed\n")
    }

    results[[name]] <- list(success = success, strikes = scenario_strikes)
  }

  successful <- sum(sapply(results, function(x) x$success))
  cat(sprintf("\n📈 Production Ready: %d/3 scenarios (%.0f%%)\n",
              successful, successful/3*100))

  return(results)
}

# Debug parquet files issue
debug_parquet_issue <- function() {
  cat("🔍 Debugging Parquet Files Issue\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  # Check if we have any data at all
  cat("1. Checking for cached data...\n")

  tryCatch({
    # Use print_storage_summary to see what exists
    tdata_py$print_storage_summary()
  }, error = function(e) {
    cat("❌ print_storage_summary failed:", e$message, "\n")
  })

  cat("\n2. Testing list_parquet_files...\n")

  # Test with no parameters
  cat("   All files:")
  tryCatch({
    tdata_py$list_parquet_files()
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })

  # Test with ESTX50 filter
  cat("   ESTX50 files:")
  tryCatch({
    tdata_py$list_parquet_files("ESTX50")
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌", e$message, "\n")
  })

  cat("\n3. If no files shown, try initializing cache:\n")
  cat("   Run: initialize_cache()\n")
}

# Master test runner
run_all_tests <- function() {
  cat("🚀 ESTX50 Master Test Runner\n")
  cat(paste(rep("=", 45), collapse = ""), "\n")
  cat("Timestamp:", as.character(Sys.time()), "\n\n")

  # Check IB connection
  tryCatch({
    is_available <- tdata_py$isIBAvailable()
    cat("🔌 IB Available:", if(is_available) "✅" else "❌", "\n\n")
  }, error = function(e) {
    cat("⚠️  IB check failed:", e$message, "\n\n")
  })

  # Run tests in sequence
  cat("PHASE 1: Parquet Utils\n")
  test_parquet_utils()

  cat("\nPHASE 2: Quick Validation\n")
  quick_test()

  cat("\nPHASE 3: Comprehensive Test\n")
  results <- test_estx50_comprehensive()

  cat("\nPHASE 4: Production Scenarios\n")
  prod_results <- test_production_scenarios()

  cat("\n🎉 All tests complete!\n")
  return(list(main_results = results, production_results = prod_results))
}

# Print available functions
cat("📚 ESTX50 Compact Test Suite\n")
cat("============================\n")
cat("Functions:\n")
cat("• quick_test()                - Quick validation\n")
cat("• test_estx50_comprehensive() - Full expiration testing\n")
cat("• test_single_expiration()    - Single expiration detail\n")
cat("• compare_trading_classes()   - Compare trading classes\n")
cat("• test_production_scenarios() - Production strategy tests\n")
cat("• benchmark_cache()           - Cache performance\n")
cat("• test_parquet_utils()        - Test parquet utilities\n")
cat("• debug_parquet_issue()       - Debug file listing\n")
cat("• initialize_cache()          - Initialize ESTX50 cache\n")
cat("• run_all_tests()             - Complete test suite\n")
cat("\n🚀 Start with: quick_test()\n")

