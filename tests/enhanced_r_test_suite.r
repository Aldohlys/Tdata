# enhanced_test_parquet_chains.R
# Comprehensive test suite for multi-exchange option chains with detailed strike analysis
# Tests US, European, and other international exchanges

library(reticulate)
library(testthat)

#' Setup enhanced test environment
setup_enhanced_parquet_tests <- function() {
  cat("Setting up Enhanced Parquet chain discovery tests...\n")
  cat("Testing multiple exchanges and detailed strike analysis...\n")

  # Import the enhanced parquet module
  parquet <<- import("tdata_py")

  # Enhanced test symbols across different exchanges and asset classes
  test_symbols <<- list(
    # US Equities - SMART exchange
    us_equities = list(
      symbols = c("MSTR", "BAC"),
      exchange = "SMART",
      description = "US Large Cap ETFs and Stocks"
    ),

    # US ETFs with unique characteristics
    us_etfs = list(
      symbols = c("USO", "SLV", "TLT", "VXX"),
      exchange = "SMART",
      description = "Commodity and Bond ETFs"
    ),

    # European stocks - EUREX
    european = list(
      symbols = c("SIE", "AI", "SU", "ABBN", "SGO"),  # Siemens, Air Liquide, Schneider
      exchange = "EUREX",
      description = "European Large Cap Stocks"
    ),

    # Index options with different characteristics
    indices = list(
      symbols = c("SPX", "NDX"),  # S&P 500, NASDAQ 100
      exchange = "CBOE",
      description = "Major US Index Options"
    ),

    # International ETFs
    international = list(
      symbols = c("EWJ", "FXI", "EWZ"),  # Japan, China, Brazil ETFs
      exchange = "SMART",
      description = "International Country ETFs"
    )
  )

  cat("Test environment ready with", length(unlist(lapply(test_symbols, function(x) x$symbols))), "symbols across", length(test_symbols), "categories\n")
}

#' Test 1: Multi-Exchange Chain Discovery
test_multi_exchange_discovery <- function() {
  cat("\n=== Test 1: Multi-Exchange Chain Discovery ===\n")

  results <- list()

  for (category_name in names(test_symbols)) {
    category <- test_symbols[[category_name]]
    cat(sprintf("\n--- Testing %s (%s) ---\n", category$description, category$exchange))

    category_results <- list()

    for (symbol in category$symbols) {
      cat(sprintf("Testing %s on %s...\n", symbol, category$exchange))

      tryCatch({
        # Get chains with specific exchange
        chains <- parquet$getChains(symbol, exchangeOpt = category$exchange)

        if (is.null(chains)) {
          cat(sprintf("  ❌ %s: getChains returned NULL (connection issue)\n", symbol))
          category_results[[symbol]] <- list(status = "connection_error")
          next
        }

        if (is.numeric(chains) && is.nan(chains)) {
          cat(sprintf("  ⚠️  %s: No chains found on %s\n", symbol, category$exchange))
          category_results[[symbol]] <- list(status = "not_found", exchange = category$exchange)
          next
        }

        if (is.list(chains) && length(chains) > 0) {
          # Analyze trading classes
          trading_classes <- unique(sapply(chains, function(x) x[[3]]))
          total_expirations <- sum(sapply(chains, function(x) length(x[[5]])))
          total_strikes <- sum(sapply(chains, function(x) length(x[[6]])))

          cat(sprintf("  ✅ %s: %d chains, %d trading classes\n",
                      symbol, length(chains), length(trading_classes)))
          cat(sprintf("    Trading Classes: %s\n", paste(trading_classes, collapse = ", ")))
          cat(sprintf("    Total Expirations: %d, Total Strikes: %d\n",
                      total_expirations, total_strikes))

          # Store detailed results
          category_results[[symbol]] <- list(
            status = "success",
            exchange = category$exchange,
            chain_count = length(chains),
            trading_classes = trading_classes,
            total_expirations = total_expirations,
            total_strikes = total_strikes,
            chains = chains
          )

          # Test enhanced discovery functions
          tryCatch({
            discovery <- parquet$discoverSymbol(symbol, format = "summary")
            if (!is.null(discovery) && !is.null(discovery$recommended)) {
              recommended_tc <- discovery$recommended$trading_class
              cat(sprintf("    💡 Recommended TC: %s\n", recommended_tc))
            }
          }, error = function(e) {
            cat(sprintf("    ⚠️  Discovery failed: %s\n", e$message))
          })

        } else {
          cat(sprintf("  ❌ %s: Unexpected chains format: %s\n", symbol, class(chains)))
          category_results[[symbol]] <- list(status = "unexpected_format")
        }

        Sys.sleep(1)  # Rate limiting

      }, error = function(e) {
        cat(sprintf("  ❌ %s: Error - %s\n", symbol, e$message))
        category_results[[symbol]] <- list(status = "error", message = e$message)
      })
    }

    results[[category_name]] <- category_results
  }

  # Store results for later analysis
  assign("discovery_results", results, envir = .GlobalEnv)

  return(results)
}

#' Test 2: Detailed Expiration and Strike Analysis
test_expiration_strike_analysis <- function() {
  cat("\n=== Test 2: Detailed Expiration and Strike Analysis ===\n")

  if (!exists("discovery_results")) {
    cat("⚠️  Running discovery first...\n")
    test_multi_exchange_discovery()
  }

  strike_analysis <- list()

  for (category_name in names(discovery_results)) {
    cat(sprintf("\n--- Strike Analysis for %s ---\n", category_name))

    category_analysis <- list()

    for (symbol in names(discovery_results[[category_name]])) {
      result <- discovery_results[[category_name]][[symbol]]

      if (result$status != "success") {
        next
      }

      cat(sprintf("\nAnalyzing strikes for %s:\n", symbol))

      symbol_analysis <- list()

      # Analyze each trading class
      for (chain in result$chains) {
        trading_class <- chain[[3]]
        expirations <- chain[[5]]
        all_strikes <- chain[[6]]

        cat(sprintf("  Trading Class: %s\n", trading_class))
        cat(sprintf("    Expirations (%d): %s\n",
                    length(expirations),
                    paste(head(expirations, 5), collapse = ", ")))

        if (length(expirations) > 5) {
          cat(sprintf("    ... and %d more\n", length(expirations) - 5))
        }

        cat(sprintf("    Strike Range: $%.2f - $%.2f (%d strikes)\n",
                    min(all_strikes), max(all_strikes), length(all_strikes)))

        # Test strike qualification for first few expirations
        tested_expirations <- head(expirations, 3)  # Test first 3 expirations

        tc_analysis <- list(
          trading_class = trading_class,
          total_expirations = length(expirations),
          total_strikes = length(all_strikes),
          strike_range = c(min(all_strikes), max(all_strikes)),
          tested_strikes = list()
        )

        for (expiration in tested_expirations) {
          cat(sprintf("    Testing strikes for expiration %s...\n", expiration))

          tryCatch({
            # Calculate reasonable strike range (around middle of available strikes)
            mid_strike <- median(all_strikes)
            strike_range_pct <- 0.1  # 10% range

            # Test getOptionStrikes with range
            qualified_strikes <- parquet$getOptionStrikes(
              symbol,
              trading_class,
              expiration,
              strike_min = mid_strike * (1 - strike_range_pct),
              strike_max = mid_strike * (1 + strike_range_pct)
            )

            if (!is.null(qualified_strikes) && length(qualified_strikes) > 0) {
              cat(sprintf("      ✅ Qualified %d strikes: $%.2f - $%.2f\n",
                          length(qualified_strikes),
                          min(qualified_strikes),
                          max(qualified_strikes)))

              tc_analysis$tested_strikes[[expiration]] <- list(
                qualified_count = length(qualified_strikes),
                strike_range = c(min(qualified_strikes), max(qualified_strikes)),
                sample_strikes = head(qualified_strikes, 5)
              )

            } else if (length(qualified_strikes) == 0) {
              cat(sprintf("      ⚠️  No strikes qualified in range\n"))
              tc_analysis$tested_strikes[[expiration]] <- list(qualified_count = 0)
            } else {
              cat(sprintf("      ❌ Strike qualification failed\n"))
            }

            Sys.sleep(2)  # Longer sleep for strike qualification

          }, error = function(e) {
            cat(sprintf("      ❌ Error qualifying strikes: %s\n", e$message))
            tc_analysis$tested_strikes[[expiration]] <- list(error = e$message)
          })
        }

        symbol_analysis[[trading_class]] <- tc_analysis
      }

      category_analysis[[symbol]] <- symbol_analysis
    }

    strike_analysis[[category_name]] <- category_analysis
  }

  # Store strike analysis results
  assign("strike_analysis_results", strike_analysis, envir = .GlobalEnv)

  return(strike_analysis)
}

#' Test 3: Trading Class Comparison Analysis
test_trading_class_comparison <- function() {
  cat("\n=== Test 3: Trading Class Comparison Analysis ===\n")

  if (!exists("discovery_results")) {
    cat("⚠️  Running discovery first...\n")
    test_multi_exchange_discovery()
  }

  # Find symbols with multiple trading classes for comparison
  multi_tc_symbols <- list()

  for (category_name in names(discovery_results)) {
    for (symbol in names(discovery_results[[category_name]])) {
      result <- discovery_results[[category_name]][[symbol]]

      if (result$status == "success" && length(result$trading_classes) > 1) {
        multi_tc_symbols[[symbol]] <- result
      }
    }
  }

  cat(sprintf("Found %d symbols with multiple trading classes:\n", length(multi_tc_symbols)))

  for (symbol in names(multi_tc_symbols)) {
    result <- multi_tc_symbols[[symbol]]
    cat(sprintf("\n--- Comparing Trading Classes for %s ---\n", symbol))
    cat(sprintf("Available Trading Classes: %s\n", paste(result$trading_classes, collapse = ", ")))

    tryCatch({
      # Use enhanced comparison function
      comparison <- parquet$compareTradingClasses(symbol)

      if (!is.null(comparison)) {
        if (is.data.frame(comparison)) {
          cat("Comparison Table:\n")
          print(comparison)
        } else if (is.list(comparison)) {
          for (tc in names(comparison)) {
            if (tc != "error") {
              cat(sprintf("  %s: %d expirations, %d strikes\n",
                          tc,
                          comparison[[tc]]$expiration_count %||% 0,
                          comparison[[tc]]$strike_count %||% 0))
            }
          }
        }
      }

      # Test auto-selecting functions
      cat("\nTesting auto-selection functions:\n")

      best_tc <- parquet$findBestTradingClass(symbol, criteria = "most_strikes")
      if (!is.null(best_tc)) {
        cat(sprintf("  💡 Best TC (most strikes): %s (%d strikes)\n",
                    best_tc$trading_class, best_tc$strike_count))
      }

      # Test auto strikes function
      if (length(result$chains) > 0 && length(result$chains[[1]][[5]]) > 0) {
        first_expiration <- result$chains[[1]][[5]][[1]]
        auto_strikes <- parquet$getStrikesAuto(symbol, first_expiration)

        if (!is.null(auto_strikes) && length(auto_strikes) > 0) {
          cat(sprintf("  🎯 Auto strikes for %s: %d strikes\n", first_expiration, length(auto_strikes)))
        }
      }

    }, error = function(e) {
      cat(sprintf("  ❌ Comparison failed: %s\n", e$message))
    })
  }
}

#' Test 4: Exchange-Specific Features
test_exchange_specific_features <- function() {
  cat("\n=== Test 4: Exchange-Specific Features ===\n")

  exchange_features <- list(
    "SMART" = list(
      description = "US Equities and ETFs",
      expected_features = c("Multiple trading classes", "Weekly options", "High liquidity")
    ),
    "CBOE" = list(
      description = "Index Options",
      expected_features = c("European style", "Cash settlement", "AM/PM settlement")
    ),
    "EUREX" = list(
      description = "European Options",
      expected_features = c("EUR denomination", "European trading hours")
    )
  )

  for (exchange in names(exchange_features)) {
    cat(sprintf("\n--- %s Exchange Features ---\n", exchange))
    cat(sprintf("Description: %s\n", exchange_features[[exchange]]$description))

    # Find symbols that trade on this exchange
    exchange_symbols <- c()

    if (exists("discovery_results")) {
      for (category_name in names(discovery_results)) {
        for (symbol in names(discovery_results[[category_name]])) {
          result <- discovery_results[[category_name]][[symbol]]
          if (result$status == "success" && result$exchange == exchange) {
            exchange_symbols <- c(exchange_symbols, symbol)
          }
        }
      }
    }

    if (length(exchange_symbols) > 0) {
      cat(sprintf("Symbols found: %s\n", paste(exchange_symbols, collapse = ", ")))

      # Analyze first symbol in detail
      first_symbol <- exchange_symbols[1]
      cat(sprintf("\nDetailed analysis of %s:\n", first_symbol))

      tryCatch({
        exploration <- parquet$exploreSymbol(first_symbol)

        if (!is.null(exploration) && exploration$status == "success") {
          summary <- exploration$summary
          cat(sprintf("  Total Chains: %d\n", summary$total_chains))
          cat(sprintf("  Trading Classes: %d\n", summary$unique_trading_classes))
          cat(sprintf("  Total Expirations: %d\n", summary$total_expirations))
          cat(sprintf("  Total Strikes: %d\n", summary$total_strikes))

          if (!is.null(exploration$recommended_trading_class)) {
            recommended <- exploration$recommended_trading_class
            cat(sprintf("  Recommended: %s (%s)\n",
                        recommended$trading_class, recommended$reason))
          }
        }

      }, error = function(e) {
        cat(sprintf("  ❌ Analysis failed: %s\n", e$message))
      })

    } else {
      cat("⚠️  No symbols found for this exchange in current test\n")
    }
  }
}

#' Test 5: Performance and Caching Analysis
test_performance_caching <- function() {
  cat("\n=== Test 5: Performance and Caching Analysis ===\n")

  # Select a few symbols for performance testing
  test_symbols_perf <- c("SPY", "QQQ")

  for (symbol in test_symbols_perf) {
    cat(sprintf("\n--- Performance Test for %s ---\n", symbol))

    # First call (likely from cache or fresh fetch)
    cat("First call (cache check)...\n")
    start_time <- Sys.time()
    chains1 <- parquet$getChains(symbol)
    time1 <- as.numeric(Sys.time() - start_time)

    if (!is.null(chains1) && is.list(chains1)) {
      cat(sprintf("  ✅ First call: %.2f seconds, %d chains\n", time1, length(chains1)))

      # Second call (should be faster from cache)
      cat("Second call (cache test)...\n")
      start_time <- Sys.time()
      chains2 <- parquet$getChains(symbol)
      time2 <- as.numeric(Sys.time() - start_time)

      cat(sprintf("  ✅ Second call: %.2f seconds\n", time2))

      # Compare results
      if (identical(chains1, chains2)) {
        cat("  ✅ Results identical - caching working correctly\n")
      } else {
        cat("  ⚠️  Results differ between calls\n")
      }

      # Speed analysis
      if (time2 < time1 * 0.5) {
        cat("  🚀 Significant speedup - caching is effective\n")
      } else if (time2 < time1) {
        cat("  ⚡ Some speedup detected\n")
      } else {
        cat("  ⚠️  No significant speedup - check caching implementation\n")
      }

      # Test enhanced functions performance
      cat("Testing enhanced functions...\n")
      start_time <- Sys.time()
      exploration <- parquet$exploreSymbol(symbol)
      explore_time <- as.numeric(Sys.time() - start_time)

      cat(sprintf("  📊 Exploration: %.2f seconds\n", explore_time))

    } else {
      cat("  ❌ Performance test failed - no valid chains\n")
    }
  }
}

#' Generate comprehensive test report
generate_test_report <- function() {
  cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
  cat("COMPREHENSIVE TEST REPORT\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")

  if (exists("discovery_results")) {
    # Summary statistics
    total_symbols <- 0
    successful_symbols <- 0
    total_chains <- 0
    total_trading_classes <- 0

    for (category_name in names(discovery_results)) {
      for (symbol in names(discovery_results[[category_name]])) {
        total_symbols <- total_symbols + 1
        result <- discovery_results[[category_name]][[symbol]]

        if (result$status == "success") {
          successful_symbols <- successful_symbols + 1
          total_chains <- total_chains + result$chain_count
          total_trading_classes <- total_trading_classes + length(result$trading_classes)
        }
      }
    }

    cat(sprintf("📊 SUMMARY STATISTICS:\n"))
    cat(sprintf("   Total Symbols Tested: %d\n", total_symbols))
    cat(sprintf("   Successful: %d (%.1f%%)\n",
                successful_symbols, (successful_symbols/total_symbols)*100))
    cat(sprintf("   Total Chains Found: %d\n", total_chains))
    cat(sprintf("   Total Trading Classes: %d\n", total_trading_classes))
    cat(sprintf("   Average Chains per Symbol: %.1f\n", total_chains/successful_symbols))

    # Exchange breakdown
    cat(sprintf("\n📈 EXCHANGE BREAKDOWN:\n"))
    exchange_summary <- list()

    for (category_name in names(discovery_results)) {
      for (symbol in names(discovery_results[[category_name]])) {
        result <- discovery_results[[category_name]][[symbol]]
        if (result$status == "success") {
          exchange <- result$exchange
          if (is.null(exchange_summary[[exchange]])) {
            exchange_summary[[exchange]] <- list(symbols = 0, chains = 0)
          }
          exchange_summary[[exchange]]$symbols <- exchange_summary[[exchange]]$symbols + 1
          exchange_summary[[exchange]]$chains <- exchange_summary[[exchange]]$chains + result$chain_count
        }
      }
    }

    for (exchange in names(exchange_summary)) {
      summary <- exchange_summary[[exchange]]
      cat(sprintf("   %s: %d symbols, %d chains\n",
                  exchange, summary$symbols, summary$chains))
    }
  }

  if (exists("strike_analysis_results")) {
    cat(sprintf("\n🎯 STRIKE ANALYSIS SUMMARY:\n"))

    total_tested_expirations <- 0
    successful_qualifications <- 0

    for (category in strike_analysis_results) {
      for (symbol_data in category) {
        for (tc_data in symbol_data) {
          tested_strikes <- tc_data$tested_strikes
          total_tested_expirations <- total_tested_expirations + length(tested_strikes)

          for (exp_data in tested_strikes) {
            if (!is.null(exp_data$qualified_count) && exp_data$qualified_count > 0) {
              successful_qualifications <- successful_qualifications + 1
            }
          }
        }
      }
    }

    if (total_tested_expirations > 0) {
      cat(sprintf("   Expirations Tested: %d\n", total_tested_expirations))
      cat(sprintf("   Successful Qualifications: %d (%.1f%%)\n",
                  successful_qualifications,
                  (successful_qualifications/total_tested_expirations)*100))
    }
  }

  cat("\n🎉 TEST SUITE COMPLETE!\n")
}

#' Main enhanced test runner
run_enhanced_parquet_tests <- function() {
  cat("Starting Enhanced Multi-Exchange Option Chain Tests...\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")

  # Setup
  setup_enhanced_parquet_tests()

  # Run all enhanced tests
  test_multi_exchange_discovery()
  test_expiration_strike_analysis()
  test_trading_class_comparison()
  test_exchange_specific_features()
  test_performance_caching()

  # Generate comprehensive report
  generate_test_report()

  cat("\n📋 NEXT STEPS:\n")
  cat("1. Review any failed symbols and investigate exchange connectivity\n")
  cat("2. Test additional expiration dates during market hours\n")
  cat("3. Implement symbol-specific strike range optimizations\n")
  cat("4. Consider adding more international exchanges\n")
}

# Safe null operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Usage:
cat("Enhanced Test Suite Loaded. Run with: run_enhanced_parquet_tests()\n")

# Auto-run if sourced directly
if (interactive()) {
  run_enhanced_parquet_tests()
}
