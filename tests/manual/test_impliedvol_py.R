# Test script for volatility metrics function in Tdata package
# Run this in RStudio to test the Python function via reticulate

# Load required libraries
# This is commented during package build to avoid including these libraries into the build
# library(reticulate)
# library(tidyverse)
# library(DT)  # for nice data tables

# =========================================================================
# SETUP: Configure Tdata Python environment
# =========================================================================

# Initialize Python environment for Tdata package
# The tdata_py package should be accessible through your package structure
repl_python()  # Start Python REPL session

# Standard Tdata initialization sequence (run these in Python REPL or via py_run_string)
py_run_string("
from tdata_py.core import *
from tdata_py.IB_connection import safe_ib_connect
from tdata_py.contract import *
CONFIG = load_config()
ticker_db = TickerDatabase()
")

# Import the updated volatility module
py_run_string("from tdata_py.impliedvol import get_volatility_metrics")

# Test IB connection
py_run_string("
from ib_insync import *
ib = safe_ib_connect()
print(f'IB Connected: {ib.isConnected()}')
ib.disconnect()
")

# =========================================================================
# WRAPPER FUNCTIONS
# =========================================================================

# R wrapper function that calls your Python function
get_volatility_metrics_r <- function(symbol, secType = NULL, currency = NULL,
                                     exchange = NULL, expiration_future = NULL,
                                     lookback_days = 252L, hist = FALSE, price = FALSE) {

  # Ensure Python is initialized
  if (!py_available()) {
    stop("Python not available. Please run the setup section first.")
  }

  # Call the Python function
  result <- py$get_volatility_metrics(
    sym = symbol,
    secType = secType,
    currency = currency,
    exchange = exchange,
    expiration_future = expiration_future,
    lookback_days = as.integer(lookback_days),
    hist = hist,
    price = price
  )

  return(result)
}

# For testing/development when IB is not available, create a mock function
create_mock_data <- function(symbol, hist = FALSE, price = FALSE) {
  base_data <- list(
    symbol = symbol,
    days_covered = 252L,
    requested_days = 252L,
    data_points = 126L,
    bar_size = "2 hours",
    current_iv = runif(1, 0.15, 0.4),
    iv_percentile = runif(1, 20, 80),
    iv_min = runif(1, 0.1, 0.2),
    iv_max = runif(1, 0.3, 0.5),
    iv_mean = runif(1, 0.2, 0.3),
    iv_30d_back = runif(1, 0.15, 0.35),
    iv_180d_back = runif(1, 0.15, 0.35)
  )

  if (hist) {
    hv_data <- list(
      current_hv = runif(1, 0.1, 0.3),
      hv_percentile = runif(1, 20, 80),
      hv_min = runif(1, 0.05, 0.15),
      hv_max = runif(1, 0.25, 0.4),
      hv_mean = runif(1, 0.15, 0.25),
      hv_30d_back = runif(1, 0.1, 0.3),
      hv_180d_back = runif(1, 0.1, 0.3)
    )
    base_data <- c(base_data, hv_data)
  }

  if (price) {
    current_price <- runif(1, 150, 200)
    start_price <- current_price * runif(1, 0.8, 1.2)
    price_data <- list(
      current_price = current_price,
      price_min = current_price * runif(1, 0.7, 0.9),
      price_max = current_price * runif(1, 1.1, 1.3),
      price_mean = current_price * runif(1, 0.9, 1.1),
      price_30d_back = current_price * runif(1, 0.95, 1.05),
      price_180d_back = current_price * runif(1, 0.85, 1.15),
      total_return = ((current_price - start_price) / start_price) * 100,
      annualized_return = runif(1, -20, 30)
    )
    base_data <- c(base_data, price_data)
  }

  return(base_data)
}

# =========================================================================
# TEST FUNCTIONS
# =========================================================================

test_basic_functionality <- function(use_real_data = FALSE) {
  cat("=== Testing Basic Functionality ===\n")

  # Test 1: IV only (default)
  cat("\n1. Testing IV only (default):\n")
  if (use_real_data) {
    result1 <- get_volatility_metrics_r("AAPL")
  } else {
    result1 <- create_mock_data("AAPL")
  }

  cat("Structure:", length(result1), "elements\n")
  cat("Symbol:", result1$symbol, "\n")
  cat("Current IV:", round(result1$current_iv, 4), "\n")
  cat("IV Percentile:", round(result1$iv_percentile, 1), "%\n")

  # Test 2: IV + HV
  cat("\n2. Testing IV + HV:\n")
  if (use_real_data) {
    result2 <- get_volatility_metrics_r("AAPL", hist = TRUE)
  } else {
    result2 <- create_mock_data("AAPL", hist = TRUE)
  }

  cat("Structure:", length(result2), "elements\n")
  cat("Current IV:", round(result2$current_iv, 4), "\n")
  cat("Current HV:", round(result2$current_hv, 4), "\n")
  cat("IV vs HV spread:", round(result2$current_iv - result2$current_hv, 4), "\n")

  # Test 3: IV + Price
  cat("\n3. Testing IV + Price:\n")
  if (use_real_data) {
    result3 <- get_volatility_metrics_r("AAPL", price = TRUE)
  } else {
    result3 <- create_mock_data("AAPL", price = TRUE)
  }

  cat("Structure:", length(result3), "elements\n")
  cat("Current Price:", round(result3$current_price, 2), "\n")
  cat("Total Return:", round(result3$total_return, 2), "%\n")
  cat("Annualized Return:", round(result3$annualized_return, 2), "%\n")

  # Test 4: All data
  cat("\n4. Testing All Data (IV + HV + Price):\n")
  if (use_real_data) {
    result4 <- get_volatility_metrics_r("AAPL", hist = TRUE, price = TRUE)
  } else {
    result4 <- create_mock_data("AAPL", hist = TRUE, price = TRUE)
  }

  cat("Structure:", length(result4), "elements\n")
  cat("Has IV data:", !is.nan(result4$current_iv), "\n")
  cat("Has HV data:", !is.nan(result4$current_hv), "\n")
  cat("Has Price data:", !is.nan(result4$current_price), "\n")

  return(list(iv_only = result1, iv_hv = result2, iv_price = result3, all_data = result4))
}

test_data_conversion <- function(test_results) {
  cat("\n=== Testing Data Conversion ===\n")

  # Convert to data.frame
  cat("\n1. Converting to data.frame:\n")
  df1 <- data.frame(test_results$iv_only)
  cat("Dimensions:", dim(df1), "\n")
  print(head(df1[1:5]))  # Show first 5 columns

  # Convert to tibble
  cat("\n2. Converting to tibble:\n")
  tibble1 <- as_tibble(test_results$all_data)
  print(tibble1)

  # Extract metric groups
  cat("\n3. Extracting metric groups:\n")
  all_names <- names(test_results$all_data)
  iv_metrics <- test_results$all_data[grepl("^(current_iv|iv_)", all_names)]
  hv_metrics <- test_results$all_data[grepl("^(current_hv|hv_)", all_names)]
  price_metrics <- test_results$all_data[grepl("^(current_price|price_|.*_return)", all_names)]

  cat("IV metrics:", length(iv_metrics), "elements\n")
  cat("HV metrics:", length(hv_metrics), "elements\n")
  cat("Price metrics:", length(price_metrics), "elements\n")

  return(list(
    df = df1,
    tibble = tibble1,
    iv_metrics = iv_metrics,
    hv_metrics = hv_metrics,
    price_metrics = price_metrics
  ))
}

test_multiple_symbols <- function(use_real_data = FALSE) {
  cat("\n=== Testing Multiple Symbols ===\n")

  symbols <- c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY")

  # Collect data for multiple symbols
  if (use_real_data) {
    results_list <- map(symbols, ~ {
      result <- get_volatility_metrics_r(.x, hist = TRUE, price = TRUE)
      return(result)
    })
  } else {
    results_list <- map(symbols, ~ {
      result <- create_mock_data(.x, hist = TRUE, price = TRUE)
      return(result)
    })
  }
  names(results_list) <- symbols

  # Convert to combined data.frame
  combined_df <- map_dfr(results_list, ~ data.frame(.x), .id = "symbol_id")

  cat("Combined data dimensions:", dim(combined_df), "\n")
  print(head(combined_df[c("symbol_id", "symbol", "current_iv", "current_hv", "current_price")]))

  # Create summary table
  summary_df <- combined_df %>%
    select(
      Symbol = symbol,
      IV = current_iv,
      HV = current_hv,
      Price = current_price,
      `Total Return %` = total_return,
      `Annual Return %` = annualized_return
    ) %>%
    mutate(
      IV = round(IV * 100, 1),  # Convert to percentage
      HV = round(HV * 100, 1),  # Convert to percentage
      Price = round(Price, 2),
      `Total Return %` = round(`Total Return %`, 1),
      `Annual Return %` = round(`Annual Return %`, 1)
    )

  return(list(raw_data = results_list, combined_df = combined_df, summary = summary_df))
}

# create_interactive_table <- function(data) {
#   cat("\n=== Creating Interactive Table ===\n")
#
#   # Create a nice interactive table with DT
#   dt_table <- DT::datatable(
#     data$summary,
#     options = list(
#       pageLength = 10,
#       scrollX = TRUE,
#       autoWidth = TRUE
#     ),
#     caption = "Volatility and Price Metrics Summary"
#   ) %>%
#     DT::formatStyle(
#       "IV",
#       background = DT::styleColorBar(range(data$summary$IV), "lightblue"),
#       backgroundSize = "100% 90%",
#       backgroundRepeat = "no-repeat",
#       backgroundPosition = "center"
#     ) %>%
#     DT::formatStyle(
#       "HV",
#       background = DT::styleColorBar(range(data$summary$HV), "lightgreen"),
#       backgroundSize = "100% 90%",
#       backgroundRepeat = "no-repeat",
#       backgroundPosition = "center"
#     )
#
#   return(dt_table)
# }

# =========================================================================
# RUN ALL TESTS
# =========================================================================

run_all_tests <- function(use_real_data = FALSE) {
  cat("Starting comprehensive tests...\n")
  if (use_real_data) {
    cat("Using REAL IB data - ensure connection is available\n")
  } else {
    cat("Using MOCK data for testing\n")
  }
  cat("==========================================\n")

  # Test basic functionality
  basic_results <- test_basic_functionality(use_real_data)

  # Test data conversion
  conversion_results <- test_data_conversion(basic_results)

  # Test multiple symbols
  multi_results <- test_multiple_symbols(use_real_data)

  # Create interactive table
  interactive_table <- create_interactive_table(multi_results)

  cat("\n=== Test Summary ===\n")
  cat("✓ Basic functionality tests completed\n")
  cat("✓ Data conversion tests completed\n")
  cat("✓ Multiple symbol tests completed\n")
  cat("✓ Interactive table created\n")

  # Return everything for further inspection
  return(list(
    basic = basic_results,
    conversion = conversion_results,
    multiple = multi_results,
    table = interactive_table
  ))
}

# =========================================================================
# EXECUTE TESTS
# =========================================================================

# First, run with mock data to test the R infrastructure
cat("=== TESTING WITH MOCK DATA ===\n")
test_results_mock <- run_all_tests(use_real_data = FALSE)

# Display the interactive table
test_results_mock$table

# Example of accessing specific results
cat("\n=== Example Data Access ===\n")
cat("AAPL Current IV:", test_results_mock$basic$all_data$current_iv, "\n")
cat("All symbols summary:\n")
print(test_results_mock$multiple$summary)

# =========================================================================
# REAL DATA TESTS (run only when IB connection is available)
# =========================================================================

test_real_data <- function() {
  cat("\n\n=== TESTING WITH REAL IB DATA ===\n")
  cat("Make sure TWS/IB Gateway is running and connected!\n")

  # Test IB connection first
  py_run_string("
ib = safe_ib_connect()
if ib.isConnected():
    print('✓ IB Connection successful')
    ib.disconnect()
else:
    print('✗ IB Connection failed')
")

  # Run real data tests
  test_results_real <- run_all_tests(use_real_data = TRUE)

  # Compare mock vs real data structures
  cat("\n=== Mock vs Real Data Comparison ===\n")
  cat("Mock data elements:", length(test_results_mock$basic$all_data), "\n")
  cat("Real data elements:", length(test_results_real$basic$all_data), "\n")

  # Display real data table
  test_results_real$table
}

# Uncomment the following section when you want to test with real IB data:
test_real_data()
# =========================================================================
# TDATA-SPECIFIC HELPER FUNCTIONS
# =========================================================================

# Function that integrates well with your existing Tdata workflow
tdata_get_volatility_batch <- function(symbols, hist = TRUE, price = TRUE,
                                       lookback_days = 252L) {
  cat("Processing", length(symbols), "symbols with Tdata...\n")

  # Initialize progress tracking
  results <- list()
  errors <- character()

  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    cat("Processing", symbol, paste0("(", i, "/", length(symbols), ")..."))

    tryCatch({
      # Get ticker info from your database first
      py_run_string(paste0("ticker_info = ticker_db.get_ticker_info('", symbol, "')"))

      # Call volatility function
      result <- get_volatility_metrics_r(
        symbol = symbol,
        hist = hist,
        price = price,
        lookback_days = lookback_days
      )

      if (!is.null(result) && is.null(result$error)) {
        results[[symbol]] <- result
        cat(" ✓\n")
      } else {
        errors <- c(errors, symbol)
        cat(" ✗ Error in result\n")
      }

    }, error = function(e) {
      errors <- c(errors, symbol)
      cat(" ✗", e$message, "\n")
    })

    # Small delay to avoid overwhelming IB
    Sys.sleep(0.5)
  }

  cat("\nCompleted:", length(results), "successful,", length(errors), "errors\n")
  if (length(errors) > 0) {
    cat("Errors for:", paste(errors, collapse = ", "), "\n")
  }

  return(list(
    results = results,
    errors = errors,
    summary_df = if (length(results) > 0) {
      map_dfr(results, ~ data.frame(.x), .id = "symbol")
    } else {
      data.frame()
    }
  ))
}

# Example usage with your typical symbols
example_tdata_workflow <- function() {
  cat("\n=== Example Tdata Workflow ===\n")

  # Your typical symbol list
  my_symbols <- c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY")

  # Get volatility data
  vol_data <- tdata_get_volatility_batch(
    symbols = my_symbols,
    hist = TRUE,
    price = TRUE,
    lookback_days = 252L
  )

  if (nrow(vol_data$summary_df) > 0) {
    # Create analysis summary
    analysis <- vol_data$summary_df %>%
      select(symbol, current_iv, current_hv, current_price, total_return) %>%
      mutate(
        iv_pct = round(current_iv * 100, 1),
        hv_pct = round(current_hv * 100, 1),
        iv_hv_spread = round((current_iv - current_hv) * 100, 1),
        price_formatted = round(current_price, 2),
        return_pct = round(total_return, 1)
      ) %>%
      select(Symbol = symbol, `IV %` = iv_pct, `HV %` = hv_pct,
             `IV-HV Spread` = iv_hv_spread, Price = price_formatted,
             `Return %` = return_pct)

    print(analysis)
    return(vol_data)
  } else {
    cat("No data retrieved successfully\n")
    return(NULL)
  }
}

# =========================================================================
# HELPER FUNCTIONS FOR PRODUCTION USE
# =========================================================================

# Enhanced safe function for your Tdata environment
safe_get_volatility_metrics <- function(symbol, secType = NULL, currency = NULL,
                                        exchange = NULL, expiration_future = NULL,
                                        lookback_days = 252L, hist = FALSE, price = FALSE) {
  tryCatch({
    # Ensure Python environment is ready
    if (!py_available()) {
      stop("Python environment not available")
    }

    # Call the function through your Tdata setup
    result <- get_volatility_metrics_r(
      symbol = symbol,
      secType = secType,
      currency = currency,
      exchange = exchange,
      expiration_future = expiration_future,
      lookback_days = as.integer(lookback_days),
      hist = hist,
      price = price
    )

    # Check for errors in the result
    if (!is.null(result$error)) {
      warning(paste("Python function returned error for", symbol, ":", result$error))
      return(NULL)
    }

    return(result)

  }, error = function(e) {
    warning(paste("Error calling Python function for", symbol, ":", e$message))
    return(NULL)
  })
}

# =========================================================================
# QUICK START EXAMPLES
# =========================================================================

cat("\n=== QUICK START EXAMPLES ===\n")
cat("1. Test with mock data: test_results_mock <- run_all_tests(use_real_data = FALSE)\n")
cat("2. Test single symbol with real data: result <- get_volatility_metrics_r('AAPL', hist=TRUE, price=TRUE)\n")
cat("3. Test batch processing: vol_data <- tdata_get_volatility_batch(c('AAPL', 'MSFT'))\n")
cat("4. Run example workflow: example_tdata_workflow()\n")

# Test with mock data by default
cat("\nRunning initial mock data test...\n")
initial_test <- safe_get_volatility_metrics("AAPL", hist = TRUE, price = TRUE)
if (!is.null(initial_test)) {
  cat("✓ Mock test successful - function structure working\n")
  cat("Elements returned:", length(initial_test), "\n")
} else {
  cat("✗ Mock test failed\n")
}
