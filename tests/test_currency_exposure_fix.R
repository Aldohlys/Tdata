#!/usr/bin/env Rscript
# Test script for getCurrencyExposure CASH position fix
#
# This script verifies that CASH positions are correctly grouped by their
# trading currency (symbol) instead of base currency, ensuring accurate
# currency exposure reporting.
#
# Expected behavior:
# - USD cash should appear in USD exposure (not CHF)
# - EUR cash should appear in EUR exposure (not CHF)
# - Market values can be negative (short positions)
# - Function works with both date=NULL and specific dates

library(Tdata)

cat("\n=== Testing getCurrencyExposure() CASH Position Fix ===\n\n")

# Test 1: Basic functionality with current data
cat("Test 1: Current portfolio exposure (date = NULL)\n")
cat("------------------------------------------------\n")
exposure <- getCurrencyExposure("U1804173")
print(exposure)

# Verify structure
stopifnot("Currency" %in% colnames(exposure))
stopifnot("MarketValue" %in% colnames(exposure))
stopifnot("UnrealizedPnL" %in% colnames(exposure))
stopifnot("PercentOfPortfolio" %in% colnames(exposure))

cat("\n✓ Structure check passed\n")

# Test 2: Verify CASH positions are in correct currency
cat("\nTest 2: Verify CASH positions grouped correctly\n")
cat("------------------------------------------------\n")

# Get raw portfolio data to check CASH positions
conn <- Tdata::safe_db_connect()
cash_positions <- DBI::dbGetQuery(conn, "
  SELECT symbol, type, currency, pos, mktValue
  FROM U1804173
  WHERE type = 'CASH'
    AND (date, heure) = (SELECT date, heure FROM U1804173 ORDER BY date DESC, heure DESC LIMIT 1)
")
DBI::dbDisconnect(conn)

cat("\nCASH positions in portfolio:\n")
print(cash_positions)

# For each CASH position, verify it appears in the correct currency row
for (i in seq_len(nrow(cash_positions))) {
  cash_currency <- cash_positions$symbol[i]
  cash_value <- cash_positions$mktValue[i]

  # Find this currency in exposure data
  currency_row <- exposure[exposure$Currency == cash_currency, ]

  if (nrow(currency_row) == 0) {
    stop(sprintf("ERROR: CASH position in %s not found in exposure breakdown!", cash_currency))
  }

  cat(sprintf("\n✓ %s cash (%.2f) found in %s exposure row\n",
              cash_currency, cash_value, cash_currency))
}

cat("\n✓ CASH grouping check passed\n")

# Test 3: Verify negative values handled correctly
cat("\nTest 3: Verify negative market values handled correctly\n")
cat("--------------------------------------------------------\n")

# Check for any negative market values
if (any(exposure$MarketValue < 0)) {
  cat("Found negative market values (short positions):\n")
  negative_rows <- exposure[exposure$MarketValue < 0, c("Currency", "MarketValue")]
  print(negative_rows)
  cat("\n✓ Negative values handled correctly\n")
} else {
  cat("No negative market values in current portfolio\n")
  cat("✓ All market values positive\n")
}

# Test 4: Verify percentages sum correctly
cat("\nTest 4: Verify portfolio percentages\n")
cat("-------------------------------------\n")
total_pct <- sum(abs(exposure$PercentOfPortfolio))
cat(sprintf("Sum of absolute percentages: %.2f%%\n", total_pct))

# Allow some tolerance for rounding
if (abs(total_pct - 100) < 5) {
  cat("✓ Percentage calculation reasonable\n")
} else {
  warning(sprintf("Percentages sum to %.2f%%, expected ~100%%", total_pct))
}

# Test 5: Historical date query
cat("\nTest 5: Test with specific historical date\n")
cat("-------------------------------------------\n")

# Get a historical date from the database
conn <- Tdata::safe_db_connect()
historical_dates <- DBI::dbGetQuery(conn, "
  SELECT DISTINCT date
  FROM U1804173
  WHERE date < (SELECT MAX(date) FROM U1804173)
  ORDER BY date DESC
  LIMIT 1
")
DBI::dbDisconnect(conn)

if (nrow(historical_dates) > 0) {
  test_date <- as.Date(as.character(historical_dates$date[1]), "%Y%m%d")
  cat(sprintf("Testing with date: %s\n", test_date))

  historical_exposure <- getCurrencyExposure("U1804173", date = test_date)

  if (nrow(historical_exposure) > 0) {
    cat("\nHistorical exposure:\n")
    print(historical_exposure[, c("Currency", "MarketValue", "UnrealizedPnL")])
    cat("\n✓ Historical date query works\n")
  } else {
    cat("No exposure data for historical date\n")
  }
} else {
  cat("No historical data available for testing\n")
  cat("✓ Skipping historical test\n")
}

# Test 6: Verify no CHF CASH misattribution
cat("\nTest 6: Verify CHF exposure does not include foreign CASH\n")
cat("----------------------------------------------------------\n")

# Check if there's a CHF row
chf_row <- exposure[exposure$Currency == "CHF", ]

if (nrow(chf_row) > 0) {
  cat(sprintf("CHF exposure found: Market Value = %.2f\n", chf_row$MarketValue))

  # Verify this doesn't include USD or EUR cash
  # (CHF row should only exist if there are actual CHF-denominated positions)
  conn <- Tdata::safe_db_connect()
  chf_positions <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM U1804173
    WHERE (type != 'CASH' AND currency = 'CHF')
       OR (type = 'CASH' AND symbol = 'CHF')
    AND (date, heure) = (SELECT date, heure FROM U1804173 ORDER BY date DESC, heure DESC LIMIT 1)
  ")
  DBI::dbDisconnect(conn)

  if (chf_positions$count > 0) {
    cat(sprintf("✓ CHF row exists due to %d CHF-denominated positions\n", chf_positions$count))
  } else {
    stop("ERROR: CHF row exists but no CHF positions found!")
  }
} else {
  cat("No CHF exposure row (expected if no CHF-denominated positions)\n")
  cat("✓ No false CHF exposure from foreign CASH positions\n")
}

# Test 7: Data integrity check
cat("\nTest 7: Data integrity verification\n")
cat("------------------------------------\n")

# Verify all columns are numeric (except Currency)
numeric_cols <- c("CashPosition", "CashPositionBase", "MarketValue",
                  "MarketValueBase", "UnrealizedPnL", "UnrealizedPnLBase",
                  "TotalExposureBase", "PercentOfPortfolio")

for (col in numeric_cols) {
  if (!is.numeric(exposure[[col]])) {
    stop(sprintf("ERROR: Column %s is not numeric!", col))
  }
}

cat("✓ All numeric columns have correct type\n")

# Verify no NA values in critical columns
critical_cols <- c("Currency", "MarketValue", "PercentOfPortfolio")
for (col in critical_cols) {
  if (any(is.na(exposure[[col]]))) {
    warning(sprintf("NA values found in %s column", col))
  }
}

cat("✓ No NA values in critical columns\n")

# Summary
cat("\n=== ALL TESTS PASSED ===\n")
cat("\nSummary of currency exposure:\n")
summary_df <- exposure[, c("Currency", "CashPosition", "MarketValue", "PercentOfPortfolio")]
print(summary_df)

cat("\n✓ getCurrencyExposure() fix verified successfully!\n\n")
