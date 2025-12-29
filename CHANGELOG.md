# Changelog - Tdata

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [5.7.58] - 2025-12-29

### Added
- **getGonet()**: Automatic price fetching for precious metals from ZKB website
  - **Problem**: Gold coins and other precious metals not available in IBKR or Yahoo Finance
  - **Solution**: Added web scraping for positions with type "Precious Metals" 
  - **Implementation**:
    1. Identifies positions with type == "Precious Metals" in GonetPos.csv
    2. Extracts URL from exchange field (e.g., ZKB finance URL)
    3. Fetches price using httr::GET() and regex pattern matching
    4. Adds price to last_price data frame for portfolio calculations
    5. Falls back to manual entry if web fetch fails
    6. Updates sym_ibkr to PM_[ID] before IBKR price fetch to avoid NA symbol warnings
    7. Preserves original type field (e.g., "Precious Metals") instead of hardcoding "Stock"
    8. IBKR price fetch skips PM_ symbols to prevent unnecessary API calls
  - **Usage**: Store URL in exchange field of GonetPos.csv, use "PM_" prefix for sym_ibkr in GonetTrades.csv
  - **Example**: 67 gold coins with URL https://zkb-finance.mdgms.com/home/commodities/metals/detail.html?FI_ID_NOTATION=15606539
  - **Location**: R/account.R:811-818, 844-897
  - **Dependencies**: Requires httr package

## [5.7.57] - 2025-12-24

### Fixed
- **getValue()**: Fixed critical variable shadowing bug causing contract qualification failures
  - **Problem**: ConId values became NaN for all symbols after the first one, causing IBKR errors "No security definition has been found"
  - **Root cause**: Local variable `conId` on line 108 shadowed function parameter `conId`, leaking across loop iterations
  - **Impact**: Second and subsequent symbols in batch getValue() calls failed with Error 200 or Error 321
  - **Example failure**: IE00B67T5G21 showed `conId=433080107` (correct) in first position, but `conId=nan` (wrong) in subsequent calls
  - **Solution**: Renamed local variable from `conId` to `db_conId` to avoid parameter shadowing
  - **Location**: inst/python/tdata_py/contract.py:108-111
  - **Regression introduced in**: v5.7.56 (commit 1702c07)

## [5.7.54] - 2025-12-18

### Added
- **reloadTickerCache()**: New function to refresh Python ticker cache without R restart
  - **Problem**: Tickers added to database after R session start weren't recognized by Python code
  - **Impact**: Python functions (getValue, updateTicker, etc.) defaulted to USD for new tickers, causing IBKR contract qualification failures
  - **Solution**: Exposes Python's `ticker_db.load_tickers()` method to R
  - **Usage**: Call after `addTicker()` or manual database updates
  - **Benefits**: No R session restart required, immediate ticker availability
  - **Location**: R/ticker.R:520-540

- **Contract ID (ConId) Support**: Full support for ISIN-based securities like funds
  - **Problem**: ISIN codes (e.g., IE00B67T5G21) don't work as symbols in IBKR API
  - **Solution**: Added ConId column to Tickers database and implemented contract ID-based qualification
  - **Implementation**:
    1. Added ConId INTEGER column to Tickers database table
    2. Updated Python getValue() to use Stock(conId=...) when ConId available
    3. Added ConId parameter to addTicker() function (optional)
    4. Added ConId to setTicker() updatable columns
    5. TickerDatabase automatically loads ConId (no changes needed - uses SELECT *)
  - **Usage**: `addTicker("IE00B67T5G21", "NUCL Fund", "STK", "ALLFUNDS", "EUR", ConId = 433080107)`
  - **Benefits**: Supports funds and securities without simple ticker symbols
  - **Location**:
    - Database: Tickers table ConId column
    - Python: inst/python/tdata_py/contract.py:104-140
    - R: R/ticker.R:47,91,490

### Changed
- **getStockPrice()**: Automatic delayed market data for LSEETF and ALLFUNDS exchanges
  - **Problem**: Some exchanges (LSEETF, ALLFUNDS) require real-time subscription (Error 354) not available to all users
  - **Solution**: Automatically uses reqType=4 (Delayed Frozen) instead of reqType=2 (Frozen) for these exchanges
  - **Benefits**: Funds and ETFs on these exchanges can retrieve delayed price data without subscription
  - **Impact**: 15-20 minute delayed data for LSEETF/ALLFUNDS, real-time for SMART
  - **Exchanges**:
    - LSEETF (NUCL, DTLA, TRE7): Delayed data working
    - ALLFUNDS (IE00B67T5G21): Delayed data working with ConId
    - EBS (CSBGU0): Listed for reqType=4 attempt, but requires subscription (see Known Limitations)
  - **Location**: R/prices.R:13-29

- **getGonet()**: Automatically calls getAccountGonet() at end
  - **Problem**: Cash balances weren't stored unless user manually called getAccountGonet()
  - **Solution**: getGonet() now automatically calls getAccountGonet(cash_values) after prompting for cash
  - **Benefits**: Simplified workflow - just call getGonet() once instead of two functions
  - **Impact**: Cash balances are now stored automatically and appear as defaults on next run
  - **Location**: R/account.R:909-920

### Fixed
- **getGonet()**: Fixed cash balance prompts always showing 0 as default
  - **Problem**: Database query for stored cash balances executed AFTER connection was closed
  - **Root cause**: `DBI::dbDisconnect(conn)` at line 878, then `DBI::dbGetQuery(conn, ...)` at line 888
  - **Impact**: tryCatch caught error and returned default dataframe with all zeros
  - **Solution**: Moved stored_cash query before dbDisconnect() call
  - **Result**: User now sees previously entered cash balances as defaults
  - **Location**: R/account.R:879-893

- **getValue()**: Fixed async data retrieval and fund price handling
  - **Problem**: Called `ticker.marketPrice()` before waiting for IBKR data, causing nan for funds
  - **Root cause**: `ib.sleep(1)` was AFTER accessing ticker data (line 156), should be before
  - **Impact**: Funds like NUCL (ALLFUNDS exchange) returned nan prices
  - **Solution**:
    1. Moved `ib.sleep(0.5)` before accessing ticker data (now line 153)
    2. Added fallback to `ticker.close` when `marketPrice()` returns nan
  - **Result**: Funds now return close price (NAV) correctly
  - **Location**: inst/python/tdata_py/contract.py:148-162

- **getGonet()**: Fixed mixed exchange batching causing Error 354
  - **Problem**: Using reqType=4 for ALL symbols when only SOME need delayed data caused errors for regular exchanges
  - **Root cause**: Single getValue() call with mixed delayed/regular exchanges
  - **Impact**: Regular exchange symbols (SMART) failed with Error 354 when batched with delayed exchange symbols
  - **Solution**: Split symbols by exchange type, call getValue() separately with appropriate reqType
  - **Implementation**:
    1. Identify delayed exchanges (LSEETF, EBS, ALLFUNDS) vs regular exchanges (SMART, etc.)
    2. Create two symbol lists: symbols_delayed and symbols_regular
    3. Call getValue(reqType=4) for delayed exchanges
    4. Call getValue(reqType=2) for regular exchanges
    5. Combine results from both calls
  - **Result**:
    - LSEETF exchanges (NUCL, DTLA, TRE7): Working with delayed data
    - ALLFUNDS exchanges (IE00B67T5G21): Working with ConId + delayed data
    - SMART exchanges: Working with frozen data
    - EBS exchange (CSBGU0): Error 354 logged (expected - see Known Limitations)
  - **Location**: R/account.R:812-843

### Known Limitations
- **EBS Exchange (CSBGU0)**: Does not support delayed market data, requires subscription
  - Error 354 will be logged but is expected behavior
  - getGonet() handles this gracefully via manual price entry workflow
  - Stored prices from database shown as defaults
  - User can press Enter to keep stored price or enter new value
  - This is the correct workflow for subscription-required securities

## [5.7.53] - 2025-12-16

### Fixed
- **historical_option.py**: Fixed merge logic to preserve historical data while updating with fresh data
  - Changed `drop_duplicates(keep='first')` to `keep='last'` (line 159)
  - **Problem**: Old cached data was overwriting new data from IBKR force refresh
  - **Impact**: SGO 87P historical data showed gap between Oct 31 and Dec 10, despite trading on Nov 12
  - **Root cause**: Merge strategy prioritized old cache over new IBKR data
  - **Solution**: `keep='last'` ensures new data updates overlapping periods while preserving old data IBKR no longer provides
  - **Benefits**:
    - Preserves valuable historical data that IBKR may no longer serve (e.g., data older than 1 year)
    - Updates with fresh data when IBKR provides corrections or new bars
    - Fills gaps when force_refresh fetches full year of data
    - Eliminates need for cache deletion - old data remains valuable

## [5.7.48] - 2025-12-09

### Added
- **migrate_pos_to_real.R**: Database schema migration script (inst/scripts/)
  - Migrates `Trades.Pos` column from INTEGER to REAL
  - Migrates portfolio tables `pos` column from INTEGER to REAL (all U*, DU* tables)
  - Supports fractional shares and high-precision currency amounts
  - Includes automatic backup, transaction safety, dry-run mode
  - Successfully migrated 2,188 trades and 26,975 portfolio rows

### Changed
- **db_validation_functions.r**: Updated type validation for position columns
  - Removed `Pos` from integer columns in `validate_trades_data()` (line 139)
  - Removed `pos` from integer columns in `validate_portfolio_data()` (line 101)
  - Added numeric conversion for both fields using `standardize_numeric()`
  - Preserves decimal precision for fractional positions

### Fixed
- **CASH position decimal truncation**: Fixed INTEGER type truncating currency amounts
  - Before: 12154.52 EUR → 12154 EUR (lost $0.52)
  - After: Full precision preserved (12154.52 EUR)
  - Impact: Accurate P&L for CASH positions with fractional amounts
  - Future: Supports fractional shares (e.g., 0.40 shares × $1000 = $400)

## [5.7.47] - 2025-11-19

### Added
- **convert_to_chf_date()**: Added defensive type checks before currency conversion multiplication
  - Validates `result_df$amount` is numeric before multiplication (R/currency.R:730-733)
  - Validates `result_df$chf_value` is numeric before multiplication (R/currency.R:734-737)
  - Logs detailed error with class and sample values if non-numeric types detected
  - Helps diagnose edge cases where database queries return unexpected data types
  - Prevents cryptic "argument non numérique pour un opérateur binaire" errors

## [5.7.46] - 2025-11-18

### Added
- **getCurrencyExposure()**: New function to analyze currency exposure across portfolio
  - Tracks cash positions by currency (CHF, USD, EUR) with origin date Sep 9, 2025
  - Aggregates market value and unrealized PnL by currency
  - Converts all values to base currency for consistent reporting
  - Returns: Currency, CashPosition, MarketValue, UnrealizedPnL (both raw and base), TotalExposure, PercentOfPortfolio
  - Supports both current snapshot and historical date queries
  - Location: R/account.R lines 1049-1232

### Changed
- **getGonet()**: Enhanced to prompt user for cash positions (CHF, USD, EUR)
  - Displays previously stored values as defaults
  - Returns cash data as list for consumption by getAccountGonet
  - Backward compatible: returns NULL if user cancels prompts
  - Location: R/account.R lines 626-680

- **getAccountGonet()**: Modified to accept and store cash positions
  - New parameter: `gonet_cash` (list with cash_chf, cash_usd, cash_eur)
  - Stores individual currency balances in account table (CashBalanceCHF/USD/EUR)
  - Backward compatible: falls back to legacy hard-coded values if gonet_cash is NULL
  - Location: R/account.R lines 714-815

### Fixed
- **getCurrencyExposure()**: Fixed historical date query summing multiple snapshots
  - Bug: WHERE date = X with ORDER BY selected all records for that date, then summed them
  - Symptom: Currency exposure showed inflated values (e.g., 403K instead of 45K CHF)
  - Fix: Changed to WHERE (date, heure) = (SELECT... LIMIT 1) to get only latest snapshot
  - Impact: Currency exposure now displays correct values for historical dates
  - Location: R/account.R lines 1131-1141

### Infrastructure
- **build_package.R**: Added `prompt = FALSE` parameter to renv::install() calls
  - Enables non-interactive package deployment
  - Applied to lines 207 and 223
  - Updated for automated CI/CD workflows

## [Unreleased]

### Housekeeping
- **Environment sync**: Updated renv from 1.0.11 to 1.1.5 (configured version)
- **Package dependencies**: Restored all packages to lockfile versions
  - Upgraded: shiny (1.10.0 → 1.11.1), data.table (1.17.2 → 1.17.6), Rcpp (1.1.0 → 1.0.13-1)
  - Maintained lockfile stability for: reticulate, roxygen2, usethis, purrr, logger, Matrix, cli, rlang
- **Test coverage analysis**: Documented current coverage at 41.14%
  - Critical gaps identified: ibkr.R (6.76%), account.R (13.02%), volatility.R (0.00%)
  - Well-tested areas: journal.R (96.83%), currency.R (90.28%), historical_options.R (88.00%)
  - Coverage improvement roadmap added to TODO.md
  - All existing tests passing: 346 PASS, 0 FAIL, 10 WARN, 16 SKIP
- **Documentation**: Test coverage analysis added to `docs/TODO.md` (section 8)

## [5.7.40] - 2025-11-07

### Changed
- **contract.py**: Updated Python contract implementation
- **currency.R**: Modified currency functions
- **zzz.R**: Added initialization functions (14 new lines)

### Tests
- **test-json-serialization.R**: Added comprehensive JSON serialization test suite (310 new lines)
- **test-prices.R**: Updated price tests

## [5.7.39] - 2025-11-04

### Changed
- **contract.py**: Updated Python contract implementation
- **test_contract.py**: Updated Python contract tests
- **ibkr.R**: Refactored IBKR functions (106 lines modified)
- **account.R**: Modified account functions
- **prices.R**: Updated price functions

### Tests
- **test-prices.R**: Updated price test suite

## [5.7.38] - 2025-10-31

### Added
- **on_demand_historical.py**: Enhanced historical data retrieval
  - New `_fetch_underlying_stock_data()` function for fetching underlying stock data
  - New merge function to combine option and stock data for accurate IV calculations
  - 264 lines added to on_demand_historical.py
  - 356 lines added to test_historical_option.py

## [5.7.37] - 2025-10-29

### Changed
- **prices.R**: Updated price functions (16 lines modified)

### Tests
- **test-prices.R**: Enhanced price test suite (26 lines added)

## [5.7.36] - 2025-10-29

### Changed
- **ibkr.R**: Added new IBKR functions (6 lines added)
- **prices.R**: Refactored price functions (27 lines modified)

### Tests
- **test-prices.R**: Added comprehensive price tests (49 new lines)

## [5.7.35] - 2025-10-27

### Fixed
- **account.R**: Fixed logical NA returns in twr() function causing type errors
  - Issue: twr() returns bare `NA` (logical) on error conditions instead of numeric NA
  - Root cause: Two error paths used `return(NA)` creating logical type
  - Error: "pas de méthode pour 'round_any' applicable pour un objet de classe 'logical'" when formatting TimeWeightedReturn
  - Solution: Use `return(NA_real_)` instead of `return(NA)` in error paths
  - Impact: Fixes TimeWeightedReturn display errors in accountMetrics2 table
  - Lines fixed: 273 (duplicate dates check), 306 (vector length mismatch check)

## [5.7.34] - 2025-10-27

### Fixed
- **account.R**: Fixed logical NA error in Gonet account metrics display
  - Issue: `FullInitMarginReq`, `FullMaintMarginReq`, `FullExcessLiquidity` stored as logical NA instead of numeric NA
  - Root cause: Using bare `NA` creates logical type, causing `label_percent()` to fail when formatting margin ratios
  - Error: "pas de méthode pour 'round_any' applicable pour un objet de classe 'logical'"
  - Solution: Use `NA_real_` instead of `NA` for numeric fields in getAccountGonet()
  - Impact: Fixes accountMetrics2 rendering error in Account tab for Gonet portfolios

## [5.7.32] - 2025-10-27

### Fixed
- **account.R**: Fixed Gonet account display showing NA values due to NULL Currency field
  - Issue: Gonet positions in multiple currencies (EUR, USD, CHF) were summed incorrectly, Currency field was NULL in database
  - Root cause: `getAccountGonet()` converted positions to USD but never set Currency field when writing to Account table
  - Database impact: `readAccount()` multiplies all values by NULL conversion rate → all displayed values became NA
  - Solution: Use `convert_to_base_date()` to convert all Gonet positions to base currency (CHF) before summing
  - Added Currency field to account.var list and set to base currency in account record
  - Updated cash balance calculation to also use base currency conversion
  - Financial correctness: Multi-currency positions properly converted to single base currency before aggregation
  - Impact: Fixes Account tab showing NA for all Gonet metrics (NetLiquidation, UnrealizedPnL, etc.)

## [5.7.31] - 2025-10-27

### Fixed
- **account.R**: Fixed Time-Weighted Return (TWR) calculation vector length mismatch
  - Issue: `twr()` function received 18 NLV values but 85 cash flow values causing "Cash flows number of elements different from Portfolio values" error
  - Root cause: `zoo::na.approx()` with default `na.rm = TRUE` removes leading/trailing NAs from result, shortening vector from 85 to 18 elements
  - Solution: Use `na.rm = FALSE` to keep NAs in result, then apply `zoo::na.fill(fill = "extend")` to fill leading/trailing NAs with edge values
  - Two-step approach: (1) interpolate gaps between known values, (2) extend first/last values to date range boundaries
  - Financial correctness: If portfolio value was $100k on first data point, it was $100k on all prior days in window
  - Impact: Fixes Account tab display errors for Gonet portfolios with sparse data points (18 records over 85-day window)
  - Code clarity: Explicit steps make intent obvious without relying on undocumented parameter pass-through

## [5.7.27] - 2025-10-23

### Fixed
- **on_demand_historical.py**: Fixed intraday data retrieval issue
  - `what_to_show` parameter was hardcoded to "TRADES" when retrieving stored data
  - Data was collected successfully (e.g., 9 records for BNP) but retrieval failed
  - Updated `_fetch_single_contract_data()` to accept and use `what_to_show` parameter
  - Now properly retrieves data with matching `data_type` and `what_to_show` values
  - Fixes: "No data available from IBKR" warning despite successful data collection

## [5.7.26] - 2025-10-22

### Fixed
- **historical_options.R**: Fixed "Expected a python object, received a list" error
  - Reticulate auto-converts pandas DataFrame to R data.frame before we can check it
  - Added proper handling for both cases: already-converted data.frame or still-Python object
  - Removed `py_is_null_xptr()` check which was causing issues with auto-converted objects

## [5.7.25] - 2025-10-22

### Changed
- Automated build with test updates

## [5.7.24] - 2025-10-22

### Fixed
- **on_demand_historical.py**: Fixed `_fetch_single_contract_data()` return logic
  - Removed incorrect results checking that caused "Error fetching single contract data: 0"
  - Data is successfully collected (257 records) but was returning NULL due to wrong validation
  - Now properly returns fetched data after `collect_data_for_active_contracts()` completes
  - Added proper validation on fetched_data instead of results
  - Issue: Data stored successfully but function returned None due to `results[0].get('success')` check

## [5.7.23] - 2025-10-22

### Added
- **historical_options.R**: New R wrapper functions for on-demand historical option data
  - `get_or_retrieve_option_historical()`: Retrieve historical option data with automatic on-demand fetching
  - `clear_on_demand_cache()`: Clean up cached on-demand option data files
  - Supports historical, intraday, and combined data types
  - Auto-caching and force refresh capabilities
  - Input validation and error handling
  - Returns tibble or NULL

### Changed
- **DESCRIPTION**: Added `tibble` to Imports for historical options functions

### Tests
- **test-historical-options.R**: Comprehensive test suite for historical options wrapper functions
  - Parameter validation tests (required params, right values, data_type)
  - Return type validation
  - Cache management function tests

## [5.7.22] - 2025-10-22

### Fixed
- **on_demand_historical.py**: Completed `_fetch_single_contract_data()` function to properly use `HistoricalDataManager` pattern
  - Now calls `collect_data_for_active_contracts()` to fetch and store data from IBKR
  - Retrieves stored data with `get_option_historical_data()`
  - Returns fetched data correctly instead of returning None
  - Removed dependency on non-existent `ibkr_integration` module

## [5.7.21] - 2025-10-22

### Fixed
- **historical_options.R**: Fixed Python namespace issue in R wrapper functions
  - Changed from `tdata_py$on_demand_historical$function()` to `tdata_py$function()`
  - Functions are imported at top level in Python `__init__.py`, not as submodule

## [5.7.20] - 2025-10-22

### Added
- **historical_options.R**: New R wrapper functions for on-demand historical option data
  - `get_or_retrieve_option_historical()`: Retrieve historical option data with automatic on-demand fetching
  - `clear_on_demand_cache()`: Clean up cached on-demand option data files
  - Supports historical, intraday, and combined data types
  - Auto-caching and force refresh capabilities
  - Input validation and error handling
  - Returns tibble or NULL

### Changed
- **DESCRIPTION**: Added `tibble` to Imports for historical options functions

### Tests
- **test-historical-options.R**: Comprehensive test suite for historical options wrapper functions
  - Parameter validation tests (required params, right values, data_type)
  - Return type validation
  - Cache management function tests
