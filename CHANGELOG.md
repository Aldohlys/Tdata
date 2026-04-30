# Changelog - Tdata

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [5.10.11] - 2026-04-30

### Fixed
- **contract.py** (`getOptValue`): defensive NaN guard for the `strikes` parameter. Filters out NaN entries with a WARNING log before issuing the TWS request; if no valid strikes remain after filtering, returns `None` instead of submitting a malformed request. Inserted right after `requested_strikes = [float(s) for s in strikes]` (around line 395).
  - **Problem**: when an underlying is missing from the `Tickers` table, the upstream strike-picker can return `[nan, nan, nan, nan]` for the term-structure IV calls (15d/90d/180d). Sending NaN strikes to TWS triggers `Error 320 — Unable to parse field: 'Strike' for input string: 'nan'`, which is immediately followed by `Peer closed connection` (`asyncio ConnectionResetError WinError 10054`). The dropped socket then poisons any subsequent request in the same batch.
  - **Observed on 2026-04-28**: `getVolatilityMetrics("IBIT")` returned `iv30`/`ivp`/`rv30`/`rvp`/`vrp` populated but `iv15`/`iv90`/`iv180` all `NA`, with three Error 320 + peer-closed cycles in the log.
  - **Solution**: at the TWS boundary, drop NaN strikes; if everything was NaN, log an ERROR and return `None`. The upstream missing-Tickers-row issue is fixed separately by the new Tickers CRUD utility (`Tuser/ticker/app.R`); this guard ensures any future caller that produces NaN cannot tear down the connection.
  - **Pattern**: matches `feedback_tws_drops_connection_on_malformed.md` — never let malformed values reach TWS.

### Changed
- **contract.py** (`getOptValue`) logging polish: `getOptValue data {...}` info line now fires only when TWS is actually called, and includes a `cache` field (`cache miss` / `partial hit (N/M cached)` / `force_refresh`). Full cache hits log `Quote cache full hit: N strikes for SYM EXPIRY RIGHT (TTL X min)` instead. Net: log now visually distinguishes IO from cache-served calls — previously every call printed `getOptValue data {...}` regardless, making it look like nothing was cached.

## [5.10.9] - 2026-04-27

### Added
- **parquet_storage.py** (`ParquetQuotesStorage`): new fourth parquet layer for live option-quote caching, alongside the existing chains/strikes/historical_data layers. Layout: `quotes/<sym>/<trading_class>/<expiration>_<right>_quotes.parquet` with one row per strike (`value, bid, ask, last, spread, impliedvol, delta, cached_timestamp`). Per-row TTL via `cached_timestamp`; file mtime is used only by the maintenance pass for already-expired contracts.
  - `load_fresh(sym, trading_class, expiration, right, strikes, ttl_minutes)` returns rows that are both requested AND within the TTL window — supports strike-level partial hits
  - `upsert(...)` overwrites by strike but preserves neighbouring strikes in the same file so adjacent callers can still hit cache
  - `check_and_remove_expired(...)` deletes cache files whose contract expiration is in the past
  - Helpers: `get_file_age_minutes()` and `get_quotes_ttl_minutes()` (config key `quotes_ttl_minutes`, default 30)
- **parquet_storage.py** (`ParquetMaintenanceManager`): registered `quotes_dir` and added a quotes-cleanup branch to `cleanup_expired_options()`. Total file count reflects chains + strikes + quotes deletions
- **contract.py** (`getOptValue`): two new kwargs
  - `force_refresh: bool = False` — bypass the read side of the cache and pull fresh values from TWS. Use at order-submit time (ROrder) where staleness is dangerous. Fresh quotes are still written back so the next non-forced caller benefits — ROrder warms the cache for the scanner instead of starving it
  - `cache_ttl_minutes: int = None` — per-call freshness override; falls through to `quotes_ttl_minutes` config
  - Cache lookup happens after `tradingClass` resolution and before `safe_ib_connect()`. Full hits skip the TWS round-trip entirely. Partial hits qualify+fetch only the missing strikes; results are merged with cached rows preserving the caller's requested strike order
- **R/volatility.R** (`getVolMetrics`, `getIV_DTE`, `getOptionPrices`): added `force_refresh = FALSE` parameter, plumbed through to `getOptValue`
- **R/ibkr.R** (`getOptIBKRPrice`, `getOptMarketData`): added `force_refresh = FALSE` parameter — these are the ROrder/Ligne entry points where the flag should be set `TRUE` at submit time
- **test/test_quote_cache.py** (new): 18 pytest cases covering storage round-trip, TTL filtering, upsert semantics, expired-contract cleanup, corrupt-file handling, and `getOptValue` cache decisions (full hit / partial hit / `force_refresh` / TTL override / scalar-strike normalisation) with `safe_ib_connect` mocked. Runs in ~3.5s, no TWS required

### Performance context
- Motivation: `getVolMetrics` calls `getIV_DTE(15/90/180)` independently — each one re-fetches strikes + option prices for its bracketing expiries. Sparse chains (e.g. DBA on 2026-04-27) collapse all three DTE targets to the same expiry pair but still re-pull the chain three times. Cross-symbol redundancy is even larger during full scanner sweeps. The new cache eliminates both within-symbol and cross-process redundancy because parquet survives R sessions and processes (scanner via Task Scheduler ↔ interactive `/analyze` ↔ Python scripts all share state)
- Same TTL/eviction pattern as the existing `ParquetChainsStorage` / `ParquetStrikesStorage`; minutes-grained TTL is the only new convention

### ROrder usage
- Pass `force_refresh = TRUE` at submit-time leg pricing (`getOptIBKRPrice(..., force_refresh = TRUE)`, `getOptMarketData(..., force_refresh = TRUE)`). Trade-design / what-if iterations can keep the default `FALSE` to stay fast — only the order-ticket call needs guaranteed freshness

## [5.10.5] - 2026-04-21

### Fixed
- **account.py** (`getIBKRData`, portfolio subscription path): bounded `ib.reqAccountUpdates(account)` with `asyncio.wait_for(..., timeout=60)` via `reqAccountUpdatesAsync`. Without it, a stalled TWS socket left the sub-account subscription pending forever and the scheduled `daily_portfolio_update.R` / `RGetIBKR.R` hung indefinitely (observed 2026-04-21 on `U1804173`: accountSummary returned fine, then `reqAccountUpdates` blocked with no further progress — R/reticulate can't interrupt an asyncio call from the outside). On timeout: logs a warning, best-effort `cancelAccountUpdates`, disconnects, and returns 0 so the caller's loop can move on to the next account. Matches the 5.10.4 pattern already applied to `_fetch_historical_data`.
- Added `import asyncio` to `account.py`.

## [5.10.4] - 2026-04-21

### Fixed
- **impliedvol.py** (`_fetch_historical_data`): Added `timeout=60` (seconds) to the `ib.reqHistoricalData` call. Without it, a lost-in-flight IBKR request after a socket drop could block the asyncio event loop indefinitely — observed on 2026-04-20 where `update_missing_iv_safe.R` hung for 5+ hours on a single ticker before being killed manually. With the timeout, the call now returns cleanly (bars = None / empty) and the existing downstream NaN-handling (`_process_volatility_data`, `_process_price_data`) passes through a NaN result so the pipeline can move on to the next ticker. Matches the existing pattern already used in `historical_option.py._collect_bars`. Closes TODO #51
- Error path in `_fetch_historical_data` now uses `logger.warning` (not `print`) and includes the symbol, so timeouts are visible in structured logs.

## [5.10.3] - 2026-04-21

### Added
- **volatility.R** (`getVolMetrics`): 5 new columns persisted to Prices
  - `iv15`, `iv90` — short-dated and mid-dated IV from option chains via `getIV_DTE` (variance-interpolated). Completes the term structure iv15/iv30/iv90/iv180
  - `vrp` — Volatility Risk Premium = `log(iv30/rv30) * 100` (log-ratio form, not additive). Positive = options priced richer than recent realized; negative = cheap vs realized
  - `ivr` — IV Rank over 1y Prices history = `100*(iv30-min)/(max-min)`. Requires >=5 historical rows; logs warm-up warning when <20
  - `ivp_2y` — IV Percentile over 2y Prices history (density-based, complements IBKR's 1y `ivp`). Requires >=10 historical rows
- **trend.R** (new): `isTrendContinuation(sym, bench='SPY', lookback_days=400)` — diagnostic function returning a list with stage-2 flags, HH/HL counts, RSI bucket, pullback state, and relative-strength vs benchmark. Designed as a gate for the swing scanner to narrow candidates to "confirmed trend + pullback active" setups before the expensive deep vol pull
  - MA stack check uses 5% inter-MA tolerance (qualitative — converging stacks in base-to-trend transitions can have SMAs within a couple of percent)
  - RSI in-trend band [40, 75] — upper widened beyond 70 to accommodate momentum names with pullback-in-progress
  - Stage2 criteria: price>SMA50, SMA stack (50/150/200), SMA200 rising, price within 1-25% of 52wh
  - Pullback detection: price within 5% of SMA20, OR RSI recently retraced from >65 to 40-55

### Schema migration
- Prices table: `iv15`, `iv90`, `vrp`, `ivr`, `ivp_2y` columns added (all REAL, NULL-able). Migration script: `scripts/migrate_prices_vol_metrics.R` (idempotent)

### Context
- Extensions support the "asymmetric trade" scanner design: filter universe via `isTrendContinuation`, then pull rich vol metrics only on survivors. IVR and IVP_2y answer different questions (range-position vs density-position) and especially diverge when the IV history has a single outlier spike or floor — both kept so downstream logic can read them independently
- Based on empirical review of BOT trade history (108 trades, Apr 2023 - Apr 2026): 30-60 DTE band produced best per-trade economics; filter design favors "2nd-3rd inning" trend-continuation over first-inning breakout detection

## [5.10.2] - 2026-04-21

### Fixed
- **volatility.R** (`getVolMetrics`, lines ~373-413): Skip option-chain fallback when spot price is NaN
  - Problem: when IBKR aggregate path returns NaN for both iv30 and price, the fallback called `getIV_DTE(sym, currency, NaN, ...)`. Strike selection via `Tbasics::get_nearest_values(strikes, NaN)` produced NaN strikes, which were shipped to IBKR and triggered `Error 320: Error reading request. Unable to parse field: 'Strike'` followed by a connection drop cascading to subsequent tickers in the batch
  - Observed 2026-04-20 on KGC during swing scanner — iv180 silently NA, downstream "NO DATA" cells
  - Fix: guard both iv30 fallback (line 373) and iv180 computation (line 405) with `is.finite(metrics$price)`. When price is not finite, skip the option-chain call and emit a `log_warn` instead. Preserves happy path; no API change
  - Closes TODO #49



### Added
- **earnings.R** (new): `getNextEarningsDate()`, `updateEarnings()`, `updateStaleEarnings()` — populate `Tickers.NextEarnings` (YYYYMMDD) via yfinance. Uses `YahooName` for correct resolution of European tickers (`.SW`, `.PA`, etc.)
- **earnings_utils.py** (new, `inst/python/tdata_py/`): yfinance wrapper with two-level fallback (`Ticker.calendar` → `get_earnings_dates(limit=8)`). Robust against pandas NaN in `ticker_db` lookups.
- **requirements.txt**: added `yfinance>=0.2.40` (installed via `reticulate::py_install` into the `r-reticulate` conda env).
- **Staleness policy** in `updateStaleEarnings()`:
  - NextEarnings is past → always retry (date rolled over)
  - NextEarnings is NULL → retry only if `EarningsLastUpdate` is older than 7 days. Prevents daily re-hitting for ETFs and non-US tickers with broken YahooName.

### Notes
- Original design target was IBKR Wall Street Horizon via `ib.getWshEventData`, but WSH API access requires a News Feed entitlement beyond the calendar subscription (Error 10276). Switched to yfinance which is free and covers both US + European names.
- Schema migration lives at `scripts/migrate_earnings_schema.R` (adds `NextEarnings` and `EarningsLastUpdate` TEXT columns to Tickers).

## [5.9.21] - 2026-04-20

### Fixed
- **account.py** (`getIBKRData`, cash extraction): Master-level cash wrongly attributed to sub-accounts
  - Previously used `ib.accountSummary()` filtered by `account='All'` for per-currency cash breakdown
  - Problem: 'All' is the master-level aggregate (sum across all sub-accounts) — this cash was then copied into each sub-account's portfolio table as CASH rows
  - Example: U25343478 ended up with phantom USD 13,986.45, JPY 439,109.75, etc. (actually held in master U1804173). Currency Exposure view showed 38.6% USD for U25343478, which holds zero USD positions
  - Fix: use `ib.accountValues(account)` filtered by `tag='CashBalance'` and `currency != 'BASE'`. Requires `reqAccountUpdates(account=...)` to have been called first (already done before portfolio retrieval). Returns true per-sub-account cash breakdown (verified: U25343478 reports no USD row)
  - Also reordered `reqAccountUpdates` to happen BEFORE the cash extraction (was after)

## [5.9.16] - 2026-04-20

### Fixed
- **account.py** (`getIBKRData`, lines 361-380): Mixed-currency sum for per-account StockMarketValue / OptionMarketValue / UnrealizedPnL in sub-accounts
  - Previous code did blind `df['marketValue'].sum()` across all positions regardless of `currency` field
  - Produced bogus values for sub-accounts holding JPY/EUR/GBP positions (e.g. U25343478 stored StockMV=1,233,246 which was a raw JPY+EUR+GBP sum, not CHF)
  - Fix: added `_get_chf_rates()` helper that reads latest rates from `ConvertToCHF` table; portfolio marketValue/unrealizedPNL are now multiplied by the per-position rate before summing
  - Also casts StockMarketValue/OptionMarketValue/UnrealizedPnL columns to float first to silence the pandas FutureWarning about int64 dtype assignment
- **currency.R** (`getLastCHFValue`, lines 254-273): Wrong cross-rate formula for GBP/CHF
  - Comment claimed "YahooName is USDXXX=X (foreign per USD)" and applied `1 / (ticker × CHFUSD)`
  - But GBP's YahooName is `GBPUSD=X` (direct quote, USD per 1 GBP), not inverse
  - Buggy path produced rate 0.577806 instead of ~1.06 on 2026-04-20
  - Fix: use `DirectConversion` flag to pick formula — Yes → `XXXUSD / CHFUSD`, No → `1 / (USDXXX × CHFUSD)`
- **account.R** (`getIBKRActiveCurrencyValues`, lines 672-677): USD/CHF rate never refreshed
  - SQL at line 661 excluded USD from the active-currency loop (`NOT IN ('USD', 'CHF')`)
  - USD has no IBKRPair so it can't use the main loop, but Yahoo path (CHFUSD=X) handles it
  - USD/CHF rate had been stale since 2025-11-12 (159 days) on this system
  - Fix: explicit `getLastCHFValue("USD")` call after the main Yahoo loop for USD-active configurations

## [5.9.15] - 2026-04-18

### Fixed
- **account.py**: Filter out ghost positions (position=0) from IBKR portfolio data in `getIBKRData()`
  - Ghost positions appear after internal transfers between sub-accounts, polluting portfolio with zero-quantity rows
  - Replaced `ib.reqAccountUpdates(account='')` cancel call with list comprehension filtering `p.position != 0`

## [5.9.13] - 2026-04-17

### Added
- **account.R**: `getAccountChoices(type)` — centralized account list from config.yml
  - Types: "all" (all accounts), "ibkr" (IBKR accounts only), "trade" (tradeable accounts)
  - Replaces hardcoded account lists in 12+ UI dropdown files
- **account.R**: `getIBKR(account)` now accepts optional account parameter for multi-account support
- **account.py**: `getIBKRData(account)` — filters accountSummary and portfolio by sub-account
  - Uses `reqAccountUpdates()` for sub-account portfolio subscription
  - Computes per-account StockMarketValue/OptionMarketValue/UnrealizedPnL from portfolio positions
  - Handles IBKR sub-account API where market value tags only exist under 'All'

### Changed
- **account.R**: `getAccountLive()` now aggregates U1804173 + U25343478 + Gonet (was U1804173 + Gonet)
- **trades.R**: `getActiveTrades()`, `getClosedTrades()`, `getTradeNr()` — removed Live/Simu switch() mappings, now use account codes directly matching migrated Trades table
- **cash.R**: `getCashTradeForCurrency()` — removed Live/Simu switch() mapping

## [5.9.12] - 2026-03-27

### Fixed
- **account.R**: Filter out CASH positions from IBKR portfolio data before Instrument-based trade matching
  - CASH rows from `ib.portfolio()` joined on Instrument, which doesn't match FX trade conventions (e.g., trade has Instrument="CHF" but portfolio has symbol="USD")
  - CASH positions are now handled exclusively by `create_cash_portfolio_row()` via `getCashTradeForCurrency()`, which correctly matches on both Instrument and Currency fields

## [5.9.11] - 2026-03-25

### Added
- **ticker.R**: `syncTickersToScanner()` — auto-sync Tickers (IV=YES, Type=STK) into ScannerUniverse
  - Skips tickers above configurable `max_price` (default $500)
  - Auto-detects ETFs by naming patterns and sets Role accordingly
  - Supports `dry_run = TRUE` for preview without changes
  - Called automatically by swing scanner's `get_universe()` on startup

## [5.9.9] - 2026-03-23

### Fixed
- **prices.R**: `getStockPrice()` — handle symbols without IBKR tickers (e.g., Gonet-only `PM_15606539`)
  - Filter out symbols where `getYahooName()` returns NA before IBKR API call
  - DB fallback for symbols IBKR fails to return (e.g., SPX as STK type)
  - NA rows for completely missing symbols instead of crash
  - Previously crashed entire price fetch when non-IBKR symbols were in the list

## [5.9.7] - 2026-03-23

### Fixed
- **ticker.R**: `getYahooName()` — handle `NA` in `YahooName` field (not just empty string)
  - `PM_15606539` (precious metals) had `YahooName = NA` in Tickers table
  - `NA == ""` evaluates to `NA`, crashing `purrr::map_chr` with "In index: 17"
  - Gonet portfolio correlation plot failed with "Correlation data unavailable"
  - Added `is.na()` check before empty string comparison (line 406)

## [5.9.6] - 2026-03-23

### Fixed
- **volatility.R**: `fit_volatility_parabola()` — guard against all-NA IV data before calling `lm()`
  - When IBKR returns no option IVs (e.g., ABBN with no market data subscription), `lm.fit` crashed with "aucun cas ne contient autre chose que des valeurs manquantes"
  - Now returns `c(NA, NA, NA)` when fewer than 3 valid data points, propagating NA gracefully
- **ScannerUniverse DB**: DXY ticker corrected from `DXY=X` to `DX-Y.NYB` (correct Yahoo Finance symbol)
- **macro_context/analyze.R**: Updated all `DXY=X` references to `DX-Y.NYB` so DXY data fetches succeed

## [5.9.5] - 2026-03-17

### Added
- **ticker.R**: `getScannerUniverse()` — query ScannerUniverse DB table with filters (role, sector, active_only)
- **ticker.R**: `addScannerSymbol()` — insert new symbol into ScannerUniverse table
- **ticker.R**: `removeScannerSymbol()` — soft-delete (IsActive=0) symbol from ScannerUniverse
- New `ScannerUniverse` DB table replaces hardcoded stock lists in breakout analysis scripts
- Seeded with ~170 symbols (147 scanner stocks, 14 macro tickers, 9 sector ETFs) across 10 sectors

## [5.9.4] - 2026-03-17

### Fixed
- **prices.R**: `getLastSymPrice()` — filter NA values and use `with_ties = FALSE` in `slice_max()` to guarantee one row per symbol. Yahoo returns duplicate rows with NA Close/Adjusted, causing `getLastAdjustedPrice()` to return vectors of length > 1 and crashing Shiny `renderUI`

## [5.9.3] - 2026-03-17

### Changed
- **ibkr.R**: Renamed `getOptPrice` to `getOptIBKRPrice` to eliminate namespace masking with `Tbasics::getOptPrice` (Black-Scholes theoretical pricer)

## [5.9.2] - 2026-03-16

### Fixed
- **prices.R**: `getSymMetricIntervalDate()` — deduplicate `(date, ticker)` rows before `pivot_wider` to prevent list-columns when Yahoo returns duplicate data
  - Root cause of "Error retrieving data for In index: 1" in RPreTrade technical indicators (xts cannot handle list-columns)
  - Also eliminates "Values from `Adjusted` are not uniquely identified" warnings in correlation analysis

## [5.9.1] - 2026-03-16

### Fixed
- **historical_options.R**: All functions bypassed the lazy Python active binding by calling `reticulate::import("tdata_py")` directly, which failed because the Python path was never added to `sys.path`
  - Created `get_tdata_py()` helper that uses the package active binding (triggers `zzz.R` initialization)
  - Fixed 6 functions: `get_or_retrieve_option_historical`, `clear_on_demand_cache`, `qualify_contract`, `add_option_tracking`, `remove_option_tracking`, `update_tracked_options`, `list_tracked_options`
  - Symptom: "Python module 'tdata_py' not available" error when viewing historical option data in Tuser/routine

## [5.9.0] - 2026-03-10

### Changed
- **zzz.R**: Lazy Python initialization — `tdata_py` is now an active binding that defers Python/reticulate setup to first use
  - `library(Tdata)` load time reduced from ~26s to ~1.3s on the VM
  - Pure-R and DB functions (`greeksNet`, `readAccount`, etc.) no longer pay the Python startup cost
  - Fixes Shiny app startup timeouts (`routine`, `rreporting`) caused by 50s+ combined init time

## [5.8.39] - 2026-03-05

### Fixed
- **cash.R**: Extracted `getCashTradeForCurrency()` — query now searches `Instrument OR Currency` to find CASH trades for cross-currency pairs (e.g., Trade 687 CHF.JPY matched via Currency=JPY)
- **cash.R**: Added `resolve_cash_cost_basis()` — inverts Prix when trade matched on Currency (not Instrument), fixing 101M CHF unPnL bug for JPY CASH position
- **account.R**: Split portfolio-to-trade join — stocks match on `symbol == Ssjacent` (ticker), options/futures match on `Instrument`. Fixes mismatch where IBKR portfolio has ticker (e.g., "DSY") but Trades has company name (e.g., "DASSAULT SYSTEMES SE")

## [5.8.38] - 2026-03-02

### Fixed
- **config_reader.py**: `R_CONFIG_ACTIVE` was commented out and hardcoded to "default"
  - Python logging used Windows `log_dir` path on the VM, creating a literal `C:` directory
  - Restored `os.environ.get('R_CONFIG_ACTIVE', 'default')` to read the env var
  - Added `R_CONFIG_FILE` env var lookup as primary config path (before fallback search)
  - Enabled default+production config merging via `merge_dicts()` (was returning only one section)

## [5.8.37] - 2026-02-26

### Fixed
- **currency.R**: `getLastCHFValue()` Yahoo cross-rate formula was wrong for non-direct currencies (JPY, CAD, HKD)
  - Old formula `USDXXX / CHFUSD` gave nonsensical values (e.g., 133 for JPY instead of 0.006)
  - Fixed to `1 / (USDXXX * CHFUSD)` which correctly computes CHF per 1 foreign unit
  - Also ensured `CHFUSD=X` ticker is always fetched when cross-rate currencies are processed (was missing when USD not in request)
- **currency.R**: `getLastCHFValue()` rate precision increased from `round(chf_value, 4)` to `round(chf_value, 6)`
  - JPY rate ~0.005923 was stored as 0.0059 (only 2 sig figs), causing ~0.4% error on large JPY→CHF conversions
- **prices.R**: `getYahooData()` was discarding valid FX data for long date ranges
  - quantmod's benign "contains missing values" warning was caught by `tryCatch` warning handler, which discarded the entire valid dataset and generated a synthetic NA-filled frame
  - Fixed using `withCallingHandlers` to muffle the benign warning while preserving the data
  - Actual Yahoo error warnings still propagate correctly

## [Unreleased] - 2026-02-25

### Added
- **interest_rate_utils.R**: JPY interest rate retrieval via `get_jpy_rates()`
  - 1Y and 2Y yields from MOF Japan daily JGB CSV (https://www.mof.go.jp)
  - 3-month TIBOR from FRED (`IR3TIB01JPM156N`)
  - Short-term tenors approximated from TIBOR and JGB yields
  - Hardcoded fallback values if both sources fail
- **interest_rate_utils.R**: `getInterestRates()` now includes JPY alongside USD, EUR, CHF

### Changed
- **IB_connection.py**: IBKR API port is now configurable via `config.yml` (`ibkr.api_port`)
  - `safe_ib_connect()` reads port from config instead of hardcoding 7496
  - Default: 7496 (live TWS on Windows), production: 4002 (paper Gateway on VM)

### Added
- **parquet_storage.py**: Cache TTL (time-to-live) staleness detection
  - `get_file_age_days(file_path)` — returns file age in days from `st_mtime`
  - `get_cache_ttl_days()` — reads `cache_ttl_days` from config (default 7)
  - `ParquetChainsStorage.check_and_remove_stale_chains(symbol)` — deletes chain parquets older than TTL
  - `ParquetStrikesStorage.check_and_remove_stale_strikes(symbol, tc, exp)` — deletes expired or stale strike caches
- **chains_manager.py**: Stale-warning accumulator and automatic TTL enforcement
  - `get_stale_warnings()` / `clear_stale_warnings()` — module-level warning accumulator for R callers
  - `getChains()` now evicts stale chain files before loading cache (triggers IBKR re-fetch)
  - `getOptionStrikes()` now evicts expired/stale strike files before loading cache
- **__init__.py**: Export `get_stale_warnings`, `clear_stale_warnings`, `get_file_age_days`, `get_cache_ttl_days`

### Changed
- **_core.py**: `load_config()` now honors `R_CONFIG_ACTIVE` env var (production on VM, default on Windows)

## [5.8.33] - 2026-02-20

### Fixed
- **volatility.R**: Fix `prepare_har_data()` crash on price data with NAs
  - `TTR::volatility` fails with "not enough non-NA values" when Yahoo returns sparse data (e.g. UBSG.SW)
  - Now removes NA rows from price data before row count check and TTR call
- **contract.py**: Fix `getStrikesfromExpDate()` ignoring `force_refresh` parameter (line 602)
  - Was hardcoded to `force_refresh=False`, now passes through the caller's value
  - Affects `getIV_DTE` / `getIBKRMetrics` volatility computations using stale chain data
- **focused_historical.py**: Remove `int()` cast on `bar.volume` (line 134)
  - Prepares for IBKR TWS 10.44 (Feb 23, 2026): LAST_SIZE tick type changes from Integer to Decimal
  - `int()` would truncate fractional trade sizes; now passes through as-is

### Added
- **position_sizer.py**: New generalized Monte Carlo position sizer engine
  - Accepts arbitrary option combos (strangle, vertical, diagonal, iron condor, butterfly, etc.)
  - Per-leg DTE support for multi-expiration strategies (diagonals, calendars)
  - 3-scenario simulation: HV base, HV regime-adjusted (recommended), IV conservative
  - ES/VaR risk budgeting at 95% and 99% confidence levels
  - IVP/HVP regime analysis with sizing multiplier (IDEAL/FAVORABLE/NEUTRE/DEFAVORABLE/DANGEREUX)
  - Student-t fat tails (df=5), full BS with r and div support
- **position_sizer.R**: New `sizePosition()` exported R wrapper function
  - Converts R data.frame legs to Python list-of-dicts format
  - Returns nested list with lots, regime, scenarios, recommendation
- **spread.py**: Add `force_refresh` parameter to `compute_spread_risk_reward()`
  - Passes through to `getOptionStrikes()` to refresh stale chain cache
  - Fixes issue where old cached chains had only $5-increment strikes, missing $1-increment strikes added by IBKR for near-the-money expirations

## [5.8.26] - 2026-02-14

### Fixed
- **currency.R**: Fix vector length mismatch in `c_to_chf()` and `c_to_usd()`
  - `getStoredCHFValue()`/`getStoredUSDValue()` return one row per unique currency (GROUP BY), but input vectors can have duplicates
  - Direct multiplication caused recycling warning when lengths didn't match
  - Fix: use `left_join()` by currency to align rates 1:1 with input rows before multiplication
- **trades.R**: Suppress spurious "6 failed to parse" warning in `getTradeDates()`
  - `dplyr::if_else()` evaluates both branches for all rows, so `lubridate::dmy()` ran on NA/empty `Exp.Date` values even though their result was discarded
  - Wrapped FALSE branch in `suppressWarnings()` — NA handling was already correct

## [5.8.25] - 2026-02-13

### Added
- **contract.py**: New `qualify_contract()` function to resolve FOP tradingClass via IBKR
  - Auto-detects OPT vs FOP from ticker database
  - Uses `reqContractDetails` to handle ambiguous matches (e.g., SOFR3 mid-curve options)
  - Filters by TradingClass from Tickers table when multiple matches found
  - Returns dict with conId, tradingClass, exchange, etc.
- **historical_options.R**: R wrapper `qualify_contract()` with `ib` connection reuse parameter
- **historical_option.py**: `_get_incremental_duration()` computes gap-based duration
  - Replaces hardcoded "1 W" / "2 D" incremental durations
  - Calculates days since last stored data point to fill gaps (e.g., after vacation)

### Fixed
- **historical_option.py**: `list_historical_config()` no longer truncates `active_contract_details`
  - Was limited to `max_contracts=10` by default, causing sync loop to miss contracts
  - `return_dict` path now returns ALL active contracts
- **update_historical_options.R**: Fix FOP tracking with correct tradingClass
  - Futures options (CHF, SOFR3) now qualify via IBKR instead of static Tickers lookup
  - Normalize portfolio expdate (YYYY-MM-DD) to config format (YYYYMMDD) for key comparison
  - Reuse single IBKR connection across all qualify calls
  - Add retry mechanism (3 attempts, 30s delay) for update phase

## [5.8.23] - 2026-02-06

### Fixed
- **historical_options.R**: Fix `$` partial matching bug in `update_tracked_options()`
  - `result$error` was matching Python's `result$errors` (plural, empty list) due to R partial matching
  - Caused false "Update failed:" message even when data collection succeeded
  - Changed to exact matching with `result[["error"]]`

## [5.8.22] - 2026-02-06

### Fixed
- **historical_option.py**: Add `exchange` and `active` fields to `list_historical_config()` return dict
  - `active_contract_details` was missing these fields, causing tracking manager to show defaults

## [5.8.21] - 2026-02-06

### Added
- **historical_options.R**: R wrappers for option contract tracking
  - `add_option_tracking()` - Add contract to daily incremental tracking
  - `remove_option_tracking()` - Remove contract from tracking
  - `update_tracked_options()` - Collect incremental data for all tracked contracts
  - `list_tracked_options()` - List tracked contracts and settings
  - All wrap existing Python functions (`add_historical_tracking`, `update_historical_data`, etc.)

## [5.8.20] - 2026-02-06

### Fixed
- **on_demand_historical.py**: Fetch underlying prices on cache path for IV calculation
  - Previously, underlying prices were only fetched on the fresh-fetch path (IBKR retrieval)
  - When loading from cache (common path), `underlying_price` column was missing
  - Now checks for missing `underlying_price` in cached data and fetches from IBKR if available
  - Gracefully handles IBKR unavailability (returns cached data without underlying prices)
  - Added `exchange` parameter to `_fetch_underlying_data()` as fallback when ticker_db info incomplete

## [5.8.18] - 2026-02-06

### Fixed
- **on_demand_historical.py**: Fixed underlying price retrieval for futures options
  - `_fetch_underlying_stock_data()` always created `Stock()` contract, failing for futures (e.g., 6SM6)
  - Renamed to `_fetch_underlying_data()`, now detects `FUT` type from ticker DB
  - Creates `Future()` contract using ConId (preferred) or localSymbol+expiration (backup)
  - Follows same pattern as `getValue()` in contract.py
- **focused_historical.py**: Fixed `update_watchlist_data()` contract creation for futures options
  - Was always using `Option()` (secType=OPT), now uses `Contract(secType=FOP)` for FUT underlyings
  - Same pattern as historical_option.py: looks up ticker type from DB

## [5.8.17] - 2026-02-06

### Changed
- **Migrate ib_insync to ib_async** (TODO #25): Replaced all `ib_insync` imports with `ib_async` across 18 Python files
  - `ib_insync` is unmaintained since author's passing in early 2024
  - `ib_async` is the actively maintained community fork (v2.1.0), drop-in replacement
  - Updated logger names (`ib_insync.wrapper` -> `ib_async.wrapper`) in fin_logger.py, chains_manager.py, historical_option.py
  - Updated requirements.txt: `ib_insync>=0.9.85` -> `ib_async>=2.1.0`

### Fixed
- **historical_option.py**: Fixed "No security definition" error for futures options (6SM6, 6EM6, etc.)
  - Was always creating `Option()` contract (secType=OPT), now uses `Contract(secType=FOP)` for FUT underlyings
  - Same pattern as `getOptValue()` in contract.py: looks up ticker type from DB to determine OPT vs FOP

## [5.8.15] - 2026-02-05

### Fixed
- **historical_option.py**: Recalculate spread when it's 0 but should have a value
  - Backfill mask now triggers on `spread.isna() | (spread == 0)`
  - Fixes legacy data where spread was incorrectly set to 0 using bid/ask instead of bid_low/ask_high

## [5.8.14] - 2026-02-05

### Fixed
- **historical_option.py**: Use ask_high - bid_low for spread calculation
  - IBKR BID_ASK bars often have bar.open == bar.close (time-weighted averages equal)
  - Changed spread formula: `spread = ask_high - bid_low` (max spread during bar)
  - Changed spread_pct formula: `spread_pct = spread / bid_low * 100`
  - This gives meaningful spread values instead of always 0

## [5.8.13] - 2026-02-05

### Fixed
- **historical_option.py**: Backfill spread columns for legacy BID_ASK data
  - When saving combined data, compute `spread` and `spread_pct` for any BID_ASK rows missing these columns
  - Fixes issue where Force Update didn't show spread for older cached data
  - Legacy rows with bid/ask but no spread now get spread computed on next save

## [5.8.12] - 2026-02-04

### Added
- **historical_option.py**: Add `spread` and `spread_pct` derived fields to BID_ASK records
  - `spread` = ask - bid (absolute spread in dollars)
  - `spread_pct` = (spread / bid) * 100 (spread as percentage of bid)
  - IBKR does not provide volume for BID_ASK bars, so spread metrics are the enhancement path (TODO #17)
  - Only computed for BID_ASK what_to_show type; existing parquet files unaffected (schema evolution)

## [5.8.11] - 2026-02-04

### Fixed
- **__init__.py**: Export `get_iv_percentile_levels` in package init
  - Function was defined in impliedvol.py but not imported in `__init__.py`
  - Caused `AttributeError: module 'tdata_py' has no attribute 'get_iv_percentile_levels'`

## [5.8.10] - 2026-02-04

### Added
- **impliedvol.py**: New `get_iv_percentile_levels()` function
  - Fetches 252-day OPTION_IMPLIED_VOLATILITY history from IBKR
  - Returns IV values at percentile breakpoints (p10, p25, p50, p75, p90) using numpy
  - Reuses existing `_create_contract()`, `_fetch_historical_data()`, `_calculate_days_covered()`
- **volatility.R**: New exported `getIVPercentileLevels()` R wrapper
  - Calls Python function and returns clean named list (current, p10-p90, days_covered)
  - Used by TODO #19 IV Percentile-Based Vega Risk Table

## [5.8.9] - 2026-02-03

### Changed
- **alert.R**: Made `asset` parameter optional in `addAlert()` (default `""`)
  - Supports non-asset alerts such as FOMC meetings or macro events

## [5.8.8] - 2026-02-03

### Added
- **alert.R**: New alert management database layer with 6 exported functions
  - `createAlertTable()` — creates Alerts table (id, Theme, Asset, AlertDate, Description, Active, CreatedAt)
  - `getAllAlerts(active_only)` — retrieves alerts sorted by date, with optional active filter
  - `addAlert(theme, asset, alert_date, description)` — validates theme and appends alert row
  - `removeAlert(id)` — permanently deletes an alert by id
  - `dismissAlert(id)` — sets Active=0 to hide alert without deleting
  - `getUpcomingAlerts(days_ahead)` — returns active alerts within a date range for email notifications
  - Themes: Earnings, Macro, Expiration, Technical

## [5.8.7] - 2026-01-29

### Added
- **ibkr.R**: New exported function `getOptMarketData()` for retrieving full option market data
  - Accepts single or vector of strikes (same expiration and right)
  - Returns data.frame with strike, value, bid, ask, last, mid, spread, impliedvol, delta
  - Normalizes right ("Put"→"P", "Call"→"C") and expiration formats
  - Computes mid price from bid/ask

### Changed
- **contract.py**: Added `last` price field to `getOptValue()` result dictionary
  - Extracts `ticker.last` with safe None/NaN handling
  - Backward compatible: only adds a column to the returned DataFrame

## [5.7.77] - 2026-01-13

### Fixed
- **on_demand_historical.py**: Fixed issue where option historical data returned None despite being successfully fetched
  - **Problem**: When IBKR only provides MIDPOINT or BID_ASK data (no TRADES), `_fetch_single_contract_data()` failed to return any data
  - **Root cause**: Function only tried the requested `what_to_show` type (default: TRADES), ignoring successfully saved data of other types
  - **Solution**: Added fallback logic to try alternative data types (MIDPOINT, BID_ASK) if the requested type is not available
  - **Impact**: Options with limited liquidity will now return available data instead of None

### Added
- **historical_option.py**: Added BID_ASK to `historical_what_to_show` default options
  - Updated `load_config()` default from `["TRADES", "MIDPOINT"]` to `["TRADES", "MIDPOINT", "BID_ASK"]`
  - Also updated `historical_config.json` to include BID_ASK
  - **Impact**: BID_ASK now available in RPreTrade dropdown for historical data type selection

## [5.7.72] - 2026-01-09

### Changed
- **fitHAR()**: Major enhancements for practical trading use
  - Direct multi-horizon forecasting: trains separate model for each forecast day (avoids compounding errors)
  - Blended forecasts (70% current vol + 30% HAR) for conservative, tradeable values
  - Returns `forecast` (blended), `forecast_raw` (pure HAR), and `forecast_error` (MAE in vol points)
  - Returns current realized volatility at multiple windows: `current_vol_n`, `current_vol_5d`, `current_vol_22d`
  - Parameterized volatility window (`n`) now properly flows through all calculations
  - MAE calculated directly on annualized volatility (not variance) for meaningful error bounds
- **plotHAR()**: Now returns interactive plotly chart with hover values, Y-axis in percentage format
- **evaluateHAR()**: Added comprehensive model evaluation with directional accuracy, spike detection metrics, and vol-of-vol regime indicator

## [5.7.70] - 2026-01-08

### Added
- **HAR-RV Volatility Model**: New functions for Heterogeneous Autoregressive Realized Volatility forecasting
  - **fitHAR()**: Fits HAR-RV model using daily, weekly (5-day), and monthly (22-day) RV components
    - Supports Yahoo Finance and IBKR data sources
    - IBKR source uses 15-minute bars aggregated to daily OHLCV
    - Multiple volatility calculation methods via TTR::volatility: close, garman.klass, parkinson, rogers.satchell, gk.yz, yang.zhang
    - Returns model, forecast, accuracy metrics (RMSE, MAE), and test/train data
  - **getHARForecast()**: Convenience function for quick next-day annualized volatility forecast
  - **plotHAR()**: Visualizes actual vs predicted volatility for test period
  - **get_historical_bars()** (Python): New function to retrieve OHLCV bars from IBKR TWS API
    - Configurable bar size (default: 15 mins) and duration
    - Aggregates intraday data to daily for HAR model input
- **Test file**: tests/testthat/test-har-volatility.R with comprehensive test coverage
- **TTR package dependency**: Added to DESCRIPTION Imports for volatility calculations

### Changed
- **prepare_har_data()**: Now uses TTR::volatility with n=5 rolling window instead of simple squared returns
- **get_har_price_data()**: Uses Tdata::getYahooData() instead of direct quantmod::getSymbols() for Yahoo source

## [5.7.66] - 2025-12-30

### Fixed
- **getTradeDates()**: Fixed "2 failed to parse" warning for CASH trades without expiration dates
  - **Problem**: Warning "2 failed to parse" in dplyr::mutate when processing trades with empty Exp.Date
  - **Root cause**: Condition checked for "NA" string and is.na() but not empty string ""
  - **Solution**: Added `| Exp.Date == ""` check before attempting date parsing (R/trades.R:473)
  - **Impact**: Eliminates parsing warnings when getTradeDates() processes CASH trades
  - **Affected trades**: CASH positions (TradeNr 660, 659) which have no expiration date

## [5.7.65] - 2025-12-30

### Fixed
- **getTradeDates()**: Added backward-compatible date columns to fix displaytradeUI errors
  - **Problem**: "Unknown column: exp_date/orig_date" errors in Tuser modules (displaytradeUI.R:151-152, displaysymUI.R:245-246, symf.R:234, strategief.R:23, portf.R:144)
  - **Root cause**: Function returns `exp_datetime`, `orig_datetime`, `last_datetime` (POSIXct) but calling code expects `exp_date`, `orig_date`, `last_date` (Date)
  - **Solution**: Added as.Date() conversions at end of function to provide both datetime and date columns (R/trades.R:523-530)
  - **Impact**: Fixes "time left" table in displaytradeUI and other portfolio displays
  - **Backward compatible**: Existing code using *_datetime columns unaffected

## [5.7.64] - 2025-12-30

### Fixed
- **create_cash_portfolio_row()**: Changed Statut filter to include adjusted CASH trades
  - Changed query filter from `Statut = 'Ouvert'` to `Statut != 'Fermé'`
  - Fixes: USD CASH position (TradeNr 660) not showing TradeNr after adjustment
  - Impact: CASH positions with 'Ajusté' status now correctly link to their trades
  - File: R/cash.R:244-246

## [5.7.63] - 2025-12-30

### Fixed
- **create_cash_portfolio_row()**: Added account code conversion for CASH trade queries
  - Added switch statement to convert U1804173→Live, DU5221795→Simu before database query
  - Fixes: CASH positions not finding their TradeNr due to account field mismatch
  - Impact: USD and EUR CASH positions now correctly display their TradeNr
  - File: R/cash.R:237-242

## [5.7.62] - 2025-12-30

### Changed
- **getYahooData()**: Internal refactoring for price retrieval logic

## [5.7.61] - 2025-12-30

### Fixed
- **getYahooName() and getYahooData()**: Fixed currency code handling to prevent Yahoo Finance errors
  - **Problem**: "Failed to retrieve EUR/USD after 5 attempts: Unable to import" when loading Portfolio tab; wrong forex rates (54.51 instead of 0.79)
  - **Root causes**:
    1. `getYahooName("CHF")` checked base currency BEFORE Tickers table, returning "BASE_CURRENCY" instead of "CHFUSD=X" for CHF options
    2. Code calling `getYahooData("USD")` directly without converting to "USDCHF=X", causing Yahoo to return wrong ticker data
  - **Impact**: Portfolio with CHF options failed to load; incorrect forex rates displayed
  - **Solutions**:
    1. **getYahooName()**: Reordered to check Tickers table FIRST, only treat as base currency if ticker not found (R/ticker.R:340-357)
    2. **getYahooData()**: Auto-converts 3-letter currency codes to Yahoo tickers via getYahooName() before fetching (R/prices.R:587-610)
  - **Test results**:
    - `getYahooData("USD")` now returns 0.788 (correct) instead of 54.51 (wrong stock ticker)
    - `getYahooData("EUR")` now returns forex data instead of failing
    - `getYahooData("CHF")` returns base currency rate 1.0 (correct)
  - **Backward compatible**: Transparent conversion, no API changes

## [5.7.60] - 2025-12-30

### Fixed
- **getCurrencyExposure()**: Fixed GROUP BY bug causing duplicate currency rows
  - **Problem**: Changed `GROUP BY currency` to `GROUP BY 1` to group by CASE result instead of original currency column
  - **Impact**: Eliminated duplicate rows when CASH and non-CASH positions existed in same trading currency
  - **Location**: R/account.R:1313, 1332

## [5.7.59] - 2025-12-30

### Fixed
- **getCurrencyExposure()**: Fixed CASH position currency misattribution in exposure analysis
  - **Problem**: USD and EUR cash balances were incorrectly grouped under CHF exposure instead of their respective currencies
  - **Root cause**: CASH positions stored `currency = base_currency` (CHF) instead of trading currency, causing GROUP BY to misattribute foreign cash
  - **Impact**: Currency exposure breakdown showed inflated CHF exposure and deflated USD/EUR exposure by the amount of foreign currency cash holdings
  - **Example**: 42,012.64 USD cash + 7,154.52 EUR cash (worth 40,139.83 CHF total) appeared in CHF row instead of USD/EUR rows
  - **Solution**: Modified SQL query to use CASE statement - CASH positions grouped by `symbol` (trading currency), other positions by `currency` field
  - **Implementation**:
    - Added CASE WHEN type = 'CASH' THEN symbol ELSE currency END for GROUP BY clause
    - **CRITICAL FIX**: Changed `GROUP BY currency` to `GROUP BY 1` to group by CASE result instead of original currency column
    - Bug was causing duplicate rows when CASH and non-CASH positions existed in same trading currency (e.g., EUR cash + EUR options)
    - Preserves P&L calculations (CASH unPnL already in base currency, no conversion needed)
    - Handles negative market values correctly (short options)
    - Works for both current and historical date queries
  - **Location**: R/account.R:1298-1332 (lines 1313, 1332)
  - **Testing**: test_currency_exposure_fix.R verifies CASH grouping, negative values, and data integrity
  - **Backward compatible**: No schema changes, UI rendering unchanged

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
