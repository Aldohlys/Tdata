"""
On-demand historical option data retrieval for Tdata package.

This module provides seamless data access without requiring pre-configuration
of tracking lists - ideal for exploratory analysis and simplified UX.
"""

import pandas as pd
from typing import Optional, Dict, Any
import logging
from pathlib import Path
import pytz

from .historical_option import HistoricalStorage, HistoricalDataManager
from .IB_connection import isIBAvailable

logger = logging.getLogger(__name__)

def get_or_retrieve_option_historical_data(
    symbol: str,
    trading_class: str,
    expiration: str,
    strike: float,
    right: str,
    exchange: str,
    data_type: str = "historical",
    what_to_show: str = "TRADES",
    include_archived: bool = True,
    auto_cache: bool = True,
    force_refresh: bool = False
) -> Optional[pd.DataFrame]:
    """
    Retrieve historical option data with automatic on-demand fetching.

    This function provides a seamless experience by:
    1. First checking if data exists in storage
    2. If not found (or force_refresh=True), automatically fetching from IBKR
    3. Optionally caching the retrieved data for future use

    Args:
        symbol (str): Underlying symbol
        trading_class (str): Option trading class
        expiration (str): Expiration date (YYYYMMDD format)
        strike (float): Strike price
        right (str): 'C' for Call, 'P' for Put
        data_type (str): 'intraday', 'historical', or 'combined'
        what_to_show (str): IBKR data type ('TRADES', 'BID_ASK', etc.)
        include_archived (bool): Check archived data if not in active storage
        auto_cache (bool): Automatically cache retrieved data for reuse
        force_refresh (bool): Skip cache check and fetch fresh data

    Returns:
        pd.DataFrame: Historical option data, or None if unavailable

    Example:
        >>> # Simple on-demand retrieval
        >>> data = get_or_retrieve_option_historical_data(
        ...     symbol='SPY',
        ...     trading_class='SPY',
        ...     expiration='20250321',
        ...     strike=450.0,
        ...     right='C'
        ... )

        >>> # Force fresh data
        >>> fresh_data = get_or_retrieve_option_historical_data(
        ...     symbol='AAPL',
        ...     trading_class='AAPL',
        ...     expiration='20250321',
        ...     strike=180.0,
        ...     right='P',
        ...     force_refresh=True
        ... )
    """

    try:
        storage = HistoricalStorage()

        # Step 1: Check existing data (unless force_refresh)
        if not force_refresh:
            logger.info(f"Checking cached data for {symbol} {strike}{right} {expiration}")

            # Try loading existing data using current function
            from . import get_option_historical_data
            existing_data = get_option_historical_data(
                symbol, trading_class, expiration, strike, right,
                data_type, what_to_show, include_archived
            )

            if existing_data is not None and not existing_data.empty:
                logger.info(f"Found {len(existing_data)} cached data points")

                # Fetch underlying prices if missing (needed for IV calculation)
                if 'underlying_price' not in existing_data.columns:
                    try:
                        if isIBAvailable():
                            data_manager = HistoricalDataManager()
                            underlying_data = _fetch_underlying_data(
                                symbol, data_type, data_manager, exchange
                            )
                            if underlying_data is not None and not underlying_data.empty:
                                existing_data = _merge_underlying_with_option(existing_data, underlying_data)
                                logger.info(f"Merged underlying prices with cached data")
                        else:
                            logger.info("IBKR not available, returning cached data without underlying prices")
                    except Exception as e:
                        logger.warning(f"Could not fetch underlying data: {e}")

                return existing_data

        # Step 2: No data found or force_refresh - fetch on-demand
        logger.info(f"Fetching on-demand data for {symbol} {strike}{right} {expiration}")

        # Create contract specification
        contract_spec = {
            'symbol': symbol,
            'trading_class': trading_class,
            'expiration': expiration,
            'strike': strike,
            'right': right,
            'exchange': exchange  # Default exchange
        }

        # Fetch data directly from IBKR
        fetched_data = _fetch_single_contract_data(
            contract_spec, data_type, what_to_show
        )

        if fetched_data is None or fetched_data.empty:
            logger.warning(f"No data available from IBKR for {symbol} {strike}{right}")
            return None

        logger.info(f"Retrieved {len(fetched_data)} data points from IBKR")

        # Step 3: Auto-cache if requested
        if auto_cache:
            logger.info("Caching retrieved data for future use")
            _cache_retrieved_data(
                contract_spec, fetched_data, data_type, what_to_show
            )

        return fetched_data

    except Exception as e:
        logger.error(f"Error in get_or_retrieve_option_historical_data: {e}")
        return None


def _fetch_single_contract_data(
    contract_spec: Dict[str, Any],
    data_type: str,
    what_to_show: str = "TRADES"
) -> Optional[pd.DataFrame]:
    """
    Fetch data for a single contract directly from IBKR.

    This bypasses the tracking configuration and fetches on-demand.
    Also fetches underlying stock data and merges it for accurate IV calculations.

    Args:
        contract_spec: Contract specification dictionary
        data_type: Type of data ("historical" or "intraday")
        what_to_show: What data to show ("TRADES", "MIDPOINT", "BID_ASK")
    """
    try:
        if not isIBAvailable():
            logger.error("IBKR not available for on-demand data retrieval")
            return None

        data_manager = HistoricalDataManager()

        # Fetch option data from IBKR and store it
        data_manager.collect_data_for_active_contracts(data_type, [contract_spec])

        # Retrieve the stored option data (HistoricalDataManager auto-stores to parquet)
        # Try requested what_to_show first, then fall back to alternatives if not available
        from . import get_option_historical_data
        fetched_data = get_option_historical_data(
            contract_spec['symbol'],
            contract_spec['trading_class'],
            contract_spec['expiration'],
            contract_spec['strike'],
            contract_spec['right'],
            data_type,
            what_to_show,  # Use the passed what_to_show parameter
            True  # include_archived
        )

        # Fall back to other what_to_show types if preferred type not available
        # This is common for illiquid options where IBKR may only have MIDPOINT data
        if fetched_data is None or fetched_data.empty:
            fallback_types = ["MIDPOINT", "BID_ASK", "TRADES"]
            for fallback_type in fallback_types:
                if fallback_type == what_to_show:
                    continue  # Already tried this one
                fetched_data = get_option_historical_data(
                    contract_spec['symbol'],
                    contract_spec['trading_class'],
                    contract_spec['expiration'],
                    contract_spec['strike'],
                    contract_spec['right'],
                    data_type,
                    fallback_type,
                    True
                )
                if fetched_data is not None and not fetched_data.empty:
                    logger.info(f"Using {fallback_type} data (requested {what_to_show} not available)")
                    break

        if fetched_data is None or fetched_data.empty:
            logger.warning(f"No data available from IBKR for {contract_spec['symbol']} {contract_spec['strike']}{contract_spec['right']}")
            return None

        logger.info(f"Successfully retrieved {len(fetched_data)} option records for {contract_spec['symbol']} {contract_spec['strike']}{contract_spec['right']}")

        # Also fetch underlying data for accurate IV calculations
        underlying_data = _fetch_underlying_data(
            contract_spec['symbol'],
            data_type,
            data_manager
        )

        # Merge underlying prices with option data
        if underlying_data is not None and not underlying_data.empty:
            merged_data = _merge_underlying_with_option(fetched_data, underlying_data)
            logger.info(f"Merged {len(merged_data)} records with underlying prices")
            return merged_data
        else:
            logger.warning(f"Could not retrieve underlying data for {contract_spec['symbol']}, returning option data without underlying prices")
            return fetched_data

    except Exception as e:
        logger.error(f"Error fetching single contract data: {e}")
        return None


def _fetch_underlying_data(
    symbol: str,
    data_type: str,
    data_manager: HistoricalDataManager,
    exchange: str = None
) -> Optional[pd.DataFrame]:
    """
    Fetch historical data for the underlying instrument (stock or future).

    Args:
        symbol: Underlying symbol
        data_type: Type of data ("historical" or "intraday")
        data_manager: HistoricalDataManager instance to reuse connection
        exchange: Fallback exchange if ticker_db info is incomplete

    Returns:
        DataFrame with columns: datetime, underlying_price
    """
    try:
        import math
        from .IB_connection import safe_ib_connect
        from ib_async import Stock, Future
        from .core import ticker_db

        # Get or create IB connection
        ib = safe_ib_connect()
        if not ib or not ib.isConnected():
            logger.error("No IB connection available for underlying data retrieval")
            return None

        # Get ticker info from database to determine type, exchange, currency
        ticker_info = ticker_db.get_ticker_info(symbol)

        if ticker_info is not None and len(ticker_info) > 0:
            exchange = ticker_info.get('Exchange', 'SMART')
            currency = ticker_info.get('Currency', 'USD')
            sec_type = ticker_info.get('Type', 'STK')

            # Handle empty values
            if not exchange or exchange == '':
                exchange = 'SMART'
            if not currency or currency == '':
                currency = 'USD'
            if not sec_type or sec_type == '':
                sec_type = 'STK'

            logger.info(f"Using ticker info for {symbol}: type={sec_type}, exchange={exchange}, currency={currency}")
        else:
            exchange_val = exchange if exchange else 'SMART'
            currency = 'USD'
            sec_type = 'STK'
            exchange = exchange_val
            logger.warning(f"No ticker info found for {symbol}, using default: type=STK, exchange={exchange}, currency=USD")

        # Create contract based on security type
        if sec_type == 'FUT':
            # For futures, use ConId (most reliable) or localSymbol+expiration as backup
            con_id = ticker_info.get('ConId') if ticker_info else None
            expiration = ticker_info.get('Expiration') if ticker_info else None

            if con_id is not None and not (isinstance(con_id, float) and math.isnan(con_id)):
                underlying_contract = Future(currency=currency, exchange=exchange, conId=int(con_id))
                logger.info(f"Creating Future contract for {symbol} using ConId={int(con_id)}")
            elif expiration and not (isinstance(expiration, float) and math.isnan(expiration)):
                underlying_contract = Future(localSymbol=symbol,
                                            lastTradeDateOrContractMonth=int(expiration),
                                            currency=currency, exchange=exchange)
                logger.info(f"Creating Future contract for {symbol} using localSymbol+expiration={int(expiration)}")
            else:
                underlying_contract = Future(symbol=symbol, exchange=exchange, currency=currency)
                logger.info(f"Creating Future contract for {symbol} using symbol only (fallback)")
        else:
            underlying_contract = Stock(symbol=symbol, exchange=exchange, currency=currency)

        # Use same configuration as option data
        config = data_manager.config_manager

        # Determine parameters based on data_type
        if data_type == "intraday":
            duration = config.intraday_duration
            bar_size = config.intraday_frequency
        else:  # historical
            duration = config.historical_duration
            bar_size = config.historical_frequency

        logger.info(f"Fetching underlying data for {symbol} (type={sec_type}, {data_type}: {duration} of {bar_size} bars)")

        # Fetch bars using the same method as options
        bars = data_manager._collect_bars(
            ib=ib,
            contract=underlying_contract,
            data_type=data_type,
            what_to_show="TRADES",
            timeout=config.incremental_timeout,
            is_incremental=False
        )

        if not bars or len(bars) == 0:
            logger.warning(f"No underlying data returned for {symbol}")
            return None

        # Convert to DataFrame
        underlying_df = pd.DataFrame([
            {
                'datetime': pd.to_datetime(bar.date),
                'underlying_price': bar.close
            }
            for bar in bars
        ])

        logger.info(f"Retrieved {len(underlying_df)} underlying price records for {symbol}")
        return underlying_df

    except Exception as e:
        logger.error(f"Error fetching underlying data for {symbol}: {e}")
        return None

def _get_tz_name(tzinfo) -> str | None:
    """Return timezone name regardless of whether it's pytz or zoneinfo."""
    if tzinfo is None:
        return None
    if hasattr(tzinfo, "zone"):  # pytz
        return tzinfo.zone
    if hasattr(tzinfo, "key"):   # zoneinfo
        return tzinfo.key
    return str(tzinfo)



def _merge_underlying_with_option(
    option_data: pd.DataFrame,
    underlying_data: pd.DataFrame
) -> pd.DataFrame:
    """Merge option and underlying data with timezone-aware alignment."""
    try:
        # --- 1️⃣ Ensure datetime columns are valid ---
        option_data['datetime'] = pd.to_datetime(option_data['datetime'], errors='coerce')
        underlying_data['datetime'] = pd.to_datetime(underlying_data['datetime'], errors='coerce')

        # --- 2️⃣ Unify timezone awareness (from previous code) ---
        def _get_tz_name(tzinfo):
            if tzinfo is None:
                return None
            if hasattr(tzinfo, "zone"):  # pytz
                return tzinfo.zone
            if hasattr(tzinfo, "key"):   # zoneinfo
                return tzinfo.key
            return str(tzinfo)

        def _infer_timezone(df):
            CURRENCY_TZ_MAP = {
                "USD": "America/New_York",
                "EUR": "Europe/Paris",
                "GBP": "Europe/London",
                "CHF": "Europe/Zurich",
                "JPY": "Asia/Tokyo"
            }
            SYMBOL_HINTS = {
                "SPX": "America/New_York",
                "NDX": "America/New_York",
                "DAX": "Europe/Berlin",
                "ESTX50": "Europe/Paris",
                "CAC": "Europe/Paris",
                "FTSE": "Europe/London",
                "N225": "Asia/Tokyo"
            }
            if "currency" in df.columns and pd.notna(df["currency"].iloc[0]):
                curr = str(df["currency"].iloc[0]).upper()
                if curr in CURRENCY_TZ_MAP:
                    return CURRENCY_TZ_MAP[curr]
            if "symbol" in df.columns and pd.notna(df["symbol"].iloc[0]):
                sym = str(df["symbol"].iloc[0]).upper()
                for key, val in SYMBOL_HINTS.items():
                    if key in sym:
                        return val
            return "UTC"

        def _ensure_tz(df, tz):
            if df['datetime'].dt.tz is None:
                return df.assign(datetime=df['datetime'].dt.tz_localize(tz))
            else:
                current_tz = _get_tz_name(df['datetime'].dt.tz)
                if current_tz != tz:
                    return df.assign(datetime=df['datetime'].dt.tz_convert(tz))
                else:
                    return df

        # Infer and align
        opt_tz = _get_tz_name(option_data['datetime'].dt.tz)
        und_tz = _get_tz_name(underlying_data['datetime'].dt.tz)

        if not opt_tz:
            opt_tz = _infer_timezone(option_data)
            option_data = _ensure_tz(option_data, opt_tz)
        if not und_tz:
            und_tz = _infer_timezone(underlying_data)
            underlying_data = _ensure_tz(underlying_data, und_tz)

        if opt_tz != und_tz:
            underlying_data = _ensure_tz(underlying_data, opt_tz)

        # --- 3️⃣ Normalize internal precision ---
        # This avoids ns/us mismatch
        option_data['datetime'] = option_data['datetime'].dt.tz_convert('UTC').astype('datetime64[ns, UTC]')
        underlying_data['datetime'] = underlying_data['datetime'].dt.tz_convert('UTC').astype('datetime64[ns, UTC]')

        # --- 4️⃣ Sort before merge_asof ---
        option_data = option_data.sort_values('datetime')
        underlying_data = underlying_data.sort_values('datetime')

        # --- 5️⃣ Merge ---
        merged = pd.merge_asof(
            option_data,
            underlying_data,
            on='datetime',
            direction='nearest',
            tolerance=pd.Timedelta('5 minutes')
        )

        # --- 6️⃣ Log missing matches ---
        if 'underlying_price' in merged.columns:
            missing_count = merged['underlying_price'].isna().sum()
            if missing_count > 0:
                logger.warning(f"{missing_count} option bars could not be matched with underlying prices")

        return merged

    except Exception as e:
        logger.error(f"Error merging underlying with option data: {e}")
        return option_data


def _cache_retrieved_data(
    contract_spec: Dict[str, Any],
    data: pd.DataFrame,
    data_type: str,
    what_to_show: str
) -> bool:
    """
    Cache retrieved data for future use without adding to tracking configuration.

    Note: Data is already cached by HistoricalDataManager in the standard location.
    This function is a placeholder for potential future on-demand cache optimization.
    """
    try:
        # Data is already cached by HistoricalDataManager.collect_data_for_active_contracts()
        # in the standard strikes directory structure, so no additional caching needed
        logger.debug(f"Data already cached in standard location for {contract_spec['symbol']} " +
                    f"{contract_spec['strike']}{contract_spec['right']}")
        return True

    except Exception as e:
        logger.error(f"Error in cache placeholder: {e}")
        return False


def clear_on_demand_cache(symbol: str = None, older_than_days: int = 30) -> Dict[str, int]:
    """
    Clean up on-demand cache files.

    Args:
        symbol (str): If specified, only clear cache for this symbol
        older_than_days (int): Remove files older than this many days

    Returns:
        dict: Summary of cleanup operation
    """
    try:
        storage = HistoricalStorage()
        cache_root = storage.get_on_demand_cache_root()

        if not cache_root.exists():
            return {"files_removed": 0, "space_freed_mb": 0}

        files_removed = 0
        space_freed = 0
        cutoff_time = pd.Timestamp.now() - pd.Timedelta(days=older_than_days)

        for cache_file in cache_root.rglob("*.parquet"):
            # Check symbol filter
            if symbol and symbol not in str(cache_file):
                continue

            # Check age
            file_time = pd.Timestamp(cache_file.stat().st_mtime, unit='s')
            if file_time < cutoff_time:
                file_size = cache_file.stat().st_size
                cache_file.unlink()
                files_removed += 1
                space_freed += file_size

        return {
            "files_removed": files_removed,
            "space_freed_mb": round(space_freed / (1024*1024), 2)
        }

    except Exception as e:
        logger.error(f"Error clearing on-demand cache: {e}")
        return {"error": str(e)}
