"""
On-demand historical option data retrieval for Tdata package.

This module provides seamless data access without requiring pre-configuration
of tracking lists - ideal for exploratory analysis and simplified UX.
"""

import pandas as pd
from typing import Optional, Dict, Any
import logging
from pathlib import Path

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
            contract_spec, data_type
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
    data_type: str
) -> Optional[pd.DataFrame]:
    """
    Fetch data for a single contract directly from IBKR.

    This bypasses the tracking configuration and fetches on-demand.
    """
    try:
        if not isIBAvailable():
            logger.error("IBKR not available for on-demand data retrieval")
            return None

        data_manager = HistoricalDataManager()

        # Fetch data from IBKR and store it
        results = data_manager.collect_data_for_active_contracts(data_type, [contract_spec])

        # Check if collection was successful
        if not results or not results[0].get('success', False):
            logger.warning(f"Failed to retrieve data from IBKR for {contract_spec['symbol']}")
            return None

        # Retrieve the stored data
        from . import get_option_historical_data
        fetched_data = get_option_historical_data(
            contract_spec['symbol'],
            contract_spec['trading_class'],
            contract_spec['expiration'],
            contract_spec['strike'],
            contract_spec['right'],
            data_type,
            "TRADES",
            True  # include_archived
        )

        return fetched_data

    except Exception as e:
        logger.error(f"Error fetching single contract data: {e}")
        return None


def _cache_retrieved_data(
    contract_spec: Dict[str, Any],
    data: pd.DataFrame,
    data_type: str,
    what_to_show: str
) -> bool:
    """
    Cache retrieved data for future use without adding to tracking configuration.

    This stores data in a separate "on-demand" cache to avoid cluttering
    the main tracking configuration.
    """
    try:
        storage = HistoricalStorage()

        # Create a temporary storage path for on-demand data
        cache_path = storage.get_on_demand_cache_path(
            contract_spec['symbol'],
            contract_spec['trading_class'],
            contract_spec['expiration'],
            contract_spec['strike'],
            contract_spec['right'],
            data_type,
            what_to_show
        )

        # Ensure directory exists
        cache_path.parent.mkdir(parents=True, exist_ok=True)

        # Save data
        data.to_parquet(cache_path, index=False)
        logger.info(f"Cached data to {cache_path}")

        return True

    except Exception as e:
        logger.error(f"Error caching retrieved data: {e}")
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
