"""
Earnings date retrieval via yfinance.

Uses Yahoo Finance (free, no IBKR subscription required). WSH was the original
intent but Error 10276 blocks it without a separate News Feed entitlement.

Function signature matches the original WSH version so R wrappers don't change.
"""
import datetime
import math
import pandas as pd
import yfinance as yf

from .core import ticker_db
from fin_logger import get_logger, log_execution_time

logger = get_logger("tdata_py.earnings")


def _is_nan(x):
    """Robust NaN check — pandas returns float('nan') for NULL numeric columns."""
    return isinstance(x, float) and math.isnan(x)


def _resolve_yahoo_name(symbol):
    """Prefer YahooName (handles .SW / .PA / .DE suffixes for non-US); fallback to symbol."""
    info = ticker_db.get_ticker_info(symbol)
    if info is None:
        return symbol
    yn = info.get('YahooName')
    if yn is None or _is_nan(yn) or (isinstance(yn, str) and not yn.strip()):
        return symbol
    return yn.strip() if isinstance(yn, str) else symbol


def _to_date(x):
    """Normalize a yfinance datetime/timestamp/date to a naive datetime.date."""
    if x is None:
        return None
    if isinstance(x, datetime.date) and not isinstance(x, datetime.datetime):
        return x
    if isinstance(x, datetime.datetime):
        return x.date()
    if isinstance(x, pd.Timestamp):
        return x.tz_localize(None).date() if x.tz is not None else x.date()
    return None


def _next_from_calendar(tk, today):
    """Try Ticker.calendar — typically returns a dict with 'Earnings Date' key."""
    try:
        cal = tk.calendar
    except Exception as e:
        logger.debug("calendar accessor failed", {"error": str(e)})
        return None

    if not isinstance(cal, dict):
        return None
    dates = cal.get('Earnings Date')
    if dates is None:
        return None
    if not isinstance(dates, list):
        dates = [dates]
    normed = [d for d in (_to_date(x) for x in dates) if d is not None and d >= today]
    return min(normed) if normed else None


def _next_from_earnings_dates(tk, today):
    """Fallback: Ticker.get_earnings_dates() — returns DataFrame indexed by date."""
    try:
        ed = tk.get_earnings_dates(limit=8)
    except Exception as e:
        logger.debug("get_earnings_dates failed", {"error": str(e)})
        return None

    if ed is None or len(ed) == 0:
        return None

    # Index is timezone-aware DatetimeIndex; strip tz to compare with naive today
    try:
        idx = ed.index.tz_localize(None) if ed.index.tz is not None else ed.index
        today_ts = pd.Timestamp(today)
        future = idx[idx >= today_ts]
        if len(future) == 0:
            return None
        return future.min().date()
    except Exception as e:
        logger.debug("earnings_dates index filter failed", {"error": str(e)})
        return None


@log_execution_time
def getNextEarningsDate(symbol, conId=None, secType=None, currency=None, exchange=None):
    """
    Get the next earnings date for a symbol via yfinance.

    Args:
        symbol: Ticker symbol as stored in the Tickers.Name column (e.g. "AAPL")
        conId, secType, currency, exchange: kept for signature compatibility; unused.

    Returns:
        str: "YYYYMMDD" or None if unavailable / ticker not found / no upcoming earnings.
    """
    yahoo_name = _resolve_yahoo_name(symbol)
    logger.info("Getting next earnings date (yfinance)",
                {"symbol": symbol, "yahoo_name": yahoo_name})

    try:
        tk = yf.Ticker(yahoo_name)
        today = datetime.date.today()

        # Try calendar first (fast, minimal data fetch)
        next_date = _next_from_calendar(tk, today)

        # Fallback: earnings_dates (richer, slightly slower)
        if next_date is None:
            next_date = _next_from_earnings_dates(tk, today)

        if next_date is None:
            logger.info("No future earnings date found",
                        {"symbol": symbol, "yahoo_name": yahoo_name})
            return None

        result = next_date.strftime('%Y%m%d')
        logger.info("Earnings date resolved",
                    {"symbol": symbol, "date": result})
        return result

    except Exception as e:
        logger.warning("yfinance lookup failed",
                       {"symbol": symbol, "error": str(e)})
        return None
