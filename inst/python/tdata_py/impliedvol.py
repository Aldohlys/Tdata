import math
import datetime
import locale
import pandas as pd
import logging
from ib_insync import *

# Import from other modules
# from tdata_py.core import CONFIG, ticker_db, validate_contract_params
# from tdata_py.IB_connection import safe_ib_connect

from .core import CONFIG, ticker_db, validate_contract_params
from .IB_connection import safe_ib_connect

#from .contract import getOptValue

def _determine_bar_size(lookback_days):
    """Determine appropriate bar size based on lookback period."""
    if lookback_days <= 252:  # 1 year
        return '2 hours'  # ~820 bars for 1 year
    else:  # 1-2 years
        return '4 hours'  # ~820 bars for 2 years


def _create_contract(sym, secType, currency, exchange, expiration_future):
    """Create appropriate contract based on security type."""
    if secType == "STK":
        return Stock(symbol=sym, exchange=exchange, currency=currency)
    elif secType == "FUT":
        return Future(symbol=sym, exchange=exchange, currency=currency, 
                     lastTradeDateOrContractMonth=expiration_future)
    elif secType == "IND":
        return Index(symbol=sym, exchange=exchange, currency=currency)
    elif secType == "CASH":
        return Forex(symbol=sym, exchange=exchange, currency=currency)
    else:
        return None


def _fetch_historical_data(ib, contract, duration_str, bar_size, data_type):
    """Fetch historical data from IBKR for specified data type."""
    try:
        return ib.reqHistoricalData(
            contract,
            endDateTime='',
            durationStr=duration_str,
            barSizeSetting=bar_size,
            whatToShow=data_type,
            useRTH=True,
            formatDate=1
        )
    except Exception as e:
        print(f"Error requesting {data_type} data: {e}")
        return None


def _calculate_lookback_values(df, target_days_back):
    """Calculate historical values for specified days back using window approach."""
    if df is None or len(df) == 0:
        return float("nan")
    
    last_date = df['date'].iloc[-1]
    # Convert trading days to calendar days using business day ratio
    approx_calendar_days = int(target_days_back / 0.714)  # ~5/7 ratio
    target_date = last_date - pd.Timedelta(days=approx_calendar_days)
    
    # Find data points within 7-day window of target date
    window = df[
        (df['date'] >= target_date - pd.Timedelta(days=7)) & 
        (df['date'] <= target_date + pd.Timedelta(days=7))
    ]
    
    if len(window) > 0:
        return window['close'].mean()
    return float("nan")


def _process_volatility_data(bars, data_type_name):
    """Process volatility data (IV or HV) and calculate statistics."""
    if bars is None or len(bars) == 0:
        print(f"No {data_type_name} data returned")
        return {
            f'current_{data_type_name.lower()}': float("nan"),
            f'{data_type_name.lower()}_percentile': float("nan"),
            f'{data_type_name.lower()}_min': float("nan"),
            f'{data_type_name.lower()}_max': float("nan"),
            f'{data_type_name.lower()}_mean': float("nan"),
            f'{data_type_name.lower()}_30d_back': float("nan"),
            f'{data_type_name.lower()}_180d_back': float("nan")
        }
    
    df = util.df(bars)
    df = df.copy()
    df['date'] = pd.to_datetime(df['date'])
    df = df.sort_values('date').reset_index(drop=True)
    
    # Current statistics
    current_val = df['close'].iloc[-1]
    percentile = 100 * (df['close'] <= current_val).mean()
    min_val = df['close'].min()
    max_val = df['close'].max()
    mean_val = df['close'].mean()
    
    # Historical lookback values
    val_30d_back = _calculate_lookback_values(df, 30)
    val_180d_back = _calculate_lookback_values(df, 180)
    
    return {
        f'current_{data_type_name.lower()}': current_val,
        f'{data_type_name.lower()}_percentile': percentile,
        f'{data_type_name.lower()}_min': min_val,
        f'{data_type_name.lower()}_max': max_val,
        f'{data_type_name.lower()}_mean': mean_val,
        f'{data_type_name.lower()}_30d_back': val_30d_back,
        f'{data_type_name.lower()}_180d_back': val_180d_back
    }


def _process_price_data(bars):
    """Process price data and calculate statistics including returns."""
    if bars is None or len(bars) == 0:
        print("No price data returned")
        return {
            'current_price': float("nan"),
            'price_min': float("nan"),
            'price_max': float("nan"),
            'price_mean': float("nan"),
            'price_30d_back': float("nan"),
            'price_180d_back': float("nan"),
            'total_return': float("nan"),
            'annualized_return': float("nan")
        }
    
    df = util.df(bars)
    df = df.copy()
    df['date'] = pd.to_datetime(df['date'])
    df = df.sort_values('date').reset_index(drop=True)
    
    # Current statistics
    current_price = df['close'].iloc[-1]
    price_min = df['close'].min()
    price_max = df['close'].max()
    price_mean = df['close'].mean()
    
    # Historical lookback values
    price_30d_back = _calculate_lookback_values(df, 30)
    price_180d_back = _calculate_lookback_values(df, 180)
    
    # Calculate returns
    first_price = df['close'].iloc[0]
    total_return = (current_price - first_price) / first_price * 100
    
    # Annualized return based on actual days covered
    first_date = df['date'].iloc[0]
    last_date = df['date'].iloc[-1]
    days_elapsed = (last_date - first_date).days + 1
    years_elapsed = days_elapsed / 365.25
    
    if years_elapsed > 0 and first_price > 0:
        annualized_return = ((current_price / first_price) ** (1/years_elapsed) - 1) * 100
    else:
        annualized_return = float("nan")
    
    return {
        'current_price': current_price,
        'price_min': price_min,
        'price_max': price_max,
        'price_mean': price_mean,
        'price_30d_back': price_30d_back,
        'price_180d_back': price_180d_back,
        'total_return': total_return,
        'annualized_return': annualized_return
    }


def _calculate_days_covered(df):
    """Calculate actual days covered by the data."""
    if df is None or len(df) == 0:
        return 0
    
    try:
        first_date = pd.to_datetime(df['date'].iloc[0])
        last_date = pd.to_datetime(df['date'].iloc[-1])
        return (last_date - first_date).days + 1
    except (IndexError, KeyError) as e:
        print(f"Error calculating days_covered: {e}")
        return 0


def get_volatility_metrics(sym, secType=None, currency=None, exchange=None, 
                          expiration_future=None, lookback_days=252, hist=False, price=False):
    """
    Retrieve volatility and price metrics for a given symbol.
    
    Args:
        sym: Symbol to analyze
        secType: Security type (STK, FUT, IND, CASH)
        currency: Currency (default from ticker_db)
        exchange: Exchange (default from ticker_db)
        expiration_future: Future expiration (for FUT contracts)
        lookback_days: Number of days to look back (default 252)
        hist: Whether to retrieve historical volatility data
        price: Whether to retrieve historical price data
    
    Returns:
        Dict containing all metrics
    """
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()
    if not ib.isConnected():
        return None
    
    # Determine bar size based on lookback period
    bar_size = _determine_bar_size(lookback_days)
    
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    secType = secType or ticker_info.get('Type', 'STK')
    currency = currency or ticker_info.get('Currency', 'USD')
    exchange = exchange or ticker_info.get('Exchange', 'SMART')
    
    # Handle FUT expiration
    if secType == "FUT" and expiration_future is None:
        expiration_future = ticker_info.get('Expiration')
    
    # Create contract
    contract = _create_contract(sym, secType, currency, exchange, expiration_future)
    if contract is None:
        return {
            'symbol': sym,
            'error': f"Unsupported security type: {secType}"
        }
    
    ib.qualifyContracts(contract)
    duration_str = f"{int(lookback_days)} D"
    print(f"Requesting data with duration: '{duration_str}' and bar size: '{bar_size}'")
    
    # Fetch data based on flags
    iv_bars = _fetch_historical_data(ib, contract, duration_str, bar_size, 
                                    'OPTION_IMPLIED_VOLATILITY')
    
    hv_bars = None
    if hist:
        hv_bars = _fetch_historical_data(ib, contract, duration_str, bar_size, 
                                        'HISTORICAL_VOLATILITY')
    
    price_bars = None
    if price:
        price_bars = _fetch_historical_data(ib, contract, duration_str, bar_size, 
                                           'TRADES')  # Or 'ADJUSTED_LAST' for adjusted prices
    
    # Clean up connection
    ib.sleep(1)
    ib.disconnect()
    
    # Process all data types
    iv_metrics = _process_volatility_data(iv_bars, 'IV')
    hv_metrics = _process_volatility_data(hv_bars, 'HV') if hist else {}
    price_metrics = _process_price_data(price_bars) if price else {}
    
    # Calculate days covered (use IV data as primary reference)
    iv_df = util.df(iv_bars) if iv_bars else None
    days_covered = _calculate_days_covered(iv_df)
    
    # Combine all results
    result = {
        'symbol': sym,
        'days_covered': days_covered,
        'requested_days': lookback_days,
        'data_points': len(iv_df) if iv_df is not None else 0,
        'bar_size': bar_size,
    }
    
    # Add all metrics
    result.update(iv_metrics)
    if hist:
        result.update(hv_metrics)
    if price:
        result.update(price_metrics)
    
    return result
