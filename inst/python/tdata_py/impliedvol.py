import math
import datetime
import locale
import pandas as pd
import json
import logging
from ib_insync import *

# Import from other modules
# from tdata_py.core import CONFIG, ticker_db, validate_contract_params
# from tdata_py.IB_connection import safe_ib_connect

from .core import CONFIG, ticker_db, validate_contract_params
from .IB_connection import safe_ib_connect

#from .contract import getOptValue

def get_volatility_metrics(sym, secType=None, currency=None, exchange=None, expiration_future=None, lookback_days=252, hist=False):
    
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected():
        return None
    
    # Set appropriate bar size based on lookback period
    # For up to 2 years (504 days)
    total_hours = lookback_days * 6.5  # ~6.5 trading hours per day
    
    if lookback_days <= 252:  # 1 year
        bar_size = '2 hours'  # ~820 bars for 1 year
    else:  # 1-2 years
        bar_size = '4 hours'  # ~820 bars for 2 years
        
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if secType is None:
        secType = ticker_info.get('Type', 'STK')
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
        
    if exchange is None:
        exchange = ticker_info.get('Exchange', 'SMART')
        
    # Define the underlying stock contract - first deal with specific FUT case
    if secType == "FUT" and expiration_future is None:
        expiration_future = ticker_info.get('Expiration')
    
    if secType == "STK":
      contract = Stock(symbol=sym, exchange=exchange, currency=currency)
    elif secType == "FUT":
      contract = Future(symbol=sym, exchange=exchange, currency=currency, lastTradeDateOrContractMonth=expiration_future)
    elif secType == "IND":
        contract = Index(symbol=sym, exchange=exchange, currency=currency)
    elif secType == "CASH":
        contract = Forex(symbol=sym, exchange=exchange, currency=currency)
    
        # Check if contract was created - if not, return with error
    if contract is None:
        print(f"Error: Unsupported security type '{secType}' for symbol {sym}")
        return {
            'symbol': sym,
            'error': f"Unsupported security type: {secType}"
        }
        
    ib.qualifyContracts(contract)

    # Ensure proper duration string format - IBKR requires INTEGER space unit
    duration_str = f"{int(lookback_days)} D"
    print(f"Requesting data with duration: '{duration_str}' and bar size: '{bar_size}'")
    
    # Request implied volatility data
    try:
        iv_bars = ib.reqHistoricalData(
            contract,
            endDateTime='',
            durationStr=duration_str,
            barSizeSetting=bar_size,
            whatToShow='OPTION_IMPLIED_VOLATILITY',
            useRTH=True,
            formatDate=1
        )
    except Exception as e:
        print(f"Error requesting IV data for {sym}: {e}")
        iv_bars = None
    
    # Request historical (realized) volatility data if hist is set to True
    ## This takes more time to retrieve than IV historical data
    hv_bars = None
    if hist:
        try:
            hv_bars = ib.reqHistoricalData(
                contract,
                endDateTime='', 
                durationStr=duration_str,
                barSizeSetting=bar_size,
                whatToShow='HISTORICAL_VOLATILITY',
                useRTH=True,
                formatDate=1
            )
        except Exception as e:
            print(f"Error requesting HV data for {sym}: {e}")
            hv_bars = None

    ## No more data to retrieve from IBKR
    ## Take some time to flush IB connection
    ib.sleep(1)
    ib.disconnect()
    
    # Convert to DataFrames with better error handling
    iv_df = None
    if iv_bars is not None and len(iv_bars) > 0:
        iv_df = util.df(iv_bars)
    else:
        print(f"No implied volatility data returned for {sym}")
        iv_df = pd.DataFrame(columns=['date', 'close'])
        
    hv_df = None
    if hist and hv_bars is not None and len(hv_bars) > 0:
        hv_df = util.df(hv_bars)
    elif hist:
        print(f"No historical volatility data returned for {sym}")
        hv_df = pd.DataFrame(columns=['date', 'close'])
        
    ## Initialize all data to be returned
    current_iv = float("nan")
    iv_percentile = float("nan")
    iv_min = float("nan")
    iv_max = float("nan")
    iv_mean = float("nan")
    iv_30d_back = float("nan")  # New
    iv_180d_back = float("nan")  # New
    
    current_hv = float("nan")
    hv_percentile = float("nan")
    hv_min = float("nan")
    hv_max = float("nan")
    hv_mean = float("nan")
    hv_30d_back = float("nan")  # New
    hv_180d_back = float("nan")  # New
    
    days_covered = 0
    
    # Check if we have IV data
    if iv_df is not None and len(iv_df) > 0:
        # Convert date column to datetime for calculations
        iv_df = iv_df.copy()  # Avoid SettingWithCopyWarning
        iv_df['date'] = pd.to_datetime(iv_df['date'])
        
        # Sort by date to ensure proper ordering
        iv_df = iv_df.sort_values('date').reset_index(drop=True)
        
        # Compute statistics for all data points
        current_iv = iv_df['close'].iloc[-1]
        iv_percentile = 100 * (iv_df['close'] <= current_iv).mean()
        iv_min = iv_df['close'].min()
        iv_max = iv_df['close'].max()
        iv_mean = iv_df['close'].mean()
        
        # Calculate lookback periods using actual trading days in the data
        last_date = iv_df['date'].iloc[-1]
        
        # Find approximate dates for 30 and 180 business days back
        # Using calendar days * 0.714 to approximate business days (5/7 ratio)
        approx_30d_back = last_date - pd.Timedelta(days=int(30 / 0.714))  # ~42 calendar days
        approx_180d_back = last_date - pd.Timedelta(days=int(180 / 0.714))  # ~252 calendar days
        
        # Find the closest actual data points to these target dates
        iv_30d_window = iv_df[
            (iv_df['date'] >= approx_30d_back - pd.Timedelta(days=7)) & 
            (iv_df['date'] <= approx_30d_back + pd.Timedelta(days=7))
        ]
        if len(iv_30d_window) > 0:
            iv_30d_back = iv_30d_window['close'].mean()
        
        iv_180d_window = iv_df[
            (iv_df['date'] >= approx_180d_back - pd.Timedelta(days=7)) & 
            (iv_df['date'] <= approx_180d_back + pd.Timedelta(days=7))
        ]
        if len(iv_180d_window) > 0:
            iv_180d_back = iv_180d_window['close'].mean()

        # Calculate actual days covered
        first_date = iv_df['date'].iloc[0]
        last_date = iv_df['date'].iloc[-1]
        days_covered = (last_date - first_date).days + 1
    
    # Process HV data if requested and available
    if hist and hv_df is not None and len(hv_df) > 0:
        hv_df = util.df(hv_bars)
        if len(hv_df) > 0:
            # Convert date column to datetime
            hv_df = hv_df.copy()  # Avoid SettingWithCopyWarning
            hv_df['date'] = pd.to_datetime(hv_df['date'])
            hv_df = hv_df.sort_values('date').reset_index(drop=True)
            
            # Current analysis
            current_hv = hv_df['close'].iloc[-1]
            hv_percentile = 100 * (hv_df['close'] <= current_hv).mean()
            hv_min = hv_df['close'].min()
            hv_max = hv_df['close'].max()
            hv_mean = hv_df['close'].mean()
            
            # Calculate lookback periods using actual trading days in the data
            last_date = hv_df['date'].iloc[-1]
            approx_30d_back = last_date - pd.Timedelta(days=int(30 / 0.714))
            approx_180d_back = last_date - pd.Timedelta(days=int(180 / 0.714))
            
            # Find the closest actual data points to these target dates
            hv_30d_window = hv_df[
                (hv_df['date'] >= approx_30d_back - pd.Timedelta(days=7)) & 
                (hv_df['date'] <= approx_30d_back + pd.Timedelta(days=7))
            ]
            if len(hv_30d_window) > 0:
                hv_30d_back = hv_30d_window['close'].mean()
            
            hv_180d_window = hv_df[
                (hv_df['date'] >= approx_180d_back - pd.Timedelta(days=7)) & 
                (hv_df['date'] <= approx_180d_back + pd.Timedelta(days=7))
            ]
            if len(hv_180d_window) > 0:
                hv_180d_back = hv_180d_window['close'].mean()
    else:
        print(f"Missing historical realized volatility data for {sym}")

    # Process the days_covered calculation outside the conditional block
    # to ensure it's always available in the return value
    days_covered = 0
    if iv_df is not None and len(iv_df) > 0:
        try:
            first_date = pd.to_datetime(iv_df['date'].iloc[0])
            last_date = pd.to_datetime(iv_df['date'].iloc[-1])
            days_covered = (last_date - first_date).days + 1
        except (IndexError, KeyError) as e:
            print(f"Error calculating days_covered for {sym}: {e}")
            # days_covered remains 0
            
    return {
        'symbol': sym,
        'days_covered': days_covered,
        'requested_days': lookback_days,
        'data_points': len(iv_df) if iv_df is not None else 0,
        'bar_size': bar_size,
        
        'current_iv': current_iv,
        'iv_percentile': iv_percentile,
        'iv_min': iv_min,
        'iv_max': iv_max,
        'iv_mean': iv_mean,
        'iv_30d_back': iv_30d_back,      # New
        'iv_180d_back': iv_180d_back,    # New
        
        'current_hv': current_hv,
        'hv_percentile': hv_percentile,
        'hv_min': hv_min,
        'hv_max': hv_max,
        'hv_mean': hv_mean,
        'hv_30d_back': hv_30d_back,      # New
        'hv_180d_back': hv_180d_back     # New
    }
