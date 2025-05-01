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
    
    # Request implied volatility data
    iv_bars = ib.reqHistoricalData(
        contract,
        endDateTime = '',
        durationStr = f"{lookback_days} D",
        barSizeSetting = bar_size,
        whatToShow = 'OPTION_IMPLIED_VOLATILITY',
        useRTH = True,
        formatDate = 1
    )
    
    # Request historical (realized) volatility data if hist is set to True
    ## This takes more time to retrieve than IV historical data
    hv_bars = None
    if hist:
      hv_bars = ib.reqHistoricalData(
        contract,
        endDateTime='', 
        durationStr=f"{lookback_days} D",
        barSizeSetting=bar_size,
        whatToShow='HISTORICAL_VOLATILITY',
        useRTH=True,
        formatDate=1
      )

    ## No more data to retrieve from IBKR
    ## Take some time to flush IB connection
    ib.sleep(1)
    ib.disconnect()
    
    # Convert to DataFrames
    iv_df = None
    if iv_bars is not None:
      iv_df = util.df(iv_bars)
    else:
      print(f"Missing historical implied volatility data for {sym}")
      # Initialize empty dataframe to avoid errors
      iv_df = pd.DataFrame(columns=['date', 'close'])
        
    ## Initialize all data to be returned
    current_iv = float("Nan")
    iv_percentile = float("Nan")
    iv_min = float("Nan")
    iv_max = float("Nan")
    iv_mean = float("Nan")
    current_hv = float("Nan")
    hv_percentile = float("Nan")
    hv_min =float("Nan")
    hv_max = float("Nan")
    hv_mean = float("Nan")
    days_covered = 0
    
        
    # Check if we have IV data
    if iv_df is not None and len(iv_df) > 0: 
      # Analyze implied volatility
      current_iv = iv_df['close'].iloc[-1]
      iv_percentile = 100 * (iv_df['close'] <= current_iv).mean()
      iv_min = iv_df['close'].min()
      iv_max = iv_df['close'].max()
      iv_mean = iv_df['close'].mean()

      # Calculate actual days covered
      first_date = pd.to_datetime(iv_df['date'].iloc[0])
      last_date = pd.to_datetime(iv_df['date'].iloc[-1])
      days_covered = (last_date - first_date).days + 1
    
    # Process HV data if requested and available
    if hist: 
      hv_df = None
      if hv_bars is not None:
        hv_df = util.df(hv_bars)
        # Analyze realized volatility
        if len(hv_df) > 0:
          current_hv = hv_df['close'].iloc[-1]
          hv_percentile = 100 * (hv_df['close'] <= current_hv).mean()
          hv_min = hv_df['close'].min()
          hv_max = hv_df['close'].max()
          hv_mean = hv_df['close'].mean()
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
        'data_points': len(iv_df),
        'bar_size': bar_size,
        
        'current_iv': current_iv,
        'iv_percentile': iv_percentile,
        'iv_min': iv_min,
        'iv_max': iv_max,
        'iv_mean': iv_mean,
        
        'current_hv': current_hv,
        'hv_percentile': hv_percentile,
        'hv_min': hv_min,
        'hv_max': hv_max,
        'hv_mean': hv_mean
    }
