from ib_async import *
from tdata_py.IB_connection import safe_ib_connect
import numpy as np
import pandas as pd
from tdata_py.core import ticker_db, CONFIG

def get_iv_percentile(sym, lookback_days=252):
    # Connect to TWS
    ib = safe_ib_connect()
    
   # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if ticker_info is None:
        logger.warning(f"No ticker info found for {sym}, using defaults")
        if secType is None: secType = 'STK'
        if currency is None: currency = 'USD'
        if exchangeSec is None: exchangeSec = "SMART"
        if exchangeOpt is None: exchangeOpt = 'SMART'
        if tradingClass is None: tradingClass = sym
    else:
        if secType is None: secType = ticker_info.get('Type')
        if currency is None: currency = ticker_info.get('Currency')
        if exchangeSec is None: exchangeSec = ticker_info.get('exchangeSec')
        if exchangeOpt is None: exchangeOpt = ticker_info.get('OptExchange')
        if tradingClass is None: tradingClass = ticker_info.get('tradingClass')

    logger.info(f"Argts: sym:{sym}, trading class:{tradingClass}, expiration:{expdate}, exchange_opt:{exchangeOpt}, exchange_sec:{exchangeSec}")
    
    # Define the underlying stock contract
    contract = Stock(sym, exchangeSec, currency)
    ib.qualifyContracts(contract)
    
    # Request historical implied volatility data
    bars = ib.reqHistoricalData(
        contract,
        endDateTime='',  # '' means now
        durationStr=f'{lookback_days} D',
        barSizeSetting='1 day',
        whatToShow='OPTION_IMPLIED_VOLATILITY',
        useRTH=True,
        formatDate=1
    )
    
    ib.sleep(1)
    ib.disconnect()
    
    # Convert to DataFrame for easier manipulation
    df = util.df(bars)
    
    if df.empty:
        print(f"No implied volatility data available for {symbol}")
        ib.disconnect()
        return None
    
    # Get current IV
    # For current IV we can use the last value from our historical data
    current_iv = df['close'].iloc[-1]
    
    # Calculate percentile
    iv_percentile = 100 * (df['close'] <= current_iv).mean()
    
    # Get min and max values for context
    iv_min = df['close'].min()
    iv_max = df['close'].max()
    iv_rank = (current_iv - iv_min)/(iv_max - iv_min)
    iv_mean = df['close'].mean()
    iv_median = df['close'].median()
    iv_std = df['close'].std()    
    
    ib.disconnect()
    
    return {
        'sym': sym,
        'current_iv': current_iv,
        'iv_percentile': iv_percentile,
        'iv_rank': iv_rank,
        'iv_min': iv_min,
        'iv_max': iv_max,
        'iv_mean': iv_mean,
        'iv_median': iv_median,
        'iv_std': iv_std,
    }

# Example usage
if __name__ == "__main__":
    lookback_days = 50
    result = get_iv_percentile('URA', lookback_days=lookback_days)
    if result:
        print(f"Sym: {result['sym']} on {lookback_days} days")
        print(f"Current IV: {100*result['current_iv']:.2f}%")
        print(f"IV Percentile: {result['iv_percentile']:.2f}%")
        print(f"IV Rank: {100*result['iv_rank']:.2f}%")
        print(f"IV Range: {100*result['iv_min']:.2f}% - {100*result['iv_max']:.2f}%")
        print(f"IV Mean: {100*result['iv_mean']:.2f}%")
        print(f"IV Median: {100*result['iv_median']:.2f}%")
        print(f"IV Standard deviation: {100*result['iv_std']:.2f} points")
