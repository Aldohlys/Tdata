import datetime
import xml.etree.ElementTree as ET
import calendar
import math
from ib_insync import Contract

# Import from other modules
from .core import CONFIG, ticker_db
from .IB_connection import safe_ib_connect

def getNTMDividend(symbol, secType=None, currency=None, exchange = None):
    
    ticker_info = ticker_db.get_ticker_info(symbol)
    
    if secType is None :
      secType= ticker_info['Type']
    
    if currency is None :
      currency= ticker_info['Currency']

    if exchange is None :
      exchange= ticker_info['Exchange']
      
    contract = Contract(
        secType=secType,
        symbol=symbol,
        currency=currency,
        exchange=exchange
    )
    
    # Connect to Interactive Brokers
    ib = safe_ib_connect(silent=True)
    
    # Return -1 if connection failed
    if not ib.isConnected():
        return -1
  
    d = get_NTM_dividend(ib, contract)
    
    ib.disconnect()
    return d

def get_NTM_dividend(ib, contract):
    """
    Get the annual dividend for a given security, and try a guess for index options
    
    Args:
        ib: IB connection object
        contract: The contract for the  security
        
    Returns:
        float: Next or Past 12 months dividend per share
    """
    
    try:
        ib.qualifyContracts(contract)
        
        # Try to get dividend data from ticker: expected value for 12 coming months
        ticker = ib.reqMktData(contract, '456')
        ib.sleep(1)
        
        return ticker.dividends.next12Months
        
    except Exception as e:
        print(f"Error getting next 12 months dividend from ticker: {e}")
        
        try:
            return ticker.dividends.past12Months
          
        except Exception as e:
            print(f"Error getting past 12 months dividend from ticker: {e}")

            # Extract symbol from the contract
            symbol = contract.symbol
    
            # Fallback: use typical values for common symbols
            if symbol in ['ESTX50', 'SX5E']:
                return 142  # ESTX50: ~3.0% = 2.86% as of 20.04.2025
            elif symbol in ['SPY']:
                return 7.1655  # S&P 500 ETFs: ~1.5% Quarterly dividend: 1.6955+1.9655+1.7455+1.759=7.1655
            elif symbol in ['QQQ']:
                return 1.63  # Nasdaq ETF: ~0.5%
            elif symbol in ['IWM']:
                return 0.012  # Russell 2000 ETF: ~1.2%
            elif symbol in ["SLV", "GLD", "USO"]:
                return 0
            else:
                return -1   # Don't know answer



