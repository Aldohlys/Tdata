import datetime
import xml.etree.ElementTree as ET
import calendar
import math
from ib_insync import Contract

# Import from other modules
from .core import CONFIG, ticker_db
from .IB_connection import safe_ib_connect

def get_TTM_dividend(ib, underlying_contract):
    """
    Get the annual dividend for a given underlying security - does not work for index options
    
    Args:
        ib: IB connection object
        underlying_contract: The contract for the underlying security
        
    Returns:
        float: TTM dividend per share
    """
    # Extract symbol from the contract
    symbol = underlying_contract.symbol
    
    try:
        # Try to get fundamental data for dividend yield
        ib.qualifyContracts(underlying_contract)
        dividend_data = ib.reqFundamentalData(underlying_contract, 'ReportSnapshot')
        
        # Parse XML
        root = ET.fromstring(dividend_data)
        
        # Get dividend per share
        # This value represents the sum of all dividends paid over the past 12 months
        # (TTM - trailing twelve months) or the annual projected dividend
        div_per_share_elem = root.find(".//Ratio[@FieldName='ADIVSHR']")
        
        if div_per_share_elem is not None and div_per_share_elem.text:
            return float(div_per_share_elem.text)
        
        ### dividend not found
        return float('NaN')
    
    except Exception as e:
        print(f"Error getting dividend yield from fundamental data: {e}")
        
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



def getTTMDividend(symbol, secType=None, currency=None, exchange = None):
    
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
    
    # Return 0.0125 if connection failed
    if not ib.isConnected():
        return 0.0125
  
    d = get_TTM_dividend(ib, contract)
    
    ib.disconnect()
    return d

