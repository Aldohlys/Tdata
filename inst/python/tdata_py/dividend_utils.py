import datetime
import xml.etree.ElementTree as ET
import calendar
import math
from ib_insync import Contract

# Import from other modules
from .core import CONFIG, ticker_db
from .IB_connection import safe_ib_connect

def get_dividend_yield(ib, underlying_contract):
    """
    Get the dividend yield for a given underlying security
    
    Args:
        ib: IB connection object
        underlying_contract: The contract for the underlying security
        
    Returns:
        float: The dividend yield as a decimal (e.g., 0.015 for 1.5%)
    """
    # Extract symbol from the contract
    symbol = underlying_contract.symbol
    
    try:
        # Try to get fundamental data for dividend yield
        dividend_data = ib.reqFundamentalData(underlying_contract, 'ReportSnapshot')
        
        # Parse XML
        root = ET.fromstring(dividend_data)
        
        # Get dividend per share
        div_per_share_elem = root.find(".//Ratio[@FieldName='ADIVSHR']")
        
        if div_per_share_elem is not None and div_per_share_elem.text:
            div_per_share = float(div_per_share_elem.text)
            
            # Get current price
            price_elem = root.find(".//Ratio[@FieldName='NPRICE']")
            if price_elem is not None and price_elem.text:
                price = float(price_elem.text)
                
                # Calculate yield
                if price > 0:
                    div_yield = div_per_share / price
                    print(f"Calculated dividend yield: {div_yield*100:.2f}% (from DPS {div_per_share} / Price {price})")
                    return div_yield
            
            # If price is not available, try to get market price
            ticker = ib.reqMktData(underlying_contract)
            ib.sleep(1)
            current_price = ticker.marketPrice()
            ib.cancelMktData(underlying_contract)
            
            if current_price > 0:
                div_yield = div_per_share / current_price
                print(f"Calculated dividend yield: {div_yield*100:.2f}% (from DPS {div_per_share} / Current Price {current_price})")
                return div_yield
    except Exception as e:
        print(f"Error getting dividend yield from fundamental data: {e}")
    
    # Fallback: Try to get annual dividend from dividend history
    try:
        # Get dividend history
        end_date = datetime.datetime.now().strftime("%Y%m%d")
        div_history = ib.reqHistoricalData(
            underlying_contract,
            end_date,
            "1 Y",  # One year of history
            "1 day",
            "DIVIDENDS",
            useRTH=True
        )
        
        if div_history:
            # Sum up dividends over past year
            annual_div = sum(bar.close for bar in div_history)
            
            # Get current price
            ticker = ib.reqMktData(underlying_contract)
            ib.sleep(1)
            current_price = ticker.marketPrice()
            ib.cancelMktData(underlying_contract)
            
            # Calculate yield
            if current_price > 0:
                div_yield = annual_div / current_price
                print(f"Dividend yield from historical dividends: {div_yield*100:.2f}%")
                return div_yield
    except Exception as e:
        print(f"Error getting dividend history: {e}")
    
    # Fallback: use typical values for common symbols
    if symbol in ['ESTX50', 'ESTX50-EUR', 'SX5E']:
        return 0.025  # ESTX50: ~2.5%
    elif symbol in ['SPY', 'IVV']:
        return 0.015  # S&P 500 ETFs: ~1.5%
    elif symbol in ['DIA']:
        return 0.018  # Dow Jones ETF: ~1.8%
    elif symbol in ['QQQ']:
        return 0.007  # Nasdaq ETF: ~0.7%
    elif symbol in ['IWM']:
        return 0.012  # Russell 2000 ETF: ~1.2%
    else:
        return 0.02   # Generic fallback: 2%

def getDividend(symbol):
    
    ticker_info = ticker_db.get_ticker_info(symbol)
    
    contract = Contract(
        secType=ticker_info['Type'],
        symbol=symbol,
        currency=ticker_info['Currency'],
        exchange=ticker_info['Exchange']
    )
    
    # Connect to Interactive Brokers
    ib = safe_ib_connect(silent=True)
    
    # Return 0.0125 if connection failed
    if not ib.isConnected():
        return 0.0125
  
    get_dividend_yield(ib, contract)
    
    ib.disconnect()
