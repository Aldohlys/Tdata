import datetime
import xml.etree.ElementTree as ET
import calendar
import math
import time
from ib_insync import Contract

# Import from other modules
from .core import CONFIG, ticker_db
from .IB_connection import safe_ib_connect
from fin_logger import get_logger, log_execution_time

# Créez un logger pour ce module
logger = get_logger("tdata_py.dividend")

@log_execution_time
def getNTMDividend(symbol, secType=None, currency=None, exchange = None):
    """
    Get the next 12 month dividend for a symbol
    
    Args:
        symbol: Stock symbol
        secType: Security type
        currency: Currency code
        exchange: Exchange name
        
    Returns:
        float: Dividend value or -1 if not available
    """

    ticker_info = ticker_db.get_ticker_info(symbol)
    
    if ticker_info is None:
        logger.warning(f"No ticker info found for {symbol}, using defaults")
        if secType is None: secType = 'STK'
        if currency is None: currency = 'USD'
        if exchange is None: exchange = 'SMART'
    else:
        if secType is None: secType = ticker_info.get('Type')
        if currency is None: currency = ticker_info.get('Currency')
        if exchange is None: exchange = ticker_info.get('Exchange')
    
    # Log function call
    logger.info("Getting dividend data", {
        "symbol": symbol,
        "secType": secType,
        "currency": currency,
        "exchange": exchange
    })
    
    contract = Contract(
        secType=secType,
        symbol=symbol,
        currency=currency,
        exchange=exchange
    )
    
    # Connect to Interactive Brokers
    ib = safe_ib_connect()
    
    # Return -1 if connection failed
    if not ib.isConnected():
        logger.error("Failed to connect to Interactive Brokers")
        return -1
  
    d = get_NTM_dividend(ib, contract)
    
    ib.disconnect()
    
    logger.info("Dividend data retrieved", {
        "symbol": symbol,
        "dividend": d
    })
    
    return d

@log_execution_time
def get_NTM_dividend(ib, contract):
    """
    Get the annual dividend for a given security, and try a guess for index options

    Args:
        ib: IB connection object
        contract: The contract for the  security

    Returns:
        float: Next or Past 12 months dividend per share
    """
    symbol = contract.symbol
    logger.debug("Requesting dividend data", {"symbol": symbol})
    dividend = -1  # Initialize with "unknown" value
    
    try:
        ib.qualifyContracts(contract)
        
        # Try to get dividend data from ticker: expected value for 12 coming months
        ticker = ib.reqMktData(contract, '456')
        ib.sleep(1)
        dividend = ticker.dividends.next12Months

        # Check if dividend is None or NaN
        if dividend is None or (isinstance(dividend, float) and math.isnan(dividend)):
            raise ValueError("No next 12 months dividend data available")
        
    except Exception as e:
        logger.warn("Error getting next 12 months dividend", {"error": str(e)})
        
        try:
            dividend = ticker.dividends.past12Months
            logger.debug("Using past 12 months dividend", {"dividend": dividend})

            # Check if past dividend is also None or NaN
            if dividend is None or (isinstance(dividend, float) and math.isnan(dividend)):
                raise ValueError("No past 12 months dividend data available")
            
        except Exception as e:
            logger.warn("Error getting past 12 months dividend", {"error": str(e)})

            # Fallback logic: use typical values for common symbols
            logger.debug("Using fallback dividend values for symbol", {"symbol": symbol})
            
            if symbol in ['ESTX50', 'SX5E']:
                logger.info("Using fallback dividend for ESTX50", {"dividend": 142})
                dividend = 142  # ESTX50: ~3.0% = 2.86% as of 20.04.2025
            elif symbol in ['SPY']:
                logger.info("Using fallback dividend for SPY", {"dividend": 7.1655})
                dividend =  7.1655  # S&P 500 ETFs: ~1.5% Quarterly dividend
            elif symbol in ['QQQ']:
                logger.info("Using fallback dividend for QQQ", {"dividend": 1.63})
                dividend =  1.63  # Nasdaq ETF: ~0.5%
            elif symbol in ['IWM']:
                logger.info("Using fallback dividend for IWM", {"dividend": 0.012})
                dividend =  0.012  # Russell 2000 ETF: ~1.2%
            elif symbol in ["SLV", "GLD", "USO"]:
                logger.info("Using zero dividend for commodity ETF", {"symbol": symbol})
                dividend =  0
            else:
                logger.warn("No dividend data available for symbol", {"symbol": symbol})
                dividend =  -1  # Don't know answer

    finally:
        ib.cancelMktData(contract)  # Cancel the subscription
        return dividend
