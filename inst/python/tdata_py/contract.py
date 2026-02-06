
#' Live	1	Live market data is streaming data relayed back in real time. 
#' Market data subscriptions are required to receive live market data.
#' 
#' Frozen	2	Frozen market data is the last data recorded at market close. 
#' In TWS, Frozen data is displayed in gray numbers. 
#' When you set the market data type to Frozen, you are asking TWS to send the last available quote
#' when there is not one currently available. 
#' For instance, if a market is currently closed and real time data is requested, 
#' -1 values will commonly be returned for the bid and ask prices to indicate
#' there is no current bid/ask data available. 
#' TWS will often show a 'frozen' bid/ask which represents the last value recorded by the system. 
#' To receive the last know bid/ask price before the market close, 
#' switch to market data type 2 from the API before requesting market data. 
#' API frozen data requires TWS/IBG v.962 or higher 
#' and the same market data subscriptions necessary for real time streaming data.
#' 
#' Delayed	3	
#' Free, delayed data is 15 - 20 minutes delayed.
#' In TWS, delayed data is displayed in brown background.
#'  When you set market data type to delayed, you are telling TWS to automatically switch to delayed market data
#'  if the user does not have the necessary real time data subscription.
#'  If live data is available a request for delayed data would be ignored by TWS. 
#'  Delayed market data is returned with delayed Tick Types (Tick ID 66~76).
#'  Note: TWS Build 962 or higher is required and API version 9.72.18 or higher is suggested.
#'  
#'  Delayed Frozen	4	Requests delayed "frozen" data for a user without market data subscriptions.
# 

import sys
import math
import datetime
import locale
import pandas as pd
import logging
from ib_async import *

# Import from other modules
from .core import CONFIG, ticker_db, validate_contract_params, find_nearest_number
from .IB_connection import safe_ib_connect
from .chains_manager import getChains, getAllStrikes
from .dividend_utils import getNTMDividend
from fin_logger import get_logger, log_execution_time

# Créez un logger pour ce module
logger = get_logger("tdata_py.contract")

@log_execution_time
def getValue(list_sym, secType=None, exchange=None, currency=None, expiration=None, reqType=2, conId=None, ib=None):
    """
    Get most recent market value for one or multiple securities from Interactive Brokers.
    Uses TickerDatabase to automatically determine security details.

    Args:
        list_sym (str or list): Symbol(s)
        reqType (int): Market data type (1=Live, 2=Frozen, 3=Delayed, 4=Delayed Frozen)
                      Default 2 (Frozen) returns most recent close when market is closed

    Returns:
        DataFrame or int:
            - DataFrame with datetime, symbol and price if successful
            - -1 if contract does not exist
            - 0 if connection error
    """
    # CRITICAL: Save original locale to restore it on exit
    # Without this, locale changes leak into R and break JSON serialization
    # Note: LC_ALL not supported on Windows, use LC_NUMERIC instead
    original_locale = locale.getlocale(locale.LC_NUMERIC)
    try:
        # Set locale for consistent formatting (only within this function)
        locale.setlocale(locale.LC_NUMERIC, '')

        # Convert single symbol to list format for uniform processing
        if not isinstance(list_sym, list):
            symbols = [list_sym]
        else:
            symbols = list_sym

        # Create contracts using ticker database for each symbol
        contracts = []

        for sym in symbols:

            ### Initialize values
            ticker_info = ticker_db.get_ticker_info(sym)
            sym_secType = secType
            sym_expiration = expiration
            sym_currency = currency
            sym_exchange = exchange
            sym_conId = conId

            if ticker_info is None:
                if sym_conId is None:
                    logger.warning(f"No ticker info found for {sym}, using defaults")
                    if sym_secType is None: sym_secType = 'STK'
                    if sym_currency is None: sym_currency = 'USD'
                    if sym_exchange is None: sym_exchange = 'SMART'
                else: logger.warning(f"Using contract Id: {sym_conId}")
            ### Ticker present in DB
            else:
                if sym_secType is None: sym_secType = ticker_info.get('Type')
                if sym_currency is None: sym_currency = ticker_info.get('Currency')
                if sym_exchange is None: sym_exchange = ticker_info.get('Exchange')
                if sym_expiration is None:
                    sym_expiration = ticker_info.get('Expiration')
                    if not(math.isnan(sym_expiration)): sym_expiration = int(sym_expiration)
                if sym_conId is None:
                    db_conId = ticker_info.get('ConId')
                    # Check if ticker has ConId (for ISIN-based securities like funds or futures)
                    if db_conId is not None and not (isinstance(db_conId, float) and math.isnan(db_conId)):
                        sym_conId = int(db_conId)

            # Log function call
            logger.info("getValue data", {
                "symbol": sym,
                "secType": sym_secType,
                "currency": sym_currency,
                "exchange": sym_exchange,
                "expiration": sym_expiration,
                "conId": sym_conId
            })

            if (sym_secType == "FUT"):
              if sym_conId is not None and not (isinstance(sym_conId, float) and math.isnan(sym_conId)):
              ### General solution, use contract Id - no need for symbol
                contract = Future(currency = sym_currency, exchange = sym_exchange, conId=sym_conId)
              ## Backup solution: sym used as local symbol
              else: contract = Future(localSymbol = sym, lastTradeDateOrContractMonth = sym_expiration, currency = sym_currency, exchange = sym_exchange)

            ### For T-Bill - conId is the symbol name (like in Reporting)
            elif (sym_secType == "BILL"):
              contract = Contract(secType=sym_secType, conId=sym, exchange=sym_exchange)

            ### If ConId is provided and different from nan, use it (for ISIN-based securities)
            elif (sym_conId is not None and not (isinstance(sym_conId, float) and math.isnan(sym_conId))):
              # Fund platforms (ALLFUNDS, EBS) require primary exchange routing, not SMART
              # Stock exchanges (LSEETF, etc.) work better with SMART routing
              fund_platforms = ['ALLFUNDS', 'EBS']
              routing_exchange = sym_exchange if sym_exchange in fund_platforms else 'SMART'
              contract = Contract(secType=sym_secType, conId=sym_conId, exchange=routing_exchange, currency=sym_currency)
              logger.info(f"Using ConId {sym_conId} for {sym} (type: {sym_secType}) with {routing_exchange} routing")                
            ### Other cases - use symbol
            else : contract = Contract(secType = sym_secType, symbol = sym, currency = sym_currency, exchange = sym_exchange)

            contracts.append(contract)

        ### Print info


        # Connect to Interactive Brokers if ib has not been given as argument
        ## If ib has been given as argument - do not disconnect it
        disconnect = True
        if (ib is None): ib = safe_ib_connect()
        else: disconnect= False

        # Return 0 if connection failed
        if not ib.isConnected():
            return 0

        try:
            # Try to qualify contracts
            if ib.qualifyContracts(*contracts):

                logger.info("Contracts: ", {"Contracts": [contract.conId for contract in contracts]})

                # Set market data type based on parameter
                ib.reqMarketDataType(int(reqType))

                # Request tickers for the contracts
                tickers = ib.reqTickers(*contracts)
                logger.info("Retrieve IBKR market price for all contracts")

                # Allow time for data retrieval (BEFORE accessing ticker data)
                ib.sleep(0.5)

                # Get most recent market price
                # For funds: marketPrice() returns nan, use close price instead
                values = []
                for ticker in tickers:
                    price = ticker.marketPrice()
                    if price != price:  # Check for nan
                        price = ticker.close
                    values.append(price)

                # Create timestamp for all data points
                current_time = datetime.datetime.now().strftime("%Y%m%d %H:%M")

                # Create and return DataFrame with results
                df = pd.DataFrame({
                    "datetime": [current_time] * len(values),
                    "sym": symbols,
                    "price": [round(value, 4) for value in values]
                })

                return df
            else:
                # Contracts couldn't be qualified
                logger.error("contracts could not be qualified:", {"contracts":contracts})
                return -1
        except Exception as e:
            # Handle any unexpected errors
            logger.error("Error getting values:", e)
            return -1
        finally:
            # Ensure connection is always closed - except if ib was given as parameter
            if disconnect:
                ib.disconnect()
    finally:
        # CRITICAL: Always restore original locale
        # This ensures locale changes don't leak out and break JSON serialization in R
        try:
            locale.setlocale(locale.LC_NUMERIC, original_locale)
        except Exception as e:
            logger.warning(f"Could not restore original locale: {e}")

def getOptValue(sym, expiration, strikes, right, currency=None, exchange=None, tradingClass=None):
    """
    Get option values, implied volatility and delta for one or multiple strikes.

    Args:
        sym (str): Option's underlying symbol
        expiration (str): Option expiration date in format YYYYMMDD
        strikes (float or list): Strike price(s)
        right (str): Option right ('C' for call, 'P' for put)
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchange (str, optional): Exchange name. If None, uses value from ticker database.
        tradingClass (str, optional): Trading class. If None, uses value from ticker database.

    Returns:
        DataFrame or None:
            DataFrame with strike, value, impliedvol, delta columns if successful
            None if connection error or contract doesn't exist
    """
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if ticker_info is None:
        logger.warning(f"No ticker info found for {sym}, using defaults")
        secType = 'STK'
        secOptType = "OPT"
        if currency is None: currency = 'USD'
        if exchange is None: exchange = 'SMART'
        if tradingClass is None: tradingClass = sym
    else:
        secType = ticker_info.get('Type')
        match secType:
            case "FUT":
              secOptType = "FOP"
              sym = ticker_info.get('YahooName', sym)
            case "STK" | "IND":
              secOptType = "OPT"
            case _:
              secOptType = None
        if currency is None: currency = ticker_info.get('Currency')
        if exchange is None: exchange = ticker_info.get('OptExchange')
        if tradingClass is None: tradingClass = ticker_info.get('TradingClass')
        
    if (secOptType is None):
        logger.error(f"Not possible to use derivative for {sym}, returning no value")
        return None      
      
      # Log function call
    logger.info("getOptValue data", {
        "symbol": sym,
        "secOptType": secOptType,
        "exchangeOpt": exchange,
        "tradingClass": tradingClass,
        "expiration": expiration,
        "strikes": strikes,
        "right": right,
        "currency": currency
    })
    
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected():
        return None
       
    # Convert single strike to list if needed
    if (type(strikes) != list): 
        strikes = [strikes]
        
    # Create option contracts for each strike
    
    contracts = [Contract(symbol=sym, secType=secOptType, lastTradeDateOrContractMonth=expiration,
                        strike=strike_c, right=right,
                        exchange=exchange, currency=currency, tradingClass=tradingClass) 
                for strike_c in strikes]
    
    logger.debug("CONTRACTS:", {"contracts":contracts})

    if(ib.qualifyContracts(*contracts)):
        # Use frozen market data (type 2) for consistent pricing
        ib.reqMarketDataType(2)
        
        tickers = ib.reqTickers(*contracts)
        ib.sleep(0.25)
        
        logger.debug("TICKERS:", {"tickers":tickers})
        
        # Handle potential None values in model Greeks - ROBUST VERSION
        result_dic = []
        for ticker, strike in zip(tickers, strikes):
            # Safe market price calculation
            market_price = ticker.marketPrice() if not math.isnan(ticker.marketPrice()) else ticker.close
            
            # Safe bid/ask/last calculation
            bid_price = ticker.bid if ticker.bid != -1 and not math.isnan(ticker.bid) else None
            ask_price = ticker.ask if ticker.ask != -1 and not math.isnan(ticker.ask) else None
            last_price = ticker.last if ticker.last is not None and not math.isnan(ticker.last) else None
            
            # Safe spread calculation
            if bid_price is not None and ask_price is not None and (bid_price + ask_price) != 0:
                spread = 2 * (ask_price - bid_price) / (ask_price + bid_price)
            else:
                spread = float('nan')
            
            # Safe Greeks calculation with nested None checks
            implied_vol = float('nan')
            delta = float('nan')
            
            if ticker.modelGreeks is not None:
                if ticker.modelGreeks.impliedVol is not None and not math.isnan(ticker.modelGreeks.impliedVol):
                    implied_vol = ticker.modelGreeks.impliedVol
                if ticker.modelGreeks.delta is not None and not math.isnan(ticker.modelGreeks.delta):
                    delta = ticker.modelGreeks.delta
            
            # Build result dictionary with safe rounding
            # Use 5 decimal places for prices to support forex futures options (6S, 6E, etc.)
            # where prices can be very small (e.g., 0.035)
            row = {
                "strike": strike,
                "value": round(market_price, 5) if market_price is not None and not math.isnan(market_price) else float('nan'),
                "bid": round(bid_price, 5) if bid_price is not None else float('nan'),
                "ask": round(ask_price, 5) if ask_price is not None else float('nan'),
                "last": round(last_price, 5) if last_price is not None else float('nan'),
                "spread": round(spread, 5) if not math.isnan(spread) else float('nan'),
                "impliedvol": round(implied_vol, 4) if not math.isnan(implied_vol) else float('nan'),
                "delta": round(delta, 3) if not math.isnan(delta) else float('nan')
            }
            
            result_dic.append(row)
        
        # Convert to pandas DataFrame
        result = pd.DataFrame(result_dic)
        
    else:
        result = None 
    
    ib.disconnect()
    return result

def getStraddleValue(sym, expiration, strike, currency=None, exchange=None, tradingClass=None):
    """
    Get the combined value of a put and call straddle at the same strike.
    
    Args:
        sym (str): Option's underlying symbol
        expiration (str): Option expiration date in format YYYYMMDD
        strike (float): Strike price
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchange (str, optional): Exchange name. If None, uses value from ticker database.
        tradingClass (str, optional): Trading class. If None, uses value from ticker database.
        
    Returns:
        float or None: Combined value of put and call options if successful, None otherwise
    """
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)

    if ticker_info is None:
        logger.warning(f"No ticker info found for {sym}, using defaults")
        secOptType = "OPT"
        if currency is None: currency = 'USD'
        if exchange is None: exchange = 'SMART'
        if tradingClass is None: tradingClass = sym
    else:
        match ticker_info.get('Type'):
            case "FUT":
              secOptType = "FOP"
              sym = ticker_info.get('YahooName', sym)
            case "STK" | "IND":
              secOptType = "OPT"
            case _:
              secOptType = None
        if currency is None: currency = ticker_info.get('Currency')
        if exchange is None: exchange = ticker_info.get('OptExchange')
        if tradingClass is None: tradingClass = ticker_info.get('TradingClass')
    
    if (secOptType is None):
        logger.error(f"Not possible to use derivative for {sym}, returning no value")
        return None

    # Log function call
    logger.info("getStraddleValue data", {
        "symbol": sym,
        "secOptType": secOptType,
        "expiration": expiration,
        "strike": strike,
        "currency": currency,
        "exchange": exchange
    })
    
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return None

    # Create put and call contracts with same strike and expiration
    contract1 = Contract(symbol=sym, secType=secOptType, lastTradeDateOrContractMonth=expiration,
                      strike=strike, right="Put", exchange=exchange, currency=currency, tradingClass=tradingClass)
    contract2 = Contract(symbol=sym, secType=secOptType, lastTradeDateOrContractMonth=expiration,
                      strike=strike, right="Call", exchange=exchange, currency=currency, tradingClass=tradingClass)
    
    print("Contract:", contract1, contract2)
    contract = [contract1, contract2]
    
    if(ib.qualifyContracts(*contract)):
        # Use delayed frozen data (type 4) for more reliability
        ib.reqMarketDataType(4)
        ticker = ib.reqTickers(*contract)
        
        # Sum put and call values (5 decimals for forex futures options)
        value = round(ticker[0].marketPrice() + ticker[1].marketPrice(), 5)
        ib.sleep(1)
        
        if(math.isnan(value)):
            print("from IB: Opt price is NA")
            value = None
    else:
        value = None 
    
    ib.disconnect()
    return value

def getStrikesfromExpDate(sym, secType=None, currency=None, exchangeOpt=None, tradingClass=None, expdate=None, force_refresh=False):
    """
    Get available strikes for a specific option expiration date.
    
    Args:
        sym (str): Underlying symbol
        secType (str, optional): Security type. If None, uses value from ticker database.
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchangeOpt (str, optional): Exchange for options. If None, uses value from ticker database.
        tradingClass (str, optional): Trading class. If None, uses value from ticker database.
        expdate (str): Expiration date
        force_refresh (bool): If False, will query cache
        
    Returns:
        list or None: List of available strikes if successful, None if connection error
    """
    
    logger.info("Starting getStrikesfromExpDate function...")
    
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if ticker_info is None:
        logger.warning(f"No ticker info found for {sym}, using defaults")
        if secType is None: secType = 'STK'
        if currency is None: currency = 'USD'
        if exchangeOpt is None: exchangeOpt = 'SMART'
        if tradingClass is None: tradingClass = sym
    else:
        if secType is None: secType = ticker_info.get('Type')
        if currency is None: currency = ticker_info.get('Currency')
        if exchangeOpt is None: exchangeOpt = ticker_info.get('OptExchange')
        if tradingClass is None: tradingClass = ticker_info.get('tradingClass')

    logger.info(f"Argts: sym:{sym}, tradingClass:{tradingClass}, expdate:{expdate}, exchangeOpt:{exchangeOpt}")
       
    return getAllStrikes(sym, trading_class=tradingClass, expiration=expdate, exchangeOpt=exchangeOpt, force_refresh=False)

def getStrikesFromRange(sym, current_price, expdate, implied_volatility, force_refresh=False):
    """
    Get available strikes for a specific option expiration date, within a one sigma (implied volatility) range.
    
    Args:
        sym (str): Underlying symbol
        current_price (float): symbol current price
        expdate (str): Expiration date
        implied_volatility (float): Implied volatility
        force_refresh (bool): If False, will query cache
        
    Returns:
        list or None: List of available strikes if successful, None if connection error
    """
    ### Compute the time between now and option expiration time
    ### Assume 4:30pm as end time, and convert to years
    exp_date = datetime.datetime.strptime(expdate, "%Y%m%d")
    exp_datetime = exp_date.replace(hour=16, minute=30, second=0, microsecond=0)
    time_diff = exp_datetime - datetime.now()
    time_to_expiry = time_diff.total_seconds() / (365.25 * 24 * 3600)
    
    ## This assumes sym belongs to Ticker DB - next 12 months dividend
    NTM_dividend = getNTMDividend(sym)

    # Calculate expected dividends during the option's life
    expected_dividends = NTM_dividend * time_to_expiry
    
    # Adjust the current price for dividends
    adjusted_current_price = current_price - expected_dividends
    
    # Black-Scholes-Merton parameters
    S = adjusted_current_price  # Stock price adjusted for dividends
    r = getInterestRate(time_to_expiry*12, "USD") # Risk-free rate - getInterestRate takes months as input
    q = NTM_dividend/current_price           # Dividend yield
    sigma = implied_volatility          # Volatility
    T = time_to_expiry          # Time to expiry in years
    
    # Calculate the range using the adjusted Black-Scholes formula
    adjusted_sigma_range = sigma * math.sqrt(T) * sigma_multiplier
    
    # Apply the dividend and interest rate adjustments
    lower_bound = S * math.exp((r - q - 0.5 * sigma**2) * T - adjusted_sigma_range)
    upper_bound = S * math.exp((r - q - 0.5 * sigma**2) * T + adjusted_sigma_range)
    
    return getAllStrikes(
      sym,
      trading_class=tradingClass, 
      expiration=expdate, 
      strike_min=lower_bound,
      strike_max=upper_bound, 
      exchangeOpt=exchangeOpt, 
      force_refresh=False
    )
