
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
import json
import logging
from ib_insync import *

# Import from other modules
from .core import CONFIG, ticker_db, validate_contract_params, find_nearest_number
from .IB_connection import safe_ib_connect


def getValue(list_sym, reqType=2, ib=None, close=True, silent=True):
    """
    Get current or close value for one or multiple securities from Interactive Brokers.
    Uses TickerDatabase to automatically determine security details.
    
    Args:
        list_sym (str or list): Symbol(s)
        reqType (int): Market data type (1=Live, 2=Frozen, 3=Delayed, 4=Delayed Frozen)
        close (bool): If True, returns close price; otherwise returns current price
        silent: If True, does not print comments otherwise it does
        
    Returns:
        DataFrame or int: 
            - DataFrame with datetime, symbol and price if successful
            - -1 if contract does not exist
            - 0 if connection error
    """
    # Set locale for consistent formatting
    locale.setlocale(locale.LC_ALL, '')
    
    # Convert single symbol to list format for uniform processing
    if not isinstance(list_sym, list):
        symbols = [list_sym]
    else:
        symbols = list_sym
    
    # Create contracts using ticker database for each symbol
    contracts = []
    for sym in symbols:
        ticker_info = ticker_db.get_ticker_info(sym)
        secType = ticker_info['Type']
        currency = ticker_info['Currency']
        exchange = ticker_info['Exchange']

        ## For futures only sym used as local symbol
        if (secType == "FUT"):
          contract = Contract(secType=secType, localSymbol = sym, currency = currency, exchange = exchange)
        
        ### For T-Bill - conId is the symbol name (like in Reporting)
        elif (secType == "BILL"):
          contract = Contract(secType=secType, conId=sym, exchange=exchange)

        ### Other cases
        else : contract = Contract(secType = secType, symbol = sym, currency = currency, exchange = exchange)

        contracts.append(contract)
    
    ### Print info
    if (not silent): print("\nContracts: ", contracts)
    
    # Connect to Interactive Brokers if ib has not been given as argument
    ## If ib has been given as argument - do not disconnect it
    disconnect = True
    if (ib is None): ib = safe_ib_connect(silent=silent)
    else: disconnect= False
    
    # Return 0 if connection failed
    if not ib.isConnected():
        return 0
    
    try:
        # Try to qualify contracts
        if ib.qualifyContracts(*contracts):
            # Set market data type based on parameter
            ib.reqMarketDataType(int(reqType))
            
            # Request tickers for the contracts
            tickers = ib.reqTickers(*contracts)
            print("\nRetrieve IBKR market price for ", list_sym, " contracts...")
            
            # Get price data based on close parameter
            if close:
                values = [ticker.close for ticker in tickers]
            else:
                values = [ticker.marketPrice() for ticker in tickers]
            
            # Allow time for data retrieval
            ib.sleep(1)
            
            # Create timestamp for all data points
            current_time = datetime.datetime.now().strftime("%Y%m%d %H:%M")
            
            # Create and return DataFrame with results
            df = pd.DataFrame({
                "datetime": [current_time] * len(values),
                "sym": symbols,
                "price": values
            })
            
            return df
        else:
            # Contracts couldn't be qualified
            return -1
    except Exception as e:
        # Handle any unexpected errors
        import logging
        logging.error(f"Error getting values: {str(e)}")
        return -1
    finally:
        # Ensure connection is always closed - except if ib was given as parameter
        if disconnect:
            ib.disconnect()

def getOptValue(sym, expiration, strikes, right, currency=None, exchange=None, tradingClass=None, silent=True):
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
    
    ## Check taht it is possible to have options on sym
    sym_type = ticker_info.get('Type', 'STK')
    if not (sym_type in ["FUT", "STK", "IND"]):
        return None
    
    YahooName =  ticker_info.get('YahooName', sym)
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
    
    if exchange is None:
        exchange = ticker_info.get('OptExchange', 'SMART')
        
    if tradingClass is None:
        tradingClass = ticker_info.get('TradingClass', sym)
    
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect(silent=silent)

    # If connection not available return None
    if not ib.isConnected():
        return None
       
    # Convert single strike to list if needed
    if (type(strikes) != list): 
        strikes = [strikes]
        
    # Create option contracts for each strike
    
    if (sym_type == "FUT"):
      contracts = [Contract(symbol=YahooName, secType="FOP", lastTradeDateOrContractMonth=expiration,
                        strike=strike_c, right=right,
                        exchange=exchange, currency=currency, tradingClass=tradingClass) 
                for strike_c in strikes]
    
    else :
      contracts = [Contract(symbol=sym, secType="OPT", lastTradeDateOrContractMonth=expiration,
                        strike=strike_c, right=right,
                        exchange=exchange, currency=currency, tradingClass=tradingClass) 
                for strike_c in strikes] 
    
    if (not silent): print("\nCONTRACTS:", contracts)
    
    if(ib.qualifyContracts(*contracts)):
        # Use frozen market data (type 2) for consistent pricing
        ib.reqMarketDataType(2)
        
        tickers = ib.reqTickers(*contracts)
        if (not silent): print("\nTICKERS:", tickers)
        
        # Handle potential None values in model Greeks
        result_dic = [{
               "strike": strike, 
               "value": ticker.marketPrice() if not(math.isnan(ticker.marketPrice())) else ticker.close, 
               "bid": ticker.bid,
               "ask": ticker.ask,
               "impliedvol": ticker.modelGreeks.impliedVol if ticker.modelGreeks is not None else None,
               "delta": ticker.modelGreeks.delta if ticker.modelGreeks is not None else None} 
              for ticker, strike in zip(tickers, strikes)]  
        
        # Convert to pandas DataFrame
        result = pd.DataFrame(result_dic)
        ib.sleep(1)
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
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
    
    if exchange is None:
        exchange = ticker_info.get('OptExchange', 'SMART')
        
    if tradingClass is None:
        tradingClass = ticker_info.get('TradingClass', sym)
    
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return None

    # Create put and call contracts with same strike and expiration
    contract1 = Contract(symbol=sym, secType="OPT", lastTradeDateOrContractMonth=expiration,
                      strike=strike, right="Put", exchange=exchange, currency=currency, tradingClass=tradingClass)
    contract2 = Contract(symbol=sym, secType="OPT", lastTradeDateOrContractMonth=expiration,
                      strike=strike, right="Call", exchange=exchange, currency=currency, tradingClass=tradingClass)
    
    print("Contract:", contract1, contract2)
    contract = [contract1, contract2]
    
    if(ib.qualifyContracts(*contract)):
        # Use delayed frozen data (type 4) for more reliability
        ib.reqMarketDataType(4)
        ticker = ib.reqTickers(*contract)
        
        # Sum put and call values
        value = ticker[0].marketPrice() + ticker[1].marketPrice()
        ib.sleep(1)
        
        if(math.isnan(value)):
            print("from IB: Opt price is NA")
            value = None
    else:
        value = None 
    
    ib.disconnect()
    return value

def getChains(sym, secType=None, currency=None, exchangeSec=None, silent=True):
    """
    Get options chains data for a symbol, using cached data if available.
    
    Args:
        sym (str): Underlying symbol
        secType (str, optional): Security type (STK, IND, etc.). If None, uses value from ticker database.
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchangeSec (str, optional): Exchange for the underlying security. If None, uses value from ticker database.
        
    Returns:
        list or None or float: 
            List of option chains if successful
            None if connection error
            NaN if contract doesn't exist
    """
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if secType is None:
        secType = ticker_info.get('Type', 'STK')
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
        
    if exchangeSec is None:
        exchangeSec = ticker_info.get('Exchange', 'SMART')
    
    chains_file = CONFIG.get("chains")
    
    try:
        with open(chains_file, "r") as fp:
            stored_chains = json.load(fp)
    except (FileNotFoundError, json.JSONDecodeError):
        print(f"Error: Could not load chains from {chains_file}")
        stored_chains = []
  
    # Check if symbol exists in stored chains
    chains = [chains for chains in stored_chains if (chains[0][1] == sym)]
    
    # Check if expiration dates are all greater than today
    if chains:
        chains = chains[0]
        today = int(datetime.date.today().strftime("%Y%m%d"))
        dates = [int(chain[4][0]) for chain in chains]
        if (min(dates) >= today): 
            return chains
  
    # If not in cache or dates expired, fetch from IB
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect(silent=silent)

    # If connection not available return None
    if not ib.isConnected(): 
        return None

    underlying = Contract(symbol=sym, secType=secType,
                       exchange=exchangeSec, currency=currency)
    
    if (not silent): print("\nContract: ", underlying)
    
    if (ib.qualifyContracts(underlying)):
        chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
        sub_chains = [chain for chain in chains if chain.exchange == "SMART"]
        
        if not sub_chains:
            sub_chains = [chain for chain in chains if chain.exchange == "EUREX"]
        
        ib.sleep(1)
   
        if (not silent): print("\nChains: ", sub_chains)
        
        # Update stored chains
        keep_records = [chains for chains in stored_chains if (chains[0][1] != sym)]
        sub_chains = json.loads(json.dumps(sub_chains))
        
        for chain in sub_chains:
            chain[1] = sym
            
        keep_records.append(sub_chains)
        
        try:
            with open(chains_file, "w") as fp:
                json.dump(keep_records, fp, indent=4)
        except Exception as e:
            print(f"Error saving chains to file: {e}")
            
        ib.disconnect()    
        return sub_chains
    else:
        ib.disconnect()
        return float('NaN')

def getChain(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, silent=True):
    """
    Get a specific option chain for a symbol, exchange and trading class.
    
    Args:
        sym (str): Underlying symbol
        secType (str, optional): Security type. If None, uses value from ticker database.
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchangeSec (str, optional): Exchange for underlying. If None, uses value from ticker database.
        exchangeOpt (str, optional): Exchange for options. If None, uses value from ticker database.
        tradingClass (str, optional): Trading class. If None, uses value from ticker database.
        
    Returns:
        dict or float or None: Option chain if found, NaN if not found, None if connection error
    """
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if secType is None:
        secType = ticker_info.get('Type', 'STK')
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
        
    if exchangeSec is None:
        exchangeSec = ticker_info.get('Exchange', 'SMART')
        
    if exchangeOpt is None:
        exchangeOpt = ticker_info.get('OptExchange', 'SMART')
        
    if tradingClass is None:
        tradingClass = ticker_info.get('TradingClass', sym)
    
    chains = getChains(sym, secType, currency, exchangeSec, silent)
    
    # No access to IBKR API
    if chains is None:
        return None
    
    # Test if getChains has returned a list of chains
    if (isinstance(chains, list)):
        # Find chain with requested exchange and trading class
        chain = [chain for chain in chains if chain[0] == exchangeOpt and chain[2] == tradingClass]
        if chain:
            if (not silent): print("\nChain: ", chain)
            return chain[0]
    
    # In all other cases return NaN
    if (not silent): print("\nChain: NaN")
    return float('NaN')

def getStrikesfromExpDate(sym, secType=None, currency=None, 
       exchangeSec=None, exchangeOpt=None, tradingClass=None, expdate=None, silent=True):
    """
    Get available strikes for a specific option expiration date.
    
    Args:
        sym (str): Underlying symbol
        secType (str, optional): Security type. If None, uses value from ticker database.
        currency (str, optional): Currency code. If None, uses value from ticker database.
        exchangeSec (str, optional): Exchange for underlying. If None, uses value from ticker database.
        exchangeOpt (str, optional): Exchange for options. If None, uses value from ticker database.
        tradingClass (str, optional): Trading class. If None, uses value from ticker database.
        expdate (str): Expiration date
        
    Returns:
        list or None: List of available strikes if successful, None if connection error
    """
    
    is_valid, errors = validate_contract_params(
        sym=sym, 
        secType=secType,
        currency=currency,
        exchangeOpt=exchangeOpt, 
        exchangeSec=exchangeSec, 
        tradingClass=tradingClass, 
        expdate=expdate
    )    
    
    if not is_valid:
        for error in errors:
            print(f"- {error}")
    
    if not silent:
        print("getStrikesfromExpDate arguments validated", file=sys.stderr)
        
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if secType is None:
        secType = ticker_info.get('Type', 'STK')
    
    if currency is None:
        currency = ticker_info.get('Currency', 'USD')
        
    if exchangeSec is None:
        exchangeSec = ticker_info.get('Exchange', 'SMART')
        
    if exchangeOpt is None:
        exchangeOpt = ticker_info.get('OptExchange', 'SMART')
        
    if tradingClass is None:
        tradingClass = ticker_info.get('TradingClass', sym)
    
    strikes_file = CONFIG.get("strikes")
    
    try:
        with open(strikes_file, "r") as fp:
            stored_chains = json.load(fp)
    except (FileNotFoundError, json.JSONDecodeError):
        print(f"Error: Could not load strikes from {strikes_file}")
        stored_chains = []
  
    # Check if already stored for requested symbol, trading class and expdate
    stored_strikes = [chain for chain in stored_chains if (
        chain[0] == sym and chain[1] == tradingClass and chain[2] == expdate)]
        
    if stored_strikes:
        return stored_strikes[0][3]
  
    # If not in cache, fetch from IB
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect(silent=silent)

    # If connection not available return None
    if not ib.isConnected():
        return None

    # Get the chain for symbol and trading class
    chain = getChain(sym, secType, currency, exchangeSec, exchangeOpt, tradingClass, silent)
    
    # Extract list of strikes from chain
    all_strikes = chain[5]
    
    # Try all strikes for the expdate to see which are valid, using Put options (should be the same for Call)
    if (secType == "STK") or (secType == "IND"):
        contracts = [Contract(secType='OPT', symbol=sym, lastTradeDateOrContractMonth=expdate,
                  strike=strike_c, right='Put', exchange=exchangeOpt, tradingClass=tradingClass) 
                  for strike_c in all_strikes]
    elif (secType == "FUT"):
        contracts = [Contract(secType='FOP', symbol=sym, lastTradeDateOrContractMonth=expdate,
                  strike=strike_c, right='Put', exchange=exchangeOpt, tradingClass=tradingClass) 
                  for strike_c in all_strikes]
      
    updated_strikes = []
    
    # Process in batches of 20 contracts
    batch_size = 20
    for i in range(0, len(contracts), batch_size):
        batch = contracts[i:i+batch_size]
        if(not silent): print("\nContracts batch nr.",i,": ", batch)    
        for contract in batch:
            if(ib.qualifyContracts(contract)):
                updated_strikes.append(contract.strike)
        # Add small delay between batches
        ib.sleep(0.5)
            
    ib.disconnect()
    
    if (not silent): print("Strikes:", updated_strikes)
  
    # Store the list of strikes for expdate
    record = [sym, tradingClass, expdate, updated_strikes]
    stored_chains.append(record)

    # Save updated strikes to file
    try:
        with open(strikes_file, "w") as fp:
            json.dump(stored_chains, fp, indent=4)
    except Exception as e:
        print(f"Error saving strikes to file: {e}")

    return updated_strikes

def getStrikesFromRange(sym, current_price, expdate, strikes, sigma, volatility):
    """
    Get available strikes for a specific option expiration date.
    
    Args:
        sym (str): Underlying symbol
        expdate (str): Expiration date
        
    Returns:
        list or None: List of available strikes if successful, None if connection error
    """
    
    
    div_yield = get_dividend_yield(ib, underlying_contract)
    
    # Calculate expected dividends during the option's life
    expected_dividends = current_price * div_yield * time_to_expiry
    
    # Adjust the current price for dividends
    adjusted_current_price = current_price - expected_dividends
    
    # Black-Scholes-Merton parameters
    S = adjusted_current_price  # Stock price adjusted for dividends
    r = interest_rate           # Risk-free rate
    q = div_yield               # Dividend yield
    sigma = volatility          # Volatility
    T = time_to_expiry          # Time to expiry in years
    
    # Calculate the range using the adjusted Black-Scholes formula
    adjusted_sigma_range = sigma * math.sqrt(T) * sigma_multiplier
    
    # Apply the dividend and interest rate adjustments
    lower_bound = S * math.exp((r - q - 0.5 * sigma**2) * T - adjusted_sigma_range)
    upper_bound = S * math.exp((r - q - 0.5 * sigma**2) * T + adjusted_sigma_range)
