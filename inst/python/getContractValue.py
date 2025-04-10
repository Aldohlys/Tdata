
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

import yaml
import os
import pathlib
from ib_insync import *
import math
import json
import sys
import datetime
import simplejson
import pandas as pd
import collections
import locale
import sqlite3
import re

def find_config_file(filename="config.yml", max_levels=3):
    """
    Find the configuration file by looking in the current directory and up to max_levels parent directories.
    
    Args:
        filename (str): Name of the configuration file to look for
        max_levels (int): Maximum number of parent directories to check
        
    Returns:
        pathlib.Path or None: Path to the config file if found, None otherwise
    """
    current_dir = pathlib.Path.cwd()
    
    # Check current directory first
    config_path = current_dir / filename
    if config_path.exists():
        return config_path
    
    # Check parent directories up to max_levels
    for i in range(max_levels):
        current_dir = current_dir.parent
        config_path = current_dir / filename
        if config_path.exists():
            return config_path
    
    return None

# Define custom YAML constructor for handling !expr tags
class RYamlLoader(yaml.SafeLoader):
    pass

def expr_constructor(loader, node):
    # Just return the expression as a string without evaluating it
    return f"R_EXPRESSION({loader.construct_scalar(node)})"

# Register the custom constructor with our custom loader
RYamlLoader.add_constructor('!expr', expr_constructor)

def load_config(env="default"):
    """
    Load configuration from YAML file with support for R's !expr tag.
    
    Args:
        env (str): Environment to load (default, development, production)
        
    Returns:
        dict: Configuration dictionary
    """
    config_path = find_config_file()
    if not config_path:
        # If config file not found, use default values
        print("WARNING: Config file not found. Using default values.")
        return {
            "chains": "C:/Users/aldoh/Documents/NewTrading/Chains.json",
            "strikes": "C:/Users/aldoh/Documents/NewTrading/Strikes.json",
            "DB": "data/mydb.db"
        }
    
    # Load YAML config file with custom loader
    try:
        with open(config_path, 'r') as file:
            # Use our custom loader that knows about !expr
            config = yaml.load(file, Loader=RYamlLoader)
    except Exception as e:
        print(f"Error loading YAML config: {e}")
        return {
            "chains": "C:/Users/aldoh/Documents/NewTrading/Chains.json",
            "strikes": "C:/Users/aldoh/Documents/NewTrading/Strikes.json",
            "DB": "data/mydb.db"
        }
    
    # Process any R expressions in the config
    config = process_r_expressions(config)
    
    # Merge the selected environment with the default config
    result = config.get("default", {})
    if env in config and env != "default":
        # Update with environment-specific values
        env_config = config.get(env, {})
        result.update(env_config)
    
    return result

def process_r_expressions(config):
    """
    Process any R expressions in the config dictionary.
    For now, this function just handles the DB path expression.
    
    Args:
        config (dict): The config dictionary
        
    Returns:
        dict: Processed config dictionary
    """
    # Handle common R expressions
    if isinstance(config, dict):
        for key, value in config.items():
            if isinstance(value, str) and value.startswith("R_EXPRESSION("):
                # Handle specific R expressions
                expr = value[13:-1]  # Remove R_EXPRESSION() wrapper
                if "Sys.getenv" in expr:
                    # Simple parser for Sys.getenv("ENV_VAR", unset = "default")
                    match = re.match(r'Sys\.getenv\("([^"]+)"(?:,\s*unset\s*=\s*"([^"]+)")?\)', expr)
                    if match:
                        env_var, default = match.groups()
                        if default is None:
                            default = ""
                        config[key] = os.environ.get(env_var, default)
            elif isinstance(value, (dict, list)):
                config[key] = process_r_expressions(value)
    elif isinstance(config, list):
        for i, item in enumerate(config):
            config[i] = process_r_expressions(item)
    
    return config
# Global config object to avoid reloading the config for each function call
CONFIG = load_config()

class TickerDatabase:
    """
    Manages ticker data retrieved from SQLite database.
    """
    def __init__(self, db_path=None):
        """
        Initialize the database connection.
        
        Args:
            db_path (str): Path to the SQLite database file
        """
        if db_path is None:
            db_path = CONFIG.get("DB")
            
        # Ensure the database exists
        if not os.path.exists(db_path):
            print(f"WARNING: Database not found at {db_path}")
            # Create an empty tickers dictionary
            self.tickers = {}
            self.initialized = False
            return
            
        try:
            # Connect to the database
            self.conn = sqlite3.connect(db_path)
            # Load all tickers into memory for fast lookups
            self.load_tickers()
            self.initialized = True
        except sqlite3.Error as e:
            print(f"Database error: {e}")
            self.tickers = {}
            self.initialized = False
    
    def load_tickers(self):
        """
        Load all tickers from the database into memory.
        """
        try:
            query = "SELECT * FROM Tickers"
            df = pd.read_sql_query(query, self.conn)
            
            # Convert DataFrame to a dictionary for faster lookups
            self.tickers = {row['Name']: row.to_dict() for _, row in df.iterrows()}
            print(f"Loaded {len(self.tickers)} tickers from database")
        except Exception as e:
            print(f"Error loading tickers: {e}")
            self.tickers = {}
    
    def get_ticker_info(self, symbol):
        """
        Get information for a specific ticker.
        
        Args:
            symbol (str): The ticker symbol
            
        Returns:
            dict: Ticker information or default values if not found
        """
        if not self.initialized:
            # Return default values if database is not initialized
            return {
                'Type': self.determine_sec(symbol),
                'Exchange': self.determine_primary_exch(symbol),
                'OptExchange': self.determine_exch(symbol),
                'Currency': 'USD',
                'TradingClass': symbol
            }
            
        # Clean symbol if needed (remove exchange suffix)
        clean_symbol = self.determine_sym(symbol)
        
        # Return ticker info if found, otherwise return default values
        if clean_symbol in self.tickers:
            return self.tickers[clean_symbol]
        else:
            # Return default values based on existing functions
            return {
                'Type': self.determine_sec(symbol),
                'Exchange': self.determine_primary_exch(symbol),
                'OptExchange': self.determine_exch(symbol),
                'Currency': 'USD',
                'TradingClass': symbol
            }
    
    # Keep old functions as instance methods for backward compatibility
    def determine_sec(self, sym):
        """
        Determine security type based on symbol.
        
        Args:
            sym (str): The security symbol
            
        Returns:
            str: Security type - "IND" for index or "STK" for stock
        """
        if (any(sym==x for x in ["ESTX50","XSP","SPX", "VIX"])): return "IND"
        else: return "STK"

    def determine_exch(self, sym):
        """
        Determine exchange based on symbol.
        
        Args:
            sym (str): The security symbol
            
        Returns:
            str: Exchange name for the symbol
        """
        if (any(sym==x for x in ["XSP","SPX", "VIX"])): return "CBOE"
        if (sym=="ESTX50"): return "EUREX"
        else: return "SMART"

    def determine_primary_exch(self, sym):
        """
        Determine primary exchange based on symbol.
        
        Args:
            sym (str): The security symbol
            
        Returns:
            str: Primary exchange name for the symbol
        """
        if (any(sym==x for x in ["AI","SU", "TTE", "OR", "SGO", "BN"])): return "SBF"
        if (any(sym==x for x in ["SPX","XSP", "ESTX50", "VIX"])): return ""
        if (any(sym==x for x in ["DTLA","TRE7","SXLV"])) : return "LSEETF"
        if (any(sym==x for x in ["CSBGU0","ABBN", "HOLN", "ROG", "SLHN"])): return "EBS"
        if (sym == "U.UN"): return "TSE"
        return "SMART"

    def determine_sym(self, sym):
        """
        Clean symbol by removing exchange suffix if present.
        
        Args:
            sym (str): The security symbol with potential suffix
            
        Returns:
            str: Clean symbol without exchange suffix
        """
        if (".SW" in sym): return sym[:-3]
        if (".PA" in sym): return sym[:-3]
        if (".L" in sym): return sym[:-2]
        return sym

# Initialize the ticker database
ticker_db = TickerDatabase()

def is_port_in_use(port):
    """
    Check if a port is already in use on localhost.
    
    Args:
        port (int): The port number to check
        
    Returns:
        bool: True if port is in use, False otherwise
    """
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

def getPort():
    """
    Generate a random available port for IB connection.
    Keeps trying until it finds an available port.
    
    Returns:
        int: An available port number
    """
    import random
    max_attempts = 100  # Prevent infinite loops
    
    for _ in range(max_attempts):
        port_id = random.randint(1, 9990)
        if not is_port_in_use(port_id):
            print("Port id:", port_id)
            return port_id
        print(f"Port {port_id} is in use, trying another port...")
    
    # If we've tried max_attempts times and found no open port
    raise RuntimeError(f"Could not find an available port after {max_attempts} attempts")

def safe_ib_connect(host='127.0.0.1', port=7496, client_id=None, readonly=False, silent=False):
    """
    Safely connect to Interactive Brokers with proper error handling.
    
    Args:
        host (str): IB host address
        port (int): IB port number
        client_id (int): Client ID for the connection, if None a random port is used
        readonly (bool): Whether to use readonly mode
        silent (bool): Whether to suppress connection error messages
        
    Returns:
        IB instance - The IB instance
    """
    if client_id is None:
        client_id = getPort()
        
    ib = IB()
    try:
        ib.connect(host, port, clientId=client_id, readonly=readonly)
    except Exception as e:
        if not silent:
            print(f"IB connection error: {str(e)}")

    return ib

def isIBAvailable(silent=False):
    """
    Check if Interactive Brokers TWS/Gateway is available by attempting a connection.
    
    Args:
        silent (bool): Whether to suppress connection error messages
    
    Returns:
        bool: True if connection successful, False otherwise
    """
    # Use safe_ib_connect instead of direct connection
    ib= safe_ib_connect(silent=silent)

    # Check if connection was successful before proceeding

    # If connection not available return None
    if not ib.isConnected(): 
        if not silent:
            print("Interactive Brokers is not available")
        return False
    
    ib.disconnect()
    ib.sleep(0)
    return True

def getValue(list_sym, reqType=2, close=True):
    """
    Get current or close value for one or multiple securities from Interactive Brokers.
    Uses TickerDatabase to automatically determine security details.
    
    Args:
        list_sym (str or list): Symbol(s)
        reqType (int): Market data type (1=Live, 2=Frozen, 3=Delayed, 4=Delayed Frozen)
        close (bool): If True, returns close price; otherwise returns current price
        
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
        contract = Contract(
            secType=ticker_info['Type'],
            symbol=sym,
            currency=ticker_info['Currency'],
            exchange=ticker_info['Exchange']
        )
        contracts.append(contract)
    
    # Connect to Interactive Brokers
    ib = safe_ib_connect()
    
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
        # Ensure connection is always closed
        if ib.isConnected():
            ib.disconnect()

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
       
    # Convert single strike to list if needed
    if (type(strikes) != list): 
        strikes = [strikes]
        
    # Create option contracts for each strike
    contracts = [Contract(symbol=sym, secType="OPT", lastTradeDateOrContractMonth=expiration,
                        strike=strike_c, right=right,
                        exchange=exchange, currency=currency, tradingClass=tradingClass) 
                for strike_c in strikes] 
    
    print("Contracts:", contracts)
    if(ib.qualifyContracts(*contracts)):
        # Use frozen market data (type 2) for consistent pricing
        ib.reqMarketDataType(2)
        tickers = ib.reqTickers(*contracts)
        
        # Handle potential None values in model Greeks
        result_dic = [{"strike": strike, 
               "value": ticker.marketPrice() if not(math.isnan(ticker.marketPrice())) else ticker.close, 
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

def find_nearest_number(numbers, target):
    """
    Find the number in a list that is closest to the target value.
    
    Args:
        numbers (list): List of numbers to search
        target (float): Target value to find closest match
        
    Returns:
        float: Number from the list closest to the target
        
    Raises:
        ValueError: If the list is empty
    """
    if not numbers:
        raise ValueError("The list of numbers is empty.")
    
    nearest = numbers[0]
    diff = abs(nearest - target)
    
    for number in numbers:
        current_diff = abs(number - target)
        
        if current_diff < diff:
            diff = current_diff
            nearest = number
    
    return nearest

def getChains(sym, secType=None, currency=None, exchangeSec=None):
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
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return None

    underlying = Contract(symbol=sym, secType=secType,
                       exchange=exchangeSec, currency=currency)
    
    if (ib.qualifyContracts(underlying)):
        chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
        sub_chains = [chain for chain in chains if chain.exchange == "SMART"]
        
        if not sub_chains:
            sub_chains = [chain for chain in chains if chain.exchange == "EUREX"]
        
        ib.sleep(1)
        
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

def getChain(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None):
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
    
    chains = getChains(sym, secType, currency, exchangeSec)
    
    # No access to IBKR API
    if chains is None:
        return None
    
    # Test if getChains has returned a list of chains
    if (isinstance(chains, list)):
        # Find chain with requested exchange and trading class
        chain = [chain for chain in chains if chain[0] == exchangeOpt and chain[2] == tradingClass]
        if chain:
            return chain[0]
    
    # In all other cases return NaN
    return float('NaN')

def getStrikesfromExpDate(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, expdate=None):
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
    if expdate is None:
        raise ValueError("Expiration date (expdate) must be provided")
    
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
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected():
        return None

    # Get the chain for symbol and trading class
    chain = getChain(sym, secType, currency, exchangeSec, exchangeOpt, tradingClass)
    
    # Extract list of strikes from chain
    all_strikes = chain[5]
    
    # Try all strikes for the expdate to see which are valid
    contracts = [Contract(secType='OPT', symbol=sym, lastTradeDateOrContractMonth=expdate,
              strike=strike_c, right='Put', exchange=exchangeOpt, tradingClass=tradingClass) 
              for strike_c in all_strikes]
              
    updated_strikes = []
    
    for i in range(len(contracts)):
        # Validate each contract
        if(ib.qualifyContracts(contracts[i])):
            updated_strikes.append(contracts[i].strike)
            
    ib.sleep(1)
    ib.disconnect()
    
    print("Strikes:", updated_strikes)
  
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

def retrieveCurrencyPairs(currencies, currency_pairs, direct_conv):
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return float('nan')
   
    ##### Forex data retrieval ###############
    print("### Retrieve currency pairs contracts...")
    # print(currencies)
    # print(currency_pairs)
    # print(direct_conv)
    
    ### currencies is a list of currencies to which to convert from/to USD
    ### this function will return values in the same order as sorted currencies list
    contracts = [Forex(fx_pair) for fx_pair in currency_pairs]

    ib.qualifyContracts(*contracts)
    
    ib.reqMarketDataType(2) ### Request type - Should be 2 or 4
    tickers = ib.reqTickers(*contracts)
    ib.sleep(1)
    ib.disconnect()
    
    res = []
    ### This assumes that direct_conv and currency_pairs are in the same order
    ### Which is calling function responsability
    
    for ticker, direct in zip(tickers, direct_conv):
        if (direct == "Yes"): 
            res.append(round(ticker.marketPrice(), 4)) 
        else: 
            res.append(round(1/ticker.marketPrice(), 4))
    
    print(currencies, res)
    return [currencies, res]

def retrieveAccountHistory(ib, days_back=180):
    """
    Retrieve historical net liquidation values using ib_insync.
    
    Args:
        days_back (int): Number of days of history to retrieve
        ib: IB object
            
    Returns:
        pandas.DataFrame: Historical NLV values with datetime index
    """
    # Calculate date range
    end_date = datetime.datetime.now() - datetime.timedelta(days=1)
    end_str = end_date.strftime('%Y%m%d %H:%M:%S')
        
    # Create a dummy forex contract for account data request
    # Using EUR.USD as it's always available
    contract = Forex('EURUSD')
            
    # Request historical data
    bars = ib.reqHistoricalData(
            contract=contract,
            endDateTime=end_str,
            durationStr=f'{days_back} D',
            barSizeSetting='1 day',
            whatToShow='NetLiquidation',
            useRTH=True,
            formatDate=1
        )

    # Convert to DataFrame
    if bars:
        df = util.df(bars)
        # Select only date and close (NLV value)
        df = df[['date', 'close']].rename(columns={'close': 'value'})
        df['date'] = pd.to_datetime(df['date'])
        df.set_index('date', inplace=True)
        return df
            
    return pd.DataFrame()  # Return empty DataFrame if no data

def retrieveAccountData(ib):
    df = util.df(ib.accountSummary())
    dt = datetime.date.today()
    
    #### This script looks only into BASE currency stats - it does not look for currency specifics
    NetLiquidation = df[df['tag'] == 'NetLiquidation'].iloc[0,2]
    EquityWithLoanValue = df[df['tag'] == 'EquityWithLoanValue'].iloc[0,2]
    FullAvailableFunds = df[df['tag'] == 'FullAvailableFunds'].iloc[0,2]
    FullInitMarginReq = df[df['tag'] == 'FullInitMarginReq'].iloc[0,2]
    FullMaintMarginReq = df[df['tag'] == 'FullMaintMarginReq'].iloc[0,2]
    FullExcessLiquidity = df[df['tag'] == 'FullExcessLiquidity'].iloc[0,2]
    StockMarketValue = df[(df['tag'] == 'StockMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
    OptionMarketValue = df[(df['tag'] == 'OptionMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
    UnrealizedPnL = df[(df['tag'] == 'UnrealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
    RealizedPnL = df[(df['tag'] == 'RealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
    TotalCashBalance = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'BASE')].iloc[0,2]
    # TotalCashBalanceCHF = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'CHF')].iloc[0,2]
    # TotalCashBalanceEUR = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'EUR')].iloc[0,2]
    
    #### Looks only on the first account
    account = ib.managedAccounts()[0]
    
    #### Takes integer type of date
    dd = int((datetime.datetime.now()).strftime('%Y%m%d'))
    dh = (datetime.datetime.now()).strftime("%H:%M:%S")
    
    df = pd.DataFrame({'account': account,
                      'date': [dd],
                      'heure': [dh],
                      'NetLiquidation': [NetLiquidation],
                      'EquityWithLoanValue': [EquityWithLoanValue],
                      'FullAvailableFunds': [FullAvailableFunds],
                      'FullInitMarginReq': [FullInitMarginReq],
                      'FullMaintMarginReq': [FullMaintMarginReq],
                      'FullExcessLiquidity': [FullExcessLiquidity],
                      'OptionMarketValue': [OptionMarketValue],
                      'StockMarketValue': [StockMarketValue],
                      'UnrealizedPnL': [UnrealizedPnL],
                      'RealizedPnL': [RealizedPnL],
                      'TotalCashBalance': [TotalCashBalance],
                      'CashFlow': 0
                      # 'TotalCashBalanceCHF': [TotalCashBalanceCHF],
                      # 'TotalCashBalanceEUR': [TotalCashBalanceEUR]
                     })
    return df

def retrieveAccountMarginData(contracts):
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return 0
 
    print("\n#####  Retrieving account margin data for contracts... \n")
    
    ### Case where only one contract ###
    if not isinstance(contracts, list): 
        contracts = [contracts]
    
    contracts = [Contract(conId=contract) for contract in contracts]
    ib.qualifyContracts(*contracts)
    
    ### This will work only for short positions
    ### Using BUY order is necessary as there may be already a pending order and then using a SELL order is not accepted by IBKR server
    order = MarketOrder('BUY', 1)

    order_state = [ib.whatIfOrder(contract, order) for contract in contracts]
    ib.sleep(1)
    ib.disconnect()
    
    ### As we are buying back this contracts list is actually the margin cost of selling this contract list
    res = [-float(order_s.maintMarginChange) for order_s in order_state]
    print("Contracts margin:\n")
    res_dict = [{'contract name': contract.localSymbol, 'margin': res} for contract, res in zip(contracts, res)]
    print(util.df(res_dict))
    return(res)

def retrievePortfolioData(ib, df):
    options = []
    for i, row in df.iterrows():            # Use iterrows to print output
        if (row['secType'] == "OPT"): 
            options.append(row['contract'])
    
    # IB Market data type 4 works for EUREX and also for US options but in US opening hours
    # IB Market data type 2 works for only US options (in or out US opening hours)
    # 1 = Live
    # 2 = Frozen
    # 3 = Delayed
    # 4 = Delayed frozen
    ib.reqMarketDataType(2)
    
    options = ib.qualifyContracts(*options)
    tickers = ib.reqTickers(*options)
    
    ### All data has been retrieved from IBKR
    #### Look at first option contract
    
    ### optionComputation elements (9)
    # tickAttrib  impliedVol     delta  optPrice  pvDividend     gamma      vega     theta  undPrice
    option_c = pd.DataFrame(columns=["tickAttrib", "impliedVol", "delta", "optPrice", "pvDividend", "gamma", "vega", "theta", "undPrice"])
    opt = 0
    for i, row in df.iterrows():
        #### Iterate over each contract
        if (row['secType'] == "OPT"):
            optionComputation = tickers[opt].modelGreeks
            opt = opt + 1
        else:  
            optionComputation = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        ### Construction de option computation à revoir
        option_c.loc[len(option_c.index)] = optionComputation
    df = df.join(option_c)
    
    ### Extract meaningful columns
    cols = ["date", "heure", "secType", "conId", "symbol", "lastTradeDateOrContractMonth", "strike", "right", "position", 
    "marketPrice", "optPrice", "marketValue", "averageCost", "unrealizedPNL", "impliedVol", "pvDividend",
    "delta", "gamma", "vega", "theta", "undPrice", "multiplier", "currency"]
    df = df[[c for c in df.columns if c in cols]]

    dd = int((datetime.datetime.now()).strftime('%Y%m%d'))
    dh = (datetime.datetime.now()).strftime("%H:%M:%S")
    
    df = df.assign(date=dd, heure=dh)
    
    ### Re-order df columns according to col order
    df = df[cols]
    ### Rename some columns that are really ugly
    df = df.rename(columns={'lastTradeDateOrContractMonth': 'expdate', 
                            'undPrice': 'uPrice', 'impliedVol': 'IV', 'position': 'pos', 'marketPrice': 'mktPrice',
                            'marketValue': 'mktValue',
                            'averageCost': 'avgCost', 'unrealizedPNL': 'unPnL'})
    return df
  
def getIBKRData():
    """
    Retrieve account and portfolio data from Interactive Brokers.
    
    Returns:
        list: A list containing account_data and portfolio_data DataFrames
              or 0 if connection fails
    """
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return 0

    #### Get account related data #########
    print("\n#####  Retrieving account data... \n")
    account_data = retrieveAccountData(ib)
    print(account_data)

    ### Store portfolio in df, then split contract definition (first column) into multiple columns
    ### Merge resulting split with the other columns

    #### For options, get the list of contract definitions
    #### i index is necessary to iterate over df
    #### Consider only row that are of secType = OPT
    ###  Extract only 'contract' column in row 
    
    df = util.df(ib.portfolio())
    c_def = pd.DataFrame()
    #### Iterate over each line of portfolio
    for i in range(len(df)):
        line = df.iloc[i,0]
        ## ib.qualifyContracts is not needed to retrieve underlying prices and will be called anyway during portfolio data processing
        ## ib.qualifyContracts(line)
        c_def = pd.concat([c_def, pd.DataFrame([df.iloc[i,0]])], ignore_index=True)
    df = c_def.join(df)
    
    # print("\n#####  Retrieving underlying price data... \n")
    # 
    # #### Remove underlying symbol duplicates
    # du = df.drop_duplicates(subset='symbol',keep="first")
    # 
    # ### Retrieve only prices for secType = OPT not other types (for STK, FUT, data is already present in retrieved portfolio data)
    # du = du.loc[du["secType"] == "OPT"]
    # 
    # u_prices_data = retrievePricesData(ib, du)
    # print(u_prices_data)

    print("\n#####  Retrieving portfolio data... \n")
    portf_data = retrievePortfolioData(ib, df)
    print(portf_data)
    
    ### Wait until all data has been received
    ib.sleep(1)
    
    #### IB connection no more needed
    ib.disconnect()
    
    return [account_data, portf_data]
