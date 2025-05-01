import yaml
import os
import pathlib
import re
import sqlite3
import pandas as pd
from ib_insync import *

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
                'TradingClass': symbol,
                'YahooName': symbol,
                'Expiration': ''
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
                'TradingClass': symbol,
                'YahooName': symbol,
                'Expiration': ''
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

def validate_contract_params(
    sym, 
    secType=None, 
    currency=None, 
    exchangeSec=None, 
    exchangeOpt=None, 
    tradingClass=None, 
    expdate=None
):
    """
    Simple validation of contract parameters types.
    
    Args:
        sym (str): Underlying symbol (required)
        secType (str, optional): Security type
        currency (str, optional): Currency code
        exchangeSec (str, optional): Exchange for underlying
        exchangeOpt (str, optional): Exchange for options
        tradingClass (str, optional): Trading class
        expdate (str, optional): Expiration date
        
    Returns:
        tuple: (is_valid, error_messages)
    """
    errors = []
    
    # Check symbol (required)
    if not sym:
        errors.append("Symbol cannot be empty")
    elif not isinstance(sym, str):
        errors.append(f"Symbol must be a string, got {type(sym)}")
    
    # Check optional string parameters
    if secType is not None and not isinstance(secType, str):
        errors.append(f"Security type must be a string, got {type(secType)}")
    
    if currency is not None and not isinstance(currency, str):
        errors.append(f"Currency must be a string, got {type(currency)}")
    
    if exchangeSec is not None and not isinstance(exchangeSec, str):
        errors.append(f"Security exchange must be a string, got {type(exchangeSec)}")
    
    if exchangeOpt is not None and not isinstance(exchangeOpt, str):
        errors.append(f"Option exchange must be a string, got {type(exchangeOpt)}")
    
    if tradingClass is not None and not isinstance(tradingClass, str):
        errors.append(f"Trading class must be a string, got {type(tradingClass)}")
    
    # Check expiration date if provided
    if expdate is not None:
        if not isinstance(expdate, str):
            errors.append(f"Expiration date must be a string, got {type(expdate)}")
        else:
            # Check for common date formats: YYYYMMDD or YYYYMM
            import re
            if not re.match(r'^20\d{2}(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])?$', expdate):
                errors.append(f"Expiration date format invalid: {expdate}. Expected format: YYYYMMDD or YYYYMM")
    
    return (len(errors) == 0, errors)


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
