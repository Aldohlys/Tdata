import yaml
import os
import pathlib
import re
import sqlite3
import pandas as pd
from ib_async import *
from fin_logger import get_logger, log_execution_time

# Créez un logger pour ce module
logger = get_logger("tdata_py.core")

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
            "chains_dir": "C:/Users/aldoh/Documents/NewTrading/chains",
            "strikes_dir": "C:/Users/aldoh/Documents/NewTrading/strikes",
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
            "chains_dir": "C:/Users/aldoh/Documents/NewTrading/chains",
            "strikes_dir": "C:/Users/aldoh/Documents/NewTrading/strikes",
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
CONFIG = load_config(env=os.environ.get("R_CONFIG_ACTIVE", "default"))

# ── ib_async global request timeout ──────────────────────────────────────
# IB.RequestTimeout is a class attribute (default 0 = no timeout). Setting it
# here puts a ceiling on every util.run()/ib.run() call ib_async issues, so a
# stuck TWS request (e.g. reqContractDetails / reqSecDefOptParams that never
# emits its *End callback after a 1100/1102 connectivity blip) raises
# asyncio.TimeoutError instead of hanging forever. Override via
# config.yml -> default -> ibkr -> request_timeout.
_ibkr_cfg = CONFIG.get("ibkr", {}) if isinstance(CONFIG, dict) else {}
_request_timeout = _ibkr_cfg.get("request_timeout", 60) if isinstance(_ibkr_cfg, dict) else 60
try:
    IB.RequestTimeout = float(_request_timeout)
    logger.info(f"ib_async IB.RequestTimeout set to {IB.RequestTimeout}s")
except Exception as e:
    logger.warning(f"Failed to set IB.RequestTimeout={_request_timeout}: {e}")

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
            logger.info(f"Loaded {len(self.tickers)} tickers from database")
        except Exception as e:
            print(f"Error loading tickers: {e}")
            self.tickers = {}
    
    def get_ticker_info(self, symbol):
        """
        Get comprehensive information for a specific ticker with multi-trading-class support.
        
        Args:
            symbol (str): The ticker symbol
            
        Returns:
            dict: Ticker information or None if not found
        """
        
        ## Case DB could not be found or could not be opened successfully
        if not self.initialized : return None

        # Standard case - copy symbol info first to avoid modifying DB
        # Look in DB if ticker already exists
        ticker_info = None
        if symbol in self.tickers:
            ticker_info = self.tickers[symbol].copy()

        if ticker_info is not None:
            # Parse alternate trading classes from comma-separated string
            alt_classes_str = ticker_info.get('AlternateTradingClasses', '')
            if alt_classes_str and isinstance(alt_classes_str, str):
                # Split by comma and clean whitespace
                ticker_info['AlternateTradingClasses'] = [tc.strip() for tc in alt_classes_str.split(',') if tc.strip()]
            else:
                ticker_info['AlternateTradingClasses'] = []

            return ticker_info
        else: return None


# Initialize the ticker database
ticker_db = TickerDatabase()

def validate_contract_params(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, expdate=None):
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
