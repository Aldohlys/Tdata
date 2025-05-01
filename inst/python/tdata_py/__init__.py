"""
tdata_py: Python utilities for Interactive Brokers data retrieval

This package provides functions for retrieving contract values,
options data, and account information from Interactive Brokers.
"""

import sys
import os

# For debugging - print the files in the current directory
# print(f"tdata_py package directory: {os.path.dirname(os.path.abspath(__file__))}")
# print(f"Files in tdata_py package directory: {os.listdir(os.path.dirname(os.path.abspath(__file__)))}")

# Import _core module and make it available as "core"
try:
    # Import _core directly
    from . import _core
    # Also make it available as "core" (without underscore)
    sys.modules['tdata_py.core'] = sys.modules['tdata_py._core']
except ImportError as e:
    print(f"Error importing _core: {e}")
    # Create a minimal _core module to avoid errors
    import types
    _core = types.ModuleType('tdata_py._core')
    sys.modules['tdata_py._core'] = _core
    sys.modules['tdata_py.core'] = _core
    print("Created empty _core module as fallback")

# Import and expose connection functions
try:
    from .IB_connection import (
      safe_ib_connect,
      isIBAvailable
    )
except ImportError as e:
    print(f"Error importing IB_connection: {e}")
    # Provide fallback implementations
    def safe_ib_connect():
        print("Warning: Using fallback safe_ib_connect")
        return False
        
    def isIBAvailable():
        print("Warning: Using fallback isIBAvailable")
        return False

# Import and expose contract functions  
try:
    from .contract import (
        #find_nearest_numbergetValue,
        getValue,
        getOptValue,
        getStraddleValue,
        getChains,
        getChain,
        getStrikesfromExpDate
    )
except ImportError as e:
    print(f"Error importing contract: {e}")

# Import and expose account functions
try:
    from .account import (
        retrieveCurrencyPairs,
        retrieveAccountHistory,
        retrieveAccountData,
        retrieveAccountMarginData,
        retrievePortfolioData,
        getIBKRData
    )
except ImportError as e:
    print(f"Error importing account: {e}")

# Import and expose interest rate functions
try:
    from .interest_rate_utils import (
        getInterestRate
    )
except ImportError as e:
    print(f"Error importing interest_rate_utils: {e}")

# Import and expose dividend functions
try:
    from .dividend_utils import (
        getTTMDividend  
    )
except ImportError as e:
    print(f"Error importing dividend_utils: {e}")


# Import volatility function
try:
    from .impliedvol import (
        get_volatility_metrics  
    )
except ImportError as e:
    print(f"Error importing impliedvol: {e}")



# Version number
__version__ = '0.1.0'

#print("tdata_py initialization complete")
