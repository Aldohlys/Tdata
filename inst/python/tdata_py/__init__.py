"""
tdata_py: Python utilities for Interactive Brokers data retrieval
This package provides functions for retrieving contract values,
options data, and account information from Interactive Brokers.

Enhanced with systematic chain caching and multi-trading-class support.
"""
import sys
import os
import logging

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

# Import and configure logging
try:
    from fin_logger import(
        setup_logging_from_config,
        get_logger,
        setup_logging,
        set_all_loggers_level,
        configure_ibinsync_logging,
        log_with_context,
        log_execution_time,
        DEBUG,
        INFO,
        WARNING,
        ERROR,
        CRITICAL
    )
    
    setup_logging_from_config()
    # Get logger for the package
    package_logger = get_logger("tdata_py")
    package_logger.info("tdata_py package initialized with loggin enabled")
    
except ImportError as e:
    print(f"Error importing fin_logger: {e}")

# ============================================================================
#  PARQUET FUNCTIONS - Multi-Trading-Class Support
# ============================================================================

try:
    from .chains_manager import (
        getChains,
        getChain,
        getExpirationDates,
        getOptionStrikes,
        initialize_chains_for_symbols,
        getAvailableTradingClasses,
        getAllChains,
        findBestTradingClass,
        compareTradingClasses,
        exploreSymbol,
        discoverSymbol,
        getStrikesAuto,
        printSymbolSummary,
        getAllStrikes,
        getStrikesInRange
    )
      
    from .parquet_storage import (
        # Storage classes
        ParquetChainsStorage,
        ParquetStrikesStorage,
        ParquetMaintenanceManager,
        # Utility functions
        get_chain_for_trading_class,
        get_strikes_for_expiration,
        get_strikes_summary,
        compare_trading_class_strikes
    )
  
    # Utility functions for file management
    from .parquet_utils import (
        view_parquet,
        list_parquet_files,
        cleanup_symbol,
        cleanup_trading_class,
        cleanup_all,
        cleanupExpired,
        clearStrikesCache,
        clearCache,
        get_detailed_storage_breakdown,
        print_storage_summary,
        validate_parquet_structure,
        generate_cache_health_report
    )
    enhanced_functions_available = True
    package_logger.info("✅ Enhanced parquet modules loaded successfully")
  
except ImportError as e:
    enhanced_functions_available = False
    package_logger.error(f"⚠️ Enhanced chain functions not available: {e}")
    print(f"Error importing enhanced parquet modules: {e}")


# ============================================================================
# HISTORICAL DATA FUNCTIONS
# ============================================================================

try:
    from .historical_option import (
        # Core classes
        ContractConfig,
        HistoricalDataConfig,
        HistoricalStorage,
        HistoricalDataManager,

        # R-friendly functions
        add_historical_tracking,
        update_historical_settings,
        list_historical_config,
        update_historical_data,
        get_option_historical_data,
        manage_contracts
    )

    # Import on-demand retrieval functions
    from .on_demand_historical import (
        get_or_retrieve_option_historical_data,
        clear_on_demand_cache
    )
    
    package_logger.info("✅ Historical option data module loaded")
    historical_data_available = True
    
except ImportError as e:
    package_logger.warning(f"⚠️ Historical option data module not available: {e}")
    historical_data_available = False

    
# ============================================================================
# CONTRACT FUNCTIONS
# ============================================================================

try:
    from .contract import (
        getValue,
        getOptValue,
        getStraddleValue,
        getStrikesfromExpDate
    )
except ImportError as e:
    print(f"Error importing contract: {e}")
    # Create stub functions to prevent import errors
    def getValue(*args, **kwargs):
        return None
    def getOptValue(*args, **kwargs):
        return None
    def getStraddleValue(*args, **kwargs):
        return None
    def getStrikesfromExpDate(*args, **kwargs):
        return None

# ============================================================================
# ACCOUNT FUNCTIONS
# ============================================================================

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
    # Create stub functions
    def retrieveCurrencyPairs(*args, **kwargs):
        return None
    def retrieveAccountHistory(*args, **kwargs):
        return None
    def retrieveAccountData(*args, **kwargs):
        return None
    def retrieveAccountMarginData(*args, **kwargs):
        return None
    def retrievePortfolioData(*args, **kwargs):
        return None
    def getIBKRData(*args, **kwargs):
        return None

# ============================================================================
# SPECIALIZED UTILITY FUNCTIONS
# ============================================================================

# Interest rate functions
try:
    from .interest_rate_utils import (
        getInterestRate
    )
except ImportError as e:
    print(f"Error importing interest_rate_utils: {e}")
    def getInterestRate(*args, **kwargs):
        return None

# Dividend functions
try:
    from .dividend_utils import (
        getNTMDividend  
    )
except ImportError as e:
    print(f"Error importing dividend_utils: {e}")
    def getNTMDividend(*args, **kwargs):
        return None

# Volatility functions
try:
    from .impliedvol import (
        get_volatility_metrics,
        get_historical_bars,
        get_iv_percentile_levels
    )
except ImportError as e:
    print(f"Error importing impliedvol: {e}")
    def get_volatility_metrics(*args, **kwargs):
        return None
    def get_historical_bars(*args, **kwargs):
        return None

# Spread analysis functions
try:
    from .spread import (
        compute_spread_risk_reward
    )
except ImportError as e:
    print(f"Error importing spread: {e}")
    def compute_spread_risk_reward(*args, **kwargs):
        return None

# ============================================================================
# PACKAGE METADATA AND CONVENIENCE
# ============================================================================

# Version number
__version__ = '0.2.0'  # Incremented for multi-trading-class support

# Define what gets imported with "from tdata_py import *"
# A subset of all available functions nevertheless
__all__ = [
    # === CONNECTION FUNCTIONS ===
    'safe_ib_connect',
    'isIBAvailable',
    
    # === CORE CHAIN FUNCTIONS ===
    'getChains',
    'getChain', 
    'getExpirationDates',
    'getOptionStrikes',

    # === MULTI-TRADING-CLASS DISCOVERY ===
    'getAvailableTradingClasses',
    'getAllChains',
    'findBestTradingClass',
    'compareTradingClasses',
    'exploreSymbol',
    'printSymbolSummary',
    
    # ==== HISTORICAL DATA CLASSES AND UTILITIES ====
    'addp_historical_tracking',
    'update_historical_settings',
    'update_historical_data',
    'get_option_historical_data',
    'manage_contracts',
    'list_historical_config',

    # === STORAGE CLASSES AND UTILITIES ===
    'view_parquet',
    'list_parquet_files',
    'get_strikes_for_expiration',
    'get_strikes_summary',

    # === MAINTENANCE FUNCTIONS ===
    'clearStrikesCache',
    'clearCache',

    # === CONTRACT FUNCTIONS ===
    'getValue',
    'getOptValue',

    # === ACCOUNT FUNCTIONS ===
    'retrieveCurrencyPairs',
    'getIBKRData',
    
    # === SPECIALIZED UTILITIES ===
    'get_volatility_metrics',
    'get_historical_bars',
    'get_iv_percentile_levels',

    # === SPREAD ANALYSIS ===
    'compute_spread_risk_reward',

    # === LOGGING FUNCTIONS ===
    'get_logger',
    'setup_logging',
    'set_all_loggers_level',
    'configure_ibinsync_logging',
    'log_with_context',
    'log_execution_time'
]

# ============================================================================
# PACKAGE INITIALIZATION SUMMARY
# ============================================================================

def package_info():
    """Return comprehensive package information."""
    return {
        'version': __version__,
        'features': {
            'systematic_caching': True,
            'multi_trading_class': True,
            'r_friendly_interface': True,
            'batch_operations': True,
            'enhanced_discovery': True,
            'maintenance_tools': True,
            'historical_data_available' : True
        },
        'available_functions': len(__all__),
        'storage_architecture': 'hierarchical_parquet',
        'caching_strategy': 'systematic_immediate'
    }

# Initialize package with summary
try:
    info = package_info()
    package_logger.info(f"🎉 tdata_py v{info['version']} initialization complete")
    package_logger.info(f"✅ {info['available_functions']} functions available")
    package_logger.info("🚀 Ready for systematic options chain caching with multi-trading-class support")
except:
    print("tdata_py initialization complete")


