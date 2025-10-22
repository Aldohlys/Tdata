# Usage Examples - Historical Option Data (Under 300 lines)

import tdata_py as td

# 1. Setup Contract Tracking - Each contract is one strike/right combination
contracts_config = [
    {'symbol': 'SPY', 'trading_class': 'SPY', 'expiration': '20251017', 'strike': 425.0, 'right': 'C', 'exchange':'SMART'},
    {'symbol': 'SPY', 'trading_class': 'SPY', 'expiration': '20251017', 'strike': 425.0, 'right': 'P', 'exchange':'SMART'},
    {'symbol': 'QQQ', 'trading_class': 'QQQ', 'expiration': '20251017', 'strike': 390.0, 'right': 'C', 'exchange':'SMART'}
]

success = td.add_historical_tracking(contracts_config)
print(f"Add: {'Success' if success else 'Failed'}")

settings = td.HistoricalDataConfig()

data_manager = td.HistoricalDataManager()
contract_spec = {
      'symbol': "ITB",
      'trading_class': "ITB",
      'expiration': "20251128",
      'strike': 110,
      'right': "C",
      'exchange': "SMART"  # Default exchange
}
results = data_manager.collect_data_for_active_contracts("historical", [contract_spec])
td.get_option_historical_data(contract_spec["symbol"], contract_spec["trading_class"], contract_spec["expiration"],
contract_spec["strike"], contract_spec["right"], "historical", "TRADES")


# 2. Configure Collection Settings
settings.update_settings(
    market_data_type=3,  # Delayed data
    incremental_timeout=10,      # 10s for incremental updates
    full_historical_timeout=120, # 2min for full historical data
    intraday_what_to_show=["TRADES", "BID_ASK"],
    historical_what_to_show=["TRADES", "MIDPOINT"]
)

# 3. Manual Contract Management
td.add_historical_tracking({'symbol':'AAPL', 'trading_class':'AAPL', 
                   'expiration':'20250321', 'strike':160.0, 'right':'C', 'exchange':'SMART'})

td.manage_contracts('remove', symbol='AAPL', trading_class='AAPL',
                   expiration='20250321', strike=160.0, right='C', archive=True)

# List contracts
td.manage_contracts('list_active')

# 4. Data Collection
results = td.update_historical_data("both")
print(f"Processed: {results['contracts_processed']} contracts")

# 5. Retrieve Data
# Get all data types
all_data = td.get_option_historical_data("SPY", "SPY", "20250321", 425.0, "C", "historical")

# Get specific data type
trades_only = td.get_option_historical_data("SPY", "SPY", "20250321", 425.0, "C", 
                                           "historical", what_to_show="TRADES")

# 6. Available IBKR Data Types (OPTION_IMPLIED_VOLATILITY not available)
available_types = ["TRADES", "MIDPOINT", "BID_ASK", "BID", "ASK"]

# 7. Helper Function - Create Multiple Contracts
def create_contracts(symbol, trading_class, expiration, strikes, include_puts=True):
    contracts = []
    for strike in strikes:
        contracts.append({'symbol': symbol, 'trading_class': trading_class, 
                         'expiration': expiration, 'strike': strike, 'right': 'C'})
        if include_puts:
            contracts.append({'symbol': symbol, 'trading_class': trading_class,
                             'expiration': expiration, 'strike': strike, 'right': 'P'})
    return contracts

# Usage
spy_contracts = create_contracts("SPY", "SPY", "20250321", [420, 425, 430])
td.add_historical_tracking(spy_contracts)

# 8. Configuration Example
config_example = {
    "contracts": [
        {"symbol": "SPY", "trading_class": "SPY", "expiration": "20250321", 
         "strike": 425.0, "right": "C", "active": True}
    ],
    "incremental_timeout": 10,
    "full_historical_timeout": 120,
    "intraday_what_to_show": ["TRADES", "BID_ASK"],
    "historical_what_to_show": ["TRADES", "MIDPOINT"]
}

# 9. Archive Management
archive_results = td.manage_contracts("archive")
print(f"Archived: {archive_results['contracts_archived']} contracts")

# 10. Data Analysis Example
def analyze_option_data(symbol, trading_class, expiration, strike, right):
    data = td.get_option_historical_data(symbol, trading_class, expiration, strike, right)
    
    if data is None:
        return None
    
    analysis = {}
    for what_to_show in data['what_to_show'].unique():
        subset = data[data['what_to_show'] == what_to_show]
        
        if what_to_show == "TRADES":
            analysis['trades'] = {
                'avg_price': subset['close'].mean(),
                'total_volume': subset['volume'].sum()
            }
        elif what_to_show == "BID_ASK":
            subset['spread'] = subset['high'] - subset['low']
            analysis['bid_ask'] = {
                'avg_spread': subset['spread'].mean()
            }
    
    return analysis

# Usage
analysis = analyze_option_data("SPY", "SPY", "20250321", 425.0, "C")

print("\n=== KEY FEATURES ===")
print("✓ Single strike/right per contract")
print("✓ Configurable timeouts (10s incremental, 120s full)")
print("✓ Available data types: TRADES, MIDPOINT, BID_ASK")
print("✓ Files: strikes/symbol/trading_class/historical_{type}_{exp}_{strike}_{right}.parquet")
print("✓ whatToShow stored as column, not separate files")

print("\n=== SIMPLIFIED APPROACH BENEFITS ===")
print("✓ One file per contract instead of multiple files per data type")
print("✓ whatToShow as column - easier querying and analysis")  
print("✓ File format: historical_{datatype}_{expiration}_{strike}_{right}.parquet")

# 8. Configuration File Structure - Updated format
example_config = {
    "contracts": [
        {
            "symbol": "SPY",
            "trading_class": "SPY",
            "expiration": "20250321", 
            "strikes": [420.0, 425.0, 430.0],
            "rights": ["C", "P"],
            "active": True,
            "last_updated": "2025-01-01T10:30:00"
        }
    ],
    "intraday_frequency": "5 mins",
    "historical_frequency": "2 hours", 
    "intraday_duration": "1 D",
    "historical_duration": "1 Y",
    "market_data_type": 3,
    "use_rth": True,
    # NEW: Multiple data types configuration
    "intraday_what_to_show": ["TRADES", "BID_ASK"],
    "historical_what_to_show": ["TRADES", "OPTION_IMPLIED_VOLATILITY"]
}

# 9. Available IBKR Data Types for Options (from documentation)
available_data_types = {
    'TRADES': 'Price/volume data (OHLCV)',
    'MIDPOINT': 'Midpoint prices',
    'BID': 'Bid prices only', 
    'ASK': 'Ask prices only',
    'BID_ASK': 'Bid/ask spread data',
    'OPTION_IMPLIED_VOLATILITY': 'Implied volatility'
}

print("Available data types for options:")
for data_type, description in available_data_types.items():
    print(f"  {data_type}: {description}")

# 10. Export Multiple Data Types for R
def export_multi_datatype_for_r(symbol, trading_class, expiration, strikes, rights):
    """Export multiple data types in R-friendly format"""
    
    all_data = []
    
    for strike in strikes:
        for right in rights:
            for what_to_show in ["TRADES", "OPTION_IMPLIED_VOLATILITY"]:
                data = td.get_option_historical_data(
                    symbol, trading_class, expiration, strike, right, 
                    "combined", what_to_show
                )
                
                if data is not None and not data.empty:
                    data['strike'] = strike
                    data['right'] = right
                    data['what_to_show'] = what_to_show
                    data['symbol'] = symbol
                    data['trading_class'] = trading_class
                    data['expiration'] = expiration
                    all_data.append(data)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        combined = combined.sort_values(['datetime', 'what_to_show', 'strike', 'right'])
        
        filename = f"{symbol}_multitype_option_data.csv"
        combined.to_csv(filename, index=False)
        print(f"Exported {len(combined)} records with multiple data types to {filename}")
        return combined
    
    return None

# Usage
# multi_data = export_multi_datatype_for_r("SPY", "SPY", "20250321", [420, 425, 430], ["C", "P"])

print("\n=== KEY CHANGES SUMMARY ===")
print("✓ Removed base_dir - uses existing strikes directory")
print("✓ Strikes and rights fully specified in configuration")  
print("✓ Multiple IBKR data types: TRADES, BID_ASK, OPTION_IMPLIED_VOLATILITY")
print("✓ No @dataclass/@classmethod decorators - simplified classes")
print("✓ File format: historical_{datatype}_{whattoshow}_{exp}_{strike}_{right}.parquet")

# 2. Manual Contract Management
# Add specific contract with custom strikes
td.add_historical_tracking({'symbol':'AAPL', 'trading_class':'AAPL', 
                   'expiration':'20250321', 'strikes':[150, 155, 160, 165, 170]})

# List active contracts
td.manage_contracts('list_active')

# Deactivate contract (will be archived on next update)
td.manage_contracts('deactivate', symbol='IWM', trading_class='IWM', expiration='20250321')

# Remove contract with archiving
td.manage_contracts('remove', symbol='AAPL', trading_class='AAPL', 
                   expiration='20250321', archive=True)

# 3. Configure Collection Settings
# Update global settings for data collection
td.update_historical_settings(
    market_data_type=3,  # Delayed data for historical
    intraday_frequency="5 mins",
    historical_frequency="2 hours", 
    max_strikes_per_contract=15,
    use_rth=True
)

# 4. Run Data Collection (Scheduled Task)
# Update all active contracts - intraday only
results = td.update_historical_data("intraday")
print(f"Updated {results['files_updated']} intraday files")

# Update historical data only
results = td.update_historical_data("historical") 
print(f"Updated {results['files_updated']} historical files")

# Update both (typical scheduled run)
results = td.update_historical_data("both")
print(f"Processed {results['contracts_processed']} contracts")

# 5. Retrieve Historical Data
# Get historical data for specific option
historical_data = td.get_option_historical_data(
    symbol="SPY", 
    trading_class="SPY",
    expiration="20250321",
    strike=425.0,
    right="C",
    data_type="historical"
)

if historical_data is not None:
    print(f"Retrieved {len(historical_data)} historical bars")
    print(historical_data.tail())

# Get intraday data
intraday_data = td.get_option_historical_data(
    symbol="SPY", 
    trading_class="SPY", 
    expiration="20250321",
    strike=425.0,
    right="C",
    data_type="intraday"
)

# Get combined data (intraday + historical)
combined_data = td.get_option_historical_data(
    symbol="SPY",
    trading_class="SPY",
    expiration="20250321", 
    strike=425.0,
    right="C",
    data_type="combined"
)

# 6. Archive Management
# Archive all expired contracts
archive_results = td.manage_contracts("archive")
print(f"Archived {archive_results['contracts_archived']} contracts, "
      f"{archive_results['files_archived']} files")

# Access archived data
archived_data = td.get_option_historical_data(
    symbol="SPY",
    trading_class="SPY", 
    expiration="20240315",  # Expired expiration
    strike=420.0,
    right="C",
    include_archived=True
)

# 7. Configuration Management Examples

# Example: Setup for 50 symbols with 2 expirations each
def setup_large_scale_tracking():
    """Setup tracking for large number of symbols"""
    
    # Major ETFs and indices
    symbols = ["SPY", "QQQ", "IWM", "XLF", "XLE", "XLK", "XLP", "XLI", "XLV", "XLU",
               "GLD", "SLV", "TLT", "EEM", "FXI", "EWZ", "EFA", "VGK", "VWO", "IEMG"]

    # Get next 2 monthly expirations
    from datetime import datetime, timedelta
    import calendar
    
    today = datetime.now()
    expirations = []
    
    for i in range(2):
        # Find next monthly expiration (3rd Friday)
        year = today.year
        month = today.month + i
        if month > 12:
            year += 1
            month -= 12
            
        # 3rd Friday is between 15-21
        for day in range(15, 22):
            date = datetime(year, month, day)
            if date.weekday() == 4:  # Friday
                exp_str = date.strftime("%Y%m%d")
                expirations.append(exp_str)
                break
    
    # Create contracts list
    contracts = []
    for symbol in symbols:
        for exp in expirations:
            contracts.append({
                'symbol': symbol,
                'trading_class': symbol,  # Assume same as symbol
                'expiration': exp,
                'strike': 200.0,
                'right': 'P',
                'exchange': 'SMART'
            })
    
    print(f"Setting up tracking for {len(contracts)} contracts...")
    td.add_historical_tracking(contracts)
    
    td.list_historical_config(max_contracts=50)
    sleep(1)
    print("\nNow removing all these 20 new contracts...\n")
    td.manage_contracts("remove")
    for symbol in symbols:
        for exp in expirations:
            td.manage_contracts("remove",  {'symbol': symbol, 'trading_class': symbol, 'expiration': expiration,
                'strike':200, 'right': 'P'})
    
# Run large scale setup
# success = setup_large_scale_tracking()

# 8. Monitoring and Maintenance
def daily_health_check():
    """Daily health check for historical data system"""
    
    print("=== Historical Data Health Check ===")
    
    # Check active contracts
    print("\nActive Contracts:")
    td.manage_contracts('list_active')
    
    # Check for expired contracts
    print("\nExpired Contracts (need archiving):")
    td.manage_contracts('list_expired')
    
    # Check recent update results
    import glob
    import json
    
    log_files = sorted(glob.glob("logs/update_results_*.json"))
    if log_files:
        with open(log_files[-1], 'r') as f:
            latest_results = json.load(f)
        
        print(f"\nLatest Update ({log_files[-1]}):")
        print(f"  Contracts processed: {latest_results.get('contracts_processed', 0)}")
        print(f"  Errors: {latest_results.get('contracts_errors', 0)}")
        
        if latest_results.get('errors'):
            print("  Recent errors:")
            for error in latest_results['errors'][:3]:
                print(f"    - {error}")
    
    # Check disk space usage
    from pathlib import Path
    import os
    
    hist_dir = Path("historical")
    if hist_dir.exists():
        total_size = sum(f.stat().st_size for f in hist_dir.rglob("*.parquet"))
        print(f"\nStorage Usage: {total_size / (1024*1024):.1f} MB")

# Run daily check
# daily_health_check()

# 10. PowerShell Integration Example
def export_config_for_powershell():
    """Export current configuration for PowerShell scheduler"""
    
    # This would be called by PowerShell to understand current setup
    manager = td.HistoricalDataManager()
    active_contracts = manager.config_manager.get_active_contracts()
    
    config_summary = {
        'total_active_contracts': len(active_contracts),
        'symbols': list(set(c.symbol for c in active_contracts)),
        'expirations': list(set(c.expiration for c in active_contracts)),
        'last_update': datetime.datetime.now().isoformat(),
        'settings': {
            'intraday_frequency': manager.config_manager.config.intraday_frequency,
            'historical_frequency': manager.config_manager.config.historical_frequency,
            'market_data_type': manager.config_manager.config.market_data_type
        }
    }
    
    # Save for PowerShell consumption
    with open("current_config_status.json", "w") as f:
        json.dump(config_summary, f, indent=2)
    
    return config_summary

# Usage
# config_status = export_config_for_powershell()=3):

def concurrent_collect_historical_data():
    """
    Collect historical data for multiple symbols in parallel
    
    Note: Be careful with IB rate limits - this is just an example
    """
    import concurrent.futures
    from threading import Lock
    
    # Thread-safe result collection
    results_lock = Lock()
    all_results = {}
    
    def collect_symbol(symbol):
        try:
            result = td.updateHistoricalDataWeekly([symbol], force_update=True)
            with results_lock:
                all_results[symbol] = result
            return symbol, "success"
        except Exception as e:
            with results_lock:
                all_results[symbol] = {"error": str(e)}
            return symbol, "error"
    
    # Use ThreadPoolExecutor for I/O bound tasks
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(collect_symbol, sym): sym for sym in symbols}
        
        for future in concurrent.futures.as_completed(futures):
            symbol = futures[future]
            try:
                symbol_name, status = future.result()
                print(f"Completed {symbol_name}: {status}")
            except Exception as e:
                print(f"Error processing {symbol}: {e}")
    
    return all_results

