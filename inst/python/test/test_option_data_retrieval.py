
from pathlib import Path
import os
import json

import tdata_py
import tdata_py.historical_option as td
from tdata_py.IB_connection import safe_ib_connect
from ib_insync import *

config_file = Path("historical_config.json")

# Remove the test config file to start fresh
print("=== Force cleaning config file ===")

try:
    if config_file.exists():
        config_file.unlink()  # More reliable than os.remove()
        print("Successfully removed config file")
    else:
        print("Config file doesn't exist")
except Exception as e:
    print(f"Error removing file: {e}")

contracts_config = [
    {'symbol': 'URA', 'trading_class': 'URA', 'expiration': '20250919', 'strike': 39.0, 'right': 'C', 'exchange':'SMART'},
    {'symbol': 'URA', 'trading_class': 'URA', 'expiration': '20250919', 'strike': 40.0, 'right': 'P', 'exchange':'SMART'},
    {'symbol': 'SAF', 'trading_class': 'SEJ', 'expiration': '20250919', 'strike': 280.0, 'right': 'C', 'exchange':'EUREX'}
]

td.add_historical_tracking({'symbol':"SAF", 'trading_class':"SEJ", 'expiration':'20250919', 'strike':280.0, 'right':'P', 'exchange':'EUREX'})

print("Setting up original contracts...")
success = td.add_historical_tracking(contracts_config)
print(f"Setup success: {success}")

# Verify the content
if config_file.exists():
    with open(config_file, 'r') as f:
        data = json.load(f)
    print(f"Contracts in file: {len(data.get('contracts', []))}")
    for i, contract in enumerate(data.get('contracts', [])):
        print(f"  {i+1}: {contract['symbol']} {contract['trading_class']} {contract['expiration']} {contract['strike']} {contract['right']}")

# Test manager
print("\n=== Testing manager ===")
manager = td.HistoricalDataManager()
active_contracts = manager.config_manager.get_active_contracts()
print(f"Active contracts: {len(active_contracts)}")


# Retrieve historical data in one shot
print("=== Testing Historical Data retrieval ===")
# results = manager.collect_data_for_active_contracts(data_type='both')
results = td.update_historical_data(data_type='both')
    
# Check what files were actually created
print("=== Verifying Saved Data ===")

strikes_dir = Path(td.CONFIG.get("strikes_dir", "strikes"))
ura_dir = strikes_dir / "URA" / "URA"

print(f"URA directory exists: {ura_dir.exists()}")

if ura_dir.exists():
    files = list(ura_dir.glob("*.parquet"))
    print(f"Files in URA directory: {len(files)}")
    for file in files:
        print(f"  {file.name}")
        
# Check what was collected
print(f"Collection results: {results}")

def basic_stats(option_name, data):
    print(f"\n=========== {option_name} Analysis =========")
    
        # Check if data is None first
    if data is None:
        print("[ERROR] No data retrieved - data is None")
        print("Possible causes:")
        print("  - Contract not found in cache/storage")
        print("  - Invalid contract parameters")
        print("  - Data file doesn't exist")
        print("=" * 50)
        return
    
    # Check if data is empty DataFrame
    if len(data) == 0:
        print("[WARNING] Data retrieved but DataFrame is empty")
        print("=" * 50)
        return
    
    # Filter out zero-volume records for trade-based analysis
    traded_data = data[data['volume'] > 0]
    
    # Basic statistics using all data vs traded data
    print(f"Total records: {len(data)} (including {len(data) - len(traded_data)} zero-volume)")
    print(f"Records with trades: {len(traded_data)}")
    
    if len(traded_data) == 0:
        print("⚠️  No actual trades found (all volume = 0)")
        print(f"Date range (all records): {data['datetime'].min()} to {data['datetime'].max()}")
        print(f"Price range (all records): ${data['close'].min():.2f} - ${data['close'].max():.2f}")
        return
    
    # Date and price analysis - use traded data only
    print(f"Date range (trades): {traded_data['datetime'].min()} to {traded_data['datetime'].max()}")
    print(f"Price range (trades): ${traded_data['close'].min():.2f} - ${traded_data['close'].max():.2f}")
    print(f"Volume-weighted avg price: ${((traded_data['close'] * traded_data['volume']).sum() / traded_data['volume'].sum()):.2f}")
    
    # Volume analysis - only meaningful for traded data
    total_volume = traded_data['volume'].sum()
    avg_volume = traded_data['volume'].mean()
    print(f"Total volume: {total_volume:,}")
    print(f"Average volume per trade bar: {avg_volume:.1f}")
    
    # Trading activity pattern
    zero_vol_pct = ((len(data) - len(traded_data)) / len(data)) * 100
    print(f"Zero-volume bars: {zero_vol_pct:.1f}% of all records")
    
    # Recent trading activity - show last 5 actual trades
    print(f"\nMost recent 5 actual trades:")
    recent_trades = traded_data.tail(5)[['datetime', 'close', 'volume']]
    if len(recent_trades) > 0:
        print(recent_trades)
    else:
        print("No recent trades with volume > 0")
    
    # Show most recent overall record for context
    print(f"\nMost recent record (any volume):")
    latest = data.tail(1)[['datetime', 'close', 'volume']]
    print(latest)
    
    # Data type breakdown - show both total and traded counts
    print(f"\nData composition:")
    for data_type in data['data_type'].unique():
        total_count = len(data[data['data_type'] == data_type])
        traded_count = len(traded_data[traded_data['data_type'] == data_type])
        print(f"  {data_type}: {total_count} total ({traded_count} with trades)")
    
    # Volume distribution for traded bars
    if len(traded_data) > 1:
        print(f"\nVolume statistics (traded bars only):")
        print(f"  Min volume: {traded_data['volume'].min():,}")
        print(f"  Max volume: {traded_data['volume'].max():,}")
        print(f"  Median volume: {traded_data['volume'].median():.1f}")
        
        # Price volatility for traded data
        price_std = traded_data['close'].std()
        price_range = traded_data['close'].max() - traded_data['close'].min()
        print(f"\nPrice volatility (traded bars only):")
        print(f"  Standard deviation: ${price_std:.3f}")
        print(f"  Price range: ${price_range:.2f}")
        print(f"  Coefficient of variation: {(price_std / traded_data['close'].mean()) * 100:.1f}%")
    
    # Check for data quality issues
    print(f"\nData quality checks:")
    
    # Check for potential data gaps in traded data
    if len(traded_data) > 1:
        traded_sorted = traded_data.sort_values('datetime')
        time_diffs = traded_sorted['datetime'].diff().dropna()
        if len(time_diffs) > 0:
            avg_gap = time_diffs.mean()
            max_gap = time_diffs.max()
            print(f"  Average time between trades: {avg_gap}")
            print(f"  Largest gap between trades: {max_gap}")
    
    # Check for unusual price movements in traded data
    if len(traded_data) > 1:
        price_changes = traded_data['close'].pct_change().dropna()
        if len(price_changes) > 0:
            large_moves = price_changes[abs(price_changes) > 0.1]  # >10% moves
            if len(large_moves) > 0:
                print(f"  Large price moves (>10%): {len(large_moves)} occurrences")
            else:
                print(f"  No extreme price moves detected")
    
    print("=" * 50)

ura_call_historical_data = td.get_option_historical_data(symbol="URA", trading_class="URA", expiration="20250919", strike=39.0, right='C', data_type="historical", what_to_show="TRADES", include_archived=True)
ura_call_intraday_data = td.get_option_historical_data(symbol="URA", trading_class="URA", expiration="20250919", strike=39.0, right='C', data_type="intraday", what_to_show="TRADES", include_archived=True)
ura_put_historical_data = td.get_option_historical_data(symbol="URA", trading_class="URA", expiration="20250919", strike=40.0, right='P', data_type="historical", what_to_show="TRADES", include_archived=True)
ura_put_intraday_data = td.get_option_historical_data(symbol="URA", trading_class="URA", expiration="20250919", strike=40.0, right='P', data_type="intraday", what_to_show="TRADES", include_archived=True)
saf_put_intraday_data = td.get_option_historical_data(symbol="SAF", trading_class="SEJ", expiration="20250919", strike=280.0, right='P', data_type="intraday", what_to_show="BID_ASK", include_archived=True)
saf_put_historical_data = td.get_option_historical_data(symbol="SAF", trading_class="SEJ", expiration="20250919", strike=280.0, right='P', data_type="historical", what_to_show="TRADES", include_archived=True)

#### For SAF (low liquidity) - only BID_ASK will work with the following semantics:
## Type	|  Open	 | High	| Low	| Close	| Volume
## BID_ASK | Time-average bid	| Max Ask	| Min Bid	| Time-average ask |	N/A

basic_stats("URA 39 C Historical", ura_call_historical_data)
basic_stats("URA 39 C Intraday", ura_call_intraday_data)
basic_stats("URA 40 P Historical", ura_put_historical_data)
basic_stats("URA 40 P Intraday", ura_put_intraday_data)

basic_stats("SAF 280 P Intraday", saf_put_intraday_data) 
basic_stats("SAF 280 P Historical", saf_put_historical_data) 

