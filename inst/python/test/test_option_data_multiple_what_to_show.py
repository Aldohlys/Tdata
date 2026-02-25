from pathlib import Path
import os
import json

import tdata_py
import tdata_py.historical_option as td
from tdata_py.historical_option import HistoricalDataConfig
from tdata_py.IB_connection import safe_ib_connect
from ib_async import *

config_file = Path("historical_config.json")



def get_option_data_with_fallback(symbol, trading_class, expiration, strike, right, data_type, include_archived=True):
    """
    Smart data retrieval with fallback strategy:
    1. Try TRADES first (actual trading activity)
    2. If no TRADES data, fall back to BID_ASK (market quotes)
    3. For historical data, also try MIDPOINT as fallback
    
    Args:
        symbol, trading_class, expiration, strike, right: Contract parameters
        data_type: 'intraday', 'historical', or 'combined'
        include_archived: Whether to check archived data
    
    Returns:
        tuple: (data, data_type_used) where data_type_used indicates which type worked
    """
    
    # Define fallback hierarchy based on data type
    if data_type in ['intraday', 'combined']:
        fallback_order = ['TRADES', 'BID_ASK']
    elif data_type == 'historical':
        fallback_order = ['TRADES', 'MIDPOINT', 'BID_ASK'] 
    else:
        fallback_order = ['TRADES', 'BID_ASK', 'MIDPOINT']
    
    print(f"[INFO] Trying data retrieval for {symbol} {trading_class} {expiration} {strike}{right} ({data_type})")
    
    for what_to_show in fallback_order:
        print(f"  Attempting {what_to_show}...")
        
        try:
            data = td.get_option_historical_data(
                symbol=symbol,
                trading_class=trading_class,
                expiration=expiration,
                strike=strike,
                right=right,
                data_type=data_type,
                what_to_show=what_to_show,
                include_archived=include_archived
            )
            
            if data is not None and len(data) > 0:
                print(f"  [SUCCESS] Found {len(data)} records using {what_to_show}")
                return data, what_to_show
            else:
                print(f"  [EMPTY] {what_to_show} returned no data")
                
        except Exception as e:
            print(f"  [ERROR] {what_to_show} failed: {e}")
            continue
    
    print(f"  [FAILED] No data found with any method")
    return None, None

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

def smart_basic_stats(option_name, symbol, trading_class, expiration, strike, right, data_type):
    """
    Enhanced basic_stats that uses smart data retrieval with fallback.
    """
    data, data_source = get_option_data_with_fallback(
        symbol, trading_class, expiration, strike, right, data_type
    )
    
    if data is not None:
        stats_title = f"{option_name} ({data_source} data)"
        basic_stats(stats_title, data)
    else:
        print(f"\n=========== {option_name} Analysis =========")
        print("[ERROR] No data available with any method (TRADES, BID_ASK, MIDPOINT)")
        print("=" * 50)

# Enhanced version that also shows what data types are actually available
def diagnose_available_data(symbol, trading_class, expiration, strike, right):
    """
    Comprehensive diagnosis of what data types are available for a contract.
    """
    print(f"\n=== DATA AVAILABILITY DIAGNOSIS ===")
    print(f"Contract: {symbol} {trading_class} {expiration} {strike}{right}")
    
    # Test all major data types for both intraday and historical
    data_types_to_test = ['TRADES', 'BID_ASK', 'MIDPOINT', 'BID', 'ASK']
    
    results = {
        'intraday': {},
        'historical': {}
    }
    
    for data_category in ['intraday', 'historical']:
        print(f"\n{data_category.upper()} DATA:")
        
        for what_to_show in data_types_to_test:
            try:
                data = td.get_option_historical_data(
                    symbol=symbol, trading_class=trading_class,
                    expiration=expiration, strike=strike, right=right,
                    data_type=data_category, what_to_show=what_to_show,
                    include_archived=True
                )
                
                if data is not None and len(data) > 0:
                    results[data_category][what_to_show] = len(data)
                    print(f"  ✓ {what_to_show}: {len(data)} records")
                else:
                    results[data_category][what_to_show] = 0
                    print(f"  ✗ {what_to_show}: No data")
                    
            except Exception as e:
                results[data_category][what_to_show] = f"Error: {e}"
                print(f"  ❌ {what_to_show}: Error - {str(e)[:50]}...")
    
    # Summary
    print(f"\n=== SUMMARY ===")
    for category, type_results in results.items():
        available_types = [k for k, v in type_results.items() if isinstance(v, int) and v > 0]
        if available_types:
            print(f"{category.capitalize()}: {', '.join(available_types)}")
        else:
            print(f"{category.capitalize()}: No data available")
    
    return results

# Usage examples:

# 1. Smart retrieval with fallback for your SAF contract
print("=== TESTING SMART RETRIEVAL ===")

saf_intraday_data, saf_intraday_type = get_option_data_with_fallback(
    "SAF", "SEJ", "20250919", 280.0, "P", "intraday"
)

saf_historical_data, saf_historical_type = get_option_data_with_fallback(
    "SAF", "SEJ", "20250919", 280.0, "P", "historical"
)

# 2. Use smart basic stats
smart_basic_stats("SAF 280 P Intraday", "SAF", "SEJ", "20250919", 280.0, "P", "intraday")
smart_basic_stats("SAF 280 P Historical", "SAF", "SEJ", "20250919", 280.0, "P", "historical")

# 3. Diagnose what's actually available
diagnose_available_data("SAF", "SEJ", "20250919", 280.0, "P")

# 4. Test your other contracts too
print("\n" + "="*60)
print("TESTING OTHER CONTRACTS")
print("="*60)

# URA contracts
smart_basic_stats("URA 39 C Intraday", "URA", "URA", "20250919", 39.0, "C", "intraday")
smart_basic_stats("URA 40 P Intraday", "URA", "URA", "20250919", 40.0, "P", "intraday")

# 5. Batch test all your active contracts
def test_all_active_contracts():
    """Test data availability for all active contracts"""
    
    # Get active contracts from config
    config = HistoricalDataConfig()
    active_contracts = config.get_active_contracts()
    
    print(f"\n=== TESTING ALL {len(active_contracts)} ACTIVE CONTRACTS ===")
    
    for contract in active_contracts:
        print(f"\nTesting: {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike}{contract.right}")
        
        # Test intraday
        intraday_data, intraday_type = get_option_data_with_fallback(
            contract.symbol, contract.trading_class, contract.expiration,
            contract.strike, contract.right, "intraday"
        )
        
        # Test historical  
        historical_data, historical_type = get_option_data_with_fallback(
            contract.symbol, contract.trading_class, contract.expiration,
            contract.strike, contract.right, "historical"
        )
        
        print(f"  Results: Intraday={intraday_type}, Historical={historical_type}")

# Uncomment to test all contracts
test_all_active_contracts()
