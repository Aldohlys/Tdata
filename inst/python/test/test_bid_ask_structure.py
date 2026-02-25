
from pathlib import Path
import os
import json

import tdata_py
import tdata_py.historical_option as td
from tdata_py.IB_connection import safe_ib_connect
from ib_async import *

# Test with your SAF data
saf_data = td.get_option_historical_data(
    symbol="SAF", trading_class="SEJ", expiration="20250919", 
    strike=280.0, right='P', data_type="intraday", 
    what_to_show="BID_ASK", include_archived=True
)

# Test URA pattern
ura_data = td.get_option_historical_data(
    symbol="URA", trading_class="URA", expiration="20250919", 
    strike=39.0, right='C', data_type="intraday", 
    what_to_show="BID_ASK", include_archived=True
)

def detect_bid_ask_structure_fixed(data, symbol, trading_class, exchange):
    """Fixed structure detection logic."""
    
    # Check the actual patterns we care about
    open_eq_close = (data['open'] == data['close']).mean()  # Both represent same price
    high_eq_close = (data['high'] == data['close']).mean()  # High equals close
    open_eq_high = (data['open'] == data['high']).mean()    # Open equals high
    
    # The key pattern: all three (open, high, close) are identical = ask price
    all_ask_same = (
        (data['open'] == data['high']) & 
        (data['high'] == data['close']) & 
        (data['open'] == data['close'])
    ).mean()
    
    # Check if low is consistently different (bid price)
    low_different = (data['low'] != data['close']).mean()
    
    print(f"Detection metrics:")
    print(f"  open=close: {open_eq_close*100:.1f}%")
    print(f"  high=close: {high_eq_close*100:.1f}%")  
    print(f"  open=high: {open_eq_high*100:.1f}%")
    print(f"  open=high=close (all ask): {all_ask_same*100:.1f}%")
    print(f"  low different from close: {low_different*100:.1f}%")
    
    if all_ask_same > 0.9 and low_different > 0.8:
        return 'ASK_ONLY'  # SAF pattern
    elif all_ask_same < 0.1 and low_different > 0.8:
        return 'TIME-AVERAGED'         # Proper time-averaged
    else:
        return 'UNKNOWN'

def analyze_ibkr_bidask_universal(option_name, data):
    """Universal analysis for all IBKR option BID_ASK data."""
    
    print(f"\n=== {option_name} Analysis ===")
    
    # IBKR's universal option BID_ASK structure
    data['bid'] = data['low']
    data['ask'] = data['close']  # Same as open/high
    data['midpoint'] = (data['bid'] + data['ask']) / 2
    data['spread'] = data['ask'] - data['bid']
    data['spread_pct'] = (data['spread'] / data['midpoint']) * 100
    
    print(f"Records: {len(data)}")
    print(f"Midpoint: ${data['midpoint'].mean():.3f} (±${data['midpoint'].std():.3f})")
    print(f"Avg spread: ${data['spread'].mean():.3f} ({data['spread_pct'].mean():.1f}%)")
    
    # Liquidity indicator
    zero_spreads = (data['spread'] == 0).sum()
    print(f"Locked market: {zero_spreads}/{len(data)} periods ({zero_spreads/len(data)*100:.1f}%)")
    
    return data

# Apply universally to any option BID_ASK data
analyze_ibkr_bidask_universal("SAF 280P", saf_data)
analyze_ibkr_bidask_universal("URA 39C", ura_data)

print("SAF Pattern Detection:")
pattern = detect_bid_ask_structure_fixed(saf_data, "SAF", "SEJ", "EUREX")
print(f"Result: {pattern}")

####################  URA TESTS

if ura_data is not None:
    print("URA Pattern Detection:")
    ura_pattern = detect_bid_ask_structure_fixed(ura_data, "URA", "URA", "US_SMART")
    print(f"Result: {ura_pattern}")
    
    # Show sample data structure
    print("\nURA BID_ASK sample:")
    print(ura_data[['datetime', 'open', 'high', 'low', 'close']].head(3))
else:
    print("No URA BID_ASK data available")




################################  SPY TESTS 
# Add SPY ATM contract for testing
# With SPY at $646.30, use 646 strike for ATM
spy_result = td.manage_contracts(
    "add", 
    symbol="SPY", 
    trading_class="SPY", 
    expiration='20250919',  # Use same expiration as your other contracts
    strike=646.0, 
    right='C', 
    exchange='SMART'
)

# Collect data for SPY
spy_update = td.update_historical_data(data_type='intraday')

# Get SPY BID_ASK data
spy_bidask = td.get_option_historical_data(
    symbol="SPY", 
    trading_class="SPY", 
    expiration="20250919", 
    strike=646.0, 
    right='C', 
    data_type="intraday", 
    what_to_show="BID_ASK", 
    include_archived=True
)

if spy_bidask is not None:
    print("SPY 646C BID_ASK Pattern Detection:")
    spy_pattern = detect_bid_ask_structure_fixed(spy_bidask, "SPY", "SPY", "US_SMART")
    print(f"Result: {spy_pattern}")
    
    print("\nSPY BID_ASK sample (highly liquid):")
    print(spy_bidask[['datetime', 'open', 'high', 'low', 'close']].head(5))
    
    # Apply universal analysis
    spy_analyzed = analyze_ibkr_bidask_universal("SPY 646C ATM", spy_bidask)
    
    print(f"\nSPY Liquidity Comparison:")
    print(f"  SPY spread: ${spy_analyzed['spread'].mean():.3f}")
    print(f"  URA spread: ${ura_data['ask'].mean() - ura_data['bid'].mean():.3f}")  
    print(f"  SAF spread: ${saf_data['close'].mean() - saf_data['low'].mean():.3f}")
    
else:
    print("No SPY BID_ASK data collected yet - may need to wait for next collection cycle")
