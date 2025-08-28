"""
basic_python_tests.py - Basic validation of current parquet.py implementation

Run these tests to validate your existing implementation before migration.
Also helps diagnose the 'exchange' column issue in existing Parquet files.
"""

import sys
import math
import pandas as pd
from pathlib import Path

# Add your tdata_py module to path if needed
# sys.path.append('path/to/your/tdata_py')

try:
    from tdata_py.chains_manager import getChains, getChain, getOptionStrikes
    from tdata_py.core import CONFIG
    print("✅ Successfully imported parquet functions")
except ImportError as e:
    print(f"❌ Import error: {e}")
    sys.exit(1)

def test_basic_functionality():
    """Test basic functionality with your current implementation."""
    print("\n=== BASIC FUNCTIONALITY TEST ===")
    
    # Test symbols - use the ones you're already testing
    test_symbols = ["SPX", "SPY", "QQQ", "ESTX50", "AI", "TTE"]
    
    for symbol in test_symbols:
        print(f"\nTesting {symbol}...")
        
        # Test getChains
        print(f"  getChains('{symbol}')...")
        try:
            chains = getChains(symbol)
            
            if chains is None:
                print(f"    ❌ {symbol}: getChains returned None (connection issue)")
                continue
            elif isinstance(chains, float) and math.isnan(chains):
                print(f"    ⚠️  {symbol}: getChains returned NaN (not found)")
                continue
            elif isinstance(chains, list) and len(chains) > 0:
                print(f"    ✅ {symbol}: Found {len(chains)} chains")
                
                # Analyze first chain structure
                first_chain = chains[0]
                if len(first_chain) >= 6:
                    exchange = first_chain[0]
                    underlying_id = first_chain[1] 
                    trading_class = first_chain[2]
                    multiplier = first_chain[3]
                    expirations = first_chain[4]
                    strikes = first_chain[5]
                    
                    print(f"      Exchange: {exchange}")
                    print(f"      Trading Class: {trading_class}")
                    print(f"      Underlying ID: {underlying_id}")
                    print(f"      Multiplier: {multiplier}")
                    print(f"      Expirations: {len(expirations)} (first: {expirations[0] if expirations else 'None'})")
                    print(f"      Strikes: {len(strikes)} (range: {min(strikes):.0f}-{max(strikes):.0f})")
                    
                    # Test getChain for this trading class
                    print(f"  getChain('{symbol}', tradingClass='{trading_class}')...")
                    try:
                        chain = getChain(symbol, tradingClass=trading_class)
                        if chain is not None and not (isinstance(chain, float) and math.isnan(chain)):
                            print(f"    ✅ {symbol}: getChain successful")
                            
                            # Test getOptionStrikes if we have expirations
                            if len(expirations) > 0:
                                first_exp = expirations[0]
                                print(f"  getOptionStrikes('{symbol}', '{trading_class}', '{first_exp}', limited range)...")
                                
                                # Use a limited strike range for testing
                                current_price_estimate = {
                                    "SPX": 6400,
                                    "SPY": 640, 
                                    "ESTX50": 5400,
                                    "QQQ": 560
                                }.get(symbol, min(strikes) + (max(strikes) - min(strikes)) / 2)
                                
                                strike_min = current_price_estimate * 0.9
                                strike_max = current_price_estimate * 1.10
                                
                                try:
                                    strikes_result = getOptionStrikes(symbol, trading_class, first_exp, 
                                                                    strike_min=strike_min, strike_max=strike_max)
                                    
                                    if strikes_result is not None and len(strikes_result) > 0:
                                        print(f"    ✅ {symbol}: getOptionStrikes returned {len(strikes_result)} qualified strikes")
                                        print(f"      Range: {min(strikes_result):.0f}-{max(strikes_result):.0f}")
                                    else:
                                        print(f"    ⚠️  {symbol}: No qualified strikes in range")
                                except Exception as e:
                                    print(f"    ❌ {symbol}: getOptionStrikes failed: {e}")
                            else:
                                print(f"    ⚠️  {symbol}: No expirations available for strike testing")
                        else:
                            print(f"    ❌ {symbol}: getChain failed")
                    except Exception as e:
                        print(f"    ❌ {symbol}: getChain error: {e}")
                else:
                    print(f"    ❌ {symbol}: Invalid chain structure (length {len(first_chain)})")
            else:
                print(f"    ❌ {symbol}: Unexpected chains format: {type(chains)}")
                
        except Exception as e:
            print(f"    ❌ {symbol}: getChains error: {e}")


def test_data_structure_consistency():
    """Test that the data structure returned matches expected format."""
    print("\n=== DATA STRUCTURE CONSISTENCY TEST ===")
    
    try:
        chains = getChains("SPX")
        
        if chains is None or isinstance(chains, float):
            print("❌ Cannot test structure - no valid chains data")
            return
        
        print(f"Testing structure of {len(chains)} chains...")
        
        for i, chain in enumerate(chains):
            print(f"\nChain {i+1}:")
            print(f"  Type: {type(chain)}")
            print(f"  Length: {len(chain)}")
            
            if len(chain) >= 6:
                # Expected structure: [exchange, underlying_id, trading_class, multiplier, expirations, strikes]
                exchange = chain[0]
                underlying_id = chain[1]
                trading_class = chain[2] 
                multiplier = chain[3]
                expirations = chain[4]
                strikes = chain[5]
                
                print(f"  [0] Exchange: {exchange} ({type(exchange)})")
                print(f"  [1] Underlying ID: {underlying_id} ({type(underlying_id)})")
                print(f"  [2] Trading Class: {trading_class} ({type(trading_class)})")
                print(f"  [3] Multiplier: {multiplier} ({type(multiplier)})")
                print(f"  [4] Expirations: {type(expirations)} with {len(expirations)} items")
                print(f"  [5] Strikes: {type(strikes)} with {len(strikes)} items")
                
                # Validate types
                issues = []
                if not isinstance(exchange, str):
                    issues.append(f"Exchange should be string, got {type(exchange)}")
                if not isinstance(trading_class, str):
                    issues.append(f"Trading class should be string, got {type(trading_class)}")
                if not isinstance(expirations, list):
                    issues.append(f"Expirations should be list, got {type(expirations)}")
                if not isinstance(strikes, list):
                    issues.append(f"Strikes should be list, got {type(strikes)}")
                
                if issues:
                    print(f"  ❌ Issues found:")
                    for issue in issues:
                        print(f"    - {issue}")
                else:
                    print(f"  ✅ Structure looks correct")
            else:
                print(f"  ❌ Unexpected chain length: {len(chain)}")
                
    except Exception as e:
        print(f"❌ Structure test failed: {e}")


def test_caching_behavior():
    """Test current caching behavior."""
    print("\n=== CACHING BEHAVIOR TEST ===")
    
    symbol = "SPY"  # Use a different symbol than SPX for this test
    
    print(f"Testing caching behavior for {symbol}...")
    
    # First call
    print("First call to getChains...")
    import time
    start = time.time()
    chains1 = getChains(symbol)
    time1 = time.time() - start
    
    if chains1 is None or isinstance(chains1, float):
        print(f"❌ First call failed")
        return
    
    print(f"First call completed in {time1:.3f} seconds")
    
    # Second call - should be faster if caching works
    print("Second call to getChains...")
    start = time.time()
    chains2 = getChains(symbol)
    time2 = time.time() - start
    
    print(f"Second call completed in {time2:.3f} seconds")
    
    # Compare results
    if chains1 == chains2:
        print("✅ Results are identical")
    else:
        print("❌ Results differ between calls")
    
    # Speed comparison
    if time2 < time1 * 0.8:  # 20% faster indicates caching
        print(f"✅ Second call was {time1/time2:.1f}x faster - caching appears to work")
    else:
        print(f"⚠️  Second call not significantly faster - caching may not be working")


def main():
    """Run all basic tests."""
    print("BASIC PYTHON TESTING FOR PARQUET FUNCTIONALITY")
    print("=" * 50)
    
    test_basic_functionality()
    test_data_structure_consistency()
    test_caching_behavior()
    
    print("\n" + "=" * 50)
    print("BASIC TESTING COMPLETE")
    print("\nIf you see ❌ errors:")
    print("1. Check IBKR connection")
    print("2. Review Parquet file structure")
    print("3. Consider cleaning up corrupted files")
    print("\nIf you see ✅ success marks, your current implementation is working!")


if __name__ == "__main__":
    main()
