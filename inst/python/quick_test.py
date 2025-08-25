# Run this step-by-step diagnostic (save as quick_test.py):

import time
from tdata_py.parquet import getChains, getChain

print("=== QUICK DIAGNOSTIC TEST ===")

# Test 1: Fast operations (should be instant)
print("\n1. Testing getChains (should use cache)...")
start = time.time()
chains = getChains("SPX")
elapsed = time.time() - start
print(f"   Time: {elapsed:.2f}s, Result: {len(chains) if isinstance(chains, list) else 'Failed'}")

# Test 2: getChain (should be instant)
print("\n2. Testing getChain...")
start = time.time()
chain = getChain("SPX", tradingClass="SPXW")
elapsed = time.time() - start
print(f"   Time: {elapsed:.2f}s, Result: {'Success' if chain and not isinstance(chain, float) else 'Failed'}")

# Test 3: Check what we have without qualifying strikes
if chain and not isinstance(chain, float):
    expirations = chain[4]
    strikes = chain[5]
    print(f"\n3. Available data (no IBKR calls):")
    print(f"   Expirations: {len(expirations)} (first: {expirations[0] if expirations else 'None'})")
    print(f"   Strikes: {len(strikes)} (range: {min(strikes):.0f}-{max(strikes):.0f})")
    
    # Test 4: Try ONE contract qualification only
    print(f"\n4. Testing SINGLE contract qualification...")
    from tdata_py.parquet import getOptionStrikes
    
    if expirations:
        first_exp = expirations[0]
        # Test with just 1 strike
        print(f"   Qualifying just 1 strike for {first_exp}...")
        start = time.time()
        
        try:
            result = getOptionStrikes("SPX", "SPXW", first_exp, strike_min=5600, strike_max=5600)
            elapsed = time.time() - start
            print(f"   Time: {elapsed:.2f}s, Result: {result}")
            
            if elapsed > 30:
                print("   ❌ PROBLEM: Single contract took >30s - IBKR connection issue")
            elif result:
                print("   ✅ Single contract works - issue is batch size")
            else:
                print("   ⚠️  Single contract failed - may be API/connection issue")
                
        except Exception as e:
            elapsed = time.time() - start
            print(f"   ❌ Error after {elapsed:.2f}s: {e}")

print("\n=== DIAGNOSTIC COMPLETE ===")
