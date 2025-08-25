"""
usage_examples.py - Examples demonstrating the refactored parquet module

Shows both basic usage and advanced patterns for efficient batch operations.
"""

from parquet import (
    getChains, getChain, getOptionStrikes, 
    ChainsDataContext,
    getAllStrikes, getStrikesInRange,
    view_parquet, list_parquet_files,
    cleanup_all, get_storage_stats
)


def basic_usage_examples():
    """Basic usage patterns following the hierarchical design."""
    
    # Example 1: Get all chains for a symbol
    print("=== Example 1: Get all chains ===")
    chains = getChains("SPY")
    if chains:
        for chain in chains:
            exchange, underlying_id, trading_class, multiplier, expirations, strikes = chain
            print(f"Trading Class: {trading_class}, Expirations: {len(expirations)}, Strikes: {len(strikes)}")
    
    # Example 2: Get specific chain for a trading class
    print("\n=== Example 2: Get specific chain ===")
    chain = getChain("SPY", tradingClass="SPY")
    if chain and not (isinstance(chain, float)):
        exchange, underlying_id, trading_class, multiplier, expirations, strikes = chain
        print(f"Found chain: {trading_class} with {len(expirations)} expirations")
        print(f"Sample expirations: {expirations[:5]}")
        print(f"Strike range: {min(strikes)} to {max(strikes)}")
    
    # Example 3: Get qualified strikes for specific expiration
    print("\n=== Example 3: Get qualified strikes ===")
    strikes = getOptionStrikes("SPY", "SPY", "20240315", strike_min=400, strike_max=500)
    if strikes:
        print(f"Found {len(strikes)} qualified strikes: {strikes[:10]}...")


def efficient_batch_operations():
    """Efficient patterns using context to avoid redundant API calls."""
    
    print("=== Efficient Batch Operations with Context ===")
    
    # Create context to cache chain data across multiple calls
    context = ChainsDataContext()
    
    # First call fetches and caches data
    chain = getChain("SPY", tradingClass="SPY", context=context)
    print(f"First call: Got chain with {len(chain[4])} expirations")
    
    # Subsequent calls reuse cached data (no additional API calls)
    strikes_1 = getOptionStrikes("SPY", "SPY", "20240315", 
                                strike_min=400, strike_max=450, context=context)
    print(f"Second call: Got {len(strikes_1) if strikes_1 else 0} strikes (400-450)")
    
    strikes_2 = getOptionStrikes("SPY", "SPY", "20240315", 
                                strike_min=450, strike_max=500, context=context)
    print(f"Third call: Got {len(strikes_2) if strikes_2 else 0} strikes (450-500)")
    
    print("All three calls used the same cached chain data!")


def convenience_functions_examples():
    """Examples using convenience functions."""
    
    print("=== Convenience Functions ===")
    
    # Get all strikes for an expiration
    all_strikes = getAllStrikes("SPY", "SPY", "20240315")
    if all_strikes:
        print(f"All strikes: {len(all_strikes)} total")
    
    # Get strikes around a center price
    center_strikes = getStrikesInRange("SPY", "SPY", "20240315", 
                                      center_strike=450, range_pct=0.05)
    if center_strikes:
        print(f"Strikes around 450 (±5%): {center_strikes}")


def utility_functions_examples():
    """Examples of utility functions for file management."""
    
    print("=== Utility Functions ===")
    
    # List available files
    print("Available parquet files:")
    list_parquet_files(symbol="SPY")
    
    # View specific file contents
    print("\nViewing specific expiration file:")
    df = view_parquet("20240315.parquet")  # Will search for this file
    
    # Get storage statistics
    stats = get_storage_stats()
    if "error" not in stats:
        print(f"\nStorage stats: {stats['total_files']} files, "
              f"{stats['total_size_kb']:.1f} KB total")
    
    # Cleanup expired files (dry run)
    print("\nCleanup preview:")
    cleanup_results = cleanup_all(dry_run=True)
    if "error" not in cleanup_results:
        print(f"Would delete {cleanup_results['expired_files']} expired files")


def error_handling_examples():
    """Examples of proper error handling."""
    
    print("=== Error Handling Examples ===")
    
    # Handle case where symbol doesn't exist
    chains = getChains("NONEXISTENT")
    if chains is None:
        print("No connection to IBKR or connection error")
    elif isinstance(chains, float):
        print("Symbol not found or no chains available")
    elif isinstance(chains, list):
        print(f"Found {len(chains)} chains")
    
    # Handle case where trading class doesn't exist
    chain = getChain("SPY", tradingClass="INVALID")
    if chain is None:
        print("Connection error")
    elif isinstance(chain, float):
        print("Trading class not found")
    else:
        print("Chain found")
    
    # Handle case where strikes don't exist in range
    strikes = getOptionStrikes("SPY", "SPY", "20240315", 
                              strike_min=10000, strike_max=20000)
    if strikes is None:
        print("Error fetching strikes")
    elif len(strikes) == 0:
        print("No strikes found in range")
    else:
        print(f"Found {len(strikes)} strikes")


def parallel_processing_pattern():
    """Example pattern for R integration with parallel processing."""
    
    print("=== R Integration Pattern ===")
    
    # This function would be called from R with a batch of contracts
    def process_contract_batch(contracts_batch):
        """
        Process a batch of contracts efficiently.
        Designed to be called from R's parallel processing.
        
        Args:
            contracts_batch (list): List of (symbol, trading_class, expiration, strike_range) tuples
            
        Returns:
            dict: Results for each contract
        """
        results = {}
        context = ChainsDataContext()  # Share context across batch
        
        for symbol, trading_class, expiration, strike_min, strike_max in contracts_batch:
            try:
                strikes = getOptionStrikes(symbol, trading_class, expiration,
                                         strike_min, strike_max, context=context)
                results[f"{symbol}_{trading_class}_{expiration}"] = {
                    "strikes": strikes,
                    "count": len(strikes) if strikes else 0,
                    "status": "success"
                }
            except Exception as e:
                results[f"{symbol}_{trading_class}_{expiration}"] = {
                    "strikes": None,
                    "count": 0,
                    "status": f"error: {e}"
                }
        
        return results
    
    # Example batch
    sample_batch = [
        ("SPY", "SPY", "20240315", 400, 500),
        ("SPY", "SPY", "20240415", 400, 500),
        ("QQQ", "QQQ", "20240315", 300, 400)
    ]
    
    results = process_contract_batch(sample_batch)
    for contract_id, result in results.items():
        print(f"{contract_id}: {result['status']} - {result['count']} strikes")


def main():
    """Run all examples."""
    try:
        basic_usage_examples()
        print("\n" + "="*50)
        
        efficient_batch_operations()
        print("\n" + "="*50)
        
        convenience_functions_examples()
        print("\n" + "="*50)
        
        utility_functions_examples()
        print("\n" + "="*50)
        
        error_handling_examples()
        print("\n" + "="*50)
        
        parallel_processing_pattern()
        
    except Exception as e:
        print(f"Example execution error: {e}")


if __name__ == "__main__":
    main()
