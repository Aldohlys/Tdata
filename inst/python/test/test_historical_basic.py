"""
Simple tests for historical_option.py
Bottom-up validation of core functionality
"""

import pytest
import pandas as pd
import json
import tempfile
from pathlib import Path
from datetime import datetime, timedelta

from fin_logger import get_logger

# Create logger for tests
logger = get_logger("test_historical_extension")

# Import the classes we're testing
from tdata_py.historical_option import ContractConfig, HistoricalDataConfig, HistoricalStorage
from tdata_py.core import CONFIG

def test_contract_config_basic():
    """Test 1: Basic ContractConfig creation and serialization"""
    
    # Create a simple contract
    contract = ContractConfig(
        symbol="SPY",
        trading_class="SPY", 
        expiration="20250321",
        strike=425.0,
        right="C",
        exchange="SMART"
    )
    
    # Validate basic properties
    assert contract.symbol == "SPY"
    assert contract.strike == 425.0
    assert contract.active == True  # Default should be True
    
    # Test serialization round-trip
    contract_dict = contract.to_dict()
    recreated = ContractConfig.from_dict(contract_dict)
    
    assert recreated.symbol == contract.symbol
    assert recreated.strike == contract.strike
    assert recreated.expiration == contract.expiration
    
    print("✓ ContractConfig basic operations work")

def test_historical_config_file_operations():
    """Test 2: HistoricalDataConfig save/load with temporary file"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as temp_file:
        temp_path = temp_file.name
    
    try:
        # Create config with temp file
        config = HistoricalDataConfig(temp_path)
        
        # Add a contract
        success = config.add_contract(
            symbol="QQQ",
            trading_class="QQQ",
            expiration="20250418", 
            strike=390.0,
            right="P",
            exchange="SMART"
        )
        
        assert success == True
        assert len(config.contracts) == 1
        
        # Create new config instance - should load from file
        config2 = HistoricalDataConfig(temp_path)
        assert len(config2.contracts) == 1
        assert config2.contracts[0].symbol == "QQQ"
        assert config2.contracts[0].strike == 390.0
        
        print("✓ HistoricalDataConfig file operations work")
        
    finally:
        # Cleanup
        Path(temp_path).unlink(missing_ok=True)

def test_storage_file_paths():
    """Test 3: HistoricalStorage file path generation"""
    
    with tempfile.TemporaryDirectory() as temp_dir:
        # Store original strikes_dir and mock it
        original_strikes_dir = CONFIG.get("strikes_dir")
        CONFIG["strikes_dir"] = temp_dir
        
        try:
            storage = HistoricalStorage()
            
            # Test active file path generation
            active_path = storage.get_active_file_path(
                symbol="SPY",
                trading_class="SPY", 
                expiration="20250321",
                strike=425.0,
                right="C",
                data_type="historical"
            )
            
            # DEBUG: Log actual values for troubleshooting
            logger.debug(f"temp_dir: {temp_dir}")
            logger.debug(f"active_path: {active_path}")
            logger.debug(f"active_path.name: {active_path.name}")
            logger.debug(f"active_path.parts: {active_path.parts}")
            
            expected_filename = "historical_historical_20250321_425.0_C.parquet"
            logger.debug(f"expected_filename: {expected_filename}")
            
            # Check filename first
            assert active_path.name == expected_filename, f"Expected {expected_filename}, got {active_path.name}"
            
            # Check path structure more robustly using path parts
            path_parts = active_path.parts
            logger.debug(f"path_parts: {path_parts}")
            
            # Should have symbol and trading_class in path
            assert "SPY" in path_parts, f"SPY not found in path parts: {path_parts}"
            # More specific: check parent directory structure
            assert active_path.parent.name == "SPY", f"Expected parent to be SPY, got {active_path.parent.name}"
            assert active_path.parent.parent.name == "SPY", f"Expected grandparent to be SPY, got {active_path.parent.parent.name}"
            
            # Test archived file path generation
            archived_path = storage.get_archived_file_path(
                symbol="SPY",
                trading_class="SPY",
                expiration="20250321", 
                strike=425.0,
                right="C"
            )
            
            logger.debug(f"archived_path: {archived_path}")
            logger.debug(f"archived_path.parts: {archived_path.parts}")
            
            assert archived_path.name == "425.0_C_final.parquet"
            
            # Check archived path structure: archived/SPY/SPY/20250321/
            archived_parts = archived_path.parts
            assert "archived" in archived_parts, f"archived not found in: {archived_parts}"
            assert "SPY" in archived_parts, f"SPY not found in archived path: {archived_parts}"
            assert "20250321" in archived_parts, f"expiration not found in archived path: {archived_parts}"
            
            print("✓ HistoricalStorage path generation works")
            
        finally:
            # Restore original CONFIG
            if original_strikes_dir is not None:
                CONFIG["strikes_dir"] = original_strikes_dir
            else:
                CONFIG.pop("strikes_dir", None)

def test_active_expired_contract_filtering():
    """Test 4: Contract filtering by expiration date"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as temp_file:
        temp_path = temp_file.name
    
    try:
        config = HistoricalDataConfig(temp_path)
        
        # Add contracts with different expiration dates
        yesterday = (datetime.now() - timedelta(days=1)).strftime("%Y%m%d")
        tomorrow = (datetime.now() + timedelta(days=1)).strftime("%Y%m%d")
        
        # Add expired contract
        config.add_contract("SPY", "SPY", yesterday, 420.0, "C", exchange="SMART")
        
        # Add future contract  
        config.add_contract("QQQ", "QQQ", tomorrow, 380.0, "P", exchange="SMART")
        
        # Add inactive future contract
        config.add_contract("IWM", "IWM", tomorrow, 200.0, "C", exchange="SMART")
        config.deactivate_contract("IWM", "IWM", tomorrow, 200.0, "C")
        
        # Test filtering
        active_contracts = config.get_active_contracts()
        expired_contracts = config.get_expired_contracts()
        
        assert len(active_contracts) == 1  # Only QQQ should be active
        assert active_contracts[0].symbol == "QQQ"
        
        assert len(expired_contracts) == 2  # SPY (expired) + IWM (inactive)
        expired_symbols = [c.symbol for c in expired_contracts]
        assert "SPY" in expired_symbols  # Expired by date
        assert "IWM" in expired_symbols  # Inactive
        
        print("✓ Contract filtering by expiration/status works")
        
    finally:
        Path(temp_path).unlink(missing_ok=True)


def test_data_save_and_load_basic():
    """Test 5: Basic data save/load operations with mock data"""
    
    with tempfile.TemporaryDirectory() as temp_dir:
        # Store original and mock CONFIG
        original_strikes_dir = CONFIG.get("strikes_dir")
        CONFIG["strikes_dir"] = temp_dir
       
        try:
            storage = HistoricalStorage()
            
            # Create mock data for multiple whatToShow types
            trades_data = pd.DataFrame({
                'datetime': pd.date_range('2025-01-01', periods=3, freq='1H'),
                'open': [100.0, 101.0, 102.0],
                'high': [100.5, 101.5, 102.5], 
                'low': [99.5, 100.5, 101.5],
                'close': [100.2, 101.2, 102.2],
                'volume': [1000, 1100, 1200]
            })
            
            bid_ask_data = pd.DataFrame({
                'datetime': pd.date_range('2025-01-01', periods=3, freq='1H'),
                'open': [99.8, 100.8, 101.8],  # avg_bid
                'high': [100.3, 101.3, 102.3],  # max_ask
                'low': [99.7, 100.7, 101.7],   # min_bid  
                'close': [100.1, 101.1, 102.1],  # avg_ask
                'volume': [0, 0, 0],
                'bid': [99.7, 100.7, 101.7],
                'ask': [100.3, 101.3, 102.3]
            })
            
            # Save data with multiple whatToShow types
            new_data_dict = {
                "TRADES": trades_data,
                "BID_ASK": bid_ask_data
            }
            
            records_added = storage.save_active_data(
                symbol="SPY",
                trading_class="SPY",
                expiration="20250321", 
                strike=425.0,
                right="C",
                exchange="SMART",
                data_type="historical",
                new_data_dict=new_data_dict,
                incremental=False  # Fresh save
            )
            
            assert records_added == 6  # 3 TRADES + 3 BID_ASK records
            
            # Load all data
            loaded_data = storage.load_active_data(
                symbol="SPY",
                trading_class="SPY", 
                expiration="20250321",
                strike=425.0,
                right="C",
                data_type="historical"
            )
            
            assert loaded_data is not None
            assert len(loaded_data) == 6  # Both data types combined
            assert set(loaded_data['what_to_show'].unique()) == {"TRADES", "BID_ASK"}
            
            # Load filtered data - only TRADES
            trades_only = storage.load_active_data(
                symbol="SPY",
                trading_class="SPY",
                expiration="20250321", 
                strike=425.0,
                right="C",
                data_type="historical",
                what_to_show="TRADES"
            )
            
            assert trades_only is not None
            assert len(trades_only) == 3  # Only TRADES records
            assert all(trades_only['what_to_show'] == "TRADES")
            
            print("✓ Data save/load with multiple whatToShow types works")
            
        finally:
            # Restore original CONFIG
            if original_strikes_dir is not None:
                CONFIG["strikes_dir"] = original_strikes_dir
            else:
                CONFIG.pop("strikes_dir", None)

if __name__ == "__main__":
    """Run all tests as simple functions"""
    
    print("Running simple tests for historical_option.py")
    print("=" * 60)
    
    # Run each test
    test_contract_config_basic()
    test_historical_config_file_operations() 
    test_storage_file_paths()
    test_active_expired_contract_filtering()
    test_data_save_and_load_basic()
    
    print("=" * 60)
    print("✅ All tests passed!")
    print("\n💡 Key validations:")
    print("  • ContractConfig serialization round-trip")
    print("  • HistoricalDataConfig file persistence")  
    print("  • Storage file path generation logic")
    print("  • Contract expiration filtering")
    print("  • Multi-type data save/load operations")
