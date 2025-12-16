"""
historical_option.py - Extension for storing historical option data

Features:
- Complete contract specification in configuration (symbol, trading_class, expiration, strike, right)
- Uses existing strikes directory structure
- Multiple data types from IBKR API (TRADES, BID_ASK, MIDPOINT, etc.)
- Incremental and full historical data retrieval
- Configurable timeouts for different operation types
"""

import datetime
import logging
from pathlib import Path
from typing import Dict, List, Optional, Set
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import json
import shutil

from .core import CONFIG, ticker_db
from .IB_connection import safe_ib_connect
from .chains_manager import getOptionStrikes
from fin_logger import get_logger

logger = get_logger("tdata_py.historical_option_data")

### Individual option contract definition
class ContractConfig:
    """Configuration for a specific option contract to track"""
    def __init__(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, exchange:str, active: bool = True, last_updated: Optional[str] = None):
        self.symbol = symbol
        self.trading_class = trading_class
        self.expiration = expiration
        self.strike = strike
        self.right = right
        self.exchange = exchange
        self.active = active
        self.last_updated = last_updated

    def to_dict(self):
        return {
            'symbol': self.symbol,
            'trading_class': self.trading_class,
            'expiration': self.expiration,
            'strike': self.strike,
            'right': self.right,
            'exchange': self.exchange,
            'active': self.active,
            'last_updated': self.last_updated
        }

    @classmethod
    def from_dict(cls, data: dict):
        # Validate trading_class
        trading_class = data.get('trading_class')
        symbol = data.get('symbol', 'UNKNOWN')

        # Check if trading_class is empty, None, or an empty list/tuple
        if not trading_class or (isinstance(trading_class, (list, tuple)) and len(trading_class) == 0):
            raise ValueError(
                f"Invalid or missing trading_class for symbol '{symbol}'. "
                f"Got: {repr(trading_class)} (type: {type(trading_class).__name__}). "
                f"Please set trading class in database using R: setTicker('{symbol}', TradingClass='...')"
            )

        # Ensure trading_class is a string
        trading_class_str = str(trading_class)

        return cls(
            symbol=symbol,
            trading_class=trading_class_str,
            expiration=data['expiration'],
            strike=data['strike'],
            right=data['right'],
            exchange=data['exchange'],
            active=data.get('active', True),
            last_updated=data.get('last_updated')
        )


### Layer on top of actual Parquet files structure
### This provide the ability to load/save/archive data as needed
class HistoricalStorage:
    """
    Storage using existing strikes directory structure.
    
    Uses existing CONFIG paths:
    - Active: strikes_dir/symbol/trading_class/historical_{datatype}_{expiration}_{strike}_{right}.parquet
    - Archived: strikes_dir/archived/symbol/trading_class/expiration/{strike}_{right}_{datatype}_final.parquet
    """
    
    def __init__(self):
        # Use existing strikes directory from CONFIG
        self.strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
        self.archived_dir = self.strikes_dir / "archived"
        self._ensure_directories_exist()
    
    def _ensure_directories_exist(self):
        """Create archived directory if needed"""
        self.archived_dir.mkdir(parents=True, exist_ok=True)
        logger.debug(f"Historical storage using existing strikes directory: {self.strikes_dir}")
    
    def get_active_file_path(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, data_type: str) -> Path:
        """
        Get file path for active contract data.
        
        Format: historical_{data_type}_{expiration}_{strike}_{right}.parquet
        """
        filename = f"historical_{data_type}_{expiration}_{strike}_{right}.parquet"
        return self.strikes_dir / symbol / trading_class / filename
    
    def get_archived_file_path(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str) -> Path:
        """Get file path for archived contract data"""
        filename = f"{strike}_{right}_final.parquet"
        return self.archived_dir / symbol / trading_class / expiration / filename
    
    ### exchange saved for metadata - but not needed strictly to define where to look for
    def save_active_data(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, exchange:str, data_type: str, new_data_dict: Dict, incremental: bool = True):
        """
        Save data for active contract with incremental update support.
        
        Args:
            new_data_dict: Dict with whatToShow as keys, DataFrames as values
                          e.g., {"TRADES": df1, "OPTION_IMPLIED_VOLATILITY": df2}
        """
        file_path = self.get_active_file_path(symbol, trading_class, expiration, 
                                            strike, right, data_type)
        file_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Load existing data if incremental
        existing_data = None
        if incremental and file_path.exists():
            existing_data = self._load_parquet_safe(file_path)
        
        # Process each data type and combine into single DataFrame
        all_new_records = []
        
        for what_to_show, df in new_data_dict.items():
            if df is not None and not df.empty:
                # Add what_to_show column to identify data source
                df_copy = df.copy()
                df_copy['what_to_show'] = what_to_show
                all_new_records.append(df_copy)
        
        if not all_new_records:
            return 0
        
        # Combine all new data
        new_combined_data = pd.concat(all_new_records, ignore_index=True)
        
        # Merge with existing data if present
        if existing_data is not None and not existing_data.empty:
            combined_data = pd.concat([existing_data, new_combined_data], ignore_index=True)
            # Deduplicate by datetime and what_to_show
            # Use 'last' to update with new data while preserving old historical values
            # This allows: (1) keeping old data IBKR no longer provides, (2) updating with fresh data
            combined_data = combined_data.drop_duplicates(subset=['datetime', 'what_to_show'], keep='last')
            combined_data = combined_data.sort_values(['datetime', 'what_to_show'])
        else:
            combined_data = new_combined_data
        
        # Add metadata columns
        combined_data['symbol'] = symbol
        combined_data['trading_class'] = trading_class
        combined_data['expiration'] = expiration
        combined_data['strike'] = strike
        combined_data['right'] = right
        combined_data['exchange'] = exchange
        combined_data['data_type'] = data_type
        combined_data['last_updated'] = datetime.datetime.now().isoformat()
        
        # Save with compression
        table = pa.Table.from_pandas(combined_data)
        pq.write_table(table, file_path, compression='snappy')
        
        logger.debug(f"Saved {data_type} data: {file_path} ({len(combined_data)} total bars)")
        return len(new_combined_data)  # Return count of new records added
    
    def load_active_data(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, data_type: str, what_to_show: str = None) -> Optional[pd.DataFrame]:
        """
        Load data for active contract.
        
        Args:
            what_to_show: If specified, filter to only this data type. If None, return all.
        """
        file_path = self.get_active_file_path(symbol, trading_class, expiration, 
                                            strike, right, data_type)
        data = self._load_parquet_safe(file_path)
        
        if data is not None and what_to_show is not None:
            # Filter to specific what_to_show type
            filtered_data = data[data['what_to_show'] == what_to_show]
            return filtered_data if not filtered_data.empty else None
        
        return data
    
    def load_archived_data(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, what_to_show: str = None) -> Optional[pd.DataFrame]:
        """Load archived data, optionally filtered by what_to_show"""
        file_path = self.get_archived_file_path(symbol, trading_class, expiration, 
                                              strike, right)
        data = self._load_parquet_safe(file_path)
        
        if data is not None and what_to_show is not None:
            # Filter to specific what_to_show type
            filtered_data = data[data['what_to_show'] == what_to_show]
            return filtered_data if not filtered_data.empty else None
        
        return data
    
    def archive_contract(self, contract: ContractConfig, what_to_show_list: List[str]) -> Dict:
        """Archive expired or inactive contract data."""
        results = {"archived_files": 0, "errors": []}
        
        try:
            # Load both intraday and historical data (all what_to_show types combined)
            intraday_data = self.load_active_data(
                contract.symbol, contract.trading_class, contract.expiration,
                contract.strike, contract.right, "intraday"
            )
            historical_data = self.load_active_data(
                contract.symbol, contract.trading_class, contract.expiration,
                contract.strike, contract.right, "historical"
            )
            
            # Combine all available data
            combined_data = []
            if intraday_data is not None and not intraday_data.empty:
                combined_data.append(intraday_data)
            if historical_data is not None and not historical_data.empty:
                combined_data.append(historical_data)
            
            if combined_data:
                final_data = pd.concat(combined_data, ignore_index=True)
                final_data = final_data.drop_duplicates(subset=['datetime', 'what_to_show'], keep='last')
                final_data = final_data.sort_values(['datetime', 'what_to_show'])
                
                # Save to archived location
                archive_path = self.get_archived_file_path(
                    contract.symbol, contract.trading_class, 
                    contract.expiration, contract.strike, contract.right
                )
                archive_path.parent.mkdir(parents=True, exist_ok=True)
                
                # Add final metadata
                final_data['archived_date'] = datetime.datetime.now().isoformat()
                
                table = pa.Table.from_pandas(final_data)
                pq.write_table(table, archive_path, compression='snappy')
                
                # Remove active files
                for data_type in ["intraday", "historical"]:
                    active_path = self.get_active_file_path(
                        contract.symbol, contract.trading_class,
                        contract.expiration, contract.strike, contract.right, data_type
                    )
                    if active_path.exists():
                        active_path.unlink()
                
                results["archived_files"] += 1
                logger.info(f"Archived {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike}{contract.right}")
        
        except Exception as e:
            error_msg = f"Error archiving {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike}{contract.right}: {e}"
            results["errors"].append(error_msg)
            logger.error(error_msg)
        
        return results
    
    def _load_parquet_safe(self, file_path: Path, columns: Optional[List[str]] = None) -> Optional[pd.DataFrame]:
        """Safely load parquet file with error handling"""
        if not file_path.exists():
            return None
        
        try:
            table = pq.read_table(file_path)
            df = table.to_pandas()
            
            if columns:
                available_columns = [col for col in columns if col in df.columns]
                if available_columns:
                    return df[available_columns]
            return df
            
        except Exception as e:
            logger.warning(f"Error loading {file_path}: {e}")
            return None

### Global settings for historical data retrieval
### Includes also the list of historized contracts 
### Ability so save/load these settings from/to a JSON file - necessary to keep track of contracts
class HistoricalDataConfig:
    """Configuration manager for historical data collection"""
    
    def __init__(self, config_file: str = None):
        if config_file is None:
            config_file = CONFIG.get("historical_config", "historical_config.json")
        self.config_file = Path(config_file)
        self.contracts = []
        # Data collection settings
        self.intraday_frequency = "5 mins"
        self.historical_frequency = "2 hours"
        self.intraday_duration = "1 D"
        self.historical_duration = "1 Y"
        self.market_data_type = 3  # Delayed data
        self.use_rth = True
        # Timeouts for different operations
        self.incremental_timeout = 10  # 10 seconds for incremental updates
        self.full_historical_timeout = 120  # 2 minutes for full historical data
        # Data types to collect from IBKR API
        self.intraday_what_to_show = ["TRADES", "BID_ASK"]
        self.historical_what_to_show = ["TRADES", "MIDPOINT", "BID_ASK"]
        
        self.load_config()
    
    def load_config(self):
        """Load configuration from file"""
        if self.config_file.exists():
            try:
                with open(self.config_file, 'r') as f:
                    data = json.load(f)
                self.contracts = [ContractConfig.from_dict(c) for c in data.get('contracts', [])]
                self.intraday_frequency = data.get('intraday_frequency', '5 mins')
                self.historical_frequency = data.get('historical_frequency', '2 hours')
                self.intraday_duration = data.get('intraday_duration', '1 D')
                self.historical_duration = data.get('historical_duration', '1 Y')
                self.market_data_type = data.get('market_data_type', 3)
                self.use_rth = data.get('use_rth', True)
                self.incremental_timeout = data.get('incremental_timeout', 10)
                self.full_historical_timeout = data.get('full_historical_timeout', 120)
                self.intraday_what_to_show = data.get('intraday_what_to_show', ["TRADES", "BID_ASK"])
                self.historical_what_to_show = data.get('historical_what_to_show', ["TRADES", "MIDPOINT"])
                logger.info(f"Loaded config: {len(self.contracts)} contracts")
            except Exception as e:
                logger.error(f"Error loading config: {e}")
    
    def save_config(self):
        """Save configuration to file"""
        try:
            data = {
                'contracts': [c.to_dict() for c in self.contracts],
                'intraday_frequency': self.intraday_frequency,
                'historical_frequency': self.historical_frequency,
                'intraday_duration': self.intraday_duration,
                'historical_duration': self.historical_duration,
                'market_data_type': self.market_data_type,
                'use_rth': self.use_rth,
                'incremental_timeout': self.incremental_timeout,
                'full_historical_timeout': self.full_historical_timeout,
                'intraday_what_to_show': self.intraday_what_to_show,
                'historical_what_to_show': self.historical_what_to_show
            }
            with open(self.config_file, 'w') as f:
                json.dump(data, f, indent=2)
            logger.info(f"Saved config: {len(self.contracts)} contracts")
        except Exception as e:
            logger.error(f"Error saving config: {e}")
    
    def add_contract(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, exchange: str) -> bool:
        """Add contract to tracking configuration."""
        # Check if contract already exists
        for contract in self.contracts:
            if (contract.trading_class == trading_class and 
                contract.expiration == expiration and
                contract.strike == strike and
                contract.right == right):
                logger.warning(f"Contract already exists: {symbol} {trading_class} {expiration} {strike}{right}")
                return False
        
        # Add contract
        contract = ContractConfig(
            symbol=symbol,
            trading_class=trading_class,
            expiration=expiration,
            strike=strike,
            right=right,
            exchange=exchange,
            active=True
        )
        
        self.contracts.append(contract)
        self.save_config()
        
        logger.info(f"Added contract: {symbol} {trading_class} {expiration} {strike}{right}")
        return True
    
    def remove_contract(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str, archive: bool = True) -> bool:
        """Remove contract from tracking and optionally archive data."""
        contract_found = None
        contract_index = None
        
        for i, contract in enumerate(self.contracts):
            if (contract.trading_class == trading_class and 
                contract.expiration == expiration and
                contract.strike == strike and
                contract.right == right):
                contract_found = contract
                contract_index = i
                break
        
        if contract_found:
            if archive:
                # Archive the data before removing
                storage = HistoricalStorage()
                all_what_to_show = list(set(self.intraday_what_to_show + self.historical_what_to_show))
                archive_results = storage.archive_contract(contract_found, all_what_to_show)
                logger.info(f"Archived {archive_results['archived_files']} files")
            
            # Remove from config
            del self.contracts[contract_index]
            self.save_config()
            logger.info(f"Removed contract: {symbol} {trading_class} {expiration} {strike}{right}")
            return True
        else:
            logger.warning(f"Contract not found: {symbol} {trading_class} {expiration} {strike}{right}")
            return False
    
    def deactivate_contract(self, symbol: str, trading_class: str, expiration: str, strike: float, right: str) -> bool:
        """Mark contract as inactive (will be archived on next update)"""
        for contract in self.contracts:
            if (contract.trading_class == trading_class and 
                contract.expiration == expiration and
                contract.strike == strike and
                contract.right == right):
                contract.active = False
                self.save_config()
                logger.info(f"Deactivated contract: {symbol} {trading_class} {expiration} {strike}{right}")
                return True
        
        logger.warning(f"Contract not found: {symbol} {trading_class} {expiration} {strike}{right}")
        return False
    
    def get_active_contracts(self) -> List[ContractConfig]:
        """Get list of active contracts"""
        today = datetime.date.today().strftime("%Y%m%d")
        return [c for c in self.contracts if c.active and c.expiration >= today]
    
    def get_expired_contracts(self) -> List[ContractConfig]:
        """Get list of contracts that should be archived"""
        today = datetime.date.today().strftime("%Y%m%d")
        return [c for c in self.contracts if not c.active or c.expiration < today]
    
    def update_settings(self, **kwargs):
        """Update collection settings"""
        for key, value in kwargs.items():
            if hasattr(self, key):
                setattr(self, key, value)
        self.save_config()

### Top management tool for historical data
class HistoricalDataManager:
    """
    Manager for historical option data collection.
    
    Features:
    - Configuration-driven contract tracking
    - Multiple data types from IBKR API
    - Uses existing strikes directory structure
    - Incremental and full historical data collection
    - Automatic expiration handling and archiving
    """
    
    def __init__(self, config_file: str = None):
        if config_file is None:
            config_file = CONFIG.get("historical_config", "historical_config.json")
            self.config_manager = HistoricalDataConfig(config_file)
        self.storage = HistoricalStorage()
    
    def collect_data_for_active_contracts(self, data_type: str = "both", contracts: List = None, ib_connection=None) -> Dict:
        """
        Collect data for contracts.

        Args:
            data_type: 'intraday', 'historical', or 'both'
            contracts: Optional list of contracts (dicts or ContractConfig objects). If None, uses config file contracts.
            ib_connection: Optional existing IB connection to reuse
        """
        # Option 1: Use provided contracts
        if contracts is not None:
            active_contracts = []
            for contract in contracts:
                logger.debug(f"Processing {contract['symbol']} {contract.get('trading_class')} {contract.get('expiration')} {contract.get('right')}")
                if isinstance(contract, dict):
                    active_contracts.append(ContractConfig.from_dict(contract))
                elif isinstance(contract, ContractConfig):
                    active_contracts.append(contract)
                else:
                    logger.error(f"Invalid contract type: {type(contract)}")
                    continue

            if not active_contracts:
                return {"message": "No valid contracts provided"}

        # Option 2: Use config file contracts (existing behavior)
        else:
            active_contracts = self.config_manager.get_active_contracts()
            if not active_contracts:
                return {"message": "No active contracts to update"}
        
        close_connection = False
        if ib_connection is not None:
            ib = ib_connection
        else:
            ib = safe_ib_connect()
            close_connection = True
        
        if not ib.isConnected():
            return {"error": "No IB connection available"}
        
        results = {
            "contracts_processed": 0,
            "contracts_errors": 0,
            "files_updated": 0,
            "errors": []
        }
        
        try:
            # Set market data type
            ib.reqMarketDataType(self.config_manager.market_data_type)
            
            for contract in active_contracts:
                try:
                    logger.info(f"Processing {contract.symbol} {contract.trading_class} {contract.expiration}")
                    
                    contract_files = 0
                    
                    # Create IB contract
                    from ib_insync import Option
                    ib_contract = Option(
                        symbol=contract.symbol,
                        lastTradeDateOrContractMonth=contract.expiration,
                        strike=contract.strike,
                        right=contract.right,
                        tradingClass=contract.trading_class,
                        exchange=contract.exchange
                    )
                    
                    # Determine timeout based on operation type
                    is_incremental = self._has_existing_data(contract, data_type)
                    timeout = (self.config_manager.incremental_timeout if is_incremental 
                             else self.config_manager.full_historical_timeout)
                    
                    # Collect data for all what_to_show types and combine
                    if data_type in ["intraday", "both"]:
                        intraday_data = {}
                        for what_to_show in self.config_manager.intraday_what_to_show:
                            bars = self._collect_bars(ib, ib_contract, "intraday", what_to_show, timeout, is_incremental=is_incremental)
                            if bars:
                                df = self._convert_bars_to_dataframe(bars, what_to_show)
                                intraday_data[what_to_show] = df

                        if intraday_data:
                            files_added = self.storage.save_active_data(
                                contract.symbol, contract.trading_class,
                                contract.expiration, contract.strike, contract.right, contract.exchange,
                                "intraday", intraday_data, incremental=True
                            )
                            contract_files += files_added

                    if data_type in ["historical", "both"]:
                        historical_data = {}
                        for what_to_show in self.config_manager.historical_what_to_show:
                            bars = self._collect_bars(ib, ib_contract, "historical", what_to_show, timeout, is_incremental=is_incremental)
                            if bars:
                                df = self._convert_bars_to_dataframe(bars, what_to_show)
                                historical_data[what_to_show] = df

                        if historical_data:
                            files_added = self.storage.save_active_data(
                                contract.symbol, contract.trading_class,
                                contract.expiration, contract.strike, contract.right, contract.exchange,
                                "historical", historical_data, incremental=True
                            )
                            contract_files += files_added
                    
                    ib.sleep(0.1)  # Rate limiting
                    # Update contract last updated time
                    contract.last_updated = datetime.datetime.now().isoformat()
                    
                    results["files_updated"] += contract_files
                    results["contracts_processed"] += 1
                    
                    logger.info(f"Completed {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike} {contract.right}: {contract_files} records added")
                    
                except Exception as e:
                    error_msg = f"Error processing {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike} {contract.right}: {e}"
                    results["errors"].append(error_msg)
                    results["contracts_errors"] += 1
                    logger.error(error_msg)
                    continue
            
            # Save updated configuration
            self.config_manager.save_config()
            
            # Archive expired contracts
            archive_results = self.archive_expired_contracts()
            results["archive_results"] = archive_results
            
            return results
            
        finally:
            if close_connection:
                ib.disconnect()
    
    def _has_existing_data(self, contract: ContractConfig, data_type: str) -> bool:
        """Check if contract already has data (for timeout determination)"""
        existing_data = self.storage.load_active_data(
            contract.symbol, contract.trading_class, contract.expiration,
            contract.strike, contract.right, data_type
        )
        return existing_data is not None and not existing_data.empty
    
    def _collect_bars(self, ib, contract, data_type: str, what_to_show: str, timeout: int, is_incremental: bool = False):
        """Collect bars from IB API with proper parameters"""
        config = self.config_manager

        if data_type == "intraday":
            duration = config.intraday_duration
            bar_size = config.intraday_frequency
            # For incremental intraday updates, only get last 2 days to avoid overwriting old data
            if is_incremental:
                duration = "2 D"
        else:  # historical
            duration = config.historical_duration
            bar_size = config.historical_frequency
            # For incremental historical updates, only get last week to avoid overwriting old data
            if is_incremental:
                duration = "1 W"

        try:
            qualified_contracts = ib.qualifyContracts(contract)

            if not qualified_contracts:
                logger.error(f"Contract qualification failed: {contract}")
                return None

            qualified_contract = qualified_contracts[0]
            logger.debug(f"Qualified contract: {qualified_contract} (incremental: {is_incremental}, duration: {duration})")

            ib_logger = logging.getLogger('ib_insync.wrapper')
            ib_logger.addFilter(lambda record: 'Error 162' not in record.getMessage())

            bars = ib.reqHistoricalData(
                contract=qualified_contract,
                endDateTime='',
                durationStr=duration,
                barSizeSetting=bar_size,
                whatToShow=what_to_show,
                useRTH=config.use_rth,
                formatDate=1,
                keepUpToDate=False,
                timeout=timeout
            )
            return bars

        except Exception as e:
            logger.debug(f"Error collecting bars ({what_to_show}): {e}")
            return None
    
    def _convert_bars_to_dataframe(self, bars, what_to_show: str) -> pd.DataFrame:
        """Convert IB bars to standardized DataFrame format"""
        data = []
        for bar in bars:
            # Base record structure
            record = {
                'datetime': pd.to_datetime(bar.date),
                'volume': bar.volume if hasattr(bar, 'volume') else 0
            }

            # Handle different what_to_show types correctly
            if what_to_show == "BID_ASK":
                # For BID_ASK from IBKR:
                #   bar.open = time_avg_bid, bar.close = time_avg_ask
                #   bar.low = min_bid, bar.high = max_ask
                # Use raw bar values for OHLC (preserves bid/ask spread variation)
                record["open"] = bar.open
                record["high"] = bar.high
                record["low"] = bar.low
                record["close"] = bar.close
                # Store bid/ask fields for reference
                record["bid"] = bar.open      # time-weighted average bid
                record["ask"] = bar.close     # time-weighted average ask
                record["bid_low"] = bar.low   # minimum bid
                record["ask_high"] = bar.high # maximum ask
            elif what_to_show == "MIDPOINT":
                # For MIDPOINT: OHLC values are midpoint between bid/ask
                # This is what we want for charting
                record['open'] = bar.open
                record['high'] = bar.high
                record['low'] = bar.low
                record['close'] = bar.close
            else:  # TRADES or other
                # For TRADES: OHLC values are actual trade prices
                record['open'] = bar.open
                record['high'] = bar.high
                record['low'] = bar.low
                record['close'] = bar.close

            data.append(record)

        return pd.DataFrame(data)
    
    def archive_expired_contracts(self) -> Dict:
        """Archive all expired or inactive contracts"""
        expired_contracts = self.config_manager.get_expired_contracts()
        
        results = {"contracts_archived": 0, "files_archived": 0, "errors": []}
        
        if not expired_contracts:
            return results
        
        all_what_to_show = list(set(
            self.config_manager.intraday_what_to_show + 
            self.config_manager.historical_what_to_show
        ))
        
        for contract in expired_contracts:
            try:
                archive_result = self.storage.archive_contract(contract, all_what_to_show)
                
                results["files_archived"] += archive_result["archived_files"]
                results["errors"].extend(archive_result["errors"])
                
                # Remove from active configuration
                self.config_manager.remove_contract(
                    contract.symbol, contract.trading_class,
                    contract.expiration, contract.strike, contract.right, archive=False
                )
                
                results["contracts_archived"] += 1
                
            except Exception as e:
                error_msg = f"Error archiving {contract.symbol} {contract.trading_class} {contract.expiration} {contract.strike}{contract.right}: {e}"
                results["errors"].append(error_msg)
                logger.error(error_msg)
        
        return results

##################################
# R-friendly convenience functions

def list_historical_config(config_file=None, show_contracts=True, max_contracts=10, return_dict=False):
    """
    Display historical data configuration settings in R-friendly format.
    
    Args:
        config_file (str): Path to configuration file 
        show_contracts (bool): Whether to display contract details
        max_contracts (int): Maximum number of contracts to display
        return_dict (bool): If True, return dict instead of printing
        
    Returns:
        dict: Configuration summary if return_dict=True, otherwise None
    """

    # Load configuration
    config = HistoricalDataConfig(config_file)
    
    # Prepare summary data
    active_contracts = config.get_active_contracts()
    expired_contracts = config.get_expired_contracts()
    
    summary = {
        "configuration_file": str(config.config_file),
        "file_exists": config.config_file.exists(),
        "collection_settings": {
            "market_data_type": config.market_data_type,
            "intraday_frequency": config.intraday_frequency,
            "historical_frequency": config.historical_frequency,
            "intraday_duration": config.intraday_duration,
            "historical_duration": config.historical_duration,
            "use_rth": config.use_rth,
            "incremental_timeout": config.incremental_timeout,
            "full_historical_timeout": config.full_historical_timeout
        },
        "data_types": {
            "intraday_what_to_show": config.intraday_what_to_show,
            "historical_what_to_show": config.historical_what_to_show
        },
        "contract_summary": {
            "total_contracts": len(config.contracts),
            "active_contracts": len(active_contracts),
            "expired_contracts": len(expired_contracts)
        }
    }
    
    if return_dict:
        # Add contract details to dict if requested
        if show_contracts:
            summary["active_contract_details"] = [
                {
                    "symbol": c.symbol,
                    "trading_class": c.trading_class,
                    "expiration": c.expiration,
                    "strike": c.strike,
                    "right": c.right,
                    "last_updated": c.last_updated
                }
                for c in active_contracts[:max_contracts]
            ]
        return summary
    
    # Print formatted output
    print("=" * 60)
    print("HISTORICAL DATA CONFIGURATION SETTINGS")
    print("=" * 60)
    
    # File info
    print(f"Configuration file: {config.config_file}")
    print(f"File exists: {'✓' if config.config_file.exists() else '✗'}")
    
    # Collection settings
    print(f"\nCOLLECTION SETTINGS:")
    print(f"  Market data type: {config.market_data_type} " +
          f"({'Live' if config.market_data_type == 1 else 'Frozen' if config.market_data_type == 2 else 'Delayed' if config.market_data_type == 3 else 'Delayed Frozen'})")
    print(f"  Use RTH only: {config.use_rth}")
    print(f"  Incremental timeout: {config.incremental_timeout}s")
    print(f"  Full historical timeout: {config.full_historical_timeout}s")
    
    print(f"\nDATA FREQUENCIES:")
    print(f"  Intraday: {config.intraday_frequency} bars for {config.intraday_duration}")
    print(f"  Historical: {config.historical_frequency} bars for {config.historical_duration}")
    
    print(f"\nDATA TYPES COLLECTED:")
    print(f"  Intraday: {', '.join(config.intraday_what_to_show)}")
    print(f"  Historical: {', '.join(config.historical_what_to_show)}")
    
    # Contract summary
    print(f"\nCONTRACT SUMMARY:")
    print(f"  Total contracts: {len(config.contracts)}")
    print(f"  Active contracts: {len(active_contracts)}")
    print(f"  Expired contracts: {len(expired_contracts)}")
    
    # Active contracts details
    if show_contracts and active_contracts:
        print(f"\nACTIVE CONTRACTS (showing first {min(max_contracts, len(active_contracts))}):")
        print("  Symbol | Trading Class | Expiration | Strike | Right | Last Updated")
        print("  " + "-" * 70)
        
        for i, contract in enumerate(active_contracts[:max_contracts]):
            last_updated = contract.last_updated[:10] if contract.last_updated else "Never"
            print(f"  {contract.symbol:6} | {contract.trading_class:13} | {contract.expiration:10} | {contract.strike:6.1f} | {contract.right:5} | {last_updated}")
        
        if len(active_contracts) > max_contracts:
            print(f"  ... and {len(active_contracts) - max_contracts} more active contracts")
    
    # Expired contracts summary
    if expired_contracts:
        print(f"\nEXPIRED CONTRACTS: {len(expired_contracts)} (ready for archiving)")
        
        # Group by expiration date
        from collections import Counter
        exp_dates = Counter(c.expiration for c in expired_contracts)
        print("  Expiration dates:")
        for exp_date, count in sorted(exp_dates.items()):
            print(f"    {exp_date}: {count} contracts")
    
    print("=" * 60)

def update_historical_settings(config_file=None, **kwargs):
    config = HistoricalDataConfig(config_file)
    config.update_settings(**kwargs)
        
def add_historical_tracking(contracts_config) -> bool:
    """
    Add one or multiple contracts to historical tracking configuration.
    
    Args:
        contracts_config: Dict or List of dicts with keys: symbol, trading_class, expiration, strike, right, exchange
    """
    try:
        manager = HistoricalDataManager()
        
        # Convert single dict to list
        if isinstance(contracts_config, dict):
            contracts_config = [contracts_config]
        
        success_count = 0
        for config in contracts_config:
            success = manager.config_manager.add_contract(
                symbol=config['symbol'],
                trading_class=config['trading_class'],
                expiration=config['expiration'],
                strike=config['strike'],
                right=config['right'],
                exchange=config['exchange']
            )
            if success:
                success_count += 1
        
        logger.info(f"Successfully configured {success_count}/{len(contracts_config)} contracts")
        return success_count == len(contracts_config)
        
    except Exception as e:
        logger.error(f"Error setting up tracking: {e}")
        return False

def update_historical_data(data_type: str = "both", contracts: List = None) -> Dict:
    """
    Update historical data for contracts.

    Args:
        data_type: 'intraday', 'historical', or 'both'
        contracts: Optional list of contracts (dicts or ContractConfig objects).
                  If None, uses contracts from config file.

    Examples:
        # Use contracts from config file (existing behavior)
        update_historical_data("both")

        # Update single contract on-demand
        contract = {'symbol': 'SPY', 'trading_class': 'SPY', 'expiration': '20250321', 'strike': 450.0, 'right': 'C'}
        update_historical_data("historical", contracts=[contract])

        # Update multiple contracts
        contracts = [contract1, contract2, contract3]
        update_historical_data("both", contracts=contracts)
    """
    try:
        manager = HistoricalDataManager()
        return manager.collect_data_for_active_contracts(data_type, contracts=contracts)
    except Exception as e:
        logger.error(f"Error updating historical data: {e}")
        return {"error": str(e)}

def get_option_historical_data(symbol: str, trading_class: str, expiration: str, strike: float, right: str, data_type: str = "historical", what_to_show: str = "TRADES", include_archived: bool = True) -> Optional[pd.DataFrame]:
    """
    Retrieve historical option data.
    
    Args:
        data_type: 'intraday', 'historical', or 'combined'
        what_to_show: IBKR data type ('TRADES', 'BID_ASK', 'OPTION_IMPLIED_VOLATILITY', etc.)
        include_archived: If True, also check archived data
    """
    try:
        storage = HistoricalStorage()
        
        if data_type == "combined":
            # Combine intraday and historical
            intraday = storage.load_active_data(symbol, trading_class, expiration,
                                              strike, right, "intraday", what_to_show)
            historical = storage.load_active_data(symbol, trading_class, expiration,
                                                strike, right, "historical", what_to_show)
            
            combined_data = []
            if intraday is not None and not intraday.empty:
                combined_data.append(intraday)
            if historical is not None and not historical.empty:
                combined_data.append(historical)
            
            if combined_data:
                result = pd.concat(combined_data, ignore_index=True)
                result = result.drop_duplicates(subset=['datetime'], keep='last')
                return result.sort_values('datetime')
            
        elif data_type in ["intraday", "historical"]:
            # Try active data first
            data = storage.load_active_data(symbol, trading_class, expiration,
                                          strike, right, data_type, what_to_show)
            if data is not None:
                return data
        
        # Try archived data if requested and not found in active
        if include_archived:
            archived_data = storage.load_archived_data(symbol, trading_class, expiration,
                                                     strike, right, what_to_show)
            if archived_data is not None:
                return archived_data
        
        return None
        
    except Exception as e:
        logger.error(f"Error retrieving data: {e}")
        return None

def manage_contracts(action: str, **kwargs) -> bool:
    """
    Manage contract tracking configuration.
    
    Actions:
        'add': Add new contract (requires symbol, trading_class, expiration, strike, right, exchange)
        'remove': Remove contract (requires symbol, trading_class, expiration, strike, right, archive=True)
        'deactivate': Deactivate contract (requires symbol, trading_class, expiration, strike, right)
        'archive': Archive all expired contracts
        'list_active': List active contracts
        'list_expired': List expired contracts
    """
    try:
        manager = HistoricalDataManager()
        config_manager = manager.config_manager
        
        if action == "add":
            return config_manager.add_contract(
                kwargs['symbol'], kwargs['trading_class'], 
                kwargs['expiration'], kwargs['strike'], kwargs['right'], kwargs['exchange']
            )
        
        elif action == "remove":
            return config_manager.remove_contract(
                kwargs['symbol'], kwargs['trading_class'], 
                kwargs['expiration'], kwargs['strike'], kwargs['right'],
                kwargs.get('archive', True)
            )
        
        elif action == "deactivate":
            return config_manager.deactivate_contract(
                kwargs['symbol'], kwargs['trading_class'], kwargs['expiration'],
                kwargs['strike'], kwargs['right']
            )
        
        elif action == "archive":
            return manager.archive_expired_contracts()
        
        elif action == "list_active":
            contracts = config_manager.get_active_contracts()
            for c in contracts:
                print(f"Active: {c.symbol} {c.trading_class} {c.expiration} {c.strike} {c.right}")
            return True
        
        elif action == "list_expired":
            contracts = config_manager.get_expired_contracts()
            for c in contracts:
                print(f"Expired: {c.symbol} {c.trading_class} {c.expiration} {c.strike} {c.right}")
            return True
        
        else:
            logger.error(f"Unknown action: {action}")
            return False
            
    except Exception as e:
        logger.error(f"Error managing contracts: {e}")
        return False
