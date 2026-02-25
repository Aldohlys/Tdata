"""
parquet_storage.py - Parquet file storage management for option chains and strikes

Handles all Parquet I/O operations with proper error handling and caching logic.
Separates storage concerns from business logic.
"""

import datetime
import logging
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd

from .core import CONFIG
from fin_logger import get_logger

logger = get_logger("tdata_py.parquet_storage")

class ParquetChainsStorage:
    """Manages Parquet storage for option chains data."""
    
    def __init__(self):
        self.base_dir = Path(CONFIG.get("chains_dir", "chains"))
        self._ensure_directory_exists()
    
    def _ensure_directory_exists(self):
        """Create necessary directories on startup."""
        self.base_dir.mkdir(parents=True, exist_ok=True)
        logger.debug(f"Chains directory initialized: {self.base_dir}")
    
    def load_chains(self, symbol):
        """
        Load all cached chains for a symbol.
        
        Args:
            symbol (str): Symbol to load chains for
            
        Returns:
            list: List of chain tuples or empty list if none found
        """
        symbol_dir = self.base_dir / symbol
        
        if not symbol_dir.exists():
            return []
        
        cached_chains = []
        
        # Look for chain metadata files (one per trading class)
        for chain_file in symbol_dir.glob("*_chain.parquet"):
            try:
                table = pq.read_table(chain_file)
                df = table.to_pandas()
                
                if df.empty:
                    continue
                
                # Reconstruct chain from stored data
                row = df.iloc[0]  # All rows should have same metadata
                
                chain = [
                    row['exchange'],
                    row['underlying_contract_id'],
                    row['trading_class'],
                    row['multiplier'],
                    df['expiration'].unique().tolist(),  # All unique expirations
                    df['strike'].unique().tolist()       # All unique strikes
                ]
                
                cached_chains.append(chain)
                logger.debug(f"Loaded chain for {symbol} {row['trading_class']}: {len(chain[4])} expirations, {len(chain[5])} strikes")
                
            except Exception as e:
                logger.warning(f"Error reading {chain_file}: {e}")
                continue
        
        return cached_chains
    
    def save_chains(self, symbol, chains):
        """
        Save chains using one file per trading class with complete metadata.
        
        Args:
            symbol (str): Symbol to save chains for
            chains (list): List of chain tuples from IB
        """
        try:
            symbol_dir = self.base_dir / symbol
            symbol_dir.mkdir(parents=True, exist_ok=True)
            
            files_created = 0
            files_updated = 0
            
            for chain in chains:
                exchange, underlying_contract_id, trading_class, multiplier, expirations, strikes = chain
                
                chain_file = symbol_dir / f"{trading_class}_chain.parquet"
                
                # Create comprehensive data structure preserving all information
                # Ensure correct data types according to ib_async.OptionChain spec:
                # exchange: str, underlyingConId: int, tradingClass: str, multiplier: str
                # expirations: List[str], strikes: List[float]
                data = []
                
                # Create a record for each combination of expiration and strike
                # This preserves the complete relationship matrix
                for expiration in expirations:
                    for strike in strikes:
                        record = {
                            'exchange': str(exchange),                              # str
                            'underlying_contract_id': int(underlying_contract_id), # int
                            'trading_class': str(trading_class),                   # str
                            'multiplier': str(multiplier),                         # str
                            'expiration': str(expiration),                         # str (YYYYMMDD format)
                            'strike': float(strike),                               # float
                            'cached_timestamp': datetime.datetime.now().isoformat()
                        }
                        data.append(record)
                
                # Save complete chain data
                df = pd.DataFrame(data)
                table = pa.Table.from_pandas(df)
                pq.write_table(table, chain_file, compression='snappy')
                
                if chain_file.exists():
                    files_updated += 1
                    logger.debug(f"Updated chain file: {chain_file}")
                else:
                    files_created += 1
                    logger.debug(f"Created chain file: {chain_file}")
                    
            logger.info(f"Saved chains for {symbol}: {files_created} new files, {files_updated} updated files")
            
        except Exception as e:
            logger.error(f"Error saving chains for {symbol}: {e}")
    
    def get_chain_for_trading_class(self, symbol, trading_class):
        """
        Get specific trading class chain data.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            
        Returns:
            tuple or None: Chain tuple or None if not found
        """
        chain_file = self.base_dir / symbol / f"{trading_class}_chain.parquet"
        
        if not chain_file.exists():
            return None
        
        try:
            table = pq.read_table(chain_file)
            df = table.to_pandas()
            
            if df.empty:
                return None
            
            # Reconstruct chain with correct data types
            row = df.iloc[0]
            chain = [
                str(row['exchange']),                                    # str
                int(row['underlying_contract_id']),                     # int
                str(row['trading_class']),                              # str  
                str(row['multiplier']),                                 # str
                sorted(df['expiration'].astype(str).unique().tolist()), # List[str]
                sorted(df['strike'].astype(float).unique().tolist())    # List[float]
            ]
            
            return chain
            
        except Exception as e:
            logger.warning(f"Error reading chain {chain_file}: {e}")
            return None

class ParquetStrikesStorage:
    """Manages Parquet storage for qualified option strikes with range metadata."""
    
    def __init__(self):
        self.base_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    def load_strike_states(self, symbol, trading_class, expiration):
        """
        Load cached strike states (qualified + out_of_scope) for an expiration.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            expiration (str): Expiration date
            
        Returns:
            dict: {'qualified': [strikes], 'out_of_scope': [strikes]}
        """
        strikes_file = self.base_dir / symbol / trading_class / f"{expiration}_strikes.parquet"
        
        if not strikes_file.exists():
            logger.debug(f"No cached strike states: {strikes_file}")
            return {'qualified': [], 'out_of_scope': []}
        
        try:
            table = pq.read_table(strikes_file)
            df = table.to_pandas()
            
            if df.empty:
                return {'qualified': [], 'out_of_scope': []}
            
            # Separate qualified from out_of_scope strikes
            qualified_df = df[df['status'] == 'qualified']
            out_of_scope_df = df[df['status'] == 'out_of_scope']
            
            qualified_strikes = [float(s) for s in qualified_df['strike'].tolist()]
            out_of_scope_strikes = [float(s) for s in out_of_scope_df['strike'].tolist()]
            
            logger.debug(f"Loaded strike states for {symbol} {trading_class} {expiration}: {len(qualified_strikes)} qualified, {len(out_of_scope_strikes)} out of scope")
            
            return {
                'qualified': qualified_strikes,
                'out_of_scope': out_of_scope_strikes
            }
            
        except Exception as e:
            logger.warning(f"Error loading strike states {strikes_file}: {e}")
            return {'qualified': [], 'out_of_scope': []}
    
    def save_strike_states(self, symbol, trading_class, expiration, qualified_strikes, out_of_scope_strikes):
        """
        Save strike states (qualified + out_of_scope) for an expiration.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            expiration (str): Expiration date
            qualified_strikes (list): List of qualified strike prices
            out_of_scope_strikes (list): List of out-of-scope strike prices
        """
        try:
            # Create directory structure
            trading_class_dir = self.base_dir / symbol / trading_class
            trading_class_dir.mkdir(parents=True, exist_ok=True)
            
            strikes_file = trading_class_dir / f"{expiration}_strikes.parquet"
            
            # Create records for both qualified and out_of_scope strikes
            data = []
            
            # Add qualified strikes
            for strike in qualified_strikes:
                record = {
                    'symbol': str(symbol),
                    'trading_class': str(trading_class),
                    'expiration': str(expiration),
                    'strike': float(strike),
                    'status': 'qualified',
                    'cached_timestamp': datetime.datetime.now().isoformat()
                }
                data.append(record)
            
            # Add out_of_scope strikes
            for strike in out_of_scope_strikes:
                record = {
                    'symbol': str(symbol),
                    'trading_class': str(trading_class),
                    'expiration': str(expiration),
                    'strike': float(strike),
                    'status': 'out_of_scope',
                    'cached_timestamp': datetime.datetime.now().isoformat()
                }
                data.append(record)
            
            # Save to parquet
            if data:  # Only save if we have data
                df = pd.DataFrame(data)
                table = pa.Table.from_pandas(df)
                pq.write_table(table, strikes_file, compression='snappy')
                
                logger.info(f"Cached strike states for {symbol} {trading_class} {expiration}: {len(qualified_strikes)} qualified, {len(out_of_scope_strikes)} out of scope")
            
        except Exception as e:
            logger.error(f"Error caching strike states for {symbol} {trading_class} {expiration}: {e}")
    
    def get_strike_statistics(self, symbol, trading_class, expiration):
        """
        Get statistics about cached strike states.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            expiration (str): Expiration date
            
        Returns:
            dict: Statistics about strike states
        """
        states = self.load_strike_states(symbol, trading_class, expiration)
        
        qualified_count = len(states['qualified'])
        out_of_scope_count = len(states['out_of_scope'])
        total_tested = qualified_count + out_of_scope_count
        
        stats = {
            'total_tested': total_tested,
            'qualified_count': qualified_count,
            'out_of_scope_count': out_of_scope_count,
            'qualification_rate': (qualified_count / total_tested * 100) if total_tested > 0 else 0,
            'qualified_strikes': sorted(states['qualified']),
            'out_of_scope_strikes': sorted(states['out_of_scope'])
        }
        
        if qualified_count > 0:
            stats['qualified_range'] = {
                'min': min(states['qualified']),
                'max': max(states['qualified'])
            }
        
        return stats
      
    def load_all_strikes_for_expiration(self, symbol, trading_class, expiration):
        """
        Load all available strikes for a specific expiration without range filtering.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            expiration (str): Expiration date
            
        Returns:
            list or None: All available strikes or None if not cached
        """
        strikes_file = self.base_dir / symbol / trading_class / f"{expiration}_strikes.parquet"
        
        if not strikes_file.exists():
            return None
        
        try:
            table = pq.read_table(strikes_file)
            df = table.to_pandas()
            
            if df.empty:
                return None
            
            return sorted([float(s) for s in df['strike'].unique().tolist()])
            
        except Exception as e:
            logger.warning(f"Error reading all strikes from {strikes_file}: {e}")
            return None
    
    def get_cached_expirations(self, symbol, trading_class):
        """
        Get all cached expirations for a specific symbol and trading class.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            
        Returns:
            list: Sorted list of cached expiration dates (YYYYMMDD strings)
        """
        trading_class_dir = self.base_dir / symbol / trading_class
        
        if not trading_class_dir.exists():
            return []
        
        try:
            expiration_files = list(trading_class_dir.glob("*_strikes.parquet"))
            expirations = []
            
            for file_path in expiration_files:
                # Extract expiration from filename: YYYYMMDD_strikes.parquet
                expiration = file_path.stem.replace('_strikes', '')
                if len(expiration) == 8 and expiration.isdigit():
                    expirations.append(expiration)
                else:
                    logger.warning(f"Invalid expiration format in filename: {file_path}")
            
            return sorted(expirations)
            
        except Exception as e:
            logger.error(f"Error getting cached expirations for {symbol} {trading_class}: {e}")
            return []
    
    def get_cached_trading_classes(self, symbol):
        """
        Get all trading classes that have cached strikes data.
        
        Args:
            symbol (str): Symbol
            
        Returns:
            list: Sorted list of trading classes with cached data
        """
        symbol_dir = self.base_dir / symbol
        
        if not symbol_dir.exists():
            return []
        
        try:
            trading_class_dirs = [d for d in symbol_dir.iterdir() if d.is_dir()]
            trading_classes = [d.name for d in trading_class_dirs]
            return sorted(trading_classes)
            
        except Exception as e:
            logger.error(f"Error getting cached trading classes for {symbol}: {e}")
            return []
    
    def get_cached_expirations_with_stats(self, symbol, trading_class):
        """
        Get all cached expirations with strike statistics.
        
        Args:
            symbol (str): Symbol
            trading_class (str): Trading class
            
        Returns:
            list: List of dicts with expiration and strike stats
        """
        expirations = self.get_cached_expirations(symbol, trading_class)
        
        expiration_stats = []
        for expiration in expirations:
            stats = self.get_strike_statistics(symbol, trading_class, expiration)
            
            expiration_info = {
                'expiration': expiration,
                'total_tested': stats['total_tested'],
                'qualified_count': stats['qualified_count'],
                'out_of_scope_count': stats['out_of_scope_count'],
                'qualification_rate': stats['qualification_rate']
            }
            
            if stats['qualified_count'] > 0:
                expiration_info['qualified_range'] = stats['qualified_range']
            
            expiration_stats.append(expiration_info)
        
        return expiration_stats
    
    def get_cache_coverage_summary(self, symbol, trading_class=None):
        """
        Get comprehensive cache coverage summary.
        
        Args:
            symbol (str): Symbol
            trading_class (str, optional): Specific trading class
            
        Returns:
            dict: Cache coverage summary
        """
        if trading_class:
            trading_classes = [trading_class]
        else:
            trading_classes = self.get_cached_trading_classes(symbol)
        
        summary = {
            'symbol': symbol,
            'trading_classes': {},
            'total_stats': {
                'total_expirations': 0,
                'total_tested_strikes': 0,
                'total_qualified_strikes': 0,
                'total_out_of_scope_strikes': 0
            }
        }
        
        for tc in trading_classes:
            tc_stats = {
                'expirations': self.get_cached_expirations_with_stats(symbol, tc),
                'summary': {
                    'expiration_count': 0,
                    'total_tested': 0,
                    'total_qualified': 0,
                    'total_out_of_scope': 0,
                    'avg_qualification_rate': 0
                }
            }
            
            # Calculate trading class totals
            total_tested = 0
            total_qualified = 0
            qualification_rates = []
            
            for exp_stat in tc_stats['expirations']:
                total_tested += exp_stat['total_tested']
                total_qualified += exp_stat['qualified_count']
                if exp_stat['total_tested'] > 0:
                    qualification_rates.append(exp_stat['qualification_rate'])
            
            tc_stats['summary'] = {
                'expiration_count': len(tc_stats['expirations']),
                'total_tested': total_tested,
                'total_qualified': total_qualified,
                'total_out_of_scope': total_tested - total_qualified,
                'avg_qualification_rate': sum(qualification_rates) / len(qualification_rates) if qualification_rates else 0
            }
            
            summary['trading_classes'][tc] = tc_stats
            
            # Add to overall totals
            summary['total_stats']['total_expirations'] += tc_stats['summary']['expiration_count']
            summary['total_stats']['total_tested_strikes'] += total_tested
            summary['total_stats']['total_qualified_strikes'] += total_qualified
            summary['total_stats']['total_out_of_scope_strikes'] += (total_tested - total_qualified)
        
        return summary
    
    def _filter_strikes(self, strikes, strike_min, strike_max):
        """Filter strikes by range constraints, ensuring float comparison."""
        if strike_min is None and strike_max is None:
            return strikes
        
        filtered_strikes = []
        for strike in strikes:
            strike = float(strike)  # Ensure float for comparison
            if strike_min is not None and strike < float(strike_min):
                continue
            if strike_max is not None and strike > float(strike_max):
                continue
            filtered_strikes.append(strike)
        
        return filtered_strikes

class ParquetMaintenanceManager:
    """Handles cleanup and maintenance operations for Parquet files."""
    
    def __init__(self):
        self.chains_dir = Path(CONFIG.get("chains_dir", "chains"))
        self.strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    def cleanup_expired_options(self, symbol=None, trading_class=None, dry_run=True):
        """
        Remove expired option data files.
        
        Args:
            symbol (str, optional): Clean specific symbol only
            trading_class (str, optional): Clean specific trading class only 
            dry_run (bool): If True, only show what would be deleted
            
        Returns:
            dict: Summary of cleanup results
        """
        try:
            results = {
                "chains_cleanup": self._cleanup_expired_chains(symbol, trading_class, dry_run),
                "strikes_cleanup": self._cleanup_expired_strikes(symbol, trading_class, dry_run),
                "dry_run": dry_run,
                "timestamp": datetime.datetime.now().isoformat()
            }
            
            # Calculate totals
            chains_updated = results["chains_cleanup"].get("files_updated", 0)
            strikes_deleted = results["strikes_cleanup"].get("deleted_files", 0)
            results["total_files_affected"] = chains_updated + strikes_deleted
            
            # Cleanup empty directories
            if not dry_run:
                results["empty_dirs_removed"] = self._cleanup_empty_directories(dry_run=False)
            else:
                results["empty_dirs_would_remove"] = self._cleanup_empty_directories(dry_run=True)
            
            return results
            
        except Exception as e:
            logger.error(f"Cleanup error: {e}")
            return {"error": str(e)}
    
    def _cleanup_expired_chains(self, symbol, trading_class, dry_run):
        """
        Clean expired chain files - remove expired expirations from chain data.
        
        Chain files structure: symbol/tradingclass_chain.parquet
        Each file contains multiple expirations - we remove expired ones and keep valid ones.
        """
        if not self.chains_dir.exists():
            return {"error": "Chains directory not found"}
        
        today = datetime.date.today().strftime("%Y%m%d")  # Keep as string for comparison
        
        # Find chain files using corrected pattern
        pattern = self._build_chains_cleanup_pattern(symbol, trading_class)
        chain_files = list(self.chains_dir.glob(pattern))
        
        files_updated = 0
        files_unchanged = 0
        files_deleted = 0  # For files that become empty after cleanup
        errors = []
        
        for chain_file in chain_files:
            try:
                import pyarrow.parquet as pq
                import pyarrow as pa
                
                table = pq.read_table(chain_file)
                df = table.to_pandas()
                
                if df.empty:
                    # Delete empty files
                    if not dry_run:
                        chain_file.unlink()
                    files_deleted += 1
                    logger.debug(f"{'Would delete' if dry_run else 'Deleted'} empty chain file: {chain_file}")
                    continue
                
                # Filter out expired expirations
                # Ensure expiration column exists and has valid format
                if 'expiration' not in df.columns:
                    logger.warning(f"Chain file missing expiration column: {chain_file}")
                    continue
                
                original_count = len(df)
                
                # Filter for valid YYYYMMDD format and non-expired
                df_filtered = df[df['expiration'].astype(str).str.len() == 8]  # Valid YYYYMMDD format
                df_filtered = df_filtered[df_filtered['expiration'].astype(str) >= today]  # Non-expired
                
                if len(df_filtered) == 0:
                    # All expirations expired - delete entire file
                    if not dry_run:
                        chain_file.unlink()
                    files_deleted += 1
                    logger.debug(f"{'Would delete' if dry_run else 'Deleted'} fully expired chain file: {chain_file}")
                    
                elif len(df_filtered) < original_count:
                    # Some expirations expired - update file
                    if not dry_run:
                        table_filtered = pa.Table.from_pandas(df_filtered)
                        pq.write_table(table_filtered, chain_file, compression='snappy')
                    
                    files_updated += 1
                    expired_count = original_count - len(df_filtered)
                    logger.debug(f"{'Would update' if dry_run else 'Updated'} {chain_file}: removed {expired_count} expired expirations")
                    
                else:
                    files_unchanged += 1
                    
            except Exception as e:
                errors.append((str(chain_file), str(e)))
                logger.error(f"Error processing chain file {chain_file}: {e}")
        
        return {
            "files_processed": len(chain_files),
            "files_updated": files_updated,
            "files_deleted": files_deleted,
            "files_unchanged": files_unchanged,
            "errors": len(errors),
            "error_details": errors if errors else None
        }
    
    def _cleanup_expired_strikes(self, symbol, trading_class, dry_run):
        """
        Clean expired strikes files.
        
        Strikes files structure: symbol/tradingclass/expiration_strikes.parquet
        Each file represents one expiration - if expired, delete entire file.
        """
        if not self.strikes_dir.exists():
            return {"error": "Strikes directory not found"}
        
        today = datetime.date.today().strftime("%Y%m%d")
        
        # Find strikes files using corrected pattern
        pattern = self._build_strikes_cleanup_pattern(symbol, trading_class)
        strikes_files = list(self.strikes_dir.glob(pattern))
        
        expired_files = []
        valid_files = []
        invalid_format_files = []
        
        for strikes_file in strikes_files:
            # Extract expiration from filename (format: YYYYMMDD_strikes.parquet)
            expiration_str = strikes_file.stem.replace('_strikes', '')
            
            try:
                if len(expiration_str) == 8 and expiration_str.isdigit():
                    # Valid expiration format - check if expired
                    if expiration_str < today:  # String comparison works for YYYYMMDD
                        expired_files.append(strikes_file)
                    else:
                        valid_files.append(strikes_file)
                else:
                    # Invalid expiration format
                    invalid_format_files.append(strikes_file)
                    logger.warning(f"Invalid expiration format in filename: {strikes_file}")
                        
            except Exception as e:
                invalid_format_files.append(strikes_file)
                logger.warning(f"Error parsing expiration from {strikes_file}: {e}")
        
        # Delete expired files
        deleted_files = []
        deletion_errors = []
        
        files_to_delete = expired_files + invalid_format_files  # Delete both expired and invalid format
        
        if not dry_run:
            for strikes_file in files_to_delete:
                try:
                    strikes_file.unlink()
                    deleted_files.append(str(strikes_file))
                    logger.debug(f"Deleted {'expired' if strikes_file in expired_files else 'invalid format'} strikes file: {strikes_file}")
                except Exception as e:
                    deletion_errors.append((str(strikes_file), str(e)))
                    logger.error(f"Error deleting {strikes_file}: {e}")
        else:
            # Dry run - just count what would be deleted
            deleted_files = [str(f) for f in files_to_delete]
        
        return {
            "total_files": len(strikes_files),
            "expired_files": len(expired_files),
            "invalid_format_files": len(invalid_format_files),
            "valid_files": len(valid_files),
            "deleted_files": len(deleted_files),
            "deletion_errors": len(deletion_errors),
            "error_details": deletion_errors if deletion_errors else None
        }
    
    def _build_chains_cleanup_pattern(self, symbol, trading_class):
        """
        Build file search pattern for chain files.
        
        Chain structure: symbol/tradingclass_chain.parquet
        """
        if symbol and trading_class:
            # Specific symbol and trading class
            pattern = f"{symbol}/{trading_class}_chain.parquet"
        elif symbol:
            # All trading classes for specific symbol
            pattern = f"{symbol}/*_chain.parquet"
        elif trading_class:
            # Specific trading class across all symbols
            pattern = f"*/{trading_class}_chain.parquet"
        else:
            # All chain files
            pattern = "**/*_chain.parquet"
        return pattern
    
    def _build_strikes_cleanup_pattern(self, symbol, trading_class):
        """
        Build file search pattern for strikes files.
        
        Strikes structure: symbol/tradingclass/expiration_strikes.parquet
        """
        if symbol and trading_class:
            # Specific symbol and trading class
            pattern = f"{symbol}/{trading_class}/*_strikes.parquet"
        elif symbol:
            # All trading classes for specific symbol
            pattern = f"{symbol}/**/*_strikes.parquet"
        elif trading_class:
            # Specific trading class across all symbols - need to search deeper
            pattern = f"**/{trading_class}/*_strikes.parquet"
        else:
            # All strikes files
            pattern = "**/*_strikes.parquet"
        return pattern
    
    def _cleanup_empty_directories(self, dry_run=True):
        """Remove empty directories recursively."""
        removed_count = 0
        
        for base_dir in [self.chains_dir, self.strikes_dir]:
            if not base_dir.exists():
                continue
                
            try:
                # Walk from bottom up to handle nested empty directories
                # Use sorted with reverse=True to process deepest directories first
                all_dirs = [d for d in base_dir.rglob("*") if d.is_dir()]
                sorted_dirs = sorted(all_dirs, key=lambda x: len(x.parts), reverse=True)
                
                for dirpath in sorted_dirs:
                    if dirpath == base_dir:
                        continue  # Don't remove base directory
                        
                    try:
                        # Check if directory is empty
                        if not any(dirpath.iterdir()):
                            if not dry_run:
                                dirpath.rmdir()
                            removed_count += 1
                            logger.debug(f"{'Would remove' if dry_run else 'Removed'} empty dir: {dirpath}")
                    except OSError:
                        # Directory not empty or permission issue
                        pass
                    except Exception as e:
                        logger.warning(f"Error checking directory {dirpath}: {e}")
                            
            except Exception as e:
                logger.error(f"Error cleaning empty directories in {base_dir}: {e}")
        
        return removed_count

    def cleanup_invalid_structure_files(self, dry_run=True):
        """
        Clean files that don't match expected structure patterns.
        
        Args:
            dry_run (bool): If True, only show what would be deleted
            
        Returns:
            dict: Results of structure cleanup
        """
        invalid_files = []
        deleted_files = []
        errors = []
        
        # Check chains directory
        if self.chains_dir.exists():
            for parquet_file in self.chains_dir.rglob("*.parquet"):
                if not self._is_valid_chain_file(parquet_file):
                    invalid_files.append(str(parquet_file))
                    
                    if not dry_run:
                        try:
                            parquet_file.unlink()
                            deleted_files.append(str(parquet_file))
                        except Exception as e:
                            errors.append((str(parquet_file), str(e)))
        
        # Check strikes directory
        if self.strikes_dir.exists():
            for parquet_file in self.strikes_dir.rglob("*.parquet"):
                if not self._is_valid_strikes_file(parquet_file):
                    invalid_files.append(str(parquet_file))
                    
                    if not dry_run:
                        try:
                            parquet_file.unlink()
                            deleted_files.append(str(parquet_file))
                        except Exception as e:
                            errors.append((str(parquet_file), str(e)))
        
        return {
            "invalid_files_found": len(invalid_files),
            "files_deleted": len(deleted_files) if not dry_run else len(invalid_files),
            "errors": len(errors),
            "dry_run": dry_run,
            "invalid_files": invalid_files,
            "error_details": errors if errors else None
        }
    
    def _is_valid_chain_file(self, file_path):
        """Check if chain file has valid structure: symbol/tradingclass_chain.parquet"""
        try:
            parts = file_path.relative_to(self.chains_dir).parts
            
            # Should have exactly 2 parts: symbol, tradingclass_chain.parquet
            if len(parts) != 2:
                return False
            
            # Second part should end with _chain.parquet
            if not parts[1].endswith("_chain.parquet"):
                return False
            
            return True
            
        except Exception:
            return False
    
    def _is_valid_strikes_file(self, file_path):
        """Check if strikes file has valid structure: symbol/tradingclass/expiration_strikes.parquet"""
        try:
            parts = file_path.relative_to(self.strikes_dir).parts
            
            # Should have exactly 3 parts: symbol, tradingclass, expiration_strikes.parquet
            if len(parts) != 3:
                return False
            
            # Third part should end with _strikes.parquet
            if not parts[2].endswith("_strikes.parquet"):
                return False
            
            # Extract and validate expiration format
            expiration = parts[2].replace("_strikes.parquet", "")
            if len(expiration) != 8 or not expiration.isdigit():
                return False
            
            return True
            
        except Exception:
            return False

    def get_cleanup_preview(self, symbol=None, trading_class=None):
        """
        Preview what would be cleaned without actually deleting anything.
        
        Args:
            symbol (str, optional): Preview for specific symbol
            trading_class (str, optional): Preview for specific trading class
            
        Returns:
            dict: Preview of cleanup actions
        """
        preview = {
            "timestamp": datetime.datetime.now().isoformat(),
            "filters": {"symbol": symbol, "trading_class": trading_class},
            "chains_preview": None,
            "strikes_preview": None,
            "invalid_structure_preview": None,
            "empty_dirs_preview": None
        }
        
        # Get preview of chains cleanup
        chains_result = self._cleanup_expired_chains(symbol, trading_class, dry_run=True)
        preview["chains_preview"] = chains_result
        
        # Get preview of strikes cleanup
        strikes_result = self._cleanup_expired_strikes(symbol, trading_class, dry_run=True)
        preview["strikes_preview"] = strikes_result
        
        # Get preview of invalid structure cleanup
        invalid_result = self.cleanup_invalid_structure_files(dry_run=True)
        preview["invalid_structure_preview"] = invalid_result
        
        # Get preview of empty directories cleanup
        empty_dirs_count = self._cleanup_empty_directories(dry_run=True)
        preview["empty_dirs_preview"] = {"dirs_would_remove": empty_dirs_count}
        
        # Calculate totals
        total_files_affected = (
            chains_result.get("files_updated", 0) + 
            chains_result.get("files_deleted", 0) +
            strikes_result.get("deleted_files", 0) +
            invalid_result.get("files_deleted", 0)
        )
        
        preview["summary"] = {
            "total_files_would_affect": total_files_affected,
            "chain_files_would_change": chains_result.get("files_updated", 0) + chains_result.get("files_deleted", 0),
            "strikes_files_would_delete": strikes_result.get("deleted_files", 0),
            "invalid_files_would_delete": invalid_result.get("files_deleted", 0),
            "empty_dirs_would_remove": empty_dirs_count
        }
        
        return preview

# Enhanced utility functions for multi-trading-class strikes management
def get_chain_for_trading_class(symbol, trading_class):
    """
    Utility function to get chain for a specific symbol & trading class.
    Returns cached chain if available, otherwise returns None.
    
    Args:
        symbol (str): Symbol (e.g., 'SPY')
        trading_class (str): Trading class (e.g., 'SPY', '2SPY')

    Returns:
        chain or None: chain if exists or None if not cached
    """
  
    storage = ParquetChainsStorage()
    return storage.get_chain_for_trading_class(symbol, trading_class)
  
def get_strikes_for_expiration(symbol, trading_class, expiration):
    """
    Utility function to get all available strikes for a specific expiration.
    Returns cached strikes if available, otherwise returns None.
    
    Args:
        symbol (str): Symbol (e.g., 'SPY')
        trading_class (str): Trading class (e.g., 'SPY', '2SPY')
        expiration (str): Expiration date (YYYYMMDD)
        
    Returns:
        list or None: Sorted list of available strikes or None if not cached
    """
    storage = ParquetStrikesStorage()
    return storage.load_all_strikes_for_expiration(symbol, trading_class, expiration)

def get_strikes_summary(symbol):
    """
    Get a comprehensive summary of all cached strikes data for a symbol.
    
    Args:
        symbol (str): Symbol
        
    Returns:
        dict: Summary of cached strikes organized by trading class and expiration
    """
    storage = ParquetStrikesStorage()
    trading_classes = storage.get_cached_trading_classes(symbol)
    
    if not trading_classes:
        return {'symbol': symbol, 'trading_classes': {}, 'summary': 'No cached strikes data'}
    
    summary = {'symbol': symbol, 'trading_classes': {}}
    total_files = 0
    total_expirations = 0
    
    for trading_class in trading_classes:
        expirations = storage.get_cached_expirations(symbol, trading_class)
        
        tc_data = {
            'expirations': expirations,
            'expiration_count': len(expirations),
            'files': []
        }
        
        # Get strike counts for each expiration
        for expiration in expirations:
            strikes = get_strikes_for_expiration(symbol, trading_class, expiration)
            if strikes:
                tc_data['files'].append({
                    'expiration': expiration,
                    'strike_count': len(strikes),
                    'strike_range': {'min': min(strikes), 'max': max(strikes)}
                })
        
        summary['trading_classes'][trading_class] = tc_data
        total_files += len(tc_data['files'])
        total_expirations += len(expirations)
    
    summary['summary'] = {
        'trading_class_count': len(trading_classes),
        'total_expirations': total_expirations,
        'total_files': total_files
    }
    
    return summary

def compare_trading_class_strikes(symbol, expiration, trading_classes=None):
    """
    Compare available strikes across different trading classes for the same expiration.
    Useful for understanding differences between SPY vs 2SPY, SPX vs SPXW, etc.
    
    Args:
        symbol (str): Symbol
        expiration (str): Expiration date (YYYYMMDD)
        trading_classes (list, optional): Specific trading classes to compare
        
    Returns:
        dict: Comparison of strikes across trading classes
    """
    storage = ParquetStrikesStorage()
    
    if trading_classes is None:
        trading_classes = storage.get_cached_trading_classes(symbol)
    
    if not trading_classes:
        return {'error': 'No cached trading classes found'}
    
    comparison = {
        'symbol': symbol,
        'expiration': expiration,
        'trading_classes': {},
        'comparison': {}
    }
    
    all_strikes_sets = {}
    
    # Get strikes for each trading class
    for tc in trading_classes:
        strikes = get_strikes_for_expiration(symbol, tc, expiration)
        if strikes:
            all_strikes_sets[tc] = set(strikes)
            comparison['trading_classes'][tc] = {
                'strike_count': len(strikes),
                'strike_range': {'min': min(strikes), 'max': max(strikes)},
                'strikes': sorted(strikes)
            }
        else:
            comparison['trading_classes'][tc] = {
                'error': 'No cached strikes data'
            }
    
    # Perform comparison analysis
    if len(all_strikes_sets) >= 2:
        tc_names = list(all_strikes_sets.keys())
        
        # Find common and unique strikes
        all_strikes = set()
        for strikes_set in all_strikes_sets.values():
            all_strikes.update(strikes_set)
        
        # Find intersection (common strikes)
        common_strikes = all_strikes_sets[tc_names[0]]
        for strikes_set in all_strikes_sets.values():
            common_strikes = common_strikes.intersection(strikes_set)
        
        # Find unique strikes for each trading class
        unique_strikes = {}
        for tc, strikes_set in all_strikes_sets.items():
            other_strikes = set()
            for other_tc, other_set in all_strikes_sets.items():
                if other_tc != tc:
                    other_strikes.update(other_set)
            unique_strikes[tc] = strikes_set - other_strikes
        
        comparison['comparison'] = {
            'total_unique_strikes': len(all_strikes),
            'common_strikes': sorted(list(common_strikes)),
            'common_strike_count': len(common_strikes),
            'unique_to_trading_class': {tc: sorted(list(strikes)) for tc, strikes in unique_strikes.items()},
            'coverage_analysis': {
                tc: {
                    'coverage_pct': len(strikes_set) / len(all_strikes) * 100,
                    'unique_count': len(unique_strikes[tc])
                }
                for tc, strikes_set in all_strikes_sets.items()
            }
        }
    
    return comparison
