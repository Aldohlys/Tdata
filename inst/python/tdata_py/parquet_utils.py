"""
parquet_utils.py - Utility functions for viewing, listing, and managing Parquet files

Provides user-friendly functions for inspecting and maintaining the Parquet file structure.
"""
import datetime
from pathlib import Path
import pyarrow.parquet as pq

from .core import CONFIG
from .parquet_storage import ParquetMaintenanceManager, ParquetChainsStorage


## Raw viewer Parquet files
def view_parquet(filepath):
    """
    Display contents of a parquet file with detailed analysis.

    Args:
        filepath (str or Path): Full path to parquet file or just filename like "20231215.parquet"
                               If just filename provided, searches current chains directory
    
    Returns:
        pandas.DataFrame: The parquet file contents
    """
    try:
        file_path = Path(filepath)
        
        # If just filename provided, try to find it in chains directory
        if not file_path.is_absolute() and not file_path.parent.name:
            chains_base_dir = Path(CONFIG.get("chains_dir", "chains"))
            
            # Search for the file in subdirectories
            found_files = list(chains_base_dir.rglob(filepath))
            
            if not found_files:
                print(f"File '{filepath}' not found in {chains_base_dir}")
                return None
            elif len(found_files) == 1:
                file_path = found_files[0]
                print(f"Found: {file_path}")
            else:
                file_path = _select_file_from_multiple(found_files, filepath)
                if file_path is None:
                    return None
        
        # Read and display the parquet file
        if not file_path.exists():
            print(f"File does not exist: {file_path}")
            return None
          
        return _display_parquet_analysis(file_path)
        
    except Exception as e:
        print(f"Error reading parquet file: {e}")
        return None

def list_parquet_files(symbol=None, trading_class=None):
    """
    List available parquet files in the chains directory with organized display.
    
    Args:
        symbol (str, optional): Filter by symbol
        trading_class (str, optional): Filter by trading class
    """
    try:
        chains_base_dir = Path(CONFIG.get("chains_dir", "chains"))
        
        if not chains_base_dir.exists():
            print(f"Chains directory not found: {chains_base_dir}")
            return
            
        # Build search pattern
        pattern = "**/*.parquet"
        if symbol:
            pattern = f"{symbol}/**/*.parquet"
            
        parquet_files = list(chains_base_dir.glob(pattern))
        
        if trading_class:
            parquet_files = [f for f in parquet_files if trading_class in str(f)]
            
        if not parquet_files:
            print("No parquet files found")
            return
            
        print(f"Found {len(parquet_files)} parquet files:")
        
        # Group and display files
        grouped = _group_files_by_structure(parquet_files, chains_base_dir)
        _display_grouped_files(grouped)
                
    except Exception as e:
        print(f"Error listing parquet files: {e}")

# Cleanup convenience functions

def clearCache(symbol, trading_class=None, expiration=None, cache_type="strikes"):
    """
    R-friendly cache clearing function.
    
    Args:
        symbol (str): Symbol to clear
        trading_class (str, optional): Specific trading class
        expiration (str, optional): Specific expiration  
        cache_type (str): "strikes", "chains", or "all"
    """
    try:
        if cache_type in ["strikes", "all"]:
            result = clearStrikesCache(symbol, trading_class, expiration)
            package_logger.info(f"Cleared strikes cache: {result}")
        
        if cache_type in ["chains", "all"]:
            # Also clear chains cache by deleting chain files
            from .parquet_storage import ParquetChainsStorage
            chains_storage = ParquetChainsStorage()
            # Implementation for clearing chains cache...
            
        return {"success": True, "cache_type": cache_type}
        
    except Exception as e:
        package_logger.error(f"Error clearing cache: {e}")
        return {"success": False, "error": str(e)}

def cleanup_symbol(symbol, dry_run=True):
    """Clean all expired files for a specific symbol."""
    manager = ParquetMaintenanceManager()
    return manager.cleanup_expired_options(symbol=symbol, dry_run=dry_run)

def cleanup_trading_class(trading_class, dry_run=True):
    """Clean all expired files for a specific trading class across all symbols."""
    manager = ParquetMaintenanceManager()
    return manager.cleanup_expired_options(trading_class=trading_class, dry_run=dry_run)

def cleanupExpired(symbol=None, trading_class=None, dry_run=True):
    """
    R-friendly cleanup function.
    
    Args:
        symbol (str, optional): Specific symbol to clean
        trading_class (str, optional): Specific trading class to clean
        dry_run (bool): If True, only show what would be deleted
    
    Returns:
        dict: Cleanup results
    """
    try:
        maintenance = ParquetMaintenanceManager()
        return maintenance.cleanup_expired_options(symbol, trading_class, dry_run)
    except Exception as e:
        package_logger.error(f"Error in cleanup: {e}")
        return {'error': str(e)}

def clearStrikesCache(sym, trading_class=None, expiration=None):
    """
    Clear strikes cache for debugging/testing.
    
    Args:
        sym (str): Symbol
        trading_class (str, optional): Specific trading class, or None for all
        expiration (str, optional): Specific expiration, or None for all
    """
    from .parquet_storage import ParquetStrikesStorage
    import os
    from pathlib import Path
    
    strikes_storage = ParquetStrikesStorage()
    base_dir = strikes_storage.base_dir
    
    symbol_dir = base_dir / sym
    
    if not symbol_dir.exists():
        logger.info(f"No cache directory exists for {sym}")
        return {"files_deleted": 0, "message": "No cache found"}
    
    files_deleted = 0
    
    if trading_class is None:
        # Delete entire symbol directory
        import shutil
        shutil.rmtree(symbol_dir)
        files_deleted = len(list(symbol_dir.rglob("*.parquet"))) if symbol_dir.exists() else 0
        logger.info(f"Cleared all strikes cache for {sym}: {files_deleted} files")
    
    elif expiration is None:
        # Delete entire trading class directory
        tc_dir = symbol_dir / trading_class
        if tc_dir.exists():
            files_deleted = len(list(tc_dir.glob("*.parquet")))
            import shutil
            shutil.rmtree(tc_dir)
            logger.info(f"Cleared strikes cache for {sym} {trading_class}: {files_deleted} files")
    
    else:
        # Delete specific expiration file
        strikes_file = symbol_dir / trading_class / f"{expiration}_strikes.parquet"
        if strikes_file.exists():
            strikes_file.unlink()
            files_deleted = 1
            logger.info(f"Cleared strikes cache for {sym} {trading_class} {expiration}")
    
    return {"files_deleted": files_deleted, "symbol": sym, "trading_class": trading_class, "expiration": expiration}

def cleanup_all(dry_run=True):
    """Clean all expired files across all symbols and trading classes."""
    manager = ParquetMaintenanceManager()
    return manager.cleanup_expired_options(dry_run=dry_run)

### Look at global storage stats
def get_detailed_storage_breakdown():
    """
    Get detailed per-symbol storage breakdown.
    
    Returns:
        dict: Symbol-by-symbol storage analysis
    """
    stats = _get_storage_stats()
    
    if "error" in stats:
        return stats
    
    breakdown = {
        "symbols": {},
        "summary": stats["summary"].copy()
    }
    
    chains_dir = Path(CONFIG.get("chains_dir", "chains"))
    strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    # Get all unique symbols
    all_symbols = set()
    if chains_dir.exists():
        for symbol_dir in chains_dir.iterdir():
            if symbol_dir.is_dir():
                all_symbols.add(symbol_dir.name)
    
    if strikes_dir.exists():
        for symbol_dir in strikes_dir.iterdir():
            if symbol_dir.is_dir():
                all_symbols.add(symbol_dir.name)
    
    # Analyze each symbol
    for symbol in sorted(all_symbols):
        symbol_stats = {
            "chains": {"files": 0, "size_kb": 0, "trading_classes": []},
            "strikes": {"files": 0, "size_kb": 0, "trading_classes": [], "expirations": []},
            "total_files": 0,
            "total_size_kb": 0
        }
        
        # Analyze chains for this symbol
        symbol_chains_dir = chains_dir / symbol
        if symbol_chains_dir.exists():
            for chain_file in symbol_chains_dir.glob("*_chain.parquet"):
                symbol_stats["chains"]["files"] += 1
                symbol_stats["chains"]["size_kb"] += round(chain_file.stat().st_size / 1024, 2)
                
                trading_class = chain_file.name.replace("_chain.parquet", "")
                symbol_stats["chains"]["trading_classes"].append(trading_class)
        
        # Analyze strikes for this symbol
        symbol_strikes_dir = strikes_dir / symbol
        if symbol_strikes_dir.exists():
            trading_classes = set()
            expirations = set()
            
            for strikes_file in symbol_strikes_dir.rglob("*_strikes.parquet"):
                symbol_stats["strikes"]["files"] += 1
                symbol_stats["strikes"]["size_kb"] += round(strikes_file.stat().st_size / 1024, 2)
                
                # Extract trading class and expiration
                parts = strikes_file.relative_to(symbol_strikes_dir).parts
                if len(parts) == 2:  # tradingclass/expiration_strikes.parquet
                    trading_class = parts[0]
                    expiration = parts[1].replace("_strikes.parquet", "")
                    trading_classes.add(trading_class)
                    expirations.add(expiration)
            
            symbol_stats["strikes"]["trading_classes"] = sorted(list(trading_classes))
            symbol_stats["strikes"]["expirations"] = sorted(list(expirations))
        
        # Calculate totals
        symbol_stats["total_files"] = symbol_stats["chains"]["files"] + symbol_stats["strikes"]["files"]
        symbol_stats["total_size_kb"] = symbol_stats["chains"]["size_kb"] + symbol_stats["strikes"]["size_kb"]
        symbol_stats["total_size_mb"] = round(symbol_stats["total_size_kb"] / 1024, 2)
        
        breakdown["symbols"][symbol] = symbol_stats
    
    return breakdown

def print_storage_summary():
    """
    Print a human-readable storage summary.
    """
    stats = _get_storage_stats()
    
    if "error" in stats:
        print(f"❌ Error: {stats['error']}")
        return
    
    print("\n📊 PARQUET STORAGE SUMMARY")
    print("=" * 50)
    
    # Overall summary
    summary = stats["summary"]
    print(f"Total Files: {summary['total_files']:,}")
    print(f"Total Size: {summary['total_size_mb']:.1f} MB ({summary['total_size_kb']:.1f} KB)")
    print(f"Unique Symbols: {summary['symbol_count']}")
    print(f"Unique Trading Classes: {summary['trading_class_count']}")
    print(f"Unique Expirations: {summary['expiration_count']}")
    
    # Chains breakdown
    chains = stats["chains"]
    print(f"\n🔗 CHAINS CACHE:")
    print(f"  Directory: {chains['directory']}")
    print(f"  Exists: {'✅' if chains['exists'] else '❌'}")
    if chains['exists']:
        print(f"  Files: {chains['total_files']:,}")
        print(f"  Size: {chains['total_size_kb']:.1f} KB")
        print(f"  Symbols: {chains['symbol_count']} ({', '.join(chains['symbols_list'][:5])}{'...' if len(chains['symbols_list']) > 5 else ''})")
        print(f"  Trading Classes: {chains['trading_class_count']}")
        if chains['file_errors']:
            print(f"  ⚠️  Errors: {len(chains['file_errors'])}")
    
    # Strikes breakdown  
    strikes = stats["strikes"]
    print(f"\n🎯 STRIKES CACHE:")
    print(f"  Directory: {strikes['directory']}")
    print(f"  Exists: {'✅' if strikes['exists'] else '❌'}")
    if strikes['exists']:
        print(f"  Files: {strikes['total_files']:,}")
        print(f"  Size: {strikes['total_size_kb']:.1f} KB") 
        print(f"  Symbols: {strikes['symbol_count']} ({', '.join(strikes['symbols_list'][:5])}{'...' if len(strikes['symbols_list']) > 5 else ''})")
        print(f"  Trading Classes: {strikes['trading_class_count']}")
        print(f"  Expirations: {strikes['expiration_count']}")
        if strikes['file_errors']:
            print(f"  ⚠️  Errors: {len(strikes['file_errors'])}")
    
    # Show any symbols present
    if summary['symbols_list']:
        print(f"\n📋 SYMBOLS: {', '.join(summary['symbols_list'])}")
    
    print()  # Final newline

def validate_parquet_structure():
    """
    CORRECTED validation that understands the dual structure:
    - Chains: symbol/tradingclass_chain.parquet (2 levels)
    - Strikes: symbol/tradingclass/expiration_strikes.parquet (3 levels)
    """
    chains_dir = Path(CONFIG.get("chains_dir", "chains"))
    strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    issues = []
    files_checked = 0
    
    # Check chains structure
    if chains_dir.exists():
        for chain_file in chains_dir.rglob("*_chain.parquet"):
            files_checked += 1
            parts = chain_file.relative_to(chains_dir).parts
            
            # Chains should be: symbol/tradingclass_chain.parquet
            if len(parts) != 2:
                issues.append(f"Invalid chain path structure (expected 2 levels): {chain_file}")
                continue
                
            # Validate file readability
            try:
                import pyarrow.parquet as pq
                table = pq.read_table(chain_file)
                df = table.to_pandas()
                
                if df.empty:
                    issues.append(f"Empty chain file: {chain_file}")
                    continue
                    
                # Check required columns for chains
                required_columns = ['exchange', 'trading_class', 'expiration', 'strike']
                missing_columns = [col for col in required_columns if col not in df.columns]
                if missing_columns:
                    issues.append(f"Chain file missing columns {missing_columns}: {chain_file}")
                    
            except Exception as e:
                issues.append(f"Cannot read chain file {chain_file}: {e}")
    
    # Check strikes structure  
    if strikes_dir.exists():
        for strikes_file in strikes_dir.rglob("*_strikes.parquet"):
            files_checked += 1
            parts = strikes_file.relative_to(strikes_dir).parts
            
            # Strikes should be: symbol/tradingclass/expiration_strikes.parquet
            if len(parts) != 3:
                issues.append(f"Invalid strikes path structure (expected 3 levels): {strikes_file}")
                continue
                
            # Validate expiration format
            expiration = parts[2].replace("_strikes.parquet", "")
            if len(expiration) != 8 or not expiration.isdigit():
                issues.append(f"Invalid expiration format in strikes file: {strikes_file}")
                continue
                
            # Validate file readability
            try:
                import pyarrow.parquet as pq
                table = pq.read_table(strikes_file)
                df = table.to_pandas()
                
                if df.empty:
                    issues.append(f"Empty strikes file: {strikes_file}")
                    continue
                    
                # Check required columns for strikes
                required_columns = ['symbol', 'trading_class', 'expiration', 'strike']
                missing_columns = [col for col in required_columns if col not in df.columns]
                if missing_columns:
                    issues.append(f"Strikes file missing columns {missing_columns}: {strikes_file}")
                    
            except Exception as e:
                issues.append(f"Cannot read strikes file {strikes_file}: {e}")
    
    return {
        "files_checked": files_checked,
        "issues_found": len(issues),
        "issues": issues[:20] if issues else [],  # Show first 20 issues
        "total_issues": len(issues),
        "validation_passed": len(issues) == 0
    }
    
def generate_cache_health_report():
    """Generate comprehensive cache health report."""
    
    chains_storage = ParquetChainsStorage() 
    strikes_storage = ParquetStrikesStorage()
    
    report = {
        'timestamp': datetime.datetime.now().isoformat(),
        'chains_cache': _analyze_chains_cache(chains_storage),
        'strikes_cache': _analyze_strikes_cache(strikes_storage),
        'recommendations': []
    }
    
    # Add recommendations based on analysis
    if report['chains_cache']['expired_chains'] > 0:
        report['recommendations'].append("Run cache cleanup to remove expired chain data")
    
    if report['strikes_cache']['schema_migrations_needed'] > 0:
        report['recommendations'].append("Some strike files need schema migration")
        
    return report


### Private functions

def _analyze_chains_cache(chains_storage):
    """Analyze chains cache health."""
    
    base_dir = chains_storage.base_dir
    if not base_dir.exists():
        return {'status': 'no_cache_directory'}
    
    today = datetime.date.today().strftime("%Y%m%d")
    total_files = 0
    expired_chains = 0
    valid_chains = 0
    
    for chain_file in base_dir.rglob("*_chain.parquet"):
        total_files += 1
        try:
            table = pq.read_table(chain_file)
            df = table.to_pandas()
            
            # Check for expired expirations
            expired_count = len(df[df['expiration'].astype(str) < today])
            valid_count = len(df[df['expiration'].astype(str) >= today])
            
            if expired_count > 0:
                expired_chains += 1
            if valid_count > 0:
                valid_chains += 1
                
        except Exception as e:
            logger.warning(f"Error analyzing {chain_file}: {e}")
    
    return {
        'total_files': total_files,
        'expired_chains': expired_chains, 
        'valid_chains': valid_chains,
        'cache_efficiency': (valid_chains / total_files * 100) if total_files > 0 else 0
    }

def _analyze_strikes_cache(strikes_storage):
    """Analyze strikes cache health."""
    
    base_dir = strikes_storage.base_dir
    if not base_dir.exists():
        return {'status': 'no_cache_directory'}
    
    total_files = 0
    schema_migrations_needed = 0
    qualification_rates = []
    
    for strikes_file in base_dir.rglob("*_strikes.parquet"):
        total_files += 1
        try:
            table = pq.read_table(strikes_file)
            df = table.to_pandas()
            
            # Check if schema migration needed
            if 'status' not in df.columns:
                schema_migrations_needed += 1
            else:
                # Calculate qualification rate
                total_strikes = len(df)
                qualified_strikes = len(df[df['status'] == 'qualified'])
                if total_strikes > 0:
                    qualification_rates.append(qualified_strikes / total_strikes * 100)
                    
        except Exception as e:
            logger.warning(f"Error analyzing {strikes_file}: {e}")
    
    avg_qualification_rate = sum(qualification_rates) / len(qualification_rates) if qualification_rates else 0
    
    return {
        'total_files': total_files,
        'schema_migrations_needed': schema_migrations_needed,
        'avg_qualification_rate': avg_qualification_rate,
        'files_analyzed': len(qualification_rates)
    }
    
def _get_storage_stats():
    """
    Get comprehensive statistics about both chains and strikes Parquet storage.
    
    Returns:
        dict: Detailed storage statistics including file counts, sizes, symbols, etc.
    """
    try:
        chains_dir = Path(CONFIG.get("chains_dir", "chains"))
        strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
        
        stats = {
            "chains": {
                "directory": str(chains_dir),
                "exists": chains_dir.exists(),
                "total_files": 0,
                "total_size_kb": 0,
                "symbols": set(),
                "trading_classes": set(),
                "file_errors": []
            },
            "strikes": {
                "directory": str(strikes_dir), 
                "exists": strikes_dir.exists(),
                "total_files": 0,
                "total_size_kb": 0,
                "symbols": set(),
                "trading_classes": set(),
                "expirations": set(),
                "file_errors": []
            },
            "summary": {
                "total_files": 0,
                "total_size_kb": 0,
                "total_size_mb": 0,
                "unique_symbols": set(),
                "unique_trading_classes": set(),
                "unique_expirations": set()
            }
        }
        
        # Analyze chains directory
        if chains_dir.exists():
            stats["chains"] = _analyze_chains_directory(chains_dir, stats["chains"])
        
        # Analyze strikes directory  
        if strikes_dir.exists():
            stats["strikes"] = _analyze_strikes_directory(strikes_dir, stats["strikes"])
        
        # Calculate summary statistics
        stats["summary"]["total_files"] = stats["chains"]["total_files"] + stats["strikes"]["total_files"]
        stats["summary"]["total_size_kb"] = stats["chains"]["total_size_kb"] + stats["strikes"]["total_size_kb"]
        stats["summary"]["total_size_mb"] = round(stats["summary"]["total_size_kb"] / 1024, 2)
        
        # Combine unique values from both directories
        stats["summary"]["unique_symbols"] = stats["chains"]["symbols"] | stats["strikes"]["symbols"]
        stats["summary"]["unique_trading_classes"] = stats["chains"]["trading_classes"] | stats["strikes"]["trading_classes"]
        stats["summary"]["unique_expirations"] = stats["strikes"]["expirations"]  # Only strikes have expirations
        
        # Convert summary sets to counts and lists for JSON serialization
        stats["summary"]["symbol_count"] = len(stats["summary"]["unique_symbols"])
        stats["summary"]["trading_class_count"] = len(stats["summary"]["unique_trading_classes"])
        stats["summary"]["expiration_count"] = len(stats["summary"]["unique_expirations"])
        
        stats["summary"]["symbols_list"] = sorted(list(stats["summary"]["unique_symbols"]))
        stats["summary"]["trading_classes_list"] = sorted(list(stats["summary"]["unique_trading_classes"]))
        stats["summary"]["expirations_list"] = sorted(list(stats["summary"]["unique_expirations"]))
        
        # Convert individual section sets to counts and lists
        for section in ["chains", "strikes"]:
            stats[section]["symbol_count"] = len(stats[section]["symbols"])
            stats[section]["trading_class_count"] = len(stats[section]["trading_classes"])
            stats[section]["symbols_list"] = sorted(list(stats[section]["symbols"]))
            stats[section]["trading_classes_list"] = sorted(list(stats[section]["trading_classes"]))
            
            if section == "strikes":
                stats[section]["expiration_count"] = len(stats[section]["expirations"])
                stats[section]["expirations_list"] = sorted(list(stats[section]["expirations"]))
            
            # Remove sets for JSON serialization
            del stats[section]["symbols"]
            del stats[section]["trading_classes"]
            if section == "strikes":
                del stats[section]["expirations"]
        
        # Remove summary sets
        del stats["summary"]["unique_symbols"]
        del stats["summary"]["unique_trading_classes"] 
        del stats["summary"]["unique_expirations"]
        
        return stats
        
    except Exception as e:
        return {"error": f"Failed to get storage stats: {str(e)}"}

def _analyze_chains_directory(chains_dir, stats):
    """
    Analyze chains directory structure: symbol/tradingclass_chain.parquet
    """
    for parquet_file in chains_dir.rglob("*_chain.parquet"):
        try:
            # Update file count and size
            stats["total_files"] += 1
            stats["total_size_kb"] += round(parquet_file.stat().st_size / 1024, 2)
            
            # Extract structure information - chains have 2 levels
            parts = parquet_file.relative_to(chains_dir).parts
            
            if len(parts) == 2:  # symbol/tradingclass_chain.parquet
                symbol = parts[0]
                trading_class_file = parts[1]  # e.g., "SPY_chain.parquet"
                
                # Extract trading class name from filename
                trading_class = trading_class_file.replace("_chain.parquet", "")
                
                stats["symbols"].add(symbol)
                stats["trading_classes"].add(trading_class)
            else:
                stats["file_errors"].append(f"Invalid chain structure: {parquet_file}")
                
        except Exception as e:
            stats["file_errors"].append(f"Error processing {parquet_file}: {str(e)}")
    
    return stats

def _analyze_strikes_directory(strikes_dir, stats):
    """
    Analyze strikes directory structure: symbol/tradingclass/expiration_strikes.parquet
    """
    for parquet_file in strikes_dir.rglob("*_strikes.parquet"):
        try:
            # Update file count and size
            stats["total_files"] += 1
            stats["total_size_kb"] += round(parquet_file.stat().st_size / 1024, 2)
            
            # Extract structure information - strikes have 3 levels
            parts = parquet_file.relative_to(strikes_dir).parts
            
            if len(parts) == 3:  # symbol/tradingclass/expiration_strikes.parquet
                symbol = parts[0]
                trading_class = parts[1] 
                expiration_file = parts[2]  # e.g., "20250829_strikes.parquet"
                
                # Extract expiration from filename
                expiration = expiration_file.replace("_strikes.parquet", "")
                
                # Validate expiration format (YYYYMMDD)
                if len(expiration) == 8 and expiration.isdigit():
                    stats["symbols"].add(symbol)
                    stats["trading_classes"].add(trading_class)
                    stats["expirations"].add(expiration)
                else:
                    stats["file_errors"].append(f"Invalid expiration format: {parquet_file}")
            else:
                stats["file_errors"].append(f"Invalid strikes structure: {parquet_file}")
                
        except Exception as e:
            stats["file_errors"].append(f"Error processing {parquet_file}: {str(e)}")
    
    return stats

def _select_file_from_multiple(found_files, filename):
    """Handle case where multiple files match the search."""
    print(f"Multiple files found for '{filename}':")
    for i, f in enumerate(found_files, 1):
        print(f"  {i}: {f}")
    choice = input("Enter number to select file (or 'q' to quit): ")
    if choice.lower() == 'q':
        return None
    try:
        return found_files[int(choice) - 1]
    except (ValueError, IndexError):
        print("Invalid selection")
        return None

def _display_parquet_analysis(file_path):
    """Display comprehensive analysis of parquet file contents."""
    table = pq.read_table(file_path)
    df = table.to_pandas()
    
    print(f"\nFile: {file_path}")
    print(f"Shape: {df.shape[0]} rows x {df.shape[1]} columns")
    print(f"File size: {file_path.stat().st_size / 1024:.1f} KB")
    print("\n" + "="*60)
    
    # Display column information
    _display_column_info(df)
    
    print("\n" + "="*60)
    
    # Display sample data
    _display_sample_data(df)
    
    # Display strike-specific analysis if applicable
    if 'strike' in df.columns:
        _display_strike_analysis(df)
    
    return df

def _display_column_info(df):
    """Display detailed column information."""
    print("Column Info:")
    for col in df.columns:
        dtype = df[col].dtype
        non_null = df[col].notna().sum()
        unique_count = df[col].nunique()
        print(f"  {col}: {dtype} ({non_null}/{len(df)} non-null, {unique_count} unique)")

def _display_sample_data(df):
    """Display sample data from the DataFrame."""
    if len(df) <= 20:
        print("All Data:")
        print(df.to_string(index=False))
    else:
        print("First 10 rows:")
        print(df.head(10).to_string(index=False))
        print(f"\n... and {len(df)-10} more rows")

def _display_strike_analysis(df):
    """Display strike-specific analysis for option data."""
    strikes = sorted(df['strike'].unique())
    print("\n" + "="*60)
    print("Strike Information:")
    print(f"  Total strikes: {len(strikes)}")
    print(f"  Strike range: {min(strikes)} to {max(strikes)}")
    
    # Show strike distribution
    if len(strikes) > 10:
        print(f"  First 10: {strikes[:10]}")
        print(f"  Last 10:  {strikes[-10:]}")
    else:
        print(f"  All strikes: {strikes}")
        
    # Calculate common intervals
    if len(strikes) > 1:
        intervals = [strikes[i+1] - strikes[i] for i in range(len(strikes)-1)]
        common_intervals = {}
        for interval in intervals:
            common_intervals[interval] = common_intervals.get(interval, 0) + 1
        most_common = max(common_intervals.items(), key=lambda x: x[1])
        print(f"  Most common interval: {most_common[0]} ({most_common[1]} occurrences)")

def _group_files_by_structure(parquet_files, base_dir):
    """Group files by symbol and trading class structure."""
    grouped = {}
    for file_path in sorted(parquet_files):
        parts = file_path.relative_to(base_dir).parts
        if len(parts) >= 3:  # symbol/trading_class/expiration.parquet
            sym = parts[0]
            tc = parts[1] 
            exp = parts[2].replace('.parquet', '')
            
            if sym not in grouped:
                grouped[sym] = {}
            if tc not in grouped[sym]:
                grouped[sym][tc] = []
            grouped[sym][tc].append(exp)
    
    return grouped

def _display_grouped_files(grouped):
    """Display grouped files in organized format."""
    for sym in sorted(grouped.keys()):
        print(f"\n {sym}:")
        for tc in sorted(grouped[sym].keys()):
            expirations = sorted(grouped[sym][tc])
            print(f"   {tc}: {len(expirations)} files")
            print(f"     {', '.join(expirations)}")


