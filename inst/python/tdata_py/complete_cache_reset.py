"""
Complete cache reset and recreation utilities.
Use these functions to cleanly reset your Parquet cache and rebuild from scratch.
"""

import shutil
import datetime
from pathlib import Path
from .core import CONFIG
from .chains_manager import initialize_chains_for_symbols, getChains
from fin_logger import get_logger

logger = get_logger("tdata_py.cache_reset")

def nuclear_cache_reset(confirm=False):
    """
    NUCLEAR OPTION: Delete all cached Parquet files and directories.
    
    Args:
        confirm (bool): Must be True to actually execute deletion
        
    Returns:
        dict: Summary of what was deleted
    """
    if not confirm:
        return {
            "status": "aborted",
            "message": "Must set confirm=True to execute deletion. This will delete ALL cached data!"
        }
    
    chains_dir = Path(CONFIG.get("chains_dir", "chains"))
    strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    deleted_items = {
        "chains_dir": {"exists": False, "deleted": False, "files_count": 0},
        "strikes_dir": {"exists": False, "deleted": False, "files_count": 0},
        "total_files_deleted": 0
    }
    
    # Delete chains directory
    if chains_dir.exists():
        deleted_items["chains_dir"]["exists"] = True
        # Count files before deletion
        chain_files = list(chains_dir.rglob("*.parquet"))
        deleted_items["chains_dir"]["files_count"] = len(chain_files)
        
        try:
            shutil.rmtree(chains_dir)
            deleted_items["chains_dir"]["deleted"] = True
            logger.info(f"Deleted chains directory: {chains_dir}")
        except Exception as e:
            logger.error(f"Error deleting chains directory: {e}")
            return {"error": f"Failed to delete chains directory: {e}"}
    
    # Delete strikes directory  
    if strikes_dir.exists():
        deleted_items["strikes_dir"]["exists"] = True
        # Count files before deletion
        strike_files = list(strikes_dir.rglob("*.parquet"))
        deleted_items["strikes_dir"]["files_count"] = len(strike_files)
        
        try:
            shutil.rmtree(strikes_dir)
            deleted_items["strikes_dir"]["deleted"] = True
            logger.info(f"Deleted strikes directory: {strikes_dir}")
        except Exception as e:
            logger.error(f"Error deleting strikes directory: {e}")
            return {"error": f"Failed to delete strikes directory: {e}"}
    
    deleted_items["total_files_deleted"] = (
        deleted_items["chains_dir"]["files_count"] + 
        deleted_items["strikes_dir"]["files_count"]
    )
    
    logger.info(f"Nuclear cache reset complete: {deleted_items['total_files_deleted']} files deleted")
    
    return {
        "status": "success",
        "timestamp": datetime.datetime.now().isoformat(),
        "deleted_items": deleted_items,
        "message": f"Successfully deleted {deleted_items['total_files_deleted']} cached files"
    }

def selective_cache_cleanup(expired_only=True, invalid_structure=True, confirm=False):
    """
    Selective cleanup of problematic cache files.
    
    Args:
        expired_only (bool): Remove files with expired expirations
        invalid_structure (bool): Remove files that don't match expected structure
        confirm (bool): Must be True to actually execute deletion
        
    Returns:
        dict: Summary of cleanup results
    """
    if not confirm:
        return {
            "status": "aborted", 
            "message": "Must set confirm=True to execute deletion"
        }
    
    chains_dir = Path(CONFIG.get("chains_dir", "chains"))
    strikes_dir = Path(CONFIG.get("strikes_dir", "strikes"))
    
    cleanup_results = {
        "expired_files": [],
        "invalid_structure_files": [],
        "total_deleted": 0,
        "errors": []
    }
    
    today = datetime.date.today().strftime("%Y%m%d")
    
    # Process both directories
    for base_dir in [chains_dir, strikes_dir]:
        if not base_dir.exists():
            continue
            
        for parquet_file in base_dir.rglob("*.parquet"):
            should_delete = False
            reason = ""
            
            # Check for invalid structure
            if invalid_structure:
                parts = parquet_file.relative_to(base_dir).parts
                
                # For chains: expect symbol/tradingclass_chain.parquet
                if "_chain.parquet" in parquet_file.name:
                    if len(parts) != 2:
                        should_delete = True
                        reason = "invalid_chain_structure"
                
                # For strikes: expect symbol/tradingclass/expiration_strikes.parquet  
                elif "_strikes.parquet" in parquet_file.name:
                    if len(parts) != 3:
                        should_delete = True
                        reason = "invalid_strikes_structure"
                    else:
                        # Validate expiration format
                        expiration = parts[2].replace("_strikes.parquet", "")
                        if len(expiration) != 8 or not expiration.isdigit():
                            should_delete = True
                            reason = "invalid_expiration_format"
            
            # Check for expired files
            if expired_only and not should_delete:
                try:
                    import pyarrow.parquet as pq
                    table = pq.read_table(parquet_file)
                    df = table.to_pandas()
                    
                    if 'expiration' in df.columns:
                        expired_expirations = df[df['expiration'].astype(str) < today]
                        if len(expired_expirations) > 0 and len(expired_expirations) == len(df):
                            # All expirations in file are expired
                            should_delete = True
                            reason = "all_expirations_expired"
                            
                except Exception as e:
                    cleanup_results["errors"].append(f"Error reading {parquet_file}: {e}")
            
            # Delete the file if flagged
            if should_delete:
                try:
                    parquet_file.unlink()
                    if reason == "invalid_chain_structure" or reason == "invalid_strikes_structure":
                        cleanup_results["invalid_structure_files"].append(str(parquet_file))
                    else:
                        cleanup_results["expired_files"].append(str(parquet_file))
                    cleanup_results["total_deleted"] += 1
                    logger.info(f"Deleted {reason}: {parquet_file}")
                except Exception as e:
                    cleanup_results["errors"].append(f"Error deleting {parquet_file}: {e}")
    
    # Cleanup empty directories
    _cleanup_empty_dirs([chains_dir, strikes_dir])
    
    return {
        "status": "success",
        "timestamp": datetime.datetime.now().isoformat(),
        "cleanup_results": cleanup_results,
        "message": f"Deleted {cleanup_results['total_deleted']} files"
    }

def _cleanup_empty_dirs(base_dirs):
    """Remove empty directories recursively."""
    for base_dir in base_dirs:
        if not base_dir.exists():
            continue
            
        try:
            # Walk from bottom up to handle nested empty directories
            for dirpath in sorted(base_dir.rglob("*"), reverse=True):
                if dirpath.is_dir() and dirpath != base_dir:
                    try:
                        if not any(dirpath.iterdir()):
                            dirpath.rmdir()
                            logger.debug(f"Removed empty directory: {dirpath}")
                    except OSError:
                        pass  # Directory not empty or permission issue
        except Exception as e:
            logger.error(f"Error cleaning empty directories in {base_dir}: {e}")

def rebuild_cache_for_symbols(symbols, force_refresh=True):
    """
    Rebuild cache for specific symbols from scratch.
    
    Args:
        symbols (list): List of symbols to rebuild
        force_refresh (bool): Force refresh even if cache exists
        
    Returns:
        dict: Rebuild results
    """
    logger.info(f"Starting cache rebuild for {len(symbols)} symbols")
    
    results = initialize_chains_for_symbols(symbols, force_refresh=force_refresh)
    
    success_count = len([s for s in results.values() if s.startswith('success')])
    
    rebuild_summary = {
        "status": "complete",
        "timestamp": datetime.datetime.now().isoformat(),
        "symbols_requested": len(symbols),
        "symbols_successful": success_count,
        "symbols_failed": len(symbols) - success_count,
        "detailed_results": results
    }
    
    logger.info(f"Cache rebuild complete: {success_count}/{len(symbols)} symbols successful")
    
    return rebuild_summary

def full_cache_reset_and_rebuild(symbols, confirm=False):
    """
    COMPLETE RESET: Delete all cache and rebuild for specified symbols.
    
    Args:
        symbols (list): Symbols to rebuild after deletion
        confirm (bool): Must be True to execute
        
    Returns:
        dict: Complete operation results
    """
    if not confirm:
        return {
            "status": "aborted",
            "message": "Must set confirm=True. This will DELETE ALL cached data and rebuild!"
        }
    
    logger.info("Starting FULL cache reset and rebuild...")
    
    # Step 1: Nuclear reset
    reset_results = nuclear_cache_reset(confirm=True)
    
    if reset_results.get("status") != "success":
        return reset_results
    
    logger.info("Nuclear reset complete, waiting 2 seconds...")
    import time
    time.sleep(2)  # Brief pause to ensure filesystem operations complete
    
    # Step 2: Rebuild
    rebuild_results = rebuild_cache_for_symbols(symbols, force_refresh=True)
    
    return {
        "status": "complete",
        "timestamp": datetime.datetime.now().isoformat(),
        "reset_results": reset_results,
        "rebuild_results": rebuild_results,
        "message": f"Full reset complete: {rebuild_results['symbols_successful']} symbols rebuilt"
    }

def get_test_symbols_list():
    """Get the standard test symbols for rebuilding."""
    return [
        # US Large Cap ETFs and Stocks
        'SPY', 'QQQ', 'AAPL', 'MSFT', 'TSLA',
        
        # Commodity and Bond ETFs  
        'USO', 'GLD', 'SLV', 'TLT', 'VIX',
        
        # European Stocks
        'SIE',  # Remove problematic SAP, ASME for now
        
        # Major US Index Options
        'SPX',  # Remove problematic NDX, RUT for now
        
        # International ETFs
        'EWJ', 'FXI', 'EWZ'
    ]

def quick_reset_and_test():
    """
    Quick function to reset cache and rebuild with test symbols.
    Good for development/testing.
    """
    test_symbols = get_test_symbols_list()
    
    print("🚀 Starting quick reset and test...")
    print(f"Will rebuild cache for: {', '.join(test_symbols)}")
    
    # Confirm with user
    response = input("\n⚠️  This will DELETE ALL cached data. Type 'yes' to continue: ")
    if response.lower() != 'yes':
        return {"status": "cancelled", "message": "Operation cancelled by user"}
    
    return full_cache_reset_and_rebuild(test_symbols, confirm=True)

# Validation functions (corrected)
