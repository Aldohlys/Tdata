"""
Focused Historical Data Architecture for Limited Contract Sets

Optimized for:
- Weekly/monthly update frequency  
- 2-10 contracts per ticker
- 50-100 tickers total
- Hourly bar data only
- Graphing and analysis focus
"""

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
from pathlib import Path
import datetime
from typing import List, Dict, Optional, Tuple

from .core import CONFIG
from fin_logger import get_logger

logger = get_logger("tdata_py.focused_historical")


class FocusedHistoricalManager:
    """
    Simplified historical data manager for defined contract watchlists.
    Focus on efficiency for limited, well-defined contract sets.
    """
    
    def __init__(self):
        self.base_dir = Path(CONFIG.get("historical_data_dir", "historical_data"))
        self.base_dir.mkdir(parents=True, exist_ok=True)
        
        # Simple structure: symbol/contract_id.parquet
        # contract_id format: "SPY_20240315_450_C" 
    
    def get_contract_file_path(self, symbol, expiration, strike, right):
        """
        Simple file path: historical_data/SPY/SPY_20240315_450_C.parquet
        
        Benefits:
        - Flat structure, easy to manage
        - Direct contract access
        - Simple for limited contract sets
        """
        contract_id = f"{symbol}_{expiration}_{strike}_{right}"
        return self.base_dir / symbol / f"{contract_id}.parquet"
    
    def save_contract_data(self, symbol, expiration, strike, right, bars_data, 
                          metadata=None):
        """
        Save complete historical dataset for a single contract.
        Designed for weekly/monthly full updates rather than incremental.
        
        Args:
            symbol (str): Ticker symbol
            expiration (str): YYYYMMDD format
            strike (float): Strike price
            right (str): 'C' or 'P'
            bars_data (list): IB historical bars
            metadata (dict, optional): Additional contract metadata
        """
        try:
            file_path = self.get_contract_file_path(symbol, expiration, strike, right)
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Convert to clean DataFrame
            df = self._create_historical_dataframe(bars_data, symbol, expiration, 
                                                 strike, right, metadata)
            
            # Save with good compression for analysis
            df.to_parquet(file_path, compression='snappy', index=False)
            
            logger.info(f"Saved {len(df)} hourly bars for {symbol} {expiration} {strike}{right}")
            return True
            
        except Exception as e:
            logger.error(f"Error saving {symbol} {expiration} {strike}{right}: {e}")
            return False
    
    def load_contract_data(self, symbol, expiration, strike, right, 
                          start_date=None, end_date=None):
        """
        Load historical data for specific contract, optimized for graphing.
        
        Returns:
            pandas.DataFrame: Ready-to-plot data with datetime index
        """
        try:
            file_path = self.get_contract_file_path(symbol, expiration, strike, right)
            
            if not file_path.exists():
                logger.debug(f"No data file: {file_path}")
                return pd.DataFrame()
            
            df = pd.read_parquet(file_path)
            
            # Convert to datetime index for easy plotting
            df['datetime'] = pd.to_datetime(df['datetime'])
            df = df.set_index('datetime').sort_index()
            
            # Apply date filtering
            if start_date or end_date:
                if start_date:
                    df = df[df.index >= pd.to_datetime(start_date)]
                if end_date:
                    df = df[df.index <= pd.to_datetime(end_date)]
            
            return df
            
        except Exception as e:
            logger.error(f"Error loading {symbol} {expiration} {strike}{right}: {e}")
            return pd.DataFrame()
    
    def _create_historical_dataframe(self, bars_data, symbol, expiration, 
                                   strike, right, metadata=None):
        """Create clean DataFrame from IB bars data."""
        records = []
        
        for bar in bars_data:
            record = {
                'datetime': pd.to_datetime(bar.date),
                'symbol': symbol,
                'expiration': expiration,
                'strike': strike,
                'right': right,
                
                # OHLCV data
                'open': float(bar.open) if bar.open > 0 else None,
                'high': float(bar.high) if bar.high > 0 else None,
                'low': float(bar.low) if bar.low > 0 else None,
                'close': float(bar.close) if bar.close > 0 else None,
                'volume': bar.volume if bar.volume >= 0 else 0,
                
                # Option-specific market data
                'bid': getattr(bar, 'bid', None),
                'ask': getattr(bar, 'ask', None),
                'last': getattr(bar, 'last', None),
                'implied_vol': getattr(bar, 'impliedVolatility', None),
                
                # Greeks if available
                'delta': getattr(bar, 'delta', None),
                'gamma': getattr(bar, 'gamma', None),
                'theta': getattr(bar, 'theta', None),
                'vega': getattr(bar, 'vega', None),
                
                'underlying_price': getattr(bar, 'underlyingPrice', None),
                'last_updated': datetime.datetime.now()
            }
            
            # Add metadata if provided
            if metadata:
                record.update(metadata)
                
            records.append(record)
        
        df = pd.DataFrame(records)
        
        # Clean up data for analysis
        df = self._clean_data_for_analysis(df)
        
        return df
    
    def _clean_data_for_analysis(self, df):
        """Clean and validate data for graphing/analysis."""
        # Remove rows with no price data
        price_cols = ['open', 'high', 'low', 'close', 'bid', 'ask', 'last']
        df = df.dropna(subset=price_cols, how='all')
        
        # Ensure positive prices
        for col in price_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')
                df.loc[df[col] <= 0, col] = None
        
        # Sort by datetime
        df = df.sort_values('datetime').reset_index(drop=True)
        
        return df
    
    def get_watchlist_summary(self, symbols=None):
        """
        Get summary of all stored contracts, useful for portfolio overview.
        
        Returns:
            pandas.DataFrame: Summary of available data
        """
        summary_records = []
        
        base_path = self.base_dir
        if symbols:
            symbol_dirs = [base_path / sym for sym in symbols if (base_path / sym).exists()]
        else:
            symbol_dirs = [d for d in base_path.iterdir() if d.is_dir()]
        
        for symbol_dir in symbol_dirs:
            symbol = symbol_dir.name
            
            for contract_file in symbol_dir.glob("*.parquet"):
                try:
                    # Parse contract info from filename
                    contract_parts = contract_file.stem.split('_')
                    if len(contract_parts) >= 4:
                        _, expiration, strike, right = contract_parts[:4]
                        
                        # Get basic file info
                        file_size_kb = contract_file.stat().st_size / 1024
                        
                        # Quick data peek
                        df = pd.read_parquet(contract_file)
                        
                        record = {
                            'symbol': symbol,
                            'expiration': expiration,
                            'strike': float(strike),
                            'right': right,
                            'bar_count': len(df),
                            'file_size_kb': round(file_size_kb, 1),
                            'data_start': df['datetime'].min() if not df.empty else None,
                            'data_end': df['datetime'].max() if not df.empty else None,
                            'last_updated': df['last_updated'].max() if 'last_updated' in df.columns else None
                        }
                        summary_records.append(record)
                        
                except Exception as e:
                    logger.warning(f"Error reading {contract_file}: {e}")
                    continue
        
        if summary_records:
            summary_df = pd.DataFrame(summary_records)
            return summary_df.sort_values(['symbol', 'expiration', 'strike', 'right'])
        else:
            return pd.DataFrame()


class WatchlistManager:
    """
    Manages defined contract watchlists for systematic updates.
    Helps organize the 2-10 contracts per ticker × 50-100 tickers.
    """
    
    def __init__(self):
        self.watchlist_file = Path(CONFIG.get("watchlist_file", "watchlist.parquet"))
    
    def save_watchlist(self, contracts_data):
        """
        Save watchlist of contracts to monitor.
        
        Args:
            contracts_data (list): List of dicts with contract specifications
                Example: [
                    {'symbol': 'SPY', 'expiration': '20240315', 'strike': 450, 'right': 'C', 'priority': 1},
                    {'symbol': 'SPY', 'expiration': '20240315', 'strike': 450, 'right': 'P', 'priority': 1},
                ]
        """
        try:
            df = pd.DataFrame(contracts_data)
            df.to_parquet(self.watchlist_file, compression='snappy', index=False)
            logger.info(f"Saved watchlist with {len(df)} contracts")
            
        except Exception as e:
            logger.error(f"Error saving watchlist: {e}")
    
    def load_watchlist(self, symbols=None, priority=None):
        """
        Load active watchlist.
        
        Args:
            symbols (list, optional): Filter by specific symbols
            priority (int, optional): Filter by priority level
            
        Returns:
            pandas.DataFrame: Watchlist contracts
        """
        try:
            if not self.watchlist_file.exists():
                logger.info("No watchlist file found")
                return pd.DataFrame()
            
            df = pd.read_parquet(self.watchlist_file)
            
            # Apply filters
            if symbols:
                df = df[df['symbol'].isin(symbols)]
            
            if priority is not None:
                df = df[df['priority'] == priority]
            
            return df.sort_values(['symbol', 'expiration', 'strike'])
            
        except Exception as e:
            logger.error(f"Error loading watchlist: {e}")
            return pd.DataFrame()
    
    def get_symbols_summary(self):
        """Get summary of symbols in watchlist."""
        df = self.load_watchlist()
        if df.empty:
            return pd.DataFrame()
        
        summary = df.groupby('symbol').agg({
            'expiration': 'nunique',
            'strike': 'nunique', 
            'right': 'count',
            'priority': 'min'
        }).rename(columns={
            'expiration': 'expiration_count',
            'strike': 'strike_count',
            'right': 'total_contracts',
            'priority': 'min_priority'
        })
        
        return summary.sort_values('min_priority')


class BatchHistoricalUpdater:
    """
    Handles batch updates for the entire watchlist.
    Optimized for weekly/monthly update cycles.
    """
    
    def __init__(self):
        self.data_manager = FocusedHistoricalManager()
        self.watchlist_manager = WatchlistManager()
    
    def update_watchlist_data(self, duration="60 D", symbols=None, priority=None, 
                            ib_connection=None, max_contracts_per_session=50):
        """
        Update historical data for entire watchlist.
        
        Args:
            duration (str): How much historical data to fetch
            symbols (list, optional): Update specific symbols only
            priority (int, optional): Update specific priority level only
            ib_connection: Existing IB connection
            max_contracts_per_session (int): Limit contracts per session to manage API usage
            
        Returns:
            dict: Update results summary
        """
        from .IB_connection import safe_ib_connect
        from ib_async import Contract
        from .core import ticker_db

        # Load watchlist
        watchlist = self.watchlist_manager.load_watchlist(symbols, priority)

        if watchlist.empty:
            logger.info("No contracts in watchlist to update")
            return {'error': 'Empty watchlist'}

        # Limit contracts per session
        if len(watchlist) > max_contracts_per_session:
            logger.info(f"Limiting to first {max_contracts_per_session} contracts")
            watchlist = watchlist.head(max_contracts_per_session)

        # Setup IB connection
        close_connection = False
        if ib_connection is not None:
            ib = ib_connection
        else:
            ib = safe_ib_connect()
            close_connection = True

        if not ib.isConnected():
            return {'error': 'No IB connection'}

        try:
            results = {'success': [], 'failed': [], 'total': len(watchlist)}

            for _, contract_row in watchlist.iterrows():
                symbol = contract_row['symbol']
                expiration = contract_row['expiration']
                strike = contract_row['strike']
                right = contract_row['right']

                try:
                    # Determine contract type: FOP for futures options, OPT for stock options
                    ticker_info = ticker_db.get_ticker_info(symbol)
                    sec_type = ticker_info.get('Type') if ticker_info else 'STK'
                    sec_opt_type = "FOP" if sec_type == "FUT" else "OPT"

                    # Create IB contract with correct secType
                    contract = Contract(
                        symbol=symbol,
                        secType=sec_opt_type,
                        lastTradeDateOrContractMonth=expiration,
                        strike=strike,
                        right=right,
                        exchange='SMART'
                    )
                    
                    # Qualify and fetch data
                    qualified = ib.qualifyContracts(contract)
                    if not qualified:
                        results['failed'].append(f"{symbol} {expiration} {strike}{right} - not qualified")
                        continue
                    
                    # Request historical data (hourly bars)
                    bars = ib.reqHistoricalData(
                        qualified[0],
                        endDateTime='',
                        durationStr=duration,
                        barSizeSetting='1 hour',
                        whatToShow='BID_ASK',
                        useRTH=False,  # Include extended hours
                        formatDate=1
                    )
                    
                    if bars:
                        # Save the data
                        success = self.data_manager.save_contract_data(
                            symbol, expiration, strike, right, bars
                        )
                        
                        if success:
                            results['success'].append(f"{symbol} {expiration} {strike}{right} - {len(bars)} bars")
                        else:
                            results['failed'].append(f"{symbol} {expiration} {strike}{right} - save failed")
                    else:
                        results['failed'].append(f"{symbol} {expiration} {strike}{right} - no data")
                    
                    ib.sleep(0.5)  # Rate limiting between contracts
                    
                except Exception as e:
                    results['failed'].append(f"{symbol} {expiration} {strike}{right} - {str(e)}")
                    logger.error(f"Error updating {symbol} {expiration} {strike}{right}: {e}")
                    continue
            
            logger.info(f"Batch update complete: {len(results['success'])} success, {len(results['failed'])} failed")
            return results
            
        finally:
            if close_connection:
                ib.disconnect()


# Convenience functions for R integration
def load_contract_for_graph(symbol, expiration, strike, right, start_date=None, end_date=None):
    """
    Simple function to load contract data ready for graphing.
    Perfect for R integration.
    
    Returns:
        pandas.DataFrame: Data with datetime index, ready to plot
    """
    manager = FocusedHistoricalManager()
    return manager.load_contract_data(symbol, expiration, strike, right, start_date, end_date)


def get_portfolio_overview():
    """
    Get overview of entire portfolio data status.
    Useful for R dashboard creation.
    
    Returns:
        dict: {'watchlist': DataFrame, 'data_summary': DataFrame}
    """
    watchlist_mgr = WatchlistManager()
    data_mgr = FocusedHistoricalManager()
    
    return {
        'watchlist': watchlist_mgr.load_watchlist(),
        'data_summary': data_mgr.get_watchlist_summary()
    }


def update_priority_contracts(priority=1, duration="30 D"):
    """
    Update highest priority contracts only.
    Perfect for frequent updates of most important data.
    """
    updater = BatchHistoricalUpdater()
    return updater.update_watchlist_data(duration=duration, priority=priority)
