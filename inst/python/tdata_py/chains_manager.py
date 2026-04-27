"""
Systematic Chain Caching - Ensures chains are always cached after first fetch

Key principle: ANY function that fetches chains data immediately caches it,
making it available for all subsequent calls without needing context objects.
"""

import math
import datetime
import logging
from pathlib import Path
from contextlib import contextmanager

from ib_async import *
import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd

from tdata_py.core import CONFIG, ticker_db
from tdata_py.IB_connection import safe_ib_connect
from tdata_py.parquet_storage import ParquetChainsStorage, ParquetStrikesStorage
from fin_logger import get_logger

logger = get_logger("tdata_py.chains_manager")

# ---------------------------------------------------------------------------
# Stale-warning accumulator
# ---------------------------------------------------------------------------
_stale_warnings = []


def get_stale_warnings():
    """Return a copy of accumulated stale-cache warnings."""
    return list(_stale_warnings)


def clear_stale_warnings():
    """Clear the accumulated stale-cache warnings."""
    _stale_warnings.clear()

@contextmanager
def suppress_ib_errors():
    """Context manager to temporarily suppress IB wrapper errors."""
    wrapper_logger = logging.getLogger('ib_async.wrapper')
    original_level = wrapper_logger.level
    wrapper_logger.setLevel(logging.CRITICAL)
    try:
        yield
    finally:
        wrapper_logger.setLevel(original_level)

def getChains(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, ib_connection=None, force_refresh=False):
  """
  Get all option chains for a symbol - ALWAYS caches results.
  
  Key behavior: If data comes from IBKR, it's immediately saved to cache
  so subsequent calls in same session or future sessions use cached data.
  
  Args:
    sym (str): Underlying symbol
    secType (str, optional): Security type. If None, uses ticker database.
    currency (str, optional): Currency. If None, uses ticker database.
    exchangeSec (str, optional): Exchange for underlying. If None, uses ticker database.
    exchangeOpt (str, optional): Exchange for options. If None, uses ticker database.
    ib_connection: Existing IB connection to reuse
    force_refresh (bool): If True, fetch from IB even if cache exists
    
  Returns:
    list or float or None: List of chains if found, NaN if not found, None if connection error
  """
  
  storage = ParquetChainsStorage()

  # TTL check: remove stale chain files so load_chains() triggers a fresh fetch
  if not force_refresh:
    stale_deleted = storage.check_and_remove_stale_chains(sym)
    if stale_deleted:
        for tc in stale_deleted:
            warning_msg = f"Stale chain cache evicted for {sym}/{tc}"
            _stale_warnings.append(warning_msg)

  # Try cache first (unless forced refresh)
  if not force_refresh:
    cached_chains = storage.load_chains(sym)
    
    # Check if cached data is still valid (non-expired)
    if cached_chains:
      today = datetime.date.today().strftime("%Y%m%d")  # String format for comparison
      valid_chains = []
      
      for chain in cached_chains:
        ### Case where exchangeOpt does not correspond to what is stored in cache
        ### But no way to test that exchangeSec is correct - as not been recorded (and no reason to store)
        if (isinstance(exchangeOpt, str) and chain[0] != exchangeOpt): continue

        # Ensure expirations are strings and filter out expired ones
        valid_expirations = [str(exp) for exp in chain[4] if str(exp) >= today]
        if valid_expirations:
          # Create new chain with correct data types and only valid expirations
          valid_chain = [
            str(chain[0]),              # exchange: str
            int(chain[1]),              # underlyingConId: int  
            str(chain[2]),              # tradingClass: str
            str(chain[3]),              # multiplier: str
            valid_expirations,          # expirations: List[str]
            [float(s) for s in chain[5]]  # strikes: List[float]
          ]
          valid_chains.append(valid_chain)
      
      if valid_chains:
        logger.debug(f"Using cached chains for {sym}: {len(valid_chains)} chains")
        return valid_chains

  ## Force refresh = True or no valid cached chains
  ticker_info = ticker_db.get_ticker_info(sym)

  ### If any secType,... parameter is None (no value provided) then give it a value either from DB or default value
  if ticker_info is None:
    if secType is None: secType = "STK"
    if currency is None: currency = "USD"
    if exchangeSec is None: exchangeSec = "SMART"
    if exchangeOpt is None: exchangeOpt = "SMART"
  else:
    if secType is None: secType = ticker_info.get('Type')
    if currency is None: currency = ticker_info.get('Currency')
    if exchangeSec is None: exchangeSec = ticker_info.get('Exchange')
    if exchangeOpt is None: exchangeOpt = ticker_info.get('OptExchange') 

  # Need to fetch from IB - this is where systematic caching happens
  logger.info(f"Fetching chains from IBKR for {sym} (will cache immediately)")
  
  close_connection = False
  if ib_connection is not None:
    ib = ib_connection
  else:
    ib = safe_ib_connect()
    close_connection = True
    
  if not ib.isConnected(): 
    logger.error(f"No IB connection available for {sym}")
    return None
    
  try:
    underlying = Contract(symbol=sym, secType=secType,
                       exchange=exchangeSec, currency=currency)
    
    logger.debug(f"Qualifying contract: {underlying}")
    
    if ib.qualifyContracts(underlying):
      chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
      
      # Filter chains for the specified options exchange
      sub_chains = [chain for chain in chains if chain.exchange == exchangeOpt]
      
      ib.sleep(1)
      logger.debug(f"Found {len(sub_chains)} chains for {exchangeOpt}")
      
      # Remove any chain where underlying contract Id does not match
      sub_chains = [chain for chain in sub_chains if int(chain.underlyingConId) == underlying.conId]
      
      # Convert to proper data types before caching
      formatted_chains = []
      for chain in sub_chains:
        formatted_chain = [
          str(chain.exchange),                    # str
          int(chain.underlyingConId),             # int
          str(chain.tradingClass),                # str
          str(chain.multiplier),                  # str
          [str(exp) for exp in chain.expirations],  # List[str]
          [float(strike) for strike in chain.strikes]  # List[float]
        ]
        formatted_chains.append(formatted_chain)
      
      # SYSTEMATIC CACHING: Always save immediately after fetch
      if formatted_chains:
        storage.save_chains(sym, formatted_chains)
        logger.info(f"Cached {len(formatted_chains)} chains for {sym} - available for future calls")
      
      return formatted_chains
    else:
      logger.warning(f"Could not qualify underlying contract for {sym}")
      return float('NaN')
      
  except Exception as e:
    logger.error(f"Error fetching chains for {sym}: {e}")
    return None
    
  finally:
    if close_connection:
      ib.disconnect()

def getChain(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, force_refresh=False):
    """
    Get a specific option chain - leverages systematic caching from getChains.
    
    Since getChains always caches data, this function will use cached data
    for any subsequent call in the same session or future sessions.
    
    Args:
        tradingClass: If None, returns the first available trading class for the exchange
    """

    # Get all chains - will use cache if available, fetch+cache if not
    # Provide parameters "as received as arguments" - so may be value to None
    chains = getChains(sym, secType, currency, exchangeSec, exchangeOpt, 
                      force_refresh=force_refresh)
    
    # Handle error cases - already logged at getChains, could not connect to IBKR 
    if chains is None: return None

    # Handle error cases - already logged at getChains, could not qualify 
    if isinstance(chains, float) and math.isnan(chains):
        logger.error("No chains found")
        return float('NaN')
    
    if not isinstance(chains, list):
        logger.error(f"Unexpected chains format for {sym}: {type(chains)}")
        return float('NaN')
    
        # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    ### ticker does not exist in DB
    if ticker_info is None:
        if exchangeOpt is None: exchangeOpt = "SMART"
        if tradingClass is None: tradingClass =''

    else:
        if exchangeOpt is None: exchangeOpt = ticker_info.get('OptExchange')
        if tradingClass is None: tradingClass = ticker_info.get('TradingClass')
    
    # Filter chains by exchange first (since trading classes can differ by exchange)
    exchange_chains = [chain for chain in chains if chain[0] == exchangeOpt]
    
    if not exchange_chains:
        logger.debug(f"No chains found for {sym} on exchange {exchangeOpt}")
        return float('NaN')
    
    # If no trading class specified in ticker DB, return first available for this exchange
    if tradingClass == '':
        first_chain = exchange_chains[0]
        discovered_trading_class = first_chain[2]
        logger.debug(f"Auto-discovered trading class for {sym} on {exchangeOpt}: {discovered_trading_class}")
        return first_chain
    
    # Look for specific trading class on the exchange
    for chain in exchange_chains:
        if chain[2] == tradingClass:
            logger.debug(f"Found chain for {sym} {tradingClass} on {exchangeOpt}: {len(chain[4])} expirations, {len(chain[5])} strikes")
            return chain
    
    # Trading class not found on this exchange
    available_classes = [str(chain[2]) for chain in exchange_chains]
    logger.debug(f"Trading class {tradingClass} not found for {sym} on {exchangeOpt}. Available: {available_classes}")
    return float('NaN')

def getExpirationDates(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, min_date="19700131", max_date="20991231", force_refresh=False):
  
  ## Retrieve chain corresponding to sym, tradingClass and exchange
  chain = getChain(sym, secType, currency, exchangeSec, exchangeOpt, tradingClass, force_refresh)
  
  if (isinstance(chain, list)):
    available_expirations = [exp for exp in chain[4] if int(exp) >= int(min_date) and int(exp) <= int(max_date)]
    return available_expirations
  
  return float('Nan')
  
def getOptionStrikes(sym, trading_class, expiration, strike_min=None, strike_max=None, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, force_refresh=False):
    """
    Get qualified option strikes with smart three-state caching.
    
    Three-state system:
    1. UNQUALIFIED: Strike not tested yet
    2. QUALIFIED: Strike exists for this expiration (cached as valid)
    3. OUT_OF_SCOPE: Strike does not exist for this expiration (cached as invalid)
    
    Only tests UNQUALIFIED strikes via TWS API.
    """
    # Ensure expiration is string format (YYYYMMDD)
    expiration = str(expiration)
    
    # Get ticker information from database if not provided
    ticker_info = ticker_db.get_ticker_info(sym)
    
    if ticker_info is None:
        logger.warning(f"No ticker info found for {sym}, using defaults")
        if secType is None: secType = 'STK'
        if currency is None: currency = 'USD'
        if exchangeSec is None: exchangeSec = 'SMART'
        if exchangeOpt is None: exchangeOpt = 'SMART'
    else:
        if secType is None: secType = ticker_info.get('Type')
        if currency is None: currency = ticker_info.get('Currency')
        if exchangeSec is None: exchangeSec = ticker_info.get('Exchange')
        if exchangeOpt is None: exchangeOpt = ticker_info.get('OptExchange')
    
    # Get strike bounds as floats
    strike_min_float = float(strike_min) if strike_min is not None else None
    strike_max_float = float(strike_max) if strike_max is not None else None
    
    logger.info(f"Fetching strikes for {sym} {trading_class} {expiration} range [{strike_min_float}, {strike_max_float}]")
    
    # Get the specific chain to find all theoretical strikes
    chain = getChain(sym, secType, currency, exchangeSec, exchangeOpt, trading_class, 
                    force_refresh=force_refresh)
    
    if chain is None:
        logger.error(f"Could not get chain for {sym} {trading_class}")
        return None
    
    if isinstance(chain, float) and math.isnan(chain):
        logger.error(f"Chain not available for {sym} {trading_class}")
        return None
    
    # Check if expiration exists in the chain
    # available_expirations = [str(exp) for exp in chain[4]]
    # if expiration not in available_expirations:
    #     logger.error(f"Expiration {expiration} not available for {sym} {trading_class}")
    #     logger.debug(f"Available expirations: {available_expirations}")
    #     return None
    
    # Get all theoretical strikes and filter to requested range
    all_available_strikes = [float(strike) for strike in chain[5]]
    candidate_strikes = _filter_strikes_by_range(all_available_strikes, strike_min_float, strike_max_float)
    
    if not candidate_strikes:
        logger.warning(f"No strikes in range [{strike_min_float}, {strike_max_float}] for {sym} {trading_class}")
        return []
    
    # Load cached strike states (qualified + out_of_scope)
    strikes_storage = ParquetStrikesStorage()

    # TTL check: remove expired/stale strike cache before loading
    if not force_refresh:
        stale_msg = strikes_storage.check_and_remove_stale_strikes(sym, trading_class, expiration)
        if stale_msg:
            _stale_warnings.append(stale_msg)

    if force_refresh:
        # FORCE REFRESH: Don't load cache, start fresh
        qualified_strikes = set()
        out_of_scope_strikes = set()
        logger.info(f"Force refresh enabled - bypassing strikes cache for {sym} {trading_class} {expiration}")
    else:
        # Normal cache loading
        cached_data = strikes_storage.load_strike_states(sym, trading_class, expiration)
        qualified_strikes = set(cached_data.get('qualified', []))
        out_of_scope_strikes = set(cached_data.get('out_of_scope', []))
    
    logger.debug(f"Cache loaded: {len(qualified_strikes)} qualified, {len(out_of_scope_strikes)} out of scope strikes")
    
    # Classify candidate strikes into three states
    strikes_to_test = []
    already_qualified = []
    already_out_of_scope = []
    
    for strike in candidate_strikes:
        if strike in qualified_strikes:
            already_qualified.append(strike)
            logger.debug(f"Strike {strike} already QUALIFIED (cached)")
        elif strike in out_of_scope_strikes:
            already_out_of_scope.append(strike)
            logger.debug(f"Strike {strike} already OUT_OF_SCOPE (cached)")
        else:
            strikes_to_test.append(strike)
            logger.debug(f"Strike {strike} is UNQUALIFIED (needs testing)")
    
    logger.info(f"Strike analysis: {len(already_qualified)} already qualified, {len(strikes_to_test)} need testing, {len(already_out_of_scope)} out of scope")
    
    # If we have untested strikes, test them (not if force_refresh)
    newly_qualified = []
    newly_out_of_scope = []
    
    if strikes_to_test:
        logger.info(f"Testing {len(strikes_to_test)} unqualified strikes via TWS API")
        
        ib = safe_ib_connect()
        if not ib.isConnected():
            logger.error("No IB connection available for strikes qualification")
            return sorted(already_qualified)  # Return what we know is qualified
        
        try:
            option_exchange = str(chain[0])
            test_results = _qualify_strikes_with_states(ib, sym, trading_class, expiration, 
                                                      strikes_to_test, option_exchange)
            
            newly_qualified = test_results['qualified']
            newly_out_of_scope = test_results['out_of_scope']
            
            logger.info(f"TWS API results: {len(newly_qualified)} newly qualified, {len(newly_out_of_scope)} newly out of scope")
            
        except Exception as e:
            logger.error(f"Error testing strikes: {e}")
        finally:
            ib.disconnect()
    elif not strikes_to_test:
        logger.info("No unqualified strikes to test - all strikes have known states")
    else:
        logger.info("Force refresh enabled - skipping strike testing")
    
    # Update cached data with new results
    if newly_qualified or newly_out_of_scope:
        all_qualified = list(qualified_strikes) + newly_qualified
        all_out_of_scope = list(out_of_scope_strikes) + newly_out_of_scope
        
        strikes_storage.save_strike_states(sym, trading_class, expiration, 
                                         all_qualified, all_out_of_scope)
        
        logger.info(f"Updated cache: {len(all_qualified)} total qualified, {len(all_out_of_scope)} total out of scope")
    
    # Return all qualified strikes within the requested range
    final_qualified = sorted(already_qualified + newly_qualified)
    logger.info(f"Returning {len(final_qualified)} qualified strikes for range [{strike_min_float}, {strike_max_float}]")
    
    return final_qualified

def _qualify_strikes_with_states(ib, sym, trading_class, expiration, strikes, option_exchange):
    """
    Qualify strikes and return both qualified and out-of-scope results.
    
    Args:
        ib: IB connection
        sym: Symbol
        trading_class: Trading class
        expiration: Expiration date
        strikes: List of strikes to test
        option_exchange: Options exchange
        
    Returns:
        dict: {'qualified': [strikes], 'out_of_scope': [strikes]}
    """
    qualified_strikes = []
    out_of_scope_strikes = []
    batch_size = 10  # If necessary, to be reduced to 5 to avoid TWS batch truncation
    
    for i in range(0, len(strikes), batch_size):
        batch_strikes = strikes[i:i + batch_size]
        
        # Create contracts for this batch
        batch_contracts = []
        for strike in batch_strikes:
            call_option = Option(
                symbol=str(sym),
                lastTradeDateOrContractMonth=str(expiration),
                strike=float(strike),
                right='C',
                exchange=str(option_exchange),
                tradingClass=str(trading_class)
            )
            batch_contracts.append((float(strike), call_option))
        
        # Test this batch and categorize results
        batch_results = _test_contract_batch_with_states(ib, batch_contracts)
        qualified_strikes.extend(batch_results['qualified'])
        out_of_scope_strikes.extend(batch_results['out_of_scope'])
        
        ib.sleep(0.1)  # To be modified if necessary (e.g 0.2)
    
    return {
        'qualified': qualified_strikes,
        'out_of_scope': out_of_scope_strikes
    }

def _test_contract_batch_with_states(ib, batch_contracts):
    """
    Test a batch of contracts and return qualified vs out-of-scope strikes.
    
    Args:
        ib: IB connection
        batch_contracts: List of (strike, contract) tuples
        
    Returns:
        dict: {'qualified': [strikes], 'out_of_scope': [strikes]}
    """
    if not batch_contracts:
        return {'qualified': [], 'out_of_scope': []}
    
    qualified_strikes = []
    out_of_scope_strikes = []
    contracts_to_qualify = [contract for _, contract in batch_contracts]
    
    try:
        # Try batch qualification first with error suppression
        with suppress_ib_errors():
            qualified_contracts = ib.qualifyContracts(*contracts_to_qualify)
        
        # Create lookup by strike for accurate matching (not index-based)
        qualified_by_strike = {}
        for qc in qualified_contracts:
            if hasattr(qc, 'strike') and hasattr(qc, 'conId') and qc.conId != 0:
                qualified_by_strike[float(qc.strike)] = qc
        
        # Check each original contract against qualified results
        for strike, original_contract in batch_contracts:
            if float(strike) in qualified_by_strike:
                qualified_strikes.append(float(strike))
                logger.debug(f"Strike {strike}: QUALIFIED (batch)")
            else:
                # Don't mark as out_of_scope if just missing from batch response
                # Only mark as out_of_scope if explicitly failed qualification
                logger.debug(f"Strike {strike}: MISSING from batch response (will retry later)")
                # Note: Missing strikes are not added to out_of_scope_strikes
        
        # Only warn about truncation at debug level to avoid clutter
        if len(qualified_contracts) < len(batch_contracts):
            logger.debug(f"TWS returned {len(qualified_contracts)} results for {len(batch_contracts)} requests")
                
    except Exception as e:
        logger.debug(f"Batch qualification failed: {e}. Falling back to individual tests...")
        
        # Fallback to individual contract qualification with error suppression
        for strike, contract in batch_contracts:
            try:
                with suppress_ib_errors():
                    qualified = ib.qualifyContracts(contract)
                if (qualified and len(qualified) > 0 and 
                    hasattr(qualified[0], 'conId') and qualified[0].conId != 0):
                    qualified_strikes.append(float(strike))
                    logger.debug(f"Strike {strike}: QUALIFIED (individual fallback)")
                else:
                    out_of_scope_strikes.append(float(strike))
                    logger.debug(f"Strike {strike}: OUT_OF_SCOPE (individual fallback)")
                    
            except Exception:
                out_of_scope_strikes.append(float(strike))
                logger.debug(f"Strike {strike}: OUT_OF_SCOPE (individual exception)")
                continue
            
            ib.sleep(0.05)  # Rate limiting for individual calls
    
    return {
        'qualified': qualified_strikes,
        'out_of_scope': out_of_scope_strikes
    }
def _filter_strikes_by_range(all_strikes, strike_min, strike_max):
    """Filter strikes by range constraints with type-safe float operations."""
    if not all_strikes:
        return []
    
    candidate_strikes = []
    for strike in all_strikes:
        strike_float = float(strike)  # Ensure float comparison
        if strike_min is not None and strike_float < float(strike_min):
            continue
        if strike_max is not None and strike_float > float(strike_max):
            continue
        candidate_strikes.append(strike_float)
    
    return candidate_strikes


# Batch initialization function for systematic setup
def initialize_chains_for_symbols(symbols, force_refresh=False, ib_connection=None):
    """
    Initialize chain data for multiple symbols systematically.
    Perfect for initial system setup - fetches and caches all chains at once.
    
    Args:
        symbols (list): List of symbols to initialize
        force_refresh (bool): Whether to refresh even if cache exists
        ib_connection: Shared IB connection for efficiency
        
    Returns:
        dict: Results summary {symbol: status}
    """
    close_connection = False
    if ib_connection is not None:
        ib = ib_connection
    else:
        ib = safe_ib_connect()
        close_connection = True
    
    if not ib.isConnected():
        logger.error("No IB connection for batch initialization")
        return {}
    
    try:
        results = {}
        
        for symbol in symbols:
            try:
                logger.info(f"Initializing chains for {symbol}...")
                
                chains = getChains(symbol, ib_connection=ib, force_refresh=force_refresh)
                
                if chains is None:
                    results[symbol] = "connection_error"
                elif isinstance(chains, float) and math.isnan(chains):
                    results[symbol] = "not_found"
                elif isinstance(chains, list):
                    results[symbol] = f"success_{len(chains)}_chains"
                    logger.info(f"Initialized {symbol}: {len(chains)} chains cached")
                else:
                    results[symbol] = "unexpected_format"
                
                ib.sleep(0.5)  # Rate limiting between symbols
                
            except Exception as e:
                results[symbol] = f"error_{str(e)}"
                logger.error(f"Error initializing {symbol}: {e}")
                continue
        
        logger.info(f"Batch initialization complete: {len([s for s in results.values() if s.startswith('success')])} successful")
        return results
        
    finally:
        if close_connection:
            ib.disconnect()

# Discovery and utility functions for multi-trading-class support
def getAvailableTradingClasses(sym, exchangeOpt=None, force_refresh=False):
    """
    Get all available trading classes for a symbol, optionally filtered by exchange.
    
    Args:
        sym (str): Symbol
        exchangeOpt (str, optional): Filter by specific options exchange
        force_refresh (bool): Whether to refresh chain data
        
    Returns:
        dict: {exchange: [trading_classes]} or [trading_classes] if exchange specified
    """
    chains = getChains(sym, force_refresh=force_refresh)
    
    if not chains or not isinstance(chains, list):
        return {} if exchangeOpt is None else []
    
    if exchangeOpt is not None:
        # Return trading classes for specific exchange
        exchange_classes = [str(chain[2]) for chain in chains if str(chain[0]) == str(exchangeOpt)]
        return sorted(list(set(exchange_classes)))
    
    # Return all trading classes grouped by exchange
    exchange_classes = {}
    for chain in chains:
        exchange = str(chain[0])
        trading_class = str(chain[2])
        
        if exchange not in exchange_classes:
            exchange_classes[exchange] = []
        
        if trading_class not in exchange_classes[exchange]:
            exchange_classes[exchange].append(trading_class)
    
    # Sort trading classes within each exchange
    for exchange in exchange_classes:
        exchange_classes[exchange] = sorted(exchange_classes[exchange])
    
    return exchange_classes

def getAllChains(sym, exchangeOpt=None, force_refresh=False):
    """
    Get all chains for a symbol, optionally filtered by exchange.
    Returns detailed information about each trading class.
    
    Args:
        sym (str): Symbol
        exchangeOpt (str, optional): Filter by specific options exchange
        force_refresh (bool): Whether to refresh chain data
        
    Returns:
        list: List of chain dictionaries with metadata
    """
    chains = getChains(sym, force_refresh=force_refresh)
    
    if not chains or not isinstance(chains, list):
        return []
    
    # Filter by exchange if specified
    if exchangeOpt is not None:
        chains = [chain for chain in chains if str(chain[0]) == str(exchangeOpt)]
    
    # Convert to more readable format
    chain_info = []
    for chain in chains:
        chain_dict = {
            'symbol': sym,
            'exchange': str(chain[0]),
            'underlying_contract_id': int(chain[1]),
            'trading_class': str(chain[2]),
            'multiplier': str(chain[3]),
            'expirations': [str(exp) for exp in chain[4]],
            'strikes': [float(strike) for strike in chain[5]],
            'expiration_count': len(chain[4]),
            'strike_count': len(chain[5]),
            'strike_range': {
                'min': float(min(chain[5])) if chain[5] else None,
                'max': float(max(chain[5])) if chain[5] else None
            }
        }
        chain_info.append(chain_dict)
    
    # Sort by exchange and trading class
    return sorted(chain_info, key=lambda x: (x['exchange'], x['trading_class']))

def findBestTradingClass(sym, exchangeOpt=None, criteria='most_strikes', force_refresh=False):
    """
    Find the "best" trading class for a symbol based on specified criteria.
    
    Args:
        sym (str): Symbol
        exchangeOpt (str, optional): Limit to specific exchange
        criteria (str): Selection criteria:
            - 'most_strikes': Trading class with most available strikes
            - 'most_expirations': Trading class with most expirations  
            - 'alphabetical': First alphabetically
            - 'shortest_name': Shortest trading class name
        force_refresh (bool): Whether to refresh chain data
        
    Returns:
        dict or None: Best trading class info or None if not found
    """
    all_chains = getAllChains(sym, exchangeOpt, force_refresh)
    
    if not all_chains:
        return None
    
    if criteria == 'most_strikes':
        best_chain = max(all_chains, key=lambda x: x['strike_count'])
    elif criteria == 'most_expirations':
        best_chain = max(all_chains, key=lambda x: x['expiration_count'])
    elif criteria == 'alphabetical':
        best_chain = min(all_chains, key=lambda x: x['trading_class'])
    elif criteria == 'shortest_name':
        best_chain = min(all_chains, key=lambda x: len(x['trading_class']))
    else:
        logger.warning(f"Unknown criteria '{criteria}', using 'most_strikes'")
        best_chain = max(all_chains, key=lambda x: x['strike_count'])
    
    logger.debug(f"Selected trading class {best_chain['trading_class']} for {sym} based on '{criteria}' criteria")
    return best_chain

def compareTradingClasses(sym, exchangeOpt=None, force_refresh=False):
    """
    Compare all available trading classes for a symbol.
    Useful for understanding differences between trading classes.
    
    Returns:
        pandas.DataFrame or dict: Comparison table of trading classes
    """
    all_chains = getAllChains(sym, exchangeOpt, force_refresh)
    
    if not all_chains:
        return None
    
    # Create comparison data
    comparison_data = []
    for chain in all_chains:
        row = {
            'Symbol': chain['symbol'],
            'Exchange': chain['exchange'],
            'Trading_Class': chain['trading_class'],
            'Multiplier': chain['multiplier'],
            'Expirations': chain['expiration_count'],
            'Strikes': chain['strike_count'],
            'Strike_Min': chain['strike_range']['min'],
            'Strike_Max': chain['strike_range']['max'],
            'Underlying_ID': chain['underlying_contract_id']
        }
        comparison_data.append(row)
    
    try:
        df = pd.DataFrame(comparison_data)
        return df
    except ImportError:
        # Return as list of dicts if pandas not available
        return comparison_data

# Enhanced convenience functions for trading class discovery
def exploreSymbol(sym, force_refresh=False):
    """
    Comprehensive exploration of a symbol's option chains.
    Perfect for initial discovery of available trading classes and their characteristics.
    
    Returns:
        dict: Complete overview of symbol's option structure
    """
    logger.info(f"Exploring option chains for {sym}...")
    
    # Get all chains
    all_chains = getAllChains(sym, force_refresh=force_refresh)
    
    if not all_chains:
        return {
            'symbol': sym,
            'status': 'no_chains_found',
            'trading_classes': {},
            'exchanges': [],
            'summary': 'No option chains available'
        }
    
    # Group by exchange
    exchanges_data = {}
    for chain in all_chains:
        exchange = chain['exchange']
        if exchange not in exchanges_data:
            exchanges_data[exchange] = []
        exchanges_data[exchange].append(chain)
    
    # Create summary
    total_expirations = sum(chain['expiration_count'] for chain in all_chains)
    total_strikes = sum(chain['strike_count'] for chain in all_chains)
    unique_trading_classes = len(set(chain['trading_class'] for chain in all_chains))
    
    # Find recommended trading class
    recommended = findBestTradingClass(sym, criteria='most_strikes', force_refresh=False)
    
    exploration_result = {
        'symbol': sym,
        'status': 'success',
        'summary': {
            'total_chains': len(all_chains),
            'unique_trading_classes': unique_trading_classes,
            'exchanges': list(exchanges_data.keys()),
            'total_expirations': total_expirations,
            'total_strikes': total_strikes
        },
        'recommended_trading_class': {
            'trading_class': recommended['trading_class'] if recommended else None,
            'exchange': recommended['exchange'] if recommended else None,
            'reason': 'most_strikes'
        },
        'exchanges': exchanges_data,
        'all_chains': all_chains
    }
    
    logger.info(f"Exploration complete for {sym}: {unique_trading_classes} trading classes across {len(exchanges_data)} exchanges")
    return exploration_result

def printSymbolSummary(sym, force_refresh=False):
    """Print a human-readable summary of symbol's option chains."""
    exploration = exploreSymbol(sym, force_refresh)
    
    if exploration['status'] != 'success':
        print(f"❌ {sym}: {exploration['summary']}")
        return
    
    summary = exploration['summary']
    recommended = exploration['recommended_trading_class']
    
    print(f"\n📊 Option Chains Summary for {sym}")
    print(f"{'='*50}")
    print(f"Trading Classes: {summary['unique_trading_classes']}")
    print(f"Exchanges: {', '.join(summary['exchanges'])}")
    print(f"Total Expirations: {summary['total_expirations']:,}")
    print(f"Total Strikes: {summary['total_strikes']:,}")
    
    if recommended['trading_class']:
        print(f"\n💡 Recommended: {recommended['trading_class']} on {recommended['exchange']} ({recommended['reason']})")
    
    print(f"\n📋 Detailed Breakdown:")
    for exchange, chains in exploration['exchanges'].items():
        print(f"\n  Exchange: {exchange}")
        for chain in chains:
            tc = chain['trading_class']
            exp_count = chain['expiration_count'] 
            strike_count = chain['strike_count']
            strike_range = chain['strike_range']
            
            print(f"    {tc}: {exp_count} expirations, {strike_count} strikes "
                  f"(${strike_range['min']:.1f}-${strike_range['max']:.1f})")
    
    print()  # Final newline

# Convenience functions with multi-trading-class awareness
def getAllStrikes(sym, trading_class=None, expiration=None, exchangeOpt=None, force_refresh=False):
    """
    Get all available strikes for an expiration - uses systematic caching.
    If trading_class not specified, uses the best available trading class.
    """
    if trading_class is None:
        best_chain = findBestTradingClass(sym, exchangeOpt, 'most_strikes', force_refresh)
        if not best_chain:
            logger.error(f"No trading classes found for {sym}")
            return None
        trading_class = best_chain['trading_class']
        exchangeOpt = best_chain['exchange']
        
        if expiration is None:
            # Return all strikes across all expirations
            return best_chain['strikes']
    
    if expiration is None:
        logger.error("Provide specific expiration")
        return None
        
    return getOptionStrikes(sym, trading_class, expiration, exchangeOpt=exchangeOpt, force_refresh=force_refresh)

def getStrikesAuto(sym, expiration=None, center_strike=None, range_pct=0.1, exchangeOpt=None, criteria='most_strikes'):
    """
    Auto-selecting version that finds best trading class and gets strikes.
    Perfect for R users who want simplicity.
    
    Args:
        sym (str): Symbol (e.g., 'SPY')
        expiration (str, optional): Specific expiration (YYYYMMDD). If None, returns all strikes.
        center_strike (float, optional): Center strike for range. If None, returns all strikes.
        range_pct (float): Percentage range around center strike (default 10%)
        exchangeOpt (str, optional): Options exchange filter
        criteria (str): Criteria for trading class selection ('most_strikes', 'most_expirations', etc.)
    
    Returns:
        list or dict: Strikes list or comprehensive data if no expiration specified
    """
    try:
        if expiration is None:
            # Return comprehensive data for exploration
            return exploreSymbol(sym)
        
        if center_strike is None:
            # Return all strikes for expiration using best trading class
            return getAllStrikes(sym, expiration=expiration, exchangeOpt=exchangeOpt)
        
        # Return strikes in range using best trading class
        return getStrikesInRange(sym, expiration=expiration, center_strike=center_strike, 
                                range_pct=range_pct, exchangeOpt=exchangeOpt)
    
    except Exception as e:
        package_logger.error(f"Error in getStrikesAuto for {sym}: {e}")
        return None

def getStrikesInRange(sym, trading_class=None, expiration=None, center_strike=None, range_pct=0.1, exchangeOpt=None, force_refresh=False):
    """
    Get strikes within percentage range of center strike - uses systematic caching.
    Auto-selects best trading class if not specified.
    """
    if trading_class is None:
        best_chain = findBestTradingClass(sym, exchangeOpt, 'most_strikes', force_refresh)
        if not best_chain:
            logger.error(f"No trading classes found for {sym}")
            return None
        trading_class = best_chain['trading_class']
        exchangeOpt = best_chain['exchange']

    center_strike_float = float(center_strike)
    strike_range = center_strike_float * float(range_pct)
    strike_min = center_strike_float - strike_range
    strike_max = center_strike_float + strike_range
    return getOptionStrikes(sym, trading_class, expiration, strike_min, strike_max, exchangeOpt=exchangeOpt, force_refresh=force_refresh)

def discoverSymbol(sym, format='summary'):
    """
    R-friendly symbol discovery function.
    
    Args:
        sym (str): Symbol to explore
        format (str): Return format ('summary', 'detailed', 'comparison')
    
    Returns:
        dict or list: Symbol information in requested format
    """
    try:
        if format == 'summary':
            exploration = exploreSymbol(sym)
            if exploration['status'] == 'success':
                # Extract trading classes from all_chains instead of using exchanges dict
                trading_classes = []
                exchanges = []
                
                if 'all_chains' in exploration and exploration['all_chains']:
                    for chain in exploration['all_chains']:
                        trading_classes.append(chain['trading_class'])
                        if chain['exchange'] not in exchanges:
                            exchanges.append(chain['exchange'])
                
                return {
                    'symbol': sym,
                    'trading_classes': list(set(trading_classes)),  # Remove duplicates
                    'total_chains': exploration['summary']['total_chains'],
                    'recommended': exploration['recommended_trading_class'],
                    'exchanges': exchanges
                }
            else:
                return {'symbol': sym, 'error': exploration['summary']}
        
        elif format == 'detailed':
            return getAllChains(sym)
        
        elif format == 'comparison':
            return compareTradingClasses(sym)
        
        else:
            package_logger.warning(f"Unknown format '{format}', using 'summary'")
            return discoverSymbol(sym, 'summary')
    
    except Exception as e:
        package_logger.error(f"Error discovering {sym}: {e}")
        return {'symbol': sym, 'error': str(e)}

def initializeMultipleSymbols(symbols, force_refresh=False):
    """
    R-friendly batch initialization.
    
    Args:
        symbols (list): List of symbols to initialize
        force_refresh (bool): Whether to force refresh
    
    Returns:
        dict: Results summary
    """
    try:
        if isinstance(symbols, str):
            symbols = [symbols]  # Handle single symbol
        
        return initialize_chains_for_symbols(symbols, force_refresh)
    except Exception as e:
        package_logger.error(f"Error in batch initialization: {e}")
        return {'error': str(e)}

def exploreMultipleSymbols(symbols):
    """
    Explore multiple symbols and return combined summary.

    Args:
        symbols (list): List of symbols to explore

    Returns:
        dict: Combined exploration results
    """
    try:
        if isinstance(symbols, str):
            symbols = [symbols]

        results = {}
        for symbol in symbols:
            results[symbol] = discoverSymbol(symbol, 'summary')

        return results
    except Exception as e:
        package_logger.error(f"Error exploring multiple symbols: {e}")
        return {'error': str(e)}


def get_chain_oi(sym, expiration, strike_min=None, strike_max=None,
                 trading_class=None, ib_connection=None,
                 wait_seconds=2.0, batch_size=40):
    """
    Walk an option chain at a single expiry and return per-strike Open Interest.

    For each qualified strike in [strike_min, strike_max], requests both call and
    put market data with genericTickList='101, 105' (OpenInterest + avOptionVolume)
    and returns a DataFrame.

    Used by swing_scanner Step D.3 (chain positioning, per-strike OI walk).

    Args:
        sym (str): Underlying symbol
        expiration (str): YYYYMMDD
        strike_min (float, optional): Lower strike bound. None = no lower bound.
        strike_max (float, optional): Upper strike bound. None = no upper bound.
        trading_class (str, optional): If None, uses ticker_db default.
        ib_connection: Existing IB connection to reuse. If None, opens a new one.
        wait_seconds (float): Seconds to wait after issuing reqMktData calls
                              before reading values. Default 2.0.
        batch_size (int): Number of (call, put) pairs to subscribe simultaneously
                          before draining and unsubscribing. TWS market-data line
                          limit is ~100 by default. Default 40 (80 lines).

    Returns:
        pandas.DataFrame with columns:
            strike, right ('C' or 'P'), open_interest, avg_volume, qualified (bool)
        Empty DataFrame if no strikes resolved.
        None on connection error.
    """
    expiration = str(expiration)

    ticker_info = ticker_db.get_ticker_info(sym)
    if ticker_info is None:
        logger.warning(f"No ticker info for {sym}, using STK/USD/SMART defaults")
        secType = 'STK'; currency = 'USD'
        exchangeSec = 'SMART'; exchangeOpt = 'SMART'
    else:
        secType    = ticker_info.get('Type')    or 'STK'
        currency   = ticker_info.get('Currency') or 'USD'
        exchangeSec = ticker_info.get('Exchange') or 'SMART'
        exchangeOpt = ticker_info.get('OptExchange') or 'SMART'

    if trading_class is None:
        trading_class = (ticker_info or {}).get('TradingClass') or sym

    qualified = getOptionStrikes(
        sym, trading_class, expiration,
        strike_min=strike_min, strike_max=strike_max,
        secType=secType, currency=currency,
        exchangeSec=exchangeSec, exchangeOpt=exchangeOpt,
        force_refresh=False)
    if qualified is None or len(qualified) == 0:
        logger.warning(f"No qualified strikes for {sym} {trading_class} {expiration}"
                       f" in [{strike_min}, {strike_max}]")
        return pd.DataFrame(columns=['strike', 'right', 'open_interest',
                                     'avg_volume', 'qualified'])

    own_connection = ib_connection is None
    ib = ib_connection or safe_ib_connect()
    if ib is None:
        logger.error(f"Could not establish IB connection for {sym}")
        return None

    try:
        ib.reqMarketDataType(1)  # live + frozen fallback after hours

        rows = []
        # Process strikes in batches to stay under TWS market-data line limit.
        for batch_start in range(0, len(qualified), batch_size):
            batch = qualified[batch_start:batch_start + batch_size]
            contracts = []
            tickers = []
            for strike in batch:
                call = Option(sym, expiration, float(strike), 'C',
                              exchangeOpt, currency=currency,
                              tradingClass=trading_class)
                put  = Option(sym, expiration, float(strike), 'P',
                              exchangeOpt, currency=currency,
                              tradingClass=trading_class)
                contracts.extend([call, put])

            with suppress_ib_errors():
                ib.qualifyContracts(*contracts)

            for c in contracts:
                qualified_ok = c.conId != 0
                if qualified_ok:
                    t = ib.reqMktData(c, genericTickList='101, 105')
                    tickers.append((c, t, True))
                else:
                    tickers.append((c, None, False))

            ib.sleep(wait_seconds)

            for c, t, qok in tickers:
                if not qok:
                    rows.append({'strike': float(c.strike), 'right': c.right,
                                 'open_interest': float('nan'),
                                 'avg_volume': float('nan'),
                                 'qualified': False})
                    continue
                if c.right == 'C':
                    oi = getattr(t, 'callOpenInterest', float('nan'))
                else:
                    oi = getattr(t, 'putOpenInterest', float('nan'))
                avg_vol = getattr(t, 'avOptionVolume', float('nan'))
                rows.append({
                    'strike': float(c.strike), 'right': c.right,
                    'open_interest': (None if (oi is None or
                                       (isinstance(oi, float) and math.isnan(oi)))
                                      else int(oi)),
                    'avg_volume': (None if (avg_vol is None or
                                    (isinstance(avg_vol, float) and math.isnan(avg_vol)))
                                   else int(avg_vol)),
                    'qualified': True,
                })
                ib.cancelMktData(c)

        df = pd.DataFrame(rows)
        if not df.empty:
            df = df.sort_values(['strike', 'right']).reset_index(drop=True)
        return df
    finally:
        if own_connection:
            ib.disconnect()
