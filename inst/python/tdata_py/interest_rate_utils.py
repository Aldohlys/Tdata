import math
import numpy as np
import datetime
from dateutil.relativedelta import relativedelta
from ib_async import *
from tdata_py.IB_connection import safe_ib_connect
from tdata_py.contract import getValue
import calendar
import pandas as pd


# Constants for dividend yields and bounds
DIVIDEND_YIELDS = {
    'ESTX50': 0.025,  # ESTX50: ~2.5%
    'SPX': 0.015,     # S&P 500: ~1.5%
    'SMI': 0.028      # Swiss Market Index: ~2.8%
}

INTEREST_RATE_BOUNDS = {
    'min': -0.05,  # Lower bound: -5%
    'max': 0.10    # Upper bound: 10%
}

DEFAULT_RATE = 0.02  # Default interest rate when calculation fails

def create_future_contracts(symbol):
    """
    Create a list of future contracts for the given symbol
    
    Args:
        symbol: either "ESTX50", "SPX", or "SMI"
        
    Returns:
        list: List of IB Contract objects
    """
    if symbol == 'ESTX50':
        return [
            Contract(secType='FUT', symbol='ESTX50', exchange='EUREX', currency='EUR', 
                     lastTradeDateOrContractMonth='202506', tradingClass='FESX'),
            Contract(secType='FUT', symbol='ESTX50', exchange='EUREX', currency='EUR', 
                     lastTradeDateOrContractMonth='202509', tradingClass='FESX'),
            Contract(secType='FUT', symbol='ESTX50', exchange='EUREX', currency='EUR', 
                     lastTradeDateOrContractMonth='202512', tradingClass='FESX'),
            Contract(secType='FUT', symbol='ESTX50', exchange='EUREX', currency='EUR', 
                     lastTradeDateOrContractMonth='202603', tradingClass='FESX')
        ]
    elif symbol == 'SPX':
        return [
            Contract(secType='FUT', symbol='ES', exchange='CME', currency='USD', lastTradeDateOrContractMonth='202506'),
            Contract(secType='FUT', symbol='ES', exchange='CME', currency='USD', lastTradeDateOrContractMonth='202509'),
            Contract(secType='FUT', symbol='ES', exchange='CME', currency='USD', lastTradeDateOrContractMonth='202512')
        ]
    elif symbol == 'SMI':
        return [
            Contract(secType='FUT', symbol='SMI', exchange='EUREX', currency='CHF', 
                     lastTradeDateOrContractMonth='202506', tradingClass='FSMI'),
            Contract(secType='FUT', symbol='SMI', exchange='EUREX', currency='CHF', 
                     lastTradeDateOrContractMonth='202509', tradingClass='FSMI'),
            Contract(secType='FUT', symbol='SMI', exchange='EUREX', currency='CHF', 
                     lastTradeDateOrContractMonth='202512', tradingClass='FSMI'),
            Contract(secType='FUT', symbol='SMI', exchange='EUREX', currency='CHF', 
                     lastTradeDateOrContractMonth='202603', tradingClass='FSMI')
        ]    
    else:
        raise ValueError(f"Unsupported symbol: {symbol}. Only 'ESTX50', 'SMI' and 'SPX' are supported.")

def parse_contract_expiry(expiry_str):
    """
    Parse the expiry date string from a contract
    
    Args:
        expiry_str: Expiry date string in format YYYYMM or YYYYMMDD
        
    Returns:
        datetime.date: Contract expiry date
    """
    if len(expiry_str) == 6:  # YYYYMM format
        year = int(expiry_str[:4])
        month = int(expiry_str[4:6])
        last_day = calendar.monthrange(year, month)[1]
        return datetime.date(year, month, last_day)
    else:  # YYYYMMDD format
        return datetime.datetime.strptime(expiry_str, '%Y%m%d').date()

def get_spot_price(ib, symbol):
    """
    Get the spot price for the given contract
    
    Args:
        ib: IB connection object
        symbol: either "ESTX50" or "SPX"
        
    Returns:
        float: Spot price
    """
    price_record = getValue(list_sym=symbol, ib=ib)
    
    if isinstance(price_record, pd.DataFrame): return(float(price_record.price.iloc[0]))
    print(f"Error getting spot price for {symbol}: {price_record}")
    return None

def find_best_future_contract(ib, future_contracts, target_expiry):
    """
    Find the future contract closest to the target expiry date
    
    Args:
        ib: IB connection object
        future_contracts: List of IB Contract objects
        target_expiry: Target expiry date (datetime.date or datetime.datetime)
        
    Returns:
        tuple: (best_future, future_expiry)
    """
    min_days_diff = float('inf')
    best_future = None
    future_expiry = None

    # Convert target_expiry to date object if it's a datetime
    if isinstance(target_expiry, datetime.datetime):
        target_expiry = target_expiry.date()
        
    for future_contract in future_contracts:
        try:
            qualified_contracts = ib.qualifyContracts(future_contract)
            
            if qualified_contracts:
                qualified_contract = qualified_contracts[0]
                
                # Get the expiry date
                if hasattr(qualified_contract, 'lastTradeDateOrContractMonth'):
                    future_expiry_str = qualified_contract.lastTradeDateOrContractMonth
                    contract_expiry = parse_contract_expiry(future_expiry_str)
                    
                    days_diff = abs((contract_expiry - target_expiry).days)
                    
                    if days_diff < min_days_diff:
                        min_days_diff = days_diff
                        best_future = qualified_contract
                        future_expiry = contract_expiry
                        print(f"Found future contract: {best_future.localSymbol}, expiry: {future_expiry}")
        except Exception as e:
            print(f"Error with future contract {future_contract.lastTradeDateOrContractMonth}: {e}")
    
    return best_future, future_expiry

def get_future_price(ib, future_contract):
    """
    Get the price for the given future contract
    
    Args:
        ib: IB connection object
        future_contract: IB Contract object
        
    Returns:
        float: Future price
    """
    try:
        ticker = ib.reqTickers(future_contract)
        ib.sleep(1)
        
        future_price = ticker.marketPrice()
        return future_price
    except Exception as e:
        print(f"Error getting future price: {e}")
        return None

def calculate_implied_rate(spot_price, future_price, time_to_expiry, dividend_yield):
    """
    Calculate the implied interest rate
    
    Args:
        spot_price: Current spot price
        future_price: Future price
        time_to_expiry: Time to expiry in years
        dividend_yield: Dividend yield
        
    Returns:
        float: Implied interest rate
    """
    try:
        print(f"Spot price: {spot_price}")
        print(f"Future price: {future_price}")
        print(f"Ratio future/spot: {future_price/spot_price}")
        print(f"Log of ratio: {math.log(future_price/spot_price)}")
        print(f"Time component: {math.log(future_price/spot_price)/time_to_expiry}")
        implied_rate = (math.log(future_price / spot_price) / time_to_expiry) + dividend_yield
        
        # Constrain the result to reasonable bounds
        if implied_rate < INTEREST_RATE_BOUNDS['min']:
            print(f"WARNING: Calculated rate {implied_rate*100:.2f}% is very low, using floor of {INTEREST_RATE_BOUNDS['min']*100}%")
            implied_rate = INTEREST_RATE_BOUNDS['min']
        elif implied_rate > INTEREST_RATE_BOUNDS['max']:
            print(f"WARNING: Calculated rate {implied_rate*100:.2f}% is very high, using ceiling of {INTEREST_RATE_BOUNDS['max']*100}%")
            implied_rate = INTEREST_RATE_BOUNDS['max']
            
        return implied_rate
    except Exception as e:
        print(f"Error calculating implied rate: {e}")
        return DEFAULT_RATE

def get_interest_rate_from_futures(ib, symbol, expiry_date):
    """
    Calculate implied interest rate from futures prices
    
    Args:
        ib: IB connection object
        symbol: Index symbol ('ESTX50' or 'SPX' only)
        expiry_date: Option expiry date (datetime.date object)
        
    Returns:
        float: Annualized implied interest rate
    """
    # Validate input symbol
    if symbol not in list(DIVIDEND_YIELDS.keys()):
        print(f"Error: Only {', '.join(DIVIDEND_YIELDS.keys())} symbols are supported")
        return DEFAULT_RATE
    
    # Get dividend yield for this symbol
    dividend_yield = DIVIDEND_YIELDS[symbol]
    
    # Get current index price
    try:
        spot_price = get_spot_price(ib, symbol)
        if spot_price is None:
            return DEFAULT_RATE
    except Exception as e:
        print(f"Error setting up index contract: {e}")
        return DEFAULT_RATE
    
    # Define futures contracts based on symbol
    try:
        future_contracts = create_future_contracts(symbol)
    except Exception as e:
        print(f"Error creating futures contracts: {e}")
        return DEFAULT_RATE
    
    # Find the best future contract for our expiry date
    today = datetime.datetime.now().date()
    best_future, future_expiry = find_best_future_contract(ib, future_contracts, expiry_date)
    
    if best_future is None:
        print("Could not find appropriate futures contract, using default rate")
        return DEFAULT_RATE
    
    # Get futures price
    future_price = get_future_price(ib, best_future)
    if future_price is None:
        return DEFAULT_RATE
    
    # Calculate time to expiry
    time_to_expiry = (future_expiry - today).days / 365.0
    
    # Calculate implied interest rate
    implied_rate = calculate_implied_rate(spot_price, future_price, time_to_expiry, dividend_yield)
    
    # Print results
    print(f"Spot price: {spot_price}")
    print(f"Ratio future/spot: {future_price/spot_price}")
    print(f"Log component: {math.log(future_price/spot_price)/time_to_expiry}")
    print(f"Future contract: {best_future.localSymbol}")
    print(f"Future expiry: {future_expiry}")
    print(f"Future price: {future_price}")
    print(f"Time to expiry: {time_to_expiry:.4f} years")
    print(f"Dividend yield: {dividend_yield*100:.2f}%")
    print(f"Implied interest rate: {implied_rate*100:.2f}%")
    
    return implied_rate

def getInterestRate(months, currency):
      # Connect to Interactive Brokers
    ib = safe_ib_connect()
    
    # Return 0.0425 if connection failed
    if not ib.isConnected():
        return 0.0425

    today = datetime.datetime.now()
    future_date = today + relativedelta(months=months)
    rate = float('nan')
    
    ### Debug
    print("future_date:", future_date,"  currency:", currency)
    
    if (currency == "USD"): rate = get_interest_rate_from_futures(ib, "SPX", future_date)
    elif (currency == "EUR"): rate = get_interest_rate_from_futures(ib, "ESTX50", future_date)
    elif (currency == "CHF"): rate = get_interest_rate_from_futures(ib, "SMI", future_date)
    
    ### Close properly ib
    ib.disconnect()
    
    return rate

def get_interest_rate_for_expiry(ib, expiry_date):
    """
    Get risk-free interest rate using Treasury securities with maturity close to expiry date

    Args:
        ib: IB connection object
        expiry_date: The option expiry date

    Returns:
        float: The interest rate as a decimal (e.g., 0.035 for 3.5%)
    """
    today = datetime.now().date()
    days_to_expiry = (expiry_date - today).days

    # Choose the appropriate Treasury based on time to expiry
    if days_to_expiry <= 90:
        bond_symbol = "13061"  # 3-month T-Bill CUSIP prefix
    elif days_to_expiry <= 180:
        bond_symbol = "13062"  # 6-month T-Bill CUSIP prefix
    elif days_to_expiry <= 365:
        bond_symbol = "13063"  # 1-year T-Bill CUSIP prefix
    elif days_to_expiry <= 730:
        bond_symbol = "912828"  # 2-year T-Note CUSIP prefix
    elif days_to_expiry <= 1825:
        bond_symbol = "912828"  # 5-year T-Note CUSIP prefix
    else:
        bond_symbol = "912828"  # 10-year T-Note CUSIP prefix

    bond_exchange = "SMART"

    # Create a bond contract
    bond_contract = Contract(symbol=bond_symbol, secType='BOND', exchange=bond_exchange, currency='USD')

    # Try to get bond details
    try:
        bond_details = ib.reqContractDetails(bond_contract)
        ib.sleep(1)

        # Find the bond with maturity closest to our option expiry
        closest_bond = None
        min_days_diff = float('inf')

        for detail in bond_details:
            bond_maturity = detail.contractDetails.maturity
            bond_maturity_date = datetime.strptime(bond_maturity, '%Y%m%d').date()
            days_diff = abs((bond_maturity_date - expiry_date).days)

            if days_diff < min_days_diff:
                min_days_diff = days_diff
                closest_bond = detail.contract

        if closest_bond:
            # Get the yield for the closest bond
            bond_ticker = ib.reqMktData(closest_bond)
            ib.sleep(1)

            # Extract yield from market data
            bond_yield = bond_ticker.marketPrice() / 100.0  # Convert to decimal
            ib.cancelMktData(closest_bond)

            print(f"Using Treasury with maturity closest to option expiry, {min_days_diff} days difference")
            return bond_yield
    except Exception as e:
        print(f"Error getting bond data: {e}")

    # Fallback interest rates based on time to expiry
    if days_to_expiry <= 30:
        return 0.025  # Short-term rate
    elif days_to_expiry <= 365:
        return 0.03   # Medium-term rate
    else:
        return 0.035  # Long-term rate
# 
# def calculate_implied_interest_rate(ib, symbol='ESTX50', expiry_date=None):
#     """
#     Calculate implied interest rate from put-call parity using ESTX50 options
#     
#     Args:
#         ib: IB connection
#         symbol: Index symbol (default ESTX50)
#         expiry_date: Option expiry date (datetime.date object)
#         
#     Returns:
#         float: Annualized implied interest rate
#     """
#     # If no expiry provided, use a standard expiry about 3 months out
#     if expiry_date is None:
#         today = datetime.datetime.now().date()
#         # Find next quarter-end date roughly
#         month = today.month
#         if month <= 3:
#             expiry_date = datetime.date(today.year, 3, 20)
#         elif month <= 6:
#             expiry_date = datetime.date(today.year, 6, 20)
#         elif month <= 9:
#             expiry_date = datetime.date(today.year, 9, 20)
#         else:
#             expiry_date = datetime.date(today.year, 12, 20)
#         
#         # If we're past that date already, use the next one
#         if expiry_date <= today:
#             if month > 9:
#                 expiry_date = datetime.date(today.year + 1, 3, 20)
#             else:
#                 expiry_date = datetime.date(today.year, month + 3, 20)
#     
#     # Format expiry date
#     expiry_str = expiry_date.strftime('%Y%m%d')
#     
#     # Get current index price
#     underlying = Contract(symbol=symbol, secType='IND', exchange='EUREX', currency='EUR')
#     qualified = ib.qualifyContracts(underlying)
#     if qualified:
#         underlying = qualified[0]
#     
#     ticker = ib.reqMktData(underlying)
#     ib.sleep(1)
#     current_price = ticker.marketPrice()
#     ib.cancelMktData(underlying)
#     print(f"Current {symbol} price: {current_price}")
#     
#     # Find ATM strike
#     increment = 50.0  # ESTX50 standard increment
#     atm_strike = round(current_price / increment) * increment
#     print(f"Using ATM strike: {atm_strike}")
#     
#     # Get call and put with same strike and expiry
#     call_contract = Contract(
#         secType='OPT',
#         symbol=symbol,
#         lastTradeDateOrContractMonth=expiry_str,
#         strike=atm_strike,
#         right='C',
#         exchange='EUREX',
#         currency='EUR'
#     )
#     
#     put_contract = Contract(
#         secType='OPT',
#         symbol=symbol,
#         lastTradeDateOrContractMonth=expiry_str,
#         strike=atm_strike,
#         right='P',
#         exchange='EUREX',
#         currency='EUR'
#     )
#     
#     # Get contract details to ensure we have valid contracts
#     call_details = ib.reqContractDetails(call_contract)
#     put_details = ib.reqContractDetails(put_contract)
#     
#     if not call_details or not put_details:
#         print("Could not find matching call and put contracts")
#         return None
#     
#     # Use the qualified contracts
#     call_contract = call_details[0].contract
#     put_contract = put_details[0].contract
#     
#     # Get market data for both options
#     call_ticker = ib.reqMktData(call_contract)
#     put_ticker = ib.reqMktData(put_contract)
#     ib.sleep(2)  # Give time for data to arrive
#     
#     # Use mid prices (average of bid and ask)
#     if call_ticker.bid and call_ticker.ask and put_ticker.bid and put_ticker.ask:
#         call_price = (call_ticker.bid + call_ticker.ask) / 2
#         put_price = (put_ticker.bid + put_ticker.ask) / 2
#     else:
#         # Fall back to last price if bid/ask not available
#         call_price = call_ticker.last
#         put_price = put_ticker.last
#     
#     ib.cancelMktData(call_contract)
#     ib.cancelMktData(put_contract)
#     
#     print(f"Call price: {call_price}, Put price: {put_price}")
#     
#     # Calculate time to expiry in years
#     today = datetime.datetime.now().date()
#     days_to_expiry = (expiry_date - today).days
#     time_to_expiry = days_to_expiry / 365.0
#     
#     # Get estimated dividend yield for ESTX50 (approximately 2.5-3%)
#     # This could be refined with actual dividend projection data
#     div_yield = 0.025  # Typical for ESTX50
#     
#     # Calculate implied interest rate using put-call parity
#     # C - P = S * e^(-q*T) - K * e^(-r*T)
#     # Solving for r:
#     # r = -ln((S*e^(-q*T) - (C - P))/K) / T
#     
#     # First, adjust stock price for dividends
#     dividend_factor = math.exp(-div_yield * time_to_expiry)
#     adjusted_stock = current_price * dividend_factor
#     
#     # Then calculate implied interest rate
#     try:
#         implied_rate = -math.log((adjusted_stock - (call_price - put_price)) / atm_strike) / time_to_expiry
#         print(f"Implied interest rate: {implied_rate*100:.2f}%")
#         return implied_rate
#     except (ValueError, ZeroDivisionError) as e:
#         print(f"Error calculating implied rate: {e}")
#         return None
# 
