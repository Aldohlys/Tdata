import datetime
import dataclasses
import sqlite3
import pandas as pd
from ib_async import *

# Import from other modules
from .IB_connection import safe_ib_connect
from .core import util, CONFIG
from fin_logger import get_logger

# Get logger for this module
logger = get_logger("tdata_py.account")


def _get_chf_rates(currencies):
    """
    Look up the latest CHF conversion rate for each currency from the ConvertToCHF DB table.
    CHF itself always has rate 1.0. Missing rates raise a warning and default to 1.0
    (best we can do without aborting the whole portfolio retrieval).

    Args:
        currencies: iterable of ISO currency codes (e.g. ['USD', 'JPY', 'EUR'])

    Returns:
        dict[str, float]: currency code → CHF conversion rate
    """
    rates = {'CHF': 1.0}
    non_chf = [c for c in set(currencies) if c != 'CHF']
    if not non_chf:
        return rates

    db_path = CONFIG.get("DB")
    try:
        conn = sqlite3.connect(db_path)
        try:
            placeholders = ','.join('?' * len(non_chf))
            query = f"""
                SELECT currency, chf_value
                FROM ConvertToCHF
                WHERE currency IN ({placeholders})
                  AND (currency, date) IN (
                    SELECT currency, MAX(date) FROM ConvertToCHF
                    WHERE currency IN ({placeholders})
                    GROUP BY currency
                  )
            """
            cur = conn.execute(query, non_chf + non_chf)
            for curr, rate in cur.fetchall():
                rates[curr] = float(rate)
        finally:
            conn.close()
    except sqlite3.Error as e:
        logger.error(f"Failed to read ConvertToCHF: {e}")

    # Fallback for missing currencies
    for c in non_chf:
        if c not in rates:
            logger.warning(f"No ConvertToCHF rate for {c}; defaulting to 1.0 (sum will be inaccurate)")
            rates[c] = 1.0
    return rates

def retrieveCurrencyPairs(currencies, currency_pairs, direct_conv):
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return float('nan')
   
    ##### Forex data retrieval ###############
    logger.info("### Retrieve currency pairs contracts...")
    
    logger.debug("Currencies", {"currencies":currencies})
    logger.debug("Currency pairs", {"currency_pairs":currency_pairs})
    logger.debug("Direct conversion", {"direct_conv":direct_conv})

    ### currencies is a list of currencies to which to convert from/to USD
    ### this function will return values in the same order as sorted currencies list
    contracts = [Forex(fx_pair) for fx_pair in currency_pairs]

    ib.qualifyContracts(*contracts)
    
    ib.reqMarketDataType(2) ### Request type - Should be 2 or 4
    tickers = ib.reqTickers(*contracts)
    ib.sleep(1)
    ib.disconnect()
    
    res = []
    ### This assumes that direct_conv and currency_pairs are in the same order
    ### Which is calling function responsability
    
    for ticker, direct in zip(tickers, direct_conv):
        if (direct == "Yes"): 
            res.append(round(ticker.marketPrice(), 4)) 
        else: 
            res.append(round(1/ticker.marketPrice(), 4))
    
    logger.info("Result", {"currencies":currencies, "res":res})
    
    return [currencies, res]

def retrieveAccountHistory(ib, days_back=180):
    """
    Retrieve historical net liquidation values using ib_async.
    
    Args:
        days_back (int): Number of days of history to retrieve
        ib: IB object
            
    Returns:
        pandas.DataFrame: Historical NLV values with datetime index
    """
    # Calculate date range
    end_date = datetime.datetime.now() - datetime.timedelta(days=1)
    end_str = end_date.strftime('%Y%m%d %H:%M:%S')
        
    # Create a dummy forex contract for account data request
    # Using EUR.USD as it's always available
    contract = Forex('EURUSD')
            
    # Request historical data
    bars = ib.reqHistoricalData(
            contract=contract,
            endDateTime=end_str,
            durationStr=f'{days_back} D',
            barSizeSetting='1 day',
            whatToShow='NetLiquidation',
            useRTH=True,
            formatDate=1
        )

    # Convert to DataFrame
    if bars:
        df = util.df(bars)
        # Select only date and close (NLV value)
        df = df[['date', 'close']].rename(columns={'close': 'value'})
        df['date'] = pd.to_datetime(df['date'])
        df.set_index('date', inplace=True)
        log.debug("AccountHistory", {"AccountHistory":df})
        return df
    
    log.debug("No bars returned")   
    return pd.DataFrame()  # Return empty DataFrame if no data

def _get_tag_value(df, tag, currency=None, default='0'):
    """Safely extract a tag value from accountSummary DataFrame."""
    mask = df['tag'] == tag
    if currency is not None:
        mask = mask & (df['currency'] == currency)
    rows = df[mask]
    if len(rows) > 0:
        return rows.iloc[0, 2]
    return default


def retrieveAccountData(ib, account):

    df = util.df(ib.accountSummary())

    # Add a short sleep
    ib.sleep(1)

    # Per-account data: margin/equity fields exist per sub-account
    acct_df = df[df['account'] == account]
    # Aggregate data: market value, PnL, cash only exist under 'All'
    all_df = df[df['account'] == 'All']

    #### Per-account fields (margin/equity)
    NetLiquidation = _get_tag_value(acct_df, 'NetLiquidation')
    EquityWithLoanValue = _get_tag_value(acct_df, 'EquityWithLoanValue')
    FullAvailableFunds = _get_tag_value(acct_df, 'FullAvailableFunds')
    FullInitMarginReq = _get_tag_value(acct_df, 'FullInitMarginReq')
    FullMaintMarginReq = _get_tag_value(acct_df, 'FullMaintMarginReq')
    FullExcessLiquidity = _get_tag_value(acct_df, 'FullExcessLiquidity')

    #### Market value / PnL / cash fields are only under 'All' in sub-account setups.
    #### Compute per-account values from portfolio positions instead.
    #### These will be overwritten below in getIBKRData() after portfolio filtering.
    StockMarketValue = 0
    OptionMarketValue = 0
    UnrealizedPnL = 0
    RealizedPnL = _get_tag_value(all_df, 'RealizedPnL', currency='BASE', default='0')
    TotalCashBalance = _get_tag_value(acct_df, 'TotalCashValue', default='0')
    CashBalanceCHF = 0
    CashBalanceEUR = 0
    CashBalanceUSD = 0

    #### Takes integer type of date
    dd = int((datetime.datetime.now()).strftime('%Y%m%d'))
    dh = (datetime.datetime.now()).strftime("%H:%M:%S")

    df = pd.DataFrame({'account': account,
                      'date': [dd],
                      'heure': [dh],
                      'NetLiquidation': [NetLiquidation],
                      'EquityWithLoanValue': [EquityWithLoanValue],
                      'FullAvailableFunds': [FullAvailableFunds],
                      'FullInitMarginReq': [FullInitMarginReq],
                      'FullMaintMarginReq': [FullMaintMarginReq],
                      'FullExcessLiquidity': [FullExcessLiquidity],
                      'OptionMarketValue': [OptionMarketValue],
                      'StockMarketValue': [StockMarketValue],
                      'UnrealizedPnL': [UnrealizedPnL],
                      'RealizedPnL': [RealizedPnL],
                      'TotalCashBalance': [TotalCashBalance],
                      'CashFlow': 0,
                      'CashBalanceCHF': [CashBalanceCHF],
                      'CashBalanceEUR': [CashBalanceEUR],
                      'CashBalanceUSD': [CashBalanceUSD]
                     })
    return df

def retrieveAccountMarginData(contracts):
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return 0
 
    print("\n#####  Retrieving account margin data for contracts... \n")
    
    ### Case where only one contract ###
    if not isinstance(contracts, list): 
        contracts = [contracts]
    
    contracts = [Contract(conId=contract) for contract in contracts]
    ib.qualifyContracts(*contracts)
    
    ### This will work only for short positions
    ### Using BUY order is necessary as there may be already a pending order and then using a SELL order is not accepted by IBKR server
    order = MarketOrder('BUY', 1)

    order_state = [ib.whatIfOrder(contract, order) for contract in contracts]
    ib.sleep(1)
    ib.disconnect()
    
    ### Process results with error handling for failed whatIfOrder calls
    res = []
    valid_contracts = []
    
    for contract, order_s in zip(contracts, order_state):
        # Check if whatIfOrder succeeded by verifying maintMarginChange attribute exists
        if hasattr(order_s, 'maintMarginChange') and order_s.maintMarginChange is not None:
            # Convert margin change to positive value (buying back short positions)
            margin_value = -float(order_s.maintMarginChange)
            res.append(margin_value)
            valid_contracts.append(contract)
        else:
            # Log failed contract but continue processing others
            logger.warning(f"Failed to get margin data for contract: {contract.localSymbol}")
            # Optionally append 0 or skip this contract entirely
            res.append(0)  # Using 0 as fallback - adjust based on your needs
            valid_contracts.append(contract)
    
    ### Display results for valid contracts
    print("Contracts margin:\n")
    res_dict = [{'contract name': contract.localSymbol, 'margin': margin} 
                for contract, margin in zip(valid_contracts, res)]
    print(util.df(res_dict))
    
    return res

def retrievePortfolioData(ib, df):
    options = []
    for i, row in df.iterrows():            # Use iterrows to print output
        if (row['secType'] == "OPT") or (row['secType'] == 'FOP') : 
            options.append(row['contract'])
    
    logger.debug("Options", {"options": options})
    
    # IB Market data type 4 works for EUREX and also for US options but in US opening hours
    # IB Market data type 2 works for only US options (in or out US opening hours)
    # 1 = Live
    # 2 = Frozen
    # 3 = Delayed
    # 4 = Delayed frozen
    ib.reqMarketDataType(2)
    
    options = ib.qualifyContracts(*options)
    tickers = ib.reqTickers(*options)
    
    ### All data has been retrieved from IBKR
    #### Look at first option contract
    
    ### optionComputation elements (9)
    # tickAttrib  impliedVol     delta  optPrice  pvDividend     gamma      vega     theta  undPrice
    greeks_cols = ["tickAttrib", "impliedVol", "delta", "optPrice", "pvDividend", "gamma", "vega", "theta", "undPrice"]
    greeks_zero = dict.fromkeys(greeks_cols, 0)
    rows = []
    opt = 0
    for i, row in df.iterrows():
        #### Iterate over each contract
        if (row['secType'] == "OPT") or (row['secType'] == 'FOP'):
            greeks = tickers[opt].modelGreeks
            rows.append(dataclasses.asdict(greeks) if greeks is not None else greeks_zero)
            opt = opt + 1
        else:
            rows.append(greeks_zero)

    option_c = pd.DataFrame(rows, columns=greeks_cols)
    df = df.join(option_c)

    ### Extract meaningful columns
    cols = ["date", "heure", "secType", "conId", "symbol", "lastTradeDateOrContractMonth", "strike", "right", "position", 
    "marketPrice", "optPrice", "marketValue", "averageCost", "unrealizedPNL", "impliedVol", "pvDividend",
    "delta", "gamma", "vega", "theta", "undPrice", "multiplier", "currency"]
    df = df[[c for c in df.columns if c in cols]]

    dd = int((datetime.datetime.now()).strftime('%Y%m%d'))
    dh = (datetime.datetime.now()).strftime("%H:%M:%S")
    
    df = df.assign(date=dd, heure=dh)
    
    ### Re-order df columns according to col order
    df = df[cols]
    ### Rename some columns that are really ugly
    df = df.rename(columns={'lastTradeDateOrContractMonth': 'expdate', 
                            'undPrice': 'uPrice', 'impliedVol': 'IV', 'position': 'pos', 'marketPrice': 'mktPrice',
                            'marketValue': 'mktValue',
                            'averageCost': 'avgCost', 'unrealizedPNL': 'unPnL'})
    return df
  
def getIBKRData(account=None):
    """
    Retrieve account and portfolio data from Interactive Brokers for a specific account.

    Args:
        account: IBKR account code (e.g., "U1804173", "U25343478").
                 If None, uses the first managed account (backward compatible).

    Returns:
        list: A list containing account_data, portfolio_data, and currency_balances DataFrames
              or 0 if connection fails
    """
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected():
        return 0

    # Resolve account
    managed = ib.managedAccounts()
    if account is None:
        account = managed[0]
    elif account not in managed:
        logger.error(f"Account {account} not in managed accounts: {managed}")
        ib.disconnect()
        return 0

    #### Get account related data #########
    print(f"\n#####  Retrieving account data for {account}...\n")

    account_data = retrieveAccountData(ib, account)
    print(account_data)

    print(f"\n#####  Retrieving portfolio data for {account}... \n")

    # Sub-accounts require explicit subscription before portfolio() returns data
    # and before accountValues() reports per-sub-account cash balances by currency.
    ib.reqAccountUpdates(account=account)
    ib.sleep(2)

    ### Extract per-sub-account cash balances by currency.
    ### Using ib.accountValues(account) — accountSummary only exposes TotalCashBalance
    ### broken down by currency under 'All' (master-level aggregate, NOT per sub-account),
    ### which wrongly attributes master cash to every sub-account.
    ### accountValues(account) gives the sub-account's own cash split after reqAccountUpdates.
    print("\n#####  Extracting currency balances...\n")

    cash_rows = [
        v for v in ib.accountValues(account)
        if v.tag == 'CashBalance' and v.currency != 'BASE'
    ]
    currency_balances = pd.DataFrame({
        'currency': [v.currency for v in cash_rows],
        'balance': [float(v.value) for v in cash_rows]
    })
    # Filter out zero or near-zero balances
    currency_balances = currency_balances[abs(currency_balances['balance']) >= 0.01]
    print(currency_balances)

    portfolio_items = ib.portfolio(account)
    # Filter out ghost positions (position=0) from internal transfers
    portfolio_items = [p for p in portfolio_items if p.position != 0]

    ### Initialize portf data and contract definition variables
    c_def = pd.DataFrame()
    portf_data = pd.DataFrame()

    ### First check if data retrieved is not empty
    if portfolio_items:
        df = util.df(portfolio_items)

        #### Iterate over each line of portfolio, retrieve contract definition which is the first column value
        for row in df.itertuples():
            line = row[1]  # First column value (row[0] is the DataFrame index)
            c_def = pd.concat([c_def, pd.DataFrame([row[1]])], ignore_index=True)
        df = c_def.join(df)

        portf_data = retrievePortfolioData(ib, df)

        ### Wait until all data has been received
        ib.sleep(1)

        ### Compute per-account market values from portfolio positions, converted to
        ### base currency (CHF) using latest rates from ConvertToCHF DB table.
        ### accountSummary only provides these under 'All' for sub-accounts, and the
        ### portfolio marketValue/unrealizedPNL are reported in each position's native currency.
        rates = _get_chf_rates(df['currency'].unique())
        df_conv = df.assign(
            mv_chf=df['marketValue'] * df['currency'].map(rates),
            upnl_chf=df['unrealizedPNL'] * df['currency'].map(rates),
        )

        stock_mv = df_conv[df_conv['secType'].isin(['STK'])]['mv_chf'].sum()
        option_mv = df_conv[df_conv['secType'].isin(['OPT', 'FOP'])]['mv_chf'].sum()
        unrealized = df_conv['upnl_chf'].sum()

        # Cast to float first so pandas doesn't warn about incompatible int64 dtype
        account_data['StockMarketValue'] = account_data['StockMarketValue'].astype(float)
        account_data['OptionMarketValue'] = account_data['OptionMarketValue'].astype(float)
        account_data['UnrealizedPnL'] = account_data['UnrealizedPnL'].astype(float)
        account_data.at[0, 'StockMarketValue'] = round(float(stock_mv), 2)
        account_data.at[0, 'OptionMarketValue'] = round(float(option_mv), 2)
        account_data.at[0, 'UnrealizedPnL'] = round(float(unrealized), 2)

        print(portf_data)

    #### IB connection no more needed
    ib.disconnect()

    return [account_data, portf_data, currency_balances]
