import datetime
import pandas as pd
from ib_insync import *

# Import from other modules
from .IB_connection import safe_ib_connect
from .core import util

def retrieveCurrencyPairs(currencies, currency_pairs, direct_conv):
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return float('nan')
   
    ##### Forex data retrieval ###############
    print("### Retrieve currency pairs contracts...")
    # print(currencies)
    # print(currency_pairs)
    # print(direct_conv)
    
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
    
    print(currencies, res)
    return [currencies, res]

def retrieveAccountHistory(ib, days_back=180):
    """
    Retrieve historical net liquidation values using ib_insync.
    
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
        return df
            
    return pd.DataFrame()  # Return empty DataFrame if no data

def retrieveAccountData(ib):
    df = util.df(ib.accountSummary())
    dt = datetime.date.today()
    
    #### This script looks only into BASE currency stats - it does not look for currency specifics
    NetLiquidation = df[df['tag'] == 'NetLiquidation'].iloc[0,2]
    EquityWithLoanValue = df[df['tag'] == 'EquityWithLoanValue'].iloc[0,2]
    FullAvailableFunds = df[df['tag'] == 'FullAvailableFunds'].iloc[0,2]
    FullInitMarginReq = df[df['tag'] == 'FullInitMarginReq'].iloc[0,2]
    FullMaintMarginReq = df[df['tag'] == 'FullMaintMarginReq'].iloc[0,2]
    FullExcessLiquidity = df[df['tag'] == 'FullExcessLiquidity'].iloc[0,2]
    StockMarketValue = df[(df['tag'] == 'StockMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
    OptionMarketValue = df[(df['tag'] == 'OptionMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
    UnrealizedPnL = df[(df['tag'] == 'UnrealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
    RealizedPnL = df[(df['tag'] == 'RealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
    TotalCashBalance = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'BASE')].iloc[0,2]
    # TotalCashBalanceCHF = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'CHF')].iloc[0,2]
    # TotalCashBalanceEUR = df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'EUR')].iloc[0,2]
    
    #### Looks only on the first account
    account = ib.managedAccounts()[0]
    
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
                      'CashFlow': 0
                      # 'TotalCashBalanceCHF': [TotalCashBalanceCHF],
                      # 'TotalCashBalanceEUR': [TotalCashBalanceEUR]
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
    
    ### As we are buying back this contracts list is actually the margin cost of selling this contract list
    res = [-float(order_s.maintMarginChange) for order_s in order_state]
    print("Contracts margin:\n")
    res_dict = [{'contract name': contract.localSymbol, 'margin': res} for contract, res in zip(contracts, res)]
    print(util.df(res_dict))
    return(res)

def retrievePortfolioData(ib, df):
    options = []
    for i, row in df.iterrows():            # Use iterrows to print output
        if (row['secType'] == "OPT"): 
            options.append(row['contract'])
    
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
    option_c = pd.DataFrame(columns=["tickAttrib", "impliedVol", "delta", "optPrice", "pvDividend", "gamma", "vega", "theta", "undPrice"])
    opt = 0
    for i, row in df.iterrows():
        #### Iterate over each contract
        if (row['secType'] == "OPT"):
            optionComputation = tickers[opt].modelGreeks
            opt = opt + 1
        else:  
            optionComputation = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        ### Construction de option computation à revoir
        option_c.loc[len(option_c.index)] = optionComputation
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
  
def getIBKRData():
    """
    Retrieve account and portfolio data from Interactive Brokers.
    
    Returns:
        list: A list containing account_data and portfolio_data DataFrames
              or 0 if connection fails
    """
    # Use safe_ib_connect instead of direct connection
    ib = safe_ib_connect()

    # If connection not available return None
    if not ib.isConnected(): 
        return 0

    #### Get account related data #########
    print("\n#####  Retrieving account data... \n")
    account_data = retrieveAccountData(ib)
    print(account_data)

    ### Store portfolio in df, then split contract definition (first column) into multiple columns
    ### Merge resulting split with the other columns

    #### For options, get the list of contract definitions
    #### i index is necessary to iterate over df
    #### Consider only row that are of secType = OPT
    ###  Extract only 'contract' column in row 
    
    df = util.df(ib.portfolio())
    c_def = pd.DataFrame()
    #### Iterate over each line of portfolio
    for i in range(len(df)):
        line = df.iloc[i,0]
        ## ib.qualifyContracts is not needed to retrieve underlying prices and will be called anyway during portfolio data processing
        ## ib.qualifyContracts(line)
        c_def = pd.concat([c_def, pd.DataFrame([df.iloc[i,0]])], ignore_index=True)
    df = c_def.join(df)
    
    # print("\n#####  Retrieving underlying price data... \n")
    # 
    # #### Remove underlying symbol duplicates
    # du = df.drop_duplicates(subset='symbol',keep="first")
    # 
    # ### Retrieve only prices for secType = OPT not other types (for STK, FUT, data is already present in retrieved portfolio data)
    # du = du.loc[du["secType"] == "OPT"]
    # 
    # u_prices_data = retrievePricesData(ib, du)
    # print(u_prices_data)

    print("\n#####  Retrieving portfolio data... \n")
    portf_data = retrievePortfolioData(ib, df)
    print(portf_data)
    
    ### Wait until all data has been received
    ib.sleep(1)
    
    #### IB connection no more needed
    ib.disconnect()
    
    return [account_data, portf_data]
