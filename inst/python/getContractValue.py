
#' Live	1	Live market data is streaming data relayed back in real time. 
#' Market data subscriptions are required to receive live market data.
#' 
#' Frozen	2	Frozen market data is the last data recorded at market close. 
#' In TWS, Frozen data is displayed in gray numbers. 
#' When you set the market data type to Frozen, you are asking TWS to send the last available quote
#' when there is not one currently available. 
#' For instance, if a market is currently closed and real time data is requested, 
#' -1 values will commonly be returned for the bid and ask prices to indicate
#' there is no current bid/ask data available. 
#' TWS will often show a 'frozen' bid/ask which represents the last value recorded by the system. 
#' To receive the last know bid/ask price before the market close, 
#' switch to market data type 2 from the API before requesting market data. 
#' API frozen data requires TWS/IBG v.962 or higher 
#' and the same market data subscriptions necessary for real time streaming data.
#' 
#' Delayed	3	
#' Free, delayed data is 15 - 20 minutes delayed.
#' In TWS, delayed data is displayed in brown background.
#'  When you set market data type to delayed, you are telling TWS to automatically switch to delayed market data
#'  if the user does not have the necessary real time data subscription.
#'  If live data is available a request for delayed data would be ignored by TWS. 
#'  Delayed market data is returned with delayed Tick Types (Tick ID 66~76).
#'  Note: TWS Build 962 or higher is required and API version 9.72.18 or higher is suggested.
#'  
#'  Delayed Frozen	4	Requests delayed "frozen" data for a user without market data subscriptions.
# 

from ib_insync import *
import math
import json
import sys
import datetime
import simplejson
import pandas as pd
import collections
import locale

def is_port_in_use(port):
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

def getPort():
  import random
  port_id=random.randint(1,9990)
  if (is_port_in_use(port_id)):
    port_id=port_id+1
    print("Port id:",port_id)
  return port_id

def determine_sec(sym):
  if (any(sym==x for x in ["ESTX50","XSP","SPX", "VIX"])): return "IND"
  else: return "STK"

def determine_exch(sym):
  if (any(sym==x for x in ["XSP","SPX", "VIX"])): return "CBOE"
  if (sym=="ESTX50"): return "EUREX"
  else: return "SMART"

def determine_primary_exch(sym):
  if  (any(sym==x for x in ["AI","SU", "TTE", "OR", "SGO", "BN"])): return "SBF"
  if  (any(sym==x for x in ["SPX","XSP", "ESTX50", "VIX"])): return ""
  if (any(sym==x for x in ["DTLA","TRE7","SXLV"])) : return "LSEETF"
  if (any(sym==x for x in ["CSBGU0","ABBN", "HOLN", "ROG", "SLHN"])): return "EBS"
  if (sym == "U.UN"): return "TSE"
  return "SMART"

def determine_sym(sym):
  if (".SW" in sym): return sym[:-3]
  if (".PA" in sym): return sym[:-3]
  if (".L" in sym): return sym[:-2]
  return sym


def getValue(list_sec, list_sym, list_currency, list_exchange, reqType, close):
  ### This function returns either:
  ### -1 if contract does not exist or
  ### NULL if no connection to IBKR and sym does not exist in prices.csv or
  ### NA if price not available from market and sym does not exist in prices.csv or 
  ### a dataframe with date and time, symbol and price + 
  ###    store record into prices.cv file if new record
  
  ### Case where called from a batch and prices are stored
  locale.setlocale(locale.LC_ALL, '')
  
  ### case where list_sym is a list of tickers, verify that other arguments are well defined
  if (type(list_sym) == list):
    len_sym = len(list_sym)
    if (type(list_sec) != list):
      list_sec = [list_sec]*len_sym
    if (type(list_currency) != list):
      list_currency = [list_currency]*len_sym
    if (type(list_exchange) != list):
      list_exchange = [list_exchange]*len_sym
    contracts=[Contract(secType=sec, symbol=sym, currency=currency, exchange=exchange) 
      for sec, sym, currency, exchange in zip(list_sec, list_sym,list_currency,list_exchange)]

  else:
    contracts = [Contract(secType=list_sec, symbol=list_sym, currency=list_currency, exchange=list_exchange)]

  ## Try to establish connection
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    print("From IB: Connection error")
    #### If no IB connection possible then return either last value or None
    return 0
    
  ##### INDIVIDUAL CONTRACTS

  if(ib.qualifyContracts(*contracts)):
    ib.reqMarketDataType(int(reqType)) ### Request type - Should be 2 or 4
    tickers = ib.reqTickers(*contracts)
    #print("\nTicker:",ticker)
    if (close): value= [ticker.close for ticker in tickers]
    else:  value= [ticker.marketPrice() for ticker in tickers]
    ib.sleep(1)
    ib.disconnect()
  
  else:
    ib.disconnect()
    return -1

  ### Compute new record - data obtained from market
  data= {
    "datetime": [datetime.datetime.now().strftime("%e %b %Y %Hh%M")] * len(value),
    "sym":list_sym,
    "price":value
  }
  df=pd.DataFrame(data)
  
  return(df)


def getOptValue(sym,expiration,strike,right,currency,exchange,tradingClass):
  #print("\ngetOptValue")
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
   
  ##### INDIVIDUAL CONTRACTS
  #contract = Contract(symbol=sym,secType="STK",currency=currency,exchange=exchange)
  contract = Contract(symbol=sym,secType="OPT",lastTradeDateOrContractMonth=expiration,
                      strike=strike,right=right,exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract
  print("Contract:",contract)
  if(ib.qualifyContracts(contract)):
    ib.reqMarketDataType(int(2))
    [ticker] = ib.reqTickers(contract)
    #print("\nTicker:",ticker)
    value= ticker.marketPrice()
    ib.sleep(1)
    if(math.isnan(value)):
      #### Either return last stored value if available or return NaN
        print("from IB: Opt price is NA")
        value = None
    # greeks=ticker.modelGreeks  ### Another way to retrieve impliedVol
    # print("\nValue:",value)
    # print("\nImpliedVol:",greeks.impliedVol)
  else:
    value = None 
  
  ib.disconnect()
  return(value)

def getStraddleValue(sym,expiration,strike,currency,exchange,tradingClass):
  #print("\ngetStraddleValue")
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
   
  ##### INDIVIDUAL CONTRACTS
  contract1 = Contract(symbol=sym,secType="OPT",lastTradeDateOrContractMonth=expiration,
                      strike=strike,right="Put",exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract
  contract2 = Contract(symbol=sym,secType="OPT",lastTradeDateOrContractMonth=expiration,
                      strike=strike,right="Call",exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract
  print("Contract:",contract1,contract2)
  contract=[contract1,contract2]
  if(ib.qualifyContracts(*contract)):
    ib.reqMarketDataType(int(4))
    ticker = ib.reqTickers(*contract)
    #print("\nTicker:",ticker)
    value= ticker[0].marketPrice()+ticker[1].marketPrice()
    ib.sleep(1)
    if(math.isnan(value)):
      #### Either return last stored value if available or return NaN
        print("from IB: Opt price is NA")
        value = None
  else:
    value = None 
  
  ib.disconnect()
  return(value)



  
###################################  General functions about options chains ###############
def find_nearest_number(numbers, target):
    if not numbers:
        raise ValueError("The list of numbers is empty.")
    
    nearest = numbers[0]
    diff = abs(nearest - target)
    
    for number in numbers:
        current_diff = abs(number - target)
        
        if current_diff < diff:
            diff = current_diff
            nearest = number
    
    return nearest

def getChains(sym,secType,currency,exchangeSec):
  with open("C:/Users/aldoh/Documents/NewTrading/Chains.json", "r") as fp:
    stored_chains=json.load(fp)
  
  #### First verify that sym exists
  chains=[chains for chains in stored_chains if (chains[0][1]==sym) ]
  #### Then verify that min exp dates are all greater than today - this may not work for daily expiration
  if chains:
    chains=chains[0]
    today=int(datetime.date.today().strftime("%Y%m%d"))
    dates=[int(chain[4][0]) for chain in chains]
    if (min(dates)>=today): return(chains)
  
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
  
  underlying= Contract(symbol=sym,secType=secType,
                      exchange=exchangeSec,currency=currency) # Simple contract may be an index or stock
  if (ib.qualifyContracts(underlying)):
    chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
    sub_chains=[chain for chain in chains if chain.exchange == "SMART"]
    if not sub_chains:
      sub_chains=[chain for chain in chains if chain.exchange == "EUREX"]
    ib.sleep(1)
    # print("Chains:")
    # print(sub_chains)
    keep_records=[chains for chains in stored_chains if (chains[0][1]!=sym)]
    sub_chains=json.loads(json.dumps(sub_chains))
    for chain in sub_chains: 
      chain[1]=sym
    keep_records.append(sub_chains)
    with open("C:/Users/aldoh/Documents/NewTrading/Chains.json","w") as fp:
      json.dump(keep_records,fp,indent=4)
    return(sub_chains)
  
  else : return(float('NaN'))
  
def getChain(sym,secType,currency,exchangeSec,exchangeOpt,tradingClass):
  chains = getChains(sym,secType,currency,exchangeSec)
  
  ### No access to IBKR API
  if chains is None : return None
  
  #### Test if getChains has returned a list of chains
  if (isinstance(chains, list)):
    ### Test if there is one chain that has requested exchangeOpt and tradingClass
    chain=[chain for chain in chains if chain[0]==exchangeOpt and chain[2]==tradingClass]
    if chain : return chain[0]
  
  ### IN all other cases return NaN
  return(float('NaN'))
  
def getStrikesfromExpDate(sym,currency,exchange,tradingClass,expdate,strikes):
  with open("C:/Users/aldoh/Documents/NewTrading/Strikes.json", "r") as fp:
    stored_chains=json.load(fp)
  
  stored_strikes= [chain for chain in stored_chains if (chain[0]==sym and chain[1]==tradingClass and chain[2]==expdate)]
  if (stored_strikes) :
    return(stored_strikes[0][3])
  
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
    
  contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
              strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
  updated_strikes=[]
  # print("Contracts:",contracts)
  
  for i in range(len(contracts)):
   #### Iterate over each contract
   if(ib.qualifyContracts(contracts[i])):
      updated_strikes.append(contracts[i].strike)
  ib.sleep(1)
  print("Strikes:",updated_strikes)
  ib.disconnect()
  
  record=[sym,tradingClass,expdate,updated_strikes]
  stored_chains.append(record)

  with open("C:/Users/aldoh/Documents/NewTrading/Strikes.json", "w") as fp:
    json.dump(stored_chains,fp,indent=4)

  return(updated_strikes)

def retrieveCurrencyPairs(currencies, currency_pairs, direct_conv):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return float('nan')
   
  ##### Forex data retrieval ###############
  print("### Retrieve currency pairs contracts...")
  # print(currencies)
  # print(currency_pairs)
  # print(direct_conv)
  
  ### currencies is a list of currencies to which to convert from/to USD
  ### this function will return values in the same order as sorted currencies list
  contracts=[Forex(fx_pair) for fx_pair in currency_pairs]

  ib.qualifyContracts(*contracts)
  
  ib.reqMarketDataType(2) ### Request type - Should be 2 or 4
  tickers = ib.reqTickers(*contracts)
  ib.sleep(1)
  ib.disconnect()
  
  res = []
  ### This assumes that direct_conv and currency_pairs are in the same order
  ### Which is calling function responsability
  
  for ticker, direct in zip(tickers, direct_conv):
    if (direct == "Yes"): res.append(round(ticker.marketPrice(), 4)) 
    else: res.append(round(1/ticker.marketPrice(), 4))
    
  print(currencies,res)
  return [currencies, res]

def retrieveAccountData(ib):
  df=util.df(ib.accountSummary())
  dt=datetime.date.today()
  
  #### This script looks only into BASE currency stats - it does not look for currency specifics
  NetLiquidation=df[df['tag'] == 'NetLiquidation'].iloc[0,2]
  EquityWithLoanValue=df[df['tag'] == 'EquityWithLoanValue'].iloc[0,2]
  FullAvailableFunds=df[df['tag'] == 'FullAvailableFunds'].iloc[0,2]
  FullInitMarginReq=df[df['tag'] == 'FullInitMarginReq'].iloc[0,2]
  FullMaintMarginReq=df[df['tag'] == 'FullMaintMarginReq'].iloc[0,2]
  FullExcessLiquidity=df[df['tag'] == 'FullExcessLiquidity'].iloc[0,2]
  StockMarketValue=df[(df['tag'] == 'StockMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
  OptionMarketValue=df[(df['tag'] == 'OptionMarketValue') & (df['currency'] == 'BASE')].iloc[0,2]
  UnrealizedPnL=df[(df['tag'] == 'UnrealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
  RealizedPnL=df[(df['tag'] == 'RealizedPnL') & (df['currency'] == 'BASE')].iloc[0,2]
  TotalCashBalance=df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'BASE')].iloc[0,2]
  # TotalCashBalanceCHF=df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'CHF')].iloc[0,2]
  # TotalCashBalanceEUR=df[(df['tag'] == 'TotalCashBalance') & (df['currency'] == 'EUR')].iloc[0,2]
  
  #### Looks only on the first account
  account=ib.managedAccounts()[0]
  

  #### Takes integer type of date
  dd=int((datetime.datetime.now()).strftime('%Y%m%d'))
  dh=(datetime.datetime.now()).strftime("%H:%M:%S")
  
  df=pd.DataFrame({'account':account,
                'date':[dd],
             'heure':[dh],
             'NetLiquidation':[NetLiquidation],
              'EquityWithLoanValue':[EquityWithLoanValue],
              'FullAvailableFunds':[FullAvailableFunds],
              'FullInitMarginReq':[FullInitMarginReq],
              'FullMaintMarginReq':[FullMaintMarginReq],
              'FullExcessLiquidity':[FullExcessLiquidity],
              'OptionMarketValue':[OptionMarketValue],
              'StockMarketValue':[StockMarketValue],
              'UnrealizedPnL':[UnrealizedPnL],
              'RealizedPnL':[RealizedPnL],
              'TotalCashBalance':[TotalCashBalance],
              'CashFlow':0
              # 'TotalCashBalanceCHF':[TotalCashBalanceCHF],
              # 'TotalCashBalanceEUR':[TotalCashBalanceEUR]
              })
  return df

def retrieveAccountMarginData(contracts):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())
  except ConnectionError:
    return 0
 
  print("\n#####  Retrieving account margin data for contracts... \n")
  
  ### Case where only one contract ###
  if not(isinstance(contracts, list)): contracts = [contracts]
  
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

def retrievePricesData(ib, du):
  
  ### Then build contract taking into account special cases (index type, SMART vs. EUREX exchange)
  ### primary_exchange is needed to avoid ambiguities (e.g. AI) between different primary exchanges
  dg=[Contract(secType=determine_sec(sym),symbol=sym,currency=currency,exchange =determine_exch(sym), 
      primaryExchange=determine_primary_exch(sym)) for sym,currency in zip(du["symbol"],du["currency"])]
  
  ### They should all be qualified - no need to test
  ### IBKR may change primaryExchange from SMART to something else if it knows better
  ib.qualifyContracts(*dg)
  
  ### Retrieve 15 minutes delayed market values in a single go
  ib.reqMarketDataType(2) ### Request type - Should be 2 or 4
  tickers = ib.reqTickers(*dg)
  
  ### Build dataframe from prices just retrieved
  l=[[ticker.contract.symbol,ticker.marketPrice()] for ticker in tickers]
  
  dh=pd.DataFrame(l,columns=["sym","price"])
  #### Remove all lines without prices
  #### Store new prices only if there is something to store
  dh=dh.dropna(subset="price")
  if not dh.empty:
    dh.insert(0,"datetime",datetime.datetime.now().strftime('%d %b %Y %Hh%M'))

  return(dh)  

def retrievePortfolioData(ib, df):

  options=[]
  for i,row in df.iterrows():            # Use iterrows to print output
     if (row['secType']=="OPT"): options.append(row['contract'])
  
  # IB Market data type 4 works for EUREX and also for US options but in US opening hours
  # IB Market data type 2 works for only US options (in or out US opening hours)
  # 1 = Live
  # 2 = Frozen
  # 3 = Delayed
  # 4 = Delayed frozen
  ib.reqMarketDataType(2)
  
  options=ib.qualifyContracts(*options)
  tickers = ib.reqTickers(*options)
  
  ### All data has been retrieved from IBKR
  #### Look at first option contract
  
  ### optionComputation elements (9)
  #tickAttrib  impliedVol     delta  optPrice  pvDividend     gamma      vega     theta  undPrice
  option_c=pd.DataFrame(columns=["tickAttrib", "impliedVol", "delta", "optPrice", "pvDividend", "gamma", "vega", "theta", "undPrice"])
  opt=0
  for i,row in df.iterrows():
     #### Iterate over each contract
      if (row['secType']=="OPT"):
          optionComputation=tickers[opt].modelGreeks
          opt=opt+1
      else:  optionComputation=[0,0,0,0,0,0,0,0,0]
      ### Construction de option computation à revoir
      option_c.loc[len(option_c.index)]=optionComputation
  df=df.join(option_c)
  
  ### Extract meaningful columns
  cols = ["date","heure","secType", "conId", "symbol", "lastTradeDateOrContractMonth",  "strike", "right" ,"position", 
  "marketPrice", "optPrice", "marketValue",  "averageCost", "unrealizedPNL", "impliedVol", "pvDividend",
  "delta",   "gamma", "vega", "theta", "undPrice","multiplier","currency"]
  df = df[[c for c in df.columns if c in cols]]

  dd=int((datetime.datetime.now()).strftime('%Y%m%d'))
  dh=(datetime.datetime.now()).strftime("%H:%M:%S")
  
  df=df.assign(date=dd,heure=dh)
  
  ### Re-order df columns according to col order
  df = df[cols]
  ### Rename some columns that are really ugly
  df = df.rename(columns={'lastTradeDateOrContractMonth':'expdate', 
                            'undPrice':'uPrice', 'impliedVol':'IV','position':'pos', 'marketPrice':'mktPrice',
                            'marketValue':'mktValue',
                            'averageCost':'avgCost', 'unrealizedPNL':'unPnL'})
  return df
  
def getIBKRData():
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())
  except ConnectionError:
    return 0
  
  
 
  #### Get account related data #########

  print("\n#####  Retrieving account data... \n")
  account_data= retrieveAccountData(ib)
  print(account_data)

  ### Store portfolio in df, then split contract definition (first column) into multiple columns
  ### Merge resulting split with the other columns


  #### For options, get the list of contract definitions
  #### i index is necessary to iterate over df
  #### Consider only row that are of secType = OPT
  ###  Extract only 'contract' column in row 
  
  df= util.df(ib.portfolio())
  c_def=pd.DataFrame()
  #### Iterate over each line of portfolio
  for i in range(len(df)):
    line=df.iloc[i,0]
    ## ib.qualifyContracts is not needed to retrieve underlying prices and will be called anyway during portfolio data processing
    ##ib.qualifyContracts(line)
    c_def=pd.concat([c_def,pd.DataFrame([df.iloc[i,0]])],ignore_index=True)
  df=c_def.join(df)
  
  print("\n#####  Retrieving underlying price data... \n")
  
  #### Remove underlying symbol duplicates
  du = df.drop_duplicates(subset='symbol',keep="first")
  
  ### Retrieve only prices for secType = OPT not other types (for STK, FUT, data is already present in retrieved portfolio data)
  du = du.loc[du["secType"] == "OPT"]

  u_prices_data = retrievePricesData(ib, du)
  print(u_prices_data)

  print("\n#####  Retrieving portfolio data... \n")
  portf_data= retrievePortfolioData(ib, df)
  print(portf_data)
  


  ### Wait until all data has been received
  ib.sleep(1)
  
  #### IB connection no more needed
  ib.disconnect()
  
  return [account_data, u_prices_data, portf_data] 

  

# def getExpDates(sym,secType,currency,exchange,tradingClass):
#   ib = IB()
#   try:
#     ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   except ConnectionError:
#     return None
#   
#   underlying= Contract(symbol=sym,secType=secType,
#                       exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract may be an index or stock
#   if(ib.qualifyContracts(underlying)):
#     chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
#     ib.sleep(1)
#     print("Chains: ",chains)
#     chain = next(c for c in chains if c.exchange == exchange)
#     ### chain = next(c for c in chains)
#     print("ExpDates:",chain.expirations)
#     ch_expirations=chain.expirations
#   else: ch_expirations=float('nan')
#   
#   ib.disconnect()
#   return(ch_expirations)
# 
# 
#py$getStrikesfromExpDate(sym="HD",secType="STK",currency="USD", exchange="SMART",expdate='20231006',strikes=strikes_list)

# def getimpliedVol(sym,secType,date,price,currency,exchange,reqType):
#   print("getimpliedVol: ",sym,secType,date,price,currency,exchange,reqType)
#   ib = IB()
#   ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   ib.sleep(1)
#   
#   underlying= Contract(symbol=sym,secType=secType,
#                       exchange=exchange,currency=currency) # Simple contract may be an index or stock
#   ib.qualifyContracts(underlying)
#   chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
#   chain = next(c for c in chains if c.exchange == exchange)
#   print("Chain IV:",chain)
#   
#   tradClass=chain.tradingClass
#   strikes=chain.strikes
#   expirations= [int(num) for num in chain.expirations]
#   strike= find_nearest_number(strikes, price)
#   
#   expiration=find_nearest_number(expirations,date)
#   
#   ##### INDIVIDUAL CONTRACTS
#   contract = Contract(symbol=sym,secType="OPT",tradingClass=tradClass,
#                       lastTradeDateOrContractMonth=str(expiration),
#                       strike=strike,
#                       ### At the money put and call are likely to have very near impliedVol
#                       right="P",
#                       exchange=exchange,currency=currency) # Simple contract
#   print("Contract:",contract)
#   ib.qualifyContracts(contract)
#   ib.reqMarketDataType(int(reqType)) ### Request type - Should be 1 or 2 - 1=Live, 2=Frozen(closed)
#   ticker=ib.reqMktData(contract, genericTickList='106', snapshot=False, regulatorySnapshot=False, mktDataOptions=[])
#   ib.sleep(1)
#   value= ticker.impliedVolatility
#   print("\nImpliedVol:",value)
#   ib.disconnect()
#   return(value)

# def getOptExchangeList(sym,secType,currency,exchange):
#   ib = IB()
#   try:
#     ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   except ConnectionError:
#     return None
#   
#   underlying= Contract(symbol=sym,secType=secType,
#                       exchange=exchange,currency=currency) # Simple contract may be an index or stock
#   if (ib.qualifyContracts(underlying)):
#     chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
#     ib.sleep(1)
#     opt_exchange_list = [c.exchange for c in chains]
#   else: opt_exchange_list=float('nan')
# 
#   ib.disconnect()
# 
#   return opt_exchange_list
# 
# def getTradingClassList(sym,secType,currency,exchangeSec,exchangeOpt):
#   ib = IB()
#   try:
#     ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   except ConnectionError:
#     return None
#     
#   underlying= Contract(symbol=sym,secType=secType,
#                       exchange=exchangeSec,currency=currency) # Simple contract may be an index or stock
#   if (ib.qualifyContracts(underlying)):
#     chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
#     ib.sleep(1)
#     tradingClass_list = [c.tradingClass for c in chains if c.exchange==exchangeOpt]
#   else: tradingClass_list=float('nan')
#   
#   ib.disconnect()
#   return tradingClass_list

# 
# def getStockValue(sec,sym,currency,exchange,reqType,close):
#   ### This function returns either:
#   ### -1 if contract does not exist or
#   ### NULL if no connection to IBKR and sym does not exist in prices.csv or
#   ### NA if price not available from market and sym does not exist in prices.csv or 
#   ### a dataframe with date and time, symbol and price + 
#   ###    store record into prices.cv file if new record
#   
#   ### Case where called from a batch and prices are stored
#   locale.setlocale(locale.LC_ALL, '')
#   
#   ### Retrieve last prices for 'sym' if any
#   #print("getStockValue")
#   stored_prices=pd.read_csv("C:/Users/aldoh/Documents/NewTrading/prices.csv",sep=';')
#   line=stored_prices.loc[stored_prices['sym'] == sym]
#   
#   ### Return last line of lines if at least one and the line is less than 60 minutes old
#   ### Otherwise do not take it into account
#   if not line.empty: 
#     line =line.iloc[-1]
#     limit_to_reload= datetime.datetime.now()-datetime.timedelta(minutes=60)
#     last_storage=datetime.datetime.strptime(line["datetime"],'%d %b %Y %Hh%M')
#     if (last_storage > limit_to_reload): return(line)
#     line = pd.DataFrame()
#   
#   ## Try to establish connection
#   ib = IB()
#   try:
#     ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   except ConnectionError:
#     print("From IB: Connection error")
#     #### If no IB connection possible then return either last value or None
#     if line.empty: return None
#     return line
#     
#   ##### INDIVIDUAL CONTRACTS
#   contract = Contract(symbol=sym,secType=sec,exchange=exchange,currency=currency) # Simple contract
#   print("Contract:",contract)
#   
#   if(ib.qualifyContracts(contract)):
#     ib.reqMarketDataType(int(reqType)) ### Request type - Should be 2 or 4
#     [ticker] = ib.reqTickers(contract)
#     #print("\nTicker:",ticker)
#     if (close): value= ticker.close
#     else:  value= ticker.marketPrice()
#     ib.sleep(1)
#     ib.disconnect()
#     ### If no value is returned (no market price available)
#     if(math.isnan(value)):
#     #### Either return last stored value if available or return NaN
#       print("from IB: NA")
#       if line.empty: return None
#       return line
#     
#   else:
#     ib.sleep(1)
#     ib.disconnect()
#     #### Contract does not exist
#     print("from IB:",-1)
#     return(pd.DataFrame({"price":[-1]}))
#   
#   print("from IB:",value)
#   ### Compute new record - data obtained from market
#   data= {
#     "datetime": [datetime.datetime.now().strftime("%e %b %Y %Hh%M")],
#     "sym":[sym],
#     "price":[value]
#   }
#   df=pd.DataFrame(data)
#   
#   #### New value available - then store it. If same as before, store it anyway because of new timestamp
#   df.to_csv("C:/Users/aldoh/Documents/NewTrading/prices.csv", mode='a', header=False, sep=";", index=False)
#   return(df)

############ For Gonet portfolio

# def retrieve_prices(position_list,reqType):
#   
#   locale.setlocale(locale.LC_ALL, '')
#   
#   du=position_list.drop_duplicates(subset='symbol',keep="first")
#   dg=[Contract(secType=determine_sec(determine_sym(sym)),symbol=determine_sym(sym),currency=currency,exchange=determine_exch(determine_sym(sym))) for sym,currency in zip(du["symbol"],du["currency"])]
#   ib.qualifyContracts(*dg)
#   
#   ib.reqMarketDataType(reqType) ### Request type - Should be 2 or 4
#   tickers = ib.reqTickers(*dg)
#   l=[[ticker.contract.symbol,ticker.marketPrice()] for ticker in tickers]
#   
#   du=DataFrame(l,columns=["sym","price"])
#   du=du.dropna(subset="price")
#   
#   if not du.empty:
#     du.insert(0,"datetime",datetime.datetime.now().strftime('%d %b %Y %Hh%M'))
#     du.to_csv("C:/Users/aldoh/Documents/NewTrading/prices.csv",header=False, index=False, mode='a', sep=';')

##dh=itertools.islice(dh,len(dh)-1,len(dh))
# def retrieve_gonet_prices():
#   dh = pd.read_csv('C:\\Users\\aldoh\\Documents\\NewTrading\\GonetTrades.csv',sep=";")
#   dh["date"]=[datetime.datetime.strptime(d, '%d.%m.%Y').date() for d in dh.date]
#   dh = dh.groupby(["date","heure"])
#   dh = next(iter(collections.deque(dh,maxlen=1)))[1]
#   
#   #### DTLA can only be retrieved using reqType = 2 frozen data
#   retrieve_prices(dh[dh.symbol == "DTLA.L"],2)
#   #### CSBGU0 can only be retrieved using reqType = 4 delayed frozen data
#   #### Other stocks don't care
#   retrieve_prices(dh[dh.symbol!= "DTLA.L"], 4)
  
