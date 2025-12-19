  from ib_insync import *
  from tdata_py.IB_connection import safe_ib_connect
  
  ib = safe_ib_connect()

  # Create contract using conId
  #contract = Stock(conId=433080107, exchange='ALLFUNDS', currency='EUR')
  contract = Stock(symbol="CSBGU0", exchange='EBS', currency='USD')
  qualified = ib.qualifyContracts(contract)[0]
  print(f'Qualified: {qualified}')

  # Request market data
  ib.reqMarketDataType(4)  # Delayed frozen
  ticker = ib.reqTickers(qualified)[0]
  print(f'Price: {ticker.marketPrice()}')

 # Try different market data types
  for reqType in [1, 2, 3, 4]:
      ib.reqMarketDataType(reqType)
      ticker = ib.reqTickers(qualified)[0]
      ib.sleep(2)  # Wait for data
      print(f'reqType={reqType}: last={ticker.last}, close={ticker.close}, bid={ticker.bid}, ask={ticker.ask}')

  ib.disconnect()
  ticker.marketPrice()
  
