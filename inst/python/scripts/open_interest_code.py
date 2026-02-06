from ib_async import *
from tdata_py.IB_connection import safe_ib_connect
import math

ib= safe_ib_connect()

ib.reqMarketDataType(1) ## Live data but will work also after hours

ai_put = Option('AI', tradingClass='AIR', lastTradeDateOrContractMonth='20250919', strike=170.0, right='P', exchange='EUREX')
ai_call = Option('AI', tradingClass='AIR', lastTradeDateOrContractMonth='20250919', strike=170.0, right='C', exchange='EUREX')
print("=" * 50)

print(ai_put)
print(ai_call)
ib.qualifyContracts(*[ai_put, ai_call])

## ticker = ib.reqMktData(contracts[0], genericTickList='100,101') ## With volume as well 
ticker_put = ib.reqMktData(ai_put, genericTickList='101, 105')  ### Generic tick for OpenInterest and avOptionVolume - for a single contract
ticker_call = ib.reqMktData(ai_call, genericTickList='101, 105')  ### Generic tick for OpenInterest and avOptionVolume - for a single contract

# wait 1 second for the first data packets to arrive 
ib.sleep(0.5) 

print("Printing requested market data") 
if math.isnan(ticker_put.putOpenInterest):
  print("No Put Open Interest data available")
else:
  print("Put Open Interest:", int(ticker_put.putOpenInterest)) 

if math.isnan(ticker_call.callOpenInterest):
  print("No Call Open Interest data available")
else:
  print("Call Open Interest:", int(ticker_call.callOpenInterest)) 

print("Avg. Option Volume:", ticker.avOptionVolume )

ib.cancelMktData(ai_put)
ib.cancelMktData(ai_call)

print("=" * 50)
ib.disconnect()

# Try with very liquid option
# Create contract


spy_option = Option('SPY', tradingClass='SPY', lastTradeDateOrContractMonth='20250919', strike=640.0, right='C', exchange='SMART')

# MUST qualify first
qualified_contracts = ib.qualifyContracts(spy_option)
spy_option = qualified_contracts[0]  # Get the qualified contract

# Now request market data
ticker = ib.reqMktData(spy_option, genericTickList='105')
ib.sleep(1)
print(ticker.avOptionVolume if hasattr(ticker, 'avOptionVolume') else 'No avOptionVolume data')
ib.cancelMktData(spy_option)
