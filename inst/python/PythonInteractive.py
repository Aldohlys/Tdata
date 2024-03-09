library(reticulate)
py_run_file("C:/Users/aldoh/Documents/Global/getContractValue.py")

repl_python()


#### Python interactive code
from ib_insync import *


#### For a normal python script there is no need for util.startLoop, in fact it would throw an error. 
#### It is only needed when working interactively in Jupyter. 
#### Or to be more precise any console that utilizes IPythonKernel, such as the one in Spyder.
## util.startLoop()

ib = IB()
ib.connect('127.0.0.1', 7496, getPort())

ib.disconnect()


strikes=[19,20,21,22,23]
strike=21

ib.reqMarketDataType(4)
ib.reqPnL("U1804173")

sec="IND"
sec="STK"
secType="STK"
secType="IND"
secType="FUND"

sym="SPY"
sym="ESTX50"
sym="SLV"
sym="USO"
sym="MC"
sym="AI"
sym="OR"
sym="TTE"
sym="SOFR3"
sym="NESN"
sym="ABBN"
sym="SLHN"

sym="QQQ"
sym="DTLA"
sym="CSBGU0"

exchangeSec="SMART"
exchange='SMART'
exchange='EUREX'
exchange="LSEETF"
exchange="EBS"
exchange="CME"
exchangeSec='CBOE'
exchangeSec="CME"
exchangeSec='EUREX'
exchangeOpt="EUREX"
exchangeOpt="SMART"

currency="USD"
currency="EUR"
currency="CHF"

tradingClass='USO'
tradingClass='SPXW'
tradingClass='SPX'
tradingClass='SLV'
tradingClass="OESX"
tradingClass="OEXP"

expdate="20231222"
expdate='20231215'
strike=4350.0
strike=21.0

reqType=4

#### Retrieve prices and print them

dj=pd.DataFrame([["ESTX50","EUR"]],columns=["symbol","currency"])
retrieve_prices(dj,2)
retrieve_prices(dj,4)


########### For one option



option=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
                    strike=strike,right='Call',exchange=exchangeOpt,tradingClass=tradingClass)]
ib.qualifyContracts(*option)
tickers = ib.reqTickers(*option)

tickers[0].marketPrice()
tickers[0].modelGreeks


########### For a bunch of options
contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
                    strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
ib.qualifyContracts(*contracts)
tickers = ib.reqTickers(*contracts)

tickers[0].marketPrice()
tickers[0].modelGreeks


############## For a stock
contract=Contract(secType=secType,symbol=sym,exchange=exchange,currency=currency)
ib.qualifyContracts(contract)
ticker = ib.reqTickers(contract)
ticker[0].marketPrice()


