#reticulate::py_run_file("C:/Users/aldoh/Documents/RApplication/Tdata/inst/python/getContractValue.py")

#reticulate::repl_python()

#### Python interactive code
from ib_insync import *
from tdata_py.IB_connection import safe_ib_connect
from tdata_py.chains_manager import getChain
from tdata_py.contract import getValue, getStraddleValue
from tdata_py.core import find_nearest_number
import datetime
from datetime import date
import csv  

ib = safe_ib_connect()

secType="IND"
sym="ESTX50"
exchange='EUREX'
exchangeSec='EUREX'
exchangeOpt="EUREX"
tradingClass="OESX"
currency = "EUR"

chain = getChain(sym,secType,currency,exchangeSec,exchangeOpt,tradingClass)
list_expdates = [int(x) for x in chain[4]]
expdate = date.today()+ timedelta(days=30)
expdate_int = int(expdate.strftime("%Y%m%d"))

#### This is an existing exp date - looking to 30 days

### First retrieve ESTX50 index value
ibkr_estx50 = getValue(sym, close=False)
estx50 = float(ibkr_estx50.price.iloc[0])

### Second find the 30 days nearest expiration
e_expdate = find_nearest_number(list_expdates, expdate_int)
DTE = (datetime.datetime.strptime(str(e_expdate), "%Y%m%d").date() - date.today()).days
print(f"DTE: {DTE}")

### Retrieve strikes from this expiration date +/- 1% from ESTX50 (so +/- 50 points)
### Do not use cache - force refresh
strikes_list = getOptionStrikes(
  sym=sym, 
  trading_class=tradingClass, 
  expiration=e_expdate, 
  strike_min=estx50*0.99, 
  strike_max=estx50*1.01, 
  force_refresh=True
)

strikes_list = [strike for strike in strikes_list]
atm_strike = find_nearest_number(strikes_list, estx50)

straddle = getStraddleValue(sym,e_expdate,atm_strike,currency,exchange,tradingClass)

fields = [int(date.today().strftime("%Y%m%d")), estx50,e_expdate,  atm_strike,  straddle]
print(fields)

with open('C:/Users/aldoh/Documents/NewTrading/estx50.csv','a', newline='') as fd:
    writer=csv.writer(fd, delimiter=';')
    writer.writerow(fields)

ib.disconnect()
