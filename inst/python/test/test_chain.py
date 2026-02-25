
from tdata_py.chains_manager import getChain, getChains
from tdata_py.core import ticker_db

### getChain(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, force_refresh=False)

## Returns SPY trading class as is stored in DB
getChain("SPY")

## Returns SPX trading class as is stored in DB
getChain("SPX")

### If trading class explicitly set then will match it
getChain("SPX", tradingClass = "SPXW")

### Will not return any chain
getChain("AI", exchangeOpt="SMART")
getChain("AI", exchangeOpt="BEURK")

### Will return a chain
getChain("AI", exchangeOpt="EUREX")

getChain("AI", tradingClass="AIR.1")

### Trading class not found
getChain("AI", tradingClass="AIR.2")

## Returns a float nan - chain does not exist
getChain("AI", tradingClass="AIR.3")

### Will return a chain
getChain("AI", tradingClass="AIR")

### Will provide default chain (without help from ticker DB)
getChain("AIR")
## Work also
getChain("AIR", tradingClass = "AIR")

## Returns nan
getChain("AIR", tradingClass = "AIR.1")

## Works
getChain("MRVL")
