import sys
import math
import datetime
import locale
import pandas as pd
import logging
from ib_insync import *

# Import from other modules
from tdata_py.core import CONFIG, ticker_db, validate_contract_params, find_nearest_number
from tdata_py.IB_connection import safe_ib_connect
from tdata_py.chains_manager import getChains, getAllStrikes
from tdata_py.dividend_utils import getNTMDividend
from tdata_py.contract import getValue, getOptValue, getStraddleValue, getStrikesfromExpDate

print("1. Test getValue with one symbol ===")
getValue("SAF", close = False)
getValue("SAF", close = True)
getValue("ESTX50", close = True)
getValue("SAF", "STK", "SMART", "EUR", close = False)
getValue("810331198", "BILL")
getValue("MSFZ5", secType="FUT",  expiration="20251215", exchange="CME")
getValue("SR3Z6", secType="FUT", expiration = "202612")
getValue("MSFU5", secType="FUT", expiration="202509")
getValue("MSFU5")

print("2. Test getValue with multiple symbols ==")
getValue(["SAF", "SPY", "ESTX50", "SPX"], close = False)

print("3. Test getOptValue with one strike ===")
getOptValue("SAF", expiration="20251017", strikes=270.0, right='P', currency="EUR")
getOptValue("SAF", expiration="20251010", strikes=275.0, right='P', currency="EUR")
getOptValue("SAF", expiration="20250919", strikes=280.0, right='P', currency="EUR")

print("4. Test getOptValue with multiple strikes===)
getOptValue("SAF", expiration="20251017", strikes=[280.0, 270.0], right='P', currency="EUR")
getOptValue("SAF", expiration="20251010", strikes=[280.0, 275.0, 270.0], right='P', currency="EUR")
getOptValue("ESTX50", expiration="20251010", strikes=5300, right='P', currency='EUR')
getOptValue("ESTX50", expiration="20251017", strikes=[5300, 5325, 5275], right='P', currency='EUR')

print("5. Test getStraddleValue with multiple strikes ===")
getStraddleValue("URA",  expiration="20250919", strike=39)
getStraddleValue("VXX",  expiration="20250919", strike=36)

print("6. Test getStrikesfromExpDate ===")
getStrikesfromExpDate("SPX")
getStrikesfromExpDate("TTE", expdate="20250919")
