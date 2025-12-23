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
getValue("SAF")
getValue("SAF", "STK", "SMART", "EUR")
getValue("ESTX50")
getValue("810331198", "BILL")
getValue("SR3Z6", secType="FUT", expiration = "202612", exchange="CME")
getValue("SR3Z6", secType="FUT",exchange="CME", conId="385575900")
getValue("6SM6", exchange="CME", conId="496647070")
getValue("6SH6", secType="FUT",exchange="CME", conId="476639238")

print("2. Test getValue with multiple symbols ==")
getValue(["SAF", "SPY", "ESTX50", "SPX"])

print("3. Test getOptValue with one strike ===")
getOptValue("SAF", expiration="20270618", strikes=320.0, right='P', currency="EUR")
getOptValue("SAF", expiration="20260130", strikes=305.0, right='P', currency="EUR")

print("4. Test getOptValue with multiple strikes===)
getOptValue("SAF", expiration="20270618", strikes=[290.0, 320.0], right='P', currency="EUR")
getOptValue("SAF", expiration="20260130", strikes=[280.0, 275.0, 270.0], right='P', currency="EUR")
getOptValue("ESTX50", expiration="20331216", strikes=[5600, 5500, 5100], right='P', currency='EUR')

print("5. Test getStraddleValue with multiple strikes ===")
getStraddleValue("URA",  expiration="20250919", strike=39)
getStraddleValue("VXX",  expiration="20250919", strike=36)

print("6. Test getStrikesfromExpDate ===")
getStrikesfromExpDate("SPX")
getStrikesfromExpDate("TTE", expdate="20261218")
getOptValue("TTE", expiration="20261218", strikes=[62, 64, 68], right='P', currency="EUR")
