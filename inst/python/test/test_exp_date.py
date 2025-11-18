

### getExpirationDates(sym, secType=None, currency=None, exchangeSec=None, exchangeOpt=None, tradingClass=None, min_date=None, max_date=None, force_refresh=False):
  
from tdata_py.chains_manager import getExpirationDates

test = []
### 4 expiration dates returned
test.append(getExpirationDates("SPY", min_date="20250915", max_date="20251012")) 

## Works also 4 expiration dates returned (but will be all strings)
test.append(getExpirationDates("SPY", min_date=20250915, max_date=20251012)) 

### min_date = max_date and no expiration returned on Sep 15 or Sep 17 (empty list)
test.append(getExpirationDates("SPY", min_date="20250915", max_date="20250915"))
test.append(getExpirationDates("SPY", min_date="20250917", max_date="20250917"))

## One expiration returned on Sep 19
test.append(getExpirationDates("SPY", min_date="20250919", max_date="20250919"))

### min_date > max_date -> returns empty list
test.append(getExpirationDates("SPY", min_date="20251015", max_date="20251012"))

## Using Ticker DB AIR trading class this returns correctly 6 dates
test.append(getExpirationDates("AI", min_date=20250910, max_date=20251130))

### Return nan
test.append(getExpirationDates("AIR", tradingClass="AIR1", min_date=20251110, max_date=20251215))
## Return one date
test.append(getExpirationDates("AIR", tradingClass="AIR", min_date=20251110, max_date=20251215))

for t in test:
  print(t)

