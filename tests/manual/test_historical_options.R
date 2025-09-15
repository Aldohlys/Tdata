tdata_py$findBestTradingClass("STNG")

contract_config = list(symbol='STNG', trading_class = 'STNG',
expiration='20250919', strike=50.0, right='C', exchange='SMART')

tdata_py$add_historical_tracking(list(symbol='AI', trading_class = 'AIR',
                                      expiration='20250919', strike=168.0, right='P', exchange='EUREX'))

tdata_py$add_historical_tracking(contract_config)
tdata_py$add_historical_tracking(list(symbol='IWM', trading_class = 'IWM',
                                     expiration='20250930', strike=230.0, right='P', exchange='SMART'))
tdata_py$add_historical_tracking(list(symbol='IWM', trading_class = 'IWM',
                                     expiration='20250930', strike=250.0, right='C', exchange='SMART'))
tdata_py$add_historical_tracking(list(symbol='VXX', trading_class = 'VXX',
                                      expiration='20250919', strike=37.0, right='P', exchange='SMART'))
tdata_py$add_historical_tracking(list(symbol='VXX', trading_class = 'VXX',
                                      expiration='20250919', strike=46.0, right='C', exchange='SMART'))
tdata_py$manage_contracts('list_active')

tdata_py$manage_contracts('remove', symbol='SAF', trading_class='SEJ',
                    expiration='20250919', strike=280.0, right='C', archive=TRUE)

tdata_py$list_historical_config()
results = tdata_py$update_historical_data("both")
sprintf("Processed: %.0f contracts", results['contracts_processed'])

ai_bid_ask <- tdata_py$get_option_historical_data(symbol='AI', trading_class = 'AIR',
                                    expiration='20250919', strike=168.0, right='P', data_type='historical', what_to_show='BID_ASK')
saf_bid_ask <- tdata_py$get_option_historical_data(symbol='SAF', trading_class = 'SEJ',
                                                  expiration='20250919', strike=280.0, right='P', data_type='historical', what_to_show='BID_ASK')
saf_trades <- tdata_py$get_option_historical_data(symbol='SAF', trading_class = 'SEJ',
                                                  expiration='20250919', strike=280.0, right='P', data_type='historical', what_to_show='TRADES')
