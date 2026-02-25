from ib_insync import *

# Connect to the TWS API
ib = IB()
ib.connect('127.0.0.1', 7497, clientId=getPort())

# Define the Forex contract
contract = Stock('SPY', exchange='SMART', currency="USD")
contract = Forex('EURUSD')
ib.qualifyContracts(contract)

# Define the order parameters
order_type = 'LMT'  # Limit order
quantity = 1  # Number of units to trade
action = 'BUY'  # Buy or sell

# Define the bracket parameters
profit_target = 5  # Pips
stop_loss = 5  # Pips

# Calculate the order prices
ticker = ib.reqTickers(contract)
current_price = ticker[0].marketPrice()
order_price = round(current_price + 1, 5)  # Calculate the order price (1 pip above the current price)
profit_price = round(order_price + (profit_target * 1), 5)  # Calculate the profit target price
stop_price = round(order_price - (stop_loss * 1), 5)  # Calculate the stop loss price

# Create the main order
order = LimitOrder('SELL', 20000, 1.11)
main_order = LimitOrder(action=action, totalQuantity=quantity, lmtPrice=order_price)

# Create the profit target order
profit_order = Order(action='SELL' if action == 'BUY' else 'BUY', totalQuantity=quantity, orderType=order_type,  lmtPrice=profit_price, transmit=False)
##profit_order.trailer = 'TSTP'  # Set the trailer to 'Trigger to Stop'

# Create the stop loss order
stop_order = Order(action='SELL' if action == 'BUY' else 'BUY', totalQuantity=quantity, orderType="STP", auxPrice=stop_price)
##stop_order.trailer = 'TMTS'  # Set the trailer to 'Trigger to Market to Stop'

# Place the orders (but do not transmit them)

trade = ib.placeOrder(contract, order)
ib.placeOrder(contract, main_order)
profit_order.parentId = main_order.orderId  # Link the profit order to the main order
stop_order.parentId = main_order.orderId  # Link the stop order to the main order

ib.placeOrder(contract, profit_order)
ib.placeOrder(contract, stop_order)

# Disconnect from the TWS API
ib.disconnect()
