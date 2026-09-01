"""Live open (resting) order retrieval from IBKR TWS.

Separate from account.py because orders are a "right now" view: they have no
DB persistence and no historical dimension, unlike positions and account values.
"""
import math
import pandas as pd
from ib_async import Contract

from .IB_connection import safe_ib_connect
from fin_logger import get_logger

logger = get_logger("tdata_py.orders")

# TWS sends DBL_MAX for numeric order fields that are not set (typically
# trailStopPrice on a plain LMT order). Anything at or above this is "unset".
_UNSET = 1.7976931348623157e+308

OPEN_ORDER_COLUMNS = [
    'account', 'permId', 'orderId', 'parentId', 'ocaGroup',
    'isCombo', 'legCount', 'legIndex', 'legRatio', 'legAction',
    'secType', 'symbol', 'localSymbol', 'tradingClass',
    'expiry', 'strike', 'right', 'multiplier', 'currency', 'exchange',
    'action', 'quantity', 'orderType',
    'lmtPrice', 'auxPrice', 'trailStopPrice',
    'tif', 'status', 'filled', 'remaining',
]


def _num(value):
    """Return a float, or None for TWS' unset sentinel / NaN / missing."""
    if value is None:
        return None
    try:
        value = float(value)
    except (TypeError, ValueError):
        return None
    if math.isnan(value) or abs(value) >= _UNSET:
        return None
    return value


def _text(value):
    """Return a non-empty string, or None. '?' is TWS' empty `right`."""
    if value is None:
        return None
    value = str(value).strip()
    return None if value in ('', '?', '0') else value


def _invert(action):
    return 'SELL' if action == 'BUY' else 'BUY'


def _resolve_leg(ib, leg, bag):
    """Qualify one combo leg by conId so the leg's own contract is known.

    Returns the qualified Contract, or None when TWS cannot resolve it (the
    caller then falls back to reporting the leg by conId alone).
    """
    try:
        qualified = ib.qualifyContracts(
            Contract(conId=leg.conId, exchange=leg.exchange or bag.exchange))
        return qualified[0] if qualified else None
    except Exception as exc:                      # noqa: BLE001 - never fail the whole fetch
        logger.warning(f"Could not qualify combo leg conId={leg.conId}: {exc}")
        return None


def _order_row(order, contract, status, bag=None, leg=None,
               leg_index=None, leg_count=1):
    """Build one output row. `bag` / `leg` are set for expanded combo legs."""
    # The combo's root symbol is the matching key for every leg row, so a
    # BAG's legs stay attributable to the same trade as a single-leg order.
    root = (bag or contract).symbol
    if leg is None:
        leg_action, leg_ratio = None, None
    else:
        # Combo leg actions are expressed relative to BUYing the combo, so a
        # SELL of the combo inverts every leg.
        leg_action = leg.action if order.action == 'BUY' else _invert(leg.action)
        leg_ratio = leg.ratio

    return {
        'account': order.account,
        'permId': int(order.permId or 0),
        # orderId is 0 for orders placed by another client (TWS itself, a
        # different API session) - permId is the only stable identifier.
        'orderId': int(order.orderId or 0),
        'parentId': int(order.parentId or 0),
        'ocaGroup': _text(order.ocaGroup),
        'isCombo': bag is not None,
        'legCount': leg_count,
        'legIndex': leg_index,
        'legRatio': leg_ratio,
        'legAction': leg_action,
        'secType': contract.secType,
        'symbol': root,
        'localSymbol': _text(contract.localSymbol),
        'tradingClass': _text(contract.tradingClass),
        'expiry': _text(contract.lastTradeDateOrContractMonth),
        'strike': _num(contract.strike) or None,
        'right': _text(contract.right),
        'multiplier': _num(contract.multiplier),
        'currency': contract.currency,
        'exchange': contract.exchange,
        'action': order.action,
        'quantity': _num(order.totalQuantity),
        'orderType': order.orderType,
        'lmtPrice': _num(order.lmtPrice),
        'auxPrice': _num(order.auxPrice),
        'trailStopPrice': _num(getattr(order, 'trailStopPrice', None)),
        'tif': order.tif,
        'status': status.status,
        'filled': _num(status.filled),
        'remaining': _num(status.remaining),
    }


def get_open_orders(account=None, expand_combos=True):
    """Retrieve all resting (open) orders from TWS.

    Args:
        account: IBKR account code to filter on (e.g. "U1804173"). None returns
                 every account the TWS session can see.
        expand_combos: when True, a BAG (multi-leg) order is emitted as one row
                 per leg with each leg's own contract resolved by conId, so the
                 leg contracts can be matched against individual trade legs.
                 When False a combo yields a single row carrying the BAG.

    Returns:
        pandas.DataFrame with OPEN_ORDER_COLUMNS, empty when there are no
        resting orders.
        None when the orders could not be asked for at all - TWS unreachable, or
        the account not managed by this TWS session (a paper account under a live
        gateway). The caller must distinguish that from "no orders": reporting
        every trade as unprotected because nobody could be asked is a false alarm.
    """
    ib = safe_ib_connect()
    if not ib.isConnected():
        logger.info("TWS not reachable - cannot retrieve open orders")
        return None

    if account is not None and account not in ib.managedAccounts():
        logger.warning(f"Account {account} is not managed by this TWS session "
                       f"({ib.managedAccounts()}) - its orders cannot be seen")
        ib.disconnect()
        return None

    rows = []
    try:
        # reqAllOpenOrders covers orders from every client id, not just ours.
        ib.reqAllOpenOrders()
        ib.sleep(1)

        for trade in ib.openTrades():
            order, contract, status = trade.order, trade.contract, trade.orderStatus

            if contract.secType != 'BAG' or not expand_combos:
                rows.append(_order_row(order, contract, status))
                continue

            legs = contract.comboLegs or []
            for index, leg in enumerate(legs, start=1):
                resolved = _resolve_leg(ib, leg, contract)
                rows.append(_order_row(
                    order, resolved or contract, status,
                    bag=contract, leg=leg,
                    leg_index=index, leg_count=len(legs)))
    finally:
        ib.disconnect()

    orders = pd.DataFrame(rows, columns=OPEN_ORDER_COLUMNS)
    if account is not None and not orders.empty:
        orders = orders[orders['account'] == account].reset_index(drop=True)

    logger.info(f"Retrieved {len(orders)} open order row(s)"
                f"{'' if account is None else ' for ' + account}")
    return orders
