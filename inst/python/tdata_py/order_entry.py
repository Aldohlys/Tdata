"""
order_entry.py - quote, preview and stage stock orders in TWS.

Orders are STAGED, never transmitted: every order goes out with
transmit=False, so it appears in TWS and the user presses Transmit there.
They are placed under client id 0, the TWS client itself, so TWS owns them
and they can be transmitted, modified or cancelled from TWS like any manual
order.

Order types (stock):
  market  - MKT, Adaptive algo (Normal priority)
  limit   - LMT at `limit`, Adaptive algo (Normal priority)
  bracket - parent LMT at `limit`, take profit LMT at `take_profit`,
            stop loss STP at `stop_loss` (children reverse the parent's side,
            linked to it by parentId; IBKR cancels the other child on a fill)
All orders are GTC.

Adapted from Tuser/order/logic/IButilities.py (market / limit / bracket,
Adaptive algo, transmit=False), with an explicit account, prices instead of
offsets for the bracket, and a plain STP stop (STP PRT exists for CME futures
only).
"""
import math

from ib_async import Stock, MarketOrder, LimitOrder, StopOrder, TagValue

from .IB_connection import safe_ib_connect
from .core import ticker_db
from fin_logger import get_logger

logger = get_logger("tdata_py.order_entry")

# TWS's own client id: orders placed under it are TWS orders
ORDER_CLIENT_ID = 0
# Seconds before a TWS request is abandoned (reticulate cannot interrupt asyncio)
REQUEST_TIMEOUT = 20

ORDER_TYPES = ("market", "limit", "bracket")


def _finite(x):
    try:
        return x is not None and math.isfinite(float(x))
    except (TypeError, ValueError):
        return False


def _num(x):
    return float(x) if _finite(x) else None


def _check_order(action, quantity, order_type, limit, take_profit, stop_loss):
    """Message of the first problem, None when the order can be sent."""
    if action not in ("BUY", "SELL"):
        return f"Action must be BUY or SELL, not {action!r}."
    if order_type not in ORDER_TYPES:
        return f"Order type must be one of {ORDER_TYPES}, not {order_type!r}."
    if not _finite(quantity) or float(quantity) <= 0 or float(quantity) != int(float(quantity)):
        return "Quantity must be a whole number > 0."
    if order_type in ("limit", "bracket") and (not _finite(limit) or float(limit) <= 0):
        return "Limit price must be > 0."
    if order_type == "bracket":
        if not _finite(take_profit) or not _finite(stop_loss):
            return "A bracket needs a take profit and a stop loss."
        lim, tp, sl = float(limit), float(take_profit), float(stop_loss)
        if action == "BUY" and not (sl < lim < tp):
            return "Buy bracket: stop loss < limit < take profit."
        if action == "SELL" and not (tp < lim < sl):
            return "Sell bracket: take profit < limit < stop loss."
    return None


def _commission(state):
    """Commission of a whatIf: the exact value when known, else IBKR's maximum
    estimate (unset values arrive as a huge float)."""
    for v in (state.commission, state.maxCommission):
        x = _num(v)
        if x is not None and 0 <= x < 1e9:
            return x
    return None


def _stock_contract(ib, sym, currency=None, exchange=None):
    info = ticker_db.get_ticker_info(sym) or {}
    if currency is None:
        currency = info.get("Currency") or "USD"
    contract = Stock(sym, exchange or "SMART", currency)
    qualified = ib.qualifyContracts(contract)
    if not qualified or not contract.conId:
        return None
    return contract


def _connect(account):
    """(ib, error message). The account must be managed by this TWS session."""
    ib = safe_ib_connect(client_id=ORDER_CLIENT_ID)
    if not ib.isConnected():
        return None, "TWS not reachable (or client id 0 already in use)."
    ib.RequestTimeout = REQUEST_TIMEOUT
    if account not in ib.managedAccounts():
        ib.disconnect()
        return None, f"Account {account} is not managed by this TWS session ({', '.join(ib.managedAccounts())})."
    return ib, None


def _build_orders(ib, action, quantity, order_type, limit, take_profit, stop_loss, account):
    qty = int(float(quantity))
    common = dict(tif="GTC", transmit=False, account=account)
    algo = dict(algoStrategy="Adaptive", algoParams=[TagValue("adaptivePriority", "Normal")])
    if order_type == "market":
        return [MarketOrder(action, qty, **algo, **common)]
    if order_type == "limit":
        return [LimitOrder(action, qty, float(limit), **algo, **common)]
    reverse = "SELL" if action == "BUY" else "BUY"
    parent = LimitOrder(action, qty, float(limit), orderId=ib.client.getReqId(), **common)
    take = LimitOrder(reverse, qty, float(take_profit), orderId=ib.client.getReqId(),
                      parentId=parent.orderId, **common)
    stop = StopOrder(reverse, qty, float(stop_loss), orderId=ib.client.getReqId(),
                     parentId=parent.orderId, **common)
    return [parent, take, stop]


def quote_stock(sym, currency=None, exchange=None):
    """Bid / ask / last / close of a stock, live with frozen fallback.

    Returns dict(bid, ask, last, close, mid, mktdata_type, conId, currency) with
    None for a missing value, or None when TWS or the contract is unavailable.
    mktdata_type: 1 live, 2 frozen, 3 delayed, 4 delayed-frozen.
    """
    ib = safe_ib_connect()
    if not ib.isConnected():
        return None
    try:
        ib.RequestTimeout = REQUEST_TIMEOUT
        contract = _stock_contract(ib, sym, currency, exchange)
        if contract is None:
            logger.warning(f"quote_stock: {sym} did not qualify")
            return None
        ib.reqMarketDataType(1)
        ticker = ib.reqTickers(contract)[0]

        def px(v):
            return round(float(v), 4) if _finite(v) and float(v) > 0 else None
        bid, ask = px(ticker.bid), px(ticker.ask)
        return dict(bid=bid, ask=ask, last=px(ticker.last), close=px(ticker.close),
                    mid=round((bid + ask) / 2, 4) if bid and ask else None,
                    mktdata_type=int(getattr(ticker, "marketDataType", 0) or 0),
                    conId=int(contract.conId), currency=contract.currency)
    except Exception as e:
        logger.exception(f"quote_stock {sym} failed: {e}")
        return None
    finally:
        ib.disconnect()


def preview_stock_order(account, sym, action, quantity, order_type, limit=None,
                        take_profit=None, stop_loss=None, currency=None, exchange=None):
    """What TWS would do with the entry order, nothing placed (whatIfOrder).

    Returns dict(ok, message, init_margin_change, maint_margin_change,
    equity_with_loan_after, commission, commission_currency, warning).
    For a bracket, the entry (parent) order is previewed.
    """
    problem = _check_order(action, quantity, order_type, limit, take_profit, stop_loss)
    if problem:
        return dict(ok=False, message=problem)
    ib, err = _connect(account)
    if err:
        return dict(ok=False, message=err)
    try:
        contract = _stock_contract(ib, sym, currency, exchange)
        if contract is None:
            return dict(ok=False, message=f"{sym}: contract not found in TWS.")
        order = _build_orders(ib, action, quantity, order_type, limit, take_profit, stop_loss, account)[0]
        # TWS answers a whatIf only for an order marked to transmit; a whatIf
        # is never executed, so this sends nothing to the market
        order.transmit = True
        state = ib.whatIfOrder(contract, order)
        if not state or not hasattr(state, "initMarginChange"):
            return dict(ok=False, message="TWS returned no preview (order rejected or timed out).")
        return dict(ok=True, message=None,
                    init_margin_change=_num(state.initMarginChange),
                    maint_margin_change=_num(state.maintMarginChange),
                    equity_with_loan_after=_num(state.equityWithLoanAfter),
                    commission=_commission(state),
                    commission_currency=state.commissionCurrency or None,
                    warning=state.warningText or None)
    except Exception as e:
        logger.exception(f"preview_stock_order {sym} failed: {e}")
        return dict(ok=False, message=f"Preview failed: {type(e).__name__} {e}".strip())
    finally:
        ib.disconnect()


def stage_stock_order(account, sym, action, quantity, order_type, limit=None,
                      take_profit=None, stop_loss=None, currency=None, exchange=None):
    """Place the order(s) in TWS with transmit=False: staged, not sent.

    Returns dict(ok, message, order_ids, statuses).
    """
    problem = _check_order(action, quantity, order_type, limit, take_profit, stop_loss)
    if problem:
        return dict(ok=False, message=problem, order_ids=[], statuses=[])
    ib, err = _connect(account)
    if err:
        return dict(ok=False, message=err, order_ids=[], statuses=[])
    try:
        contract = _stock_contract(ib, sym, currency, exchange)
        if contract is None:
            return dict(ok=False, message=f"{sym}: contract not found in TWS.", order_ids=[], statuses=[])
        orders = _build_orders(ib, action, quantity, order_type, limit, take_profit, stop_loss, account)
        trades = []
        for order in orders:
            trades.append(ib.placeOrder(contract, order))
            ib.sleep(0.5)
        ib.sleep(1)
        ids = [int(t.order.orderId) for t in trades]
        statuses = [t.orderStatus.status or "" for t in trades]
        logger.info(f"Staged {order_type} {action} {quantity} {sym} on {account}: ids {ids} {statuses}")
        return dict(ok=True, message=None, order_ids=ids, statuses=statuses)
    except Exception as e:
        logger.exception(f"stage_stock_order {sym} failed: {e}")
        return dict(ok=False, message=f"Staging failed: {type(e).__name__} {e}".strip(), order_ids=[], statuses=[])
    finally:
        ib.disconnect()
