import sys
import math
import datetime
import locale
import pandas as pd
import logging
from ib_insync import *

# Import from other modules
from .contract import getOptValue
from .chains_manager import getOptionStrikes
from fin_logger import get_logger, log_execution_time

# Créez un logger pour ce module
logger = get_logger("tdata_py.spread")

def compute_spread_risk_reward(
    sym,
    trading_class,
    expiration,
    current_price,
    moneyness_pct,
    spread_width,
    right,                     # 'C' or 'P'
    multiplier=100,
    currency=None,
    exchangeSec=None,
    exchangeOpt=None
):
    """
    Compute risk/reward, expected value, and probability of success for
    all vertical spreads (credit and debit) using IBKR option data.
    Includes:
      - Delta-implied probability of success
      - Market-implied probability of success (scaled by multiplier)
      - Expected value
    """

    # -----------------------------
    # Strike bounds
    # -----------------------------
    strike_min = current_price * (1 - moneyness_pct)
    strike_max = current_price * (1 + moneyness_pct)

    strikes = getOptionStrikes(
        sym=sym,
        trading_class=trading_class,
        expiration=expiration,
        strike_min=strike_min,
        strike_max=strike_max,
        secType="OPT",
        currency=currency,
        exchangeSec=exchangeSec,
        exchangeOpt=exchangeOpt
    )

    if not strikes:
        return pd.DataFrame()

    strikes = sorted(strikes)
    strike_set = set(strikes)

    # -----------------------------
    # Option prices + greeks
    # -----------------------------
    opt_df = getOptValue(
        sym=sym,
        expiration=expiration,
        strikes=strikes,
        right=right,
        currency=currency,
        exchange=exchangeOpt,
        tradingClass=trading_class
    )

    if opt_df is None or opt_df.empty:
        return pd.DataFrame()

    opt_df = opt_df.set_index("strike")
    results = []

    # -----------------------------
    # Build vertical spreads
    # -----------------------------
    for k1 in strikes:
        k2 = k1 + spread_width
        if k2 not in strike_set:
            continue

        width = abs(k2 - k1)

        # Evaluate BOTH orientations
        for short_strike, long_strike in [(k1, k2), (k2, k1)]:

            short_price = opt_df.loc[short_strike, "value"]
            long_price  = opt_df.loc[long_strike,  "value"]
            short_delta = opt_df.loc[short_strike, "delta"]

            net_premium = short_price - long_price

            # -----------------------------
            # Determine spread type and risk/reward
            # -----------------------------
            if net_premium > 0:
                # CREDIT spread
                spread_type = "CREDIT"
                credit = net_premium * multiplier
                max_reward = credit
                max_risk   = (width * multiplier - credit)
                if max_risk <= 0:
                    continue
            else:
                # DEBIT spread
                spread_type = "DEBIT"
                debit = abs(net_premium) * multiplier
                max_risk   = debit
                max_reward = (width * multiplier - debit)
                if max_reward <= 0:
                    continue

            # -----------------------------
            # Delta-implied probability of success
            # -----------------------------
            if right == "C":
                delta_itm = max(0.0, min(1.0, short_delta))
            else:  # Put
                delta_itm = max(0.0, min(1.0, abs(short_delta)))

            if spread_type == "DEBIT":
                # Debit succeeds if short leg ITM
                prob_success_delta = delta_itm
            else:
                # Credit succeeds if short leg OTM
                prob_success_delta = 1.0 - delta_itm

            # -----------------------------
            # Market-implied probability of success
            # -----------------------------
            if spread_type == "CREDIT":
                # Credit received = max_reward, risk = max_risk
                prob_success_market = max_risk / (max_reward + max_risk)
            else:  # DEBIT
                # Debit paid = max_risk, reward = max_reward
                prob_success_market = max_risk / (max_risk + max_reward)


            # -----------------------------
            # Expected value (delta-weighted)
            # -----------------------------
            expected_value = prob_success_delta * max_reward - (1 - prob_success_delta) * max_risk

            results.append({
                "right": right,
                "spread_type": spread_type,
                "short_strike": short_strike,
                "long_strike": long_strike,
                "width": width,
                "net_premium": round(net_premium, 3),
                "max_risk": round(max_risk, 2),
                "max_reward": round(max_reward, 2),
                "reward_risk_ratio": round(max_reward / max_risk, 3),
                "prob_success_delta": round(prob_success_delta, 3),
                "prob_success_market": round(prob_success_market, 3),
                "edge": round(prob_success_delta - prob_success_market, 3),
                "expected_value": round(expected_value, 2)
            })

    return pd.DataFrame(results).sort_values("reward_risk_ratio").reset_index(drop=True)
