import sys
import math
import datetime
import locale
import numpy as np
import pandas as pd
import logging
from ib_async import *

# Import from other modules
from .contract import getOptValue
from .chains_manager import getOptionStrikes

# Create a dedicated logger for debug messages
debug_log = logging.getLogger("spread_debug")
debug_log.setLevel(logging.DEBUG)  # standard Logger works
handler = logging.StreamHandler()  # print to console
formatter = logging.Formatter("[%(levelname)s] %(message)s")
handler.setFormatter(formatter)
debug_log.addHandler(handler)
debug_log.propagate = False

import math
import logging
import pandas as pd

# ---------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------
log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)


# ---------------------------------------------------------------------
# Canonical DEBUG summary (single source of truth)
# ---------------------------------------------------------------------
def _log_spread_debug(
    right,
    spread_type,
    short_strike,
    long_strike,
    short_delta,
    net_premium,
    max_risk,
    max_reward,
    prob_delta,
    prob_market,
    ev,
):
    log.debug(
        f"[SPREAD] {right} {spread_type} | "
        f"short={short_strike} long={long_strike} | "
        f"delta={short_delta} | "
        f"net_premium={net_premium:.2f} | "
        f"risk={max_risk:.2f} reward={max_reward:.2f} | "
        f"p_delta={prob_delta} p_mkt={prob_market:.3f} | "
        f"EV={ev}"
    )



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
    exchangeOpt=None,
    force_refresh=False,
    market_data_type=2
):
    """
    Compute risk/reward, expected value, and probability of success for
    all vertical spreads (credit and debit) using IBKR option data.
    Includes:
      - Delta-implied probability of success
      - Market-implied probability of success (scaled by multiplier)
      - Expected value
      Compute risk/reward, expected value, and success probabilities for option vertical spreads.
      
      SUMMARY OF KEY CONCEPTS AND FORMULAS:
      
      1. Spread Types:
         - DEBIT: You pay premium to buy a spread (long call or long put). Max risk = premium paid.
         - CREDIT: You receive premium to sell a spread (short call or short put). Max reward = premium received.
      
      2. Delta-Implied Probability of Success:
         - Approximates risk-neutral probability of the spread achieving maximum profit based on option delta.
         - For DEBIT spreads: success occurs if short leg finishes ITM
             - prob_success_delta = delta_ITM (call delta or abs(put delta))
         - For CREDIT spreads: success occurs if short leg finishes OTM
             - prob_success_delta = 1 - delta_ITM (call delta or abs(put delta))
      
      3. Market-Implied Probability of Success:
         - Derived from the spread price under the assumption that expected value according to the market is zero.
         - For DEBIT spreads (you pay premium):
             EV_market = p_success * max_reward - (1 - p_success) * premium = 0
             => prob_success_market = premium / (premium + max_reward)
         - For CREDIT spreads (you receive premium):
             EV_market = p_success * premium - (1 - p_success) * max_risk = 0
             => prob_success_market = max_risk / (premium + max_risk)
         - This ensures probabilities are always between 0 and 1 and consistent with market pricing.
      
      4. Risk/Reward and Expected Value:
         - max_reward and max_risk are computed in the same units as the premium (scaled by multiplier).
         - Expected value uses delta-implied probability:
             expected_value = prob_success_delta * max_reward - (1 - prob_success_delta) * max_risk
         - Edge metric can be computed as:
             edge = prob_success_delta - prob_success_market
      
      5. Important Notes:
         - Delta-implied probability approximates the physical/risk-neutral probability for the short leg finishing ITM (or OTM for credit).
         - Market-implied probability is derived from the price of the spread assuming a simplified binary payoff (max reward or max risk) and expected value = 0.
         - For deep OTM debit spreads or tight credit spreads, delta and market probabilities can differ significantly due to skew, implied vol, or the triangular nature of vertical spread payoff.
         - Probabilities and expected value should always be computed using **the same units** for premium, max_reward, and max_risk (i.e., scaled by contract multiplier).
      
      6. Practical Interpretation:
         - High edge (prob_success_delta - prob_success_market) indicates spreads that may be mispriced relative to delta-implied risk-neutral probability.
         - Expected value may still be negative even if delta-implied probability is high, if max risk greatly exceeds max reward.
      
    """

    # --------------------------------------------------------------
    # Strike bounds
    # --------------------------------------------------------------
    strike_min = current_price * (1.0 - moneyness_pct)
    strike_max = current_price * (1.0 + moneyness_pct)

    strikes = getOptionStrikes(
        sym=sym,
        trading_class=trading_class,
        expiration=expiration,
        strike_min=strike_min,
        strike_max=strike_max,
        currency=currency,
        exchangeOpt=exchangeOpt,
        force_refresh=force_refresh,
    )

    if not strikes:
        log.warning("No strikes returned.")
        return pd.DataFrame()

    strikes = sorted(strikes)
    print(f"DEBUG SPREAD: Got {len(strikes)} strikes: {strikes}")

    # Check if any pairs can be formed with requested width
    available_widths = sorted(set(
        abs(strikes[i] - strikes[j])
        for i in range(len(strikes))
        for j in range(i + 1, len(strikes))
    ))
    has_valid_pairs = any(abs(w - spread_width) < 0.01 for w in available_widths)
    if not has_valid_pairs:
        log.warning(f"No strike pairs with width={spread_width}. Available widths: {available_widths}")
        print(f"DEBUG SPREAD: No pairs with width={spread_width}. Available widths: {available_widths}")
        return pd.DataFrame()

    # --------------------------------------------------------------
    # Fetch option prices & greeks
    # --------------------------------------------------------------
    # force_refresh reaches the QUOTES too, not just the chain: a bid/ask up to
    # 30 minutes old (the parquet quote TTL) says nothing about current
    # tradability. market_data_type=1 asks for live quotes for the same reason.
    opt_df = getOptValue(
        sym=sym,
        expiration=expiration,
        strikes=strikes,
        right=right,
        currency=currency,
        exchange=exchangeOpt,
        tradingClass=trading_class,
        force_refresh=force_refresh,
        market_data_type=market_data_type,
    )

    if opt_df is None or opt_df.empty:
        log.warning("No option pricing data returned.")
        return pd.DataFrame()

    print(f"DEBUG SPREAD: Option data:\n{opt_df.to_string()}")
    opt_df = opt_df.set_index("strike")

    results = []
    skipped_width = 0
    skipped_nan = 0

    # --------------------------------------------------------------
    # Enumerate spreads
    # --------------------------------------------------------------
    width_value = spread_width * multiplier

    for short_strike in strikes:
        for long_strike in strikes:

            diff = abs(short_strike - long_strike)
            if abs(diff - spread_width) > 0.01:
                continue

            if short_strike not in opt_df.index or long_strike not in opt_df.index:
                continue

            short_row = opt_df.loc[short_strike]
            long_row = opt_df.loc[long_strike]

            short_price = short_row["value"]
            long_price = long_row["value"]

            if pd.isna(short_price) or pd.isna(long_price):
                skipped_nan += 1
                print(f"DEBUG SPREAD: Skipping {short_strike}/{long_strike}: NaN prices (short={short_price}, long={long_price})")
                continue

            # --------------------------------------------------
            # Net premium
            # --------------------------------------------------
            net_premium = (short_price - long_price) * multiplier

            spread_type = "CREDIT" if net_premium > 0 else "DEBIT"

            if spread_type == "CREDIT":
                max_reward = abs(net_premium)
                max_risk = width_value - max_reward
            else:
                max_risk = abs(net_premium)
                max_reward = width_value - max_risk

            reward_risk_ratio = max_reward / max_risk


            # --------------------------------------------------
            # Market-implied probability of success (risk-neutral, EV = 0)
            # --------------------------------------------------
            prob_success_market = (
                max_risk / (max_risk + max_reward)
                if (max_risk + max_reward) > 0
                else np.nan
            )
            
            # --------------------------------------------------
            # Delta-implied probability of success
            # Edge and expected value
            # --------------------------------------------------
            short_delta = short_row.get("delta", np.nan)
            long_delta = long_row.get("delta", np.nan)

            # `spread` from getOptValue is the RELATIVE bid-ask width,
            # 2*(ask-bid)/(ask+bid) — i.e. (ask-bid)/mid. It is NaN exactly when
            # bid or ask is missing, which is also when `value` silently fell
            # back to last/close, so a NaN here means "no true mid" and a large
            # value means "wide market". Both are returned unfiltered: the caller
            # applies its own threshold and can report what it dropped.
            short_spread = short_row.get("spread", np.nan)
            long_spread = long_row.get("spread", np.nan)
            mktdata_type = short_row.get("mktdata_type", np.nan)

            if pd.isna(short_delta):
                prob_success_delta = np.nan
                edge = np.nan
                expected_value = np.nan
            else:
                prob_itm = abs(short_delta)

                if spread_type == "DEBIT":
                    prob_success_delta = prob_itm
                else:
                    prob_success_delta = 1.0 - prob_itm

                edge = prob_success_delta - prob_success_market

                expected_value = (
                    prob_success_delta * max_reward
                    - (1.0 - prob_success_delta) * max_risk
                )

            # --------------------------------------------------
            # Debug summary (single canonical log)
            # --------------------------------------------------
            _log_spread_debug(
                right,
                spread_type,
                short_strike,
                long_strike,
                short_delta,
                net_premium,
                max_risk,
                max_reward,
                prob_success_delta,
                prob_success_market,
                expected_value,
            )

            results.append(
                {
                    "right": right,
                    "spread_type": spread_type,
                    "short_strike": short_strike,
                    "long_strike": long_strike,
                    "short_delta": short_delta,
                    "long_delta": long_delta,
                    "short_spread": short_spread,
                    "long_spread": long_spread,
                    "mktdata_type": mktdata_type,
                    "width": spread_width,
                    "net_premium": net_premium,
                    "max_risk": max_risk,
                    "max_reward": max_reward,
                    "reward_risk_ratio": reward_risk_ratio,
                    "prob_success_delta": prob_success_delta,
                    "prob_success_market": prob_success_market,
                    "edge": edge,
                    "expected_value": expected_value,
                }
            )

    print(f"DEBUG SPREAD: Enumeration complete: {len(results)} spreads found, {skipped_nan} skipped (NaN prices)")
    return pd.DataFrame(results)
