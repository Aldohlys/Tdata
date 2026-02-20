"""
=============================================================================
 POSITION SIZER — Generalized Monte Carlo position sizing engine
=============================================================================
 Determines optimal lot count for any option combo (strangle, vertical,
 diagonal, iron condor, butterfly, etc.) using Expected Shortfall (ES)
 risk budgeting and IVP/HVP regime analysis.

 Called from R via Tdata::sizePosition().

 Dependencies: numpy, scipy
=============================================================================
"""

import numpy as np
from scipy import stats as sp_stats

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURATION DEFAULTS
# ─────────────────────────────────────────────────────────────────────────────

DEFAULT_MAX_LOSS = 600.0
DEFAULT_PROFIT_TARGET = 0.50
DEFAULT_DTE_CUT = 21
DEFAULT_N_SIMS = 200_000
DEFAULT_DF_STUDENT = 5.0
DEFAULT_HVP_NEUTRAL = 50.0
DEFAULT_HVP_SCALING = 0.5
DEFAULT_IVP_HVP_BONUS_THRESH = 20.0
DEFAULT_IVP_HVP_MAX_BONUS = 0.30


# ─────────────────────────────────────────────────────────────────────────────
# BLACK-SCHOLES (vectorized)
# ─────────────────────────────────────────────────────────────────────────────

def _bs_vec(S, K, T, sigma, r, div, opt='call'):
    """Vectorized Black-Scholes price over array of spot prices S."""
    S = np.asarray(S, dtype=float)
    if T <= 1e-8:
        return np.maximum(0, S - K) if opt == 'call' else np.maximum(0, K - S)
    sqrt_T = np.sqrt(T)
    d1 = (np.log(S / K) + (r - div + 0.5 * sigma**2) * T) / (sigma * sqrt_T)
    d2 = d1 - sigma * sqrt_T
    disc = np.exp(-r * T)
    if opt == 'call':
        return S * np.exp(-div * T) * sp_stats.norm.cdf(d1) - K * disc * sp_stats.norm.cdf(d2)
    return K * disc * sp_stats.norm.cdf(-d2) - S * np.exp(-div * T) * sp_stats.norm.cdf(-d1)


# ─────────────────────────────────────────────────────────────────────────────
# COMBO VALUATION
# ─────────────────────────────────────────────────────────────────────────────

def _combo_val_vec(S, legs, day, sim_vol, r, div):
    """
    Compute combo value (per-unit) for array of spot prices S.

    Each leg contributes: pos * mul * BS_price
    Each leg has its own remaining DTE: leg_dte - day.
    sim_vol overrides per-leg sig for simulation pricing.

    Returns shape (n,) array.
    """
    S = np.asarray(S, dtype=float)
    total = np.zeros_like(S)
    for leg in legs:
        T_leg = max((leg['dte'] - day) / 365.0, 1e-8)
        opt = 'put' if leg['right'].upper() == 'P' else 'call'
        price = _bs_vec(S, leg['strike'], T_leg, sim_vol, r, div, opt)
        total += leg['pos'] * leg['mul'] * price
    return total


# ─────────────────────────────────────────────────────────────────────────────
# REGIME IVP/HVP
# ─────────────────────────────────────────────────────────────────────────────

def _compute_regime(iv, hv, ivp, hvp):
    """Compute vol adjustment and sizing multiplier from regime analysis."""
    # Vol adjustment: penalize HVP > 50
    hvp_adj = max(0, (hvp - DEFAULT_HVP_NEUTRAL) / DEFAULT_HVP_NEUTRAL)
    vol_mult = 1 + DEFAULT_HVP_SCALING * hvp_adj
    adj_vol = hv * vol_mult

    # Sizing multiplier: bonus/penalty based on IVP-HVP spread
    spread = ivp - hvp
    if spread > DEFAULT_IVP_HVP_BONUS_THRESH:
        max_sp = 80.0
        bonus = (spread - DEFAULT_IVP_HVP_BONUS_THRESH) / (max_sp - DEFAULT_IVP_HVP_BONUS_THRESH)
        sizing_mult = 1.0 + DEFAULT_IVP_HVP_MAX_BONUS * min(1.0, bonus)
    elif spread < 0:
        penalty = min(1.0, abs(spread) / 50.0)
        sizing_mult = 1.0 - 0.50 * penalty
    else:
        sizing_mult = 1.0

    # Regime label
    if spread >= 30 and hvp <= 40:
        regime = "IDEAL"
    elif spread >= 15 and hvp <= 60:
        regime = "FAVORABLE"
    elif spread >= 0 and hvp <= 70:
        regime = "NEUTRE"
    elif spread < 0 or hvp > 80:
        regime = "DANGEREUX"
    else:
        regime = "DEFAVORABLE"

    # Edge grade
    ratio = iv / hv if hv > 0 else 0
    if ratio >= 1.30:   grade = 'A'
    elif ratio >= 1.20: grade = 'B'
    elif ratio >= 1.10: grade = 'C'
    elif ratio >= 1.0:  grade = 'D'
    else:               grade = 'F'

    return {
        'adjusted_vol': adj_vol,
        'vol_multiplier': vol_mult,
        'sizing_multiplier': sizing_mult,
        'spread': spread,
        'regime': regime,
        'iv_hv_ratio': ratio,
        'edge_grade': grade,
        'vrp_pct': (iv - hv) / iv * 100 if iv > 0 else 0,
    }


# ─────────────────────────────────────────────────────────────────────────────
# MONTE CARLO SIMULATION
# ─────────────────────────────────────────────────────────────────────────────

def _simulate(spot, legs, credit, sim_vol, pricing_vol, hv, r, div,
              profit_target_pct, dte_cut, n_sims, df_student, seed=42):
    """
    Simulate P&L with management rules (profit target exit, DTE cut).

    Returns (pnl_array, exit_stats_dict).
    """
    min_dte = min(leg['dte'] for leg in legs)
    max_days = max(1, min_dte - dte_cut)
    dt = 1.0 / 365.0
    daily_vol = sim_vol * np.sqrt(dt)
    drift = (r - div - 0.5 * sim_vol**2) * dt
    scale = np.sqrt((df_student - 2) / df_student)

    rng = np.random.default_rng(seed)
    innovations = rng.standard_t(df=df_student, size=(n_sims, max_days)) * scale
    log_ret = drift + daily_vol * innovations
    paths = spot * np.exp(np.cumsum(log_ret, axis=1))

    # Average multiplier for credit-based profit target
    avg_mul = np.mean([leg['mul'] for leg in legs])

    # Profit target: exit when P&L reaches this fraction of max credit
    max_profit = credit * avg_mul
    target_pnl = profit_target_pct * max_profit

    pnl = np.zeros(n_sims)
    exit_day = np.full(n_sims, max_days, dtype=int)
    exit_reason = np.full(n_sims, 'dte_cut', dtype='U15')
    still_open = np.ones(n_sims, dtype=bool)

    for day in range(max_days):
        if not np.any(still_open):
            break
        cur = paths[still_open, day]
        d = day + 1
        # Blend vol: pricing_vol decays toward hv over time
        dte_rem = min_dte - d
        vp = hv + (pricing_vol - hv) * (dte_rem / min_dte)
        combo_val = _combo_val_vec(cur, legs, d, vp, r, div)
        # P&L = credit received + current combo value
        # combo_val is negative for short positions (pos=-1), approaches 0 as options decay
        # So P&L = credit*mul + combo_val: starts at ~0 (credit offsets initial value),
        # increases as options decay (combo_val approaches 0)
        day_pnl = credit * avg_mul + combo_val
        hit = day_pnl >= target_pnl
        oi = np.where(still_open)[0]
        ti = oi[hit]
        if len(ti) > 0:
            pnl[ti] = day_pnl[hit]
            exit_day[ti] = d
            exit_reason[ti] = 'profit_target'
            still_open[ti] = False

    # Close remaining at DTE cut
    if np.any(still_open):
        oi = np.where(still_open)[0]
        fp = paths[oi, max_days - 1]
        dte_rem = min_dte - max_days
        vc = hv + (pricing_vol - hv) * (dte_rem / min_dte)
        combo_val = _combo_val_vec(fp, legs, max_days, vc, r, div)
        pnl[oi] = credit * avg_mul + combo_val

    tp_pct = np.sum(exit_reason == 'profit_target') / n_sims * 100
    return pnl, {
        'tp_pct': tp_pct,
        'cut_pct': 100 - tp_pct,
        'avg_days': float(np.mean(exit_day)),
    }


def _risk_metrics(pnl):
    """Compute VaR and ES at 95% and 99%."""
    losses = -pnl
    out = {}
    for a in (0.95, 0.99):
        var = float(np.percentile(losses, a * 100))
        tail = losses[losses >= var]
        es = float(np.mean(tail)) if len(tail) > 0 else var
        out[a] = {'VaR': var, 'ES': es}
    return out


# ─────────────────────────────────────────────────────────────────────────────
# MAIN FUNCTION: size_position
# ─────────────────────────────────────────────────────────────────────────────

def size_position(
    legs,
    spot,
    credit,
    iv,
    hv,
    ivp,
    hvp,
    max_loss=DEFAULT_MAX_LOSS,
    r=0.0,
    div=0.0,
    n_sims=DEFAULT_N_SIMS,
    profit_target_pct=DEFAULT_PROFIT_TARGET,
    dte_cut=DEFAULT_DTE_CUT,
    df_student=DEFAULT_DF_STUDENT,
):
    """
    Determine optimal lot count for a credit option combo using Monte Carlo + ES.

    Only for credit strategies (short premium) where risk is undefined.
    For debit strategies, max risk = debit paid — no simulation needed.

    Parameters
    ----------
    legs : list of dicts
        Each dict has: strike, right (C/P), pos (+1/-1), dte, mul, sig
    spot : float
        Current underlying price
    credit : float
        Net credit received per unit (must be > 0)
    iv : float
        Annualized implied volatility (decimal, e.g. 0.25)
    hv : float
        Annualized historical volatility (decimal)
    ivp : float
        IV percentile rank (0-100)
    hvp : float
        HV percentile rank (0-100)
    max_loss : float
        Risk budget per trade in dollars
    r : float
        Risk-free rate (annualized)
    div : float
        Dividend yield (annualized)
    n_sims : int
        Number of Monte Carlo simulations
    profit_target_pct : float
        Exit at this fraction of max profit (0.50 = 50%)
    dte_cut : int
        Mandatory exit at this DTE
    df_student : float
        Student-t degrees of freedom for fat tails

    Returns
    -------
    dict with keys: lots, regime, scenarios, recommendation, inputs
    On error, returns dict with 'error' key.
    """
    # Credit must be positive — debit strategies have defined risk, no MC needed
    if credit <= 0:
        return {'error': f'credit must be > 0 (got {credit}). '
                         'Position sizer is for credit strategies only; '
                         'for debit trades, max risk = debit paid.'}

    # Auto-detect percentage vs decimal
    if iv > 1:
        iv /= 100
    if hv > 1:
        hv /= 100

    # Ensure legs are proper dicts (handle reticulate conversion)
    legs = [dict(leg) for leg in legs]
    for leg in legs:
        leg['strike'] = float(leg['strike'])
        leg['pos'] = int(leg['pos'])
        leg['dte'] = int(leg['dte'])
        leg['mul'] = float(leg['mul'])
        leg['sig'] = float(leg['sig'])
        leg['right'] = str(leg['right']).upper()

    min_dte = min(leg['dte'] for leg in legs)

    # Regime analysis
    reg = _compute_regime(iv, hv, ivp, hvp)

    # 3 scenarios
    scenarios = {}
    for label, svol in [('hv_base', hv), ('hv_adjusted', reg['adjusted_vol']), ('iv_conservative', iv)]:
        pnl, exit_stats = _simulate(
            spot, legs, credit, svol, iv, hv, r, div,
            profit_target_pct, dte_cut, n_sims, df_student,
        )
        risk = _risk_metrics(pnl)

        # Lot sizing from ES
        for a in (0.95, 0.99):
            es = risk[a]['ES']
            raw = max(0, int(np.floor(max_loss / es))) if es > 0 else 0
            if label != 'iv_conservative':
                adj = max(0, int(np.floor(raw * reg['sizing_multiplier'])))
                adj = min(adj, raw + int(np.ceil(raw * DEFAULT_IVP_HVP_MAX_BONUS)))
            else:
                adj = raw
            risk[a]['lots_raw'] = raw
            risk[a]['lots_adj'] = adj

        scenarios[label] = {
            'sim_vol': svol,
            'es95': risk[0.95]['ES'],
            'es99': risk[0.99]['ES'],
            'var95': risk[0.95]['VaR'],
            'var99': risk[0.99]['VaR'],
            'lots_raw': risk[0.99]['lots_raw'],
            'lots_adj': risk[0.99]['lots_adj'],
            'mean_pnl': float(np.mean(pnl)),
            'win_rate': float(np.mean(pnl > 0) * 100),
            'max_loss_sim': float(np.min(pnl)),
            'tp_pct': exit_stats['tp_pct'],
            'avg_days': exit_stats['avg_days'],
        }

    # Recommendation: hv_adjusted scenario, ES 99%
    reco_sc = scenarios['hv_adjusted']
    reco_lots = reco_sc['lots_adj']
    reco_es = reco_sc['es99']
    conserv_lots = scenarios['iv_conservative']['lots_adj']

    result = {
        'lots': reco_lots,
        'regime': reg,
        'scenarios': scenarios,
        'recommendation': {
            'lots': reco_lots,
            'es99_per_lot': reco_es,
            'total_es99': reco_es * reco_lots if reco_lots > 0 else 0,
            'total_credit': credit * np.mean([leg['mul'] for leg in legs]) * reco_lots,
            'win_rate': reco_sc['win_rate'],
            'mean_pnl_per_lot': reco_sc['mean_pnl'],
            'conservative_lots': conserv_lots,
        },
        'inputs': {
            'spot': spot,
            'legs': legs,
            'credit': credit,
            'iv': iv,
            'hv': hv,
            'ivp': ivp,
            'hvp': hvp,
            'max_loss': max_loss,
            'min_dte': min_dte,
        },
    }

    # Convert numpy types to Python natives for R/reticulate compatibility
    result = _to_python_types(result)

    return result


def _to_python_types(obj):
    """Recursively convert numpy types to Python natives."""
    if isinstance(obj, dict):
        return {k: _to_python_types(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [_to_python_types(v) for v in obj]
    elif isinstance(obj, (np.integer,)):
        return int(obj)
    elif isinstance(obj, (np.floating,)):
        return float(obj)
    elif isinstance(obj, np.ndarray):
        return obj.tolist()
    return obj
