reticulate::source_python('~/RApplication/Tdata/inst/python/tdata_py/spread.py')

reticulate::repl_python()

res = compute_spread_risk_reward(
sym="QQQ",
trading_class="QQQ",
expiration="20260417",
current_price=628.58,
moneyness_pct=0.05,
spread_width=20,
right="P",
exchangeOpt="SMART")

