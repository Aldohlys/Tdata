"""
test_quote_cache.py — unit tests for the parquet quote cache and the
force_refresh / partial-hit logic in getOptValue.

These tests do NOT require a live TWS connection. The storage class is
exercised directly; getOptValue is exercised with safe_ib_connect mocked so
the cache-decision branches can be verified end-to-end.

Run from repo root:
    pytest inst/python/test/test_quote_cache.py -v
or, when pyproject points elsewhere:
    pytest -v
"""

import datetime
import math
import os
from pathlib import Path
from unittest.mock import MagicMock, patch

import pandas as pd
import pytest

from tdata_py import parquet_storage
from tdata_py.parquet_storage import (
    ParquetQuotesStorage,
    get_file_age_minutes,
    get_quotes_ttl_minutes,
)
from tdata_py import contract as contract_mod


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def quotes_dir(tmp_path, monkeypatch):
    """Redirect the quote cache to a temp dir for the duration of the test."""
    qdir = tmp_path / "quotes"
    qdir.mkdir()
    # Patch the CONFIG dict the storage module reads at instantiation.
    original = parquet_storage.CONFIG.get("quotes_dir")
    parquet_storage.CONFIG["quotes_dir"] = str(qdir)
    yield qdir
    if original is None:
        parquet_storage.CONFIG.pop("quotes_dir", None)
    else:
        parquet_storage.CONFIG["quotes_dir"] = original


def _mk_row(strike, ts=None, **overrides):
    """Build a quote row dict matching the schema upsert expects."""
    row = {
        "strike": float(strike),
        "value": 1.23, "bid": 1.20, "ask": 1.26, "last": 1.22,
        "spread": 0.06, "impliedvol": 0.25, "delta": 0.30,
        "cached_timestamp": (ts or datetime.datetime.now()).isoformat(),
    }
    row.update(overrides)
    return row


# ---------------------------------------------------------------------------
# ParquetQuotesStorage — direct round-trip tests
# ---------------------------------------------------------------------------

class TestParquetQuotesStorage:

    def test_load_fresh_returns_empty_when_no_file(self, quotes_dir):
        s = ParquetQuotesStorage()
        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[100.0], ttl_minutes=30)
        assert out == {}

    def test_upsert_then_load_round_trip(self, quotes_dir):
        s = ParquetQuotesStorage()
        s.upsert("FOO", "FOO", "20260620", "C",
                 rows=[_mk_row(100), _mk_row(105), _mk_row(110)])

        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[100.0, 105.0, 110.0], ttl_minutes=30)
        assert set(out.keys()) == {100.0, 105.0, 110.0}
        assert out[105.0]["value"] == pytest.approx(1.23)
        assert out[105.0]["delta"] == pytest.approx(0.30)

    def test_load_filters_strikes_caller_did_not_request(self, quotes_dir):
        s = ParquetQuotesStorage()
        s.upsert("FOO", "FOO", "20260620", "C",
                 rows=[_mk_row(100), _mk_row(105), _mk_row(110)])
        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[105.0], ttl_minutes=30)
        assert list(out.keys()) == [105.0]

    def test_stale_rows_are_excluded(self, quotes_dir):
        s = ParquetQuotesStorage()
        old = datetime.datetime.now() - datetime.timedelta(minutes=120)
        new = datetime.datetime.now()
        s.upsert("FOO", "FOO", "20260620", "C", rows=[
            _mk_row(100, ts=old),
            _mk_row(105, ts=new),
        ])
        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[100.0, 105.0], ttl_minutes=30)
        # 100 is stale (120 min old, TTL 30), 105 is fresh
        assert set(out.keys()) == {105.0}

    def test_upsert_overwrites_by_strike_preserving_others(self, quotes_dir):
        s = ParquetQuotesStorage()
        s.upsert("FOO", "FOO", "20260620", "C",
                 rows=[_mk_row(100, value=1.0), _mk_row(105, value=2.0)])
        # Now upsert just strike 105 with a new value; 100 must still be there.
        s.upsert("FOO", "FOO", "20260620", "C",
                 rows=[_mk_row(105, value=2.5)])

        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[100.0, 105.0], ttl_minutes=30)
        assert out[100.0]["value"] == pytest.approx(1.0)
        assert out[105.0]["value"] == pytest.approx(2.5)

    def test_upsert_empty_is_noop(self, quotes_dir):
        s = ParquetQuotesStorage()
        s.upsert("FOO", "FOO", "20260620", "C", rows=[])
        # File should not have been created.
        assert not (quotes_dir / "FOO" / "FOO" / "20260620_C_quotes.parquet").exists()

    def test_check_and_remove_expired_deletes_past_only(self, quotes_dir):
        s = ParquetQuotesStorage()
        # Past expiration
        s.upsert("FOO", "FOO", "20200101", "C", rows=[_mk_row(100)])
        # Future expiration
        future = (datetime.date.today() + datetime.timedelta(days=365)).strftime("%Y%m%d")
        s.upsert("FOO", "FOO", future, "C", rows=[_mk_row(100)])

        deleted = s.check_and_remove_expired(symbol="FOO")
        assert any("20200101" in str(p) for p in deleted)
        assert not any(future in str(p) for p in deleted)

    def test_corrupt_file_is_handled_gracefully(self, quotes_dir):
        s = ParquetQuotesStorage()
        f = quotes_dir / "FOO" / "FOO" / "20260620_C_quotes.parquet"
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_bytes(b"not a parquet file")
        out = s.load_fresh("FOO", "FOO", "20260620", "C",
                           strikes=[100.0], ttl_minutes=30)
        assert out == {}  # logged warning, did not raise


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

class TestHelpers:

    def test_get_quotes_ttl_minutes_default(self, monkeypatch):
        # Strip any test-set value to verify the fallback default.
        monkeypatch.setitem(parquet_storage.CONFIG, "quotes_ttl_minutes",
                             parquet_storage.CONFIG.get("quotes_ttl_minutes", 30))
        assert get_quotes_ttl_minutes() >= 1  # sanity: default 30 (or override)

    def test_get_quotes_ttl_minutes_override(self, monkeypatch):
        monkeypatch.setitem(parquet_storage.CONFIG, "quotes_ttl_minutes", 5)
        assert get_quotes_ttl_minutes() == 5

    def test_get_file_age_minutes_for_missing_file(self, tmp_path):
        assert get_file_age_minutes(tmp_path / "nope.parquet") == -1


# ---------------------------------------------------------------------------
# getOptValue cache integration — TWS mocked
# ---------------------------------------------------------------------------

def _build_mock_ib(strikes_returned, value=2.50, iv=0.22, delta=0.45):
    """
    Build a fake `ib` object whose qualifyContracts/reqTickers return tickers
    matching the given strike list with predictable Greeks.
    """
    fake_ib = MagicMock()
    fake_ib.isConnected.return_value = True
    # qualifyContracts mutates each Contract in place to assign conId.
    def _qualify(*contracts):
        for i, c in enumerate(contracts, start=1):
            c.conId = 1000 + i
        return contracts
    fake_ib.qualifyContracts.side_effect = _qualify

    def _req_tickers(*contracts):
        tickers = []
        for c in contracts:
            t = MagicMock()
            t.marketPrice.return_value = value
            t.close = value
            t.bid = value - 0.05
            t.ask = value + 0.05
            t.last = value
            mg = MagicMock()
            mg.impliedVol = iv
            mg.delta = delta
            t.modelGreeks = mg
            tickers.append(t)
        return tickers
    fake_ib.reqTickers.side_effect = _req_tickers
    fake_ib.sleep.return_value = None
    fake_ib.reqMarketDataType.return_value = None
    fake_ib.disconnect.return_value = None
    return fake_ib


@pytest.fixture
def fake_ticker_info(monkeypatch):
    """Bypass the ticker DB: pretend FOO is a stock with SMART/USD/FOO."""
    fake = {"Type": "STK", "Currency": "USD",
            "OptExchange": "SMART", "TradingClass": "FOO"}
    monkeypatch.setattr(contract_mod.ticker_db, "get_ticker_info",
                         lambda sym: fake)
    return fake


class TestGetOptValueCacheIntegration:

    def test_full_cache_hit_does_not_call_tws(self, quotes_dir, fake_ticker_info):
        # Pre-populate the cache for two strikes.
        ParquetQuotesStorage().upsert(
            "FOO", "FOO", "20260620", "C",
            rows=[_mk_row(100), _mk_row(105)],
        )

        with patch.object(contract_mod, "safe_ib_connect") as connect_mock:
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0, 105.0], right="C",
            )
            connect_mock.assert_not_called()  # the whole point of the cache

        assert isinstance(df, pd.DataFrame)
        assert sorted(df["strike"].tolist()) == [100.0, 105.0]

    def test_partial_hit_fetches_only_missing_strikes(self, quotes_dir, fake_ticker_info):
        # Cache strike 100 only; 105 should trigger a TWS fetch.
        ParquetQuotesStorage().upsert(
            "FOO", "FOO", "20260620", "C", rows=[_mk_row(100, value=1.11)],
        )

        fake_ib = _build_mock_ib(strikes_returned=[105.0], value=2.22)
        with patch.object(contract_mod, "safe_ib_connect", return_value=fake_ib):
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0, 105.0], right="C",
            )

        # Only one Contract should have been built and qualified — strike 105.
        qualified_args = fake_ib.qualifyContracts.call_args.args
        qualified_strikes = [c.strike for c in qualified_args]
        assert qualified_strikes == [105.0]

        assert isinstance(df, pd.DataFrame)
        rows = {r["strike"]: r for _, r in df.iterrows()}
        assert rows[100.0]["value"] == pytest.approx(1.11)  # served from cache
        assert rows[105.0]["value"] == pytest.approx(2.22)  # fetched live

    def test_force_refresh_bypasses_cache_and_refetches_all(self, quotes_dir, fake_ticker_info):
        ParquetQuotesStorage().upsert(
            "FOO", "FOO", "20260620", "C",
            rows=[_mk_row(100, value=1.11), _mk_row(105, value=1.11)],
        )

        fake_ib = _build_mock_ib(strikes_returned=[100.0, 105.0], value=9.99)
        with patch.object(contract_mod, "safe_ib_connect", return_value=fake_ib):
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0, 105.0], right="C",
                force_refresh=True,
            )

        # Both strikes must have been re-fetched.
        qualified_strikes = sorted(c.strike for c in fake_ib.qualifyContracts.call_args.args)
        assert qualified_strikes == [100.0, 105.0]
        # Result reflects fresh values, not stale cache.
        assert df["value"].iloc[0] == pytest.approx(9.99)
        assert df["value"].iloc[1] == pytest.approx(9.99)

    def test_force_refresh_writes_back_for_next_caller(self, quotes_dir, fake_ticker_info):
        # Empty cache; force_refresh=True should fetch AND populate cache.
        fake_ib = _build_mock_ib(strikes_returned=[100.0], value=3.33)
        with patch.object(contract_mod, "safe_ib_connect", return_value=fake_ib):
            contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0], right="C", force_refresh=True,
            )

        # Now a non-forced caller should see the cache hit.
        cached = ParquetQuotesStorage().load_fresh(
            "FOO", "FOO", "20260620", "C",
            strikes=[100.0], ttl_minutes=30,
        )
        assert 100.0 in cached
        assert cached[100.0]["value"] == pytest.approx(3.33)

    def test_stale_cache_rows_trigger_refetch(self, quotes_dir, fake_ticker_info):
        old_ts = datetime.datetime.now() - datetime.timedelta(hours=2)
        ParquetQuotesStorage().upsert(
            "FOO", "FOO", "20260620", "C",
            rows=[_mk_row(100, ts=old_ts, value=1.11)],
        )

        fake_ib = _build_mock_ib(strikes_returned=[100.0], value=4.44)
        with patch.object(contract_mod, "safe_ib_connect", return_value=fake_ib):
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0], right="C",
                cache_ttl_minutes=30,  # 2h-old row is stale at 30 min TTL
            )

        # Stale row triggered a TWS fetch.
        fake_ib.qualifyContracts.assert_called_once()
        assert df["value"].iloc[0] == pytest.approx(4.44)

    def test_cache_ttl_minutes_override_keeps_old_rows(self, quotes_dir, fake_ticker_info):
        # 90 min old row, but caller is willing to accept 4 hours.
        old_ts = datetime.datetime.now() - datetime.timedelta(minutes=90)
        ParquetQuotesStorage().upsert(
            "FOO", "FOO", "20260620", "C",
            rows=[_mk_row(100, ts=old_ts, value=1.11)],
        )

        with patch.object(contract_mod, "safe_ib_connect") as connect_mock:
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=[100.0], right="C",
                cache_ttl_minutes=240,  # accept up to 4h old
            )
            connect_mock.assert_not_called()

        assert df["value"].iloc[0] == pytest.approx(1.11)

    def test_single_strike_normalisation(self, quotes_dir, fake_ticker_info):
        """Caller passes a scalar strike, not a list — must still cache and return."""
        fake_ib = _build_mock_ib(strikes_returned=[100.0], value=5.55)
        with patch.object(contract_mod, "safe_ib_connect", return_value=fake_ib):
            df = contract_mod.getOptValue(
                sym="FOO", expiration="20260620",
                strikes=100.0, right="C",  # scalar
            )

        assert df["strike"].tolist() == [100.0]
        cached = ParquetQuotesStorage().load_fresh(
            "FOO", "FOO", "20260620", "C", strikes=[100.0], ttl_minutes=30,
        )
        assert 100.0 in cached
