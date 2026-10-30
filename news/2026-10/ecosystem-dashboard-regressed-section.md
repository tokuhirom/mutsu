# The ecosystem dashboard lists what regressed since the last sweep

`scripts/ecosystem-dashboard.py` now compares the ledger on disk with the previous ledger commit
(`ecosystem/.ledger-ref~1` on the `ecosystem-data` branch, or `--previous-ref REF`) and renders a
"Regressed since the last sweep" section: every distribution measured at the same version whose
status got worse (`green` to `partial`/`red`/`blocked_load`, ...) or that lost a `parity`/`accepted`
file (a file that only became `no_baseline` is not a mutsu change and is left out). Run on the
2026-10-27 and 2026-10-28 ledgers it reports exactly the five regressions that #15438 recorded by
hand: BSON::Simple, Business::CreditCard, Grammar::Extractor, Grid and Math::Root. In GitHub Actions
the one missing commit of the shallow ledger fetch is deepened first; where no history is available
the section says so. A regression no longer needs a person to diff two ledger commits (#15603).
