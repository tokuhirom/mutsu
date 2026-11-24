# Releases move to a monthly cadence gated on ecosystem parity

After the 2026-11-28 release, mutsu releases about once a month. A release ships only when a gate
holds:

- the ecosystem ledger's headline dist parity is 100%;
- its "Regressed since the previous ledger" section is empty;
- no `release:A` issue is open.

A month that misses the gate skips its release. The rule and the next cycle's priorities are
recorded in `PLAN.md` ("Release cadence and gate", "Next cycle"). The `cut-release` skill now
checks the gate as its step 0, before a version is picked.
