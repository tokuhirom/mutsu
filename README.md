# ecosystem-data

The measurements of mutsu's ecosystem parity ledger (issue #7785): one JSON
record per zef distribution under `ecosystem/dists/`, the rolled-up
`ecosystem/summary.{json,md}`, the KPI history `ecosystem/history.{tsv,svg}`
and `ecosystem/index-snapshot.json`.

This is an orphan data branch, like `bench-data`. Only
`.github/workflows/ecosystem-sweep.yml` commits here, directly, with no pull
request; the hand-maintained inputs (`ecosystem/README.md`, `exclude.txt`,
`accepted-divergences.toml`) and every script stay on `main`.

In a checkout of `main`, `scripts/ecosystem-ledger.sh pull` puts these files
into the (gitignored) `ecosystem/` directory. The published view is
`site/ecosystem.html`. Method: `docs/ecosystem-parity.md`; decisions:
`docs/adr/0085-ecosystem-testsuite-parity-measurement.md` (2026-10-03
amendment).
