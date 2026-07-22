# doc-diff sweep — committed raw data

This directory holds the **raw output of the most recent full-corpus doc-diff
sweep**, checked in so a future session can read the per-file minimal repros
without re-running the ~15-minute sweep.

- `summary.txt` — corpus totals + every signal file ranked by `mismatch + crash`
  (high-signal first). One line per file: `<total> mism=N crash=N drift=N <path>`.
- `progress.txt` — one stats line per scanned file (all 444, including the
  zero-signal ones).
- `reports/<sanitized-path>.txt` — the full harness report for one doc file:
  each divergence as a ready-made minimal repro with the program, raku stdout,
  and mutsu stdout/stderr. `/` in the doc path becomes `__` in the filename
  (e.g. `Type/IO/Path.rakudoc` → `Type__IO__Path.txt`).

## Refreshing

```
cargo build
scripts/doc-diff-sweep.sh -j8 -o tmp/sweep-final     # ~15 min, writes to tmp/ (gitignored)
rm -rf docs/doc-diff-sweep/{summary.txt,progress.txt,reports}
cp tmp/sweep-final/{summary.txt,progress.txt} docs/doc-diff-sweep/
cp -r tmp/sweep-final/reports docs/doc-diff-sweep/reports
```

Then regenerate the survey table + Corpus snapshot in
[../doc-diff-backlog.md](../doc-diff-backlog.md) from the new `summary.txt`, and
update the totals in [PLAN.md](../../PLAN.md) §8.1.

**Always re-sweep on the current `main` before trusting a row** — a report goes
stale as soon as a fix lands, and a sweep run against a stale binary reports
already-fixed examples as still-failing (seen 2026-07-22, where the pre-#5238
binary mislabelled the big-FatRat `numerics.rakudoc` rows). See the harness method
doc: [../qa-doc-diff-harness.md](../qa-doc-diff-harness.md).
