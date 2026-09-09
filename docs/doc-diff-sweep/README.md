# doc-diff sweep — committed raw data

This directory holds the **raw output of the most recent full-corpus doc-diff
sweep**, checked in so a future session can read the per-file minimal repros
without re-running the sweep (~35 min at `-j4` on a 4-core remote container;
~15 min at `-j8` on the 12-core box).

- `summary.txt` — corpus totals + every signal file ranked by `mismatch + crash`
  (high-signal first). One line per file:
  `<total> mism=N crash=N nondet=N <path>`.
- `progress.txt` — one stats line per scanned file (all 443, including the
  zero-signal ones).
- `reports/<sanitized-path>.txt` — the full harness report for one doc file:
  each divergence as a ready-made minimal repro with the program, raku stdout,
  and mutsu stdout/stderr. `/` in the doc path becomes `__` in the filename
  (e.g. `Type/IO/Path.rakudoc` → `Type__IO__Path.txt`). **Only the signal files
  named in `summary.txt` are kept** — the zero-signal reports carry no
  information and would triple the directory.

Current contents: the **2026-09-09** sweep (debug `mutsu` at `a44bd28`, `raku`
v2026.07).

## Refreshing

```
cargo build
scripts/doc-diff-sweep.sh -j8 -o tmp/sweep-final     # writes to tmp/ (gitignored)
rm -rf docs/doc-diff-sweep/{summary.txt,progress.txt,reports}
mkdir -p docs/doc-diff-sweep/reports
cp tmp/sweep-final/{summary.txt,progress.txt} docs/doc-diff-sweep/
awk '/ raku-doc\// { print $NF }' tmp/sweep-final/summary.txt \
  | sed 's|raku-doc/doc/||; s|/|__|g; s|\.rakudoc$|.txt|' \
  | while read f; do cp "tmp/sweep-final/reports/$f" docs/doc-diff-sweep/reports/; done
```

Captured output is capped by the harness itself (#7590): each section is cut to
40 lines with an explicit `... [truncated by doc-diff-harness: N more line(s) of M]`
marker, and every block runs in a scratch directory rather than the repo root.
Before that, the recipe above was not safe as written — the 2026-09-07b sweep
needed 11 MB → 412 KB of truncation BY HAND (one `sub MAIN` that `.dir`-walks
its cwd produced a 131_492-line, 8.4 MB report), and it left stray `bar` /
`foo.txt` files behind. With the cap in place the whole 2026-09-09 sweep is
1.9 MB across 442 reports, of which the 80 signal reports committed here are
500 KB.

Then regenerate the survey table + Corpus snapshot in
[../doc-diff-backlog.md](../doc-diff-backlog.md) from the new `summary.txt`:

```
awk '/ raku-doc\// { file=$NF; sub("raku-doc/doc/","",file);
                     m=$2; c=$3; n=$4; sub("mism=","",m); sub("crash=","",c); sub("nondet=","",n);
                     printf "| %s | %s | %s | %s |\n", file, m, c, n }' \
  docs/doc-diff-sweep/summary.txt
```

**Always re-sweep on the current `main` before trusting a row** — a report goes
stale as soon as a fix lands, and a sweep run against a stale binary reports
already-fixed examples as still-failing (seen 2026-07-22, where the pre-#5238
binary mislabelled the big-FatRat `numerics.rakudoc` rows). See the harness
method doc: [../qa-doc-diff-harness.md](../qa-doc-diff-harness.md).

**Two counting notes when comparing sweeps.** The `raku-drift-from-doc` bucket
was retired by #7590, so its findings now show up under `mismatch` — a
post-#7590 `mismatch` count is not comparable to a pre-#7590 one. And the
`nondet` column counts blocks dropped because the *oracle* disagreed with
itself across two runs; it is the noise floor, not a finding.
