# Ecosystem parity — measuring mutsu against rakudo, one distribution at a time

The operations manual for mutsu's headline compatibility KPI: run every zef
distribution's own test suite under **both rakudo and mutsu**, record both
sides, and publish the difference.

- **Why it is shaped this way**: [ADR-0085](adr/0085-ecosystem-testsuite-parity-measurement.md)
  — the decisions (rakudo as the denominator, flat dependency closures,
  one file per distribution, the sandbox and environment contract). Read it
  before changing the method; this file is the *how*.
- **Tracking issue**: [#7785](https://github.com/tokuhirom/mutsu/issues/7785).
- **Sibling tools**: [docs/dist-compat-sweep.md](dist-compat-sweep.md) is the
  load-level (`use <module>`) diagnostic sampler that feeds root-cause tickets;
  [BATTERIES.md](../BATTERIES.md) / `scripts/battery-testsuite.sh` is the
  release gate on the ~40 *bundled* dists. This campaign is neither: it is an
  exhaustive, re-runnable ledger over the whole ecosystem.

> **Status: design only.** Nothing in this document is implemented yet. The
> phases in §7 are the build order; each is one PR.

## 1. What gets measured

| term | meaning |
|---|---|
| **distribution** (dist) | the unit of measurement — one `Dist::Name` in the fez index, with a version, a tarball and a `META6.json` |
| **module** | one entry in a dist's `provides` |
| **test file** | the unit of comparison — one `t/**.t` / `t/**.rakutest`, run as one process |
| **baseline file** | a test file **rakudo passes cleanly** in this sweep; only these count toward the KPI |

Corpus: the fez index (`https://360.zef.pm/`, cached at
`~/.zef/store/fez/fez.json`), latest version of each dist. As of the 2026-09-10
snapshot: **1623 distributions**, 9214 provided module names.

### The three numbers

With *B* = the set of baseline files:

- **`file_parity`** = `count(f in B where mutsu passes f) / count(B)` —
  **the KPI**, the number to move.
- **`assertion_parity`** = `sum over B of min(mutsu_ok, raku_ok) / sum over B of
  raku_ok` — moves even when no file flips, so a sweep that halves a suite's
  failures is visible before the file turns green.
- **`dist_parity`** = `count(dists whose whole baseline set passes on mutsu) /
  count(dists with a non-empty baseline set)` — the user-facing "which modules
  work" number.

Per-file comparison verdicts (`cmp` in the record):

| `cmp` | meaning | in the KPI? |
|---|---|---|
| `parity` | rakudo passes, mutsu passes | yes (numerator) |
| `partial` | rakudo passes, mutsu reaches the plan but fails some assertions | yes (denominator only) |
| `regression` | rakudo passes, mutsu fails / dies / times out | yes (denominator only) — **the actionable bucket** |
| `no_baseline` | rakudo does not pass cleanly | no |
| `mutsu_better` | rakudo fails, mutsu passes | no (recorded, reported separately) |

Dist-level `status`: `green` · `partial` · `red` · `no_baseline` · `blocked_load`
(a provided module does not even `use`) · `blocked_dep` (§3) · `skipped`.

## 2. The fairness contract

Both interpreters must be running the same code under the same conditions, or
the difference is not a measurement. The harness enforces all of this rather
than leaving it to the operator:

1. **Same everything per file**: same dist tarball and version, same `-I` list,
   same working directory (the dist root — suites reach for fixtures by relative
   path), same environment, same wall-clock timeout, same order.
2. **No installed repositories on either side.** Dependencies arrive only as
   `-I` paths (§3), so module resolution and precompilation behave the same way
   for both.
3. **`MUTSU_FUDGE` must be unset.** It is roast-only; a stray `#?rakudo skip` in
   a dist would silently drop the next statement.
4. **`use Test` must resolve to the vendored `Test.rakumod`** on the mutsu side
   — `modules/Rakudo-Core/lib/Test.rakumod`, rakudo's own file, which has been
   the only provider since the native TAP provider was retired on 2026-09-10
   ([#7566](https://github.com/tokuhirom/mutsu/issues/7566)). This is the
   precondition #7785 set for starting the campaign at all: both sides must be
   counting `ok` lines emitted by the *same* `Test` implementation, or every
   number is comparing two harnesses rather than two interpreters. A sweep that
   cannot confirm it (a regression, or a `-I` dependency shadowing `Test`)
   aborts.
5. **Both sides sandboxed identically** — bubblewrap, no network, read-only
   filesystem, throwaway HOME, own PID namespace, rlimits (the wrapper
   `scripts/dist-compat-sweep.py` already uses). *Both*, because a suite that
   needs the network must fail symmetrically rather than showing up as a mutsu
   regression. A corpus sweep without a sandbox is not a supported mode.
6. **Author tests are excluded by default.** `xt/` is style/pod/meta checking of
   the dist itself, not of the language; `t/` and the dist's declared test paths
   are what `zef test` runs. The exclusion is recorded per dist so it can be
   audited (`--include-xt` re-enables it).
7. **Flakiness must not move the KPI.** A file that fails on *either* side is
   re-run up to 3 times and the best verdict is kept; a file whose verdict
   varies across attempts is marked `flaky: true` and excluded from the KPI on
   both sides. Only failures are retried, so the cost is proportional to the
   problem.

## 3. Dependencies — a flat `-I` closure, resolved offline

Per ADR-0085 D4/D5:

1. Read `depends` + `test-depends` from the index entry (and from the extracted
   `META6.json`, which is authoritative when they disagree).
2. Resolve each name against the index: first as a dist name, then through the
   index's module→dist `provides` map. Names the compiler provides (`Test`,
   `NativeCall`, `nqp`, …) and `:from<native>` / `:from<bin>` entries are not
   dependencies.
3. Recurse to a fixed point; download and extract each member (cached under
   `~/.cache/mutsu-ecosystem/`); build one `-I <dep>/lib` list.
4. Hand that identical list to **both** interpreters.

**If the closure cannot be completed, the dist is `blocked_dep` and runs on
neither side.** Letting mutsu's bundled batteries (`modules/`, which sit below
`-I` in the precedence chain) quietly supply a dependency rakudo lacks would
raise the KPI without any compatibility improving. The record instead notes
`bundled_would_supply: [...]` — a batteries statistic, reported separately.

Measured on the 2026-09-10 snapshot: **1383 / 1623 (85%)** of dists have a
first-level dependency list that resolves entirely from the fez index; 816 have
no runtime or test dependencies at all. The top unresolvable names are legacy
p6c/CPAN-era dists (`Distribution::Builder::MakeFromJSON` ×25, `Terminal::ANSI`
×19, `Hash::Merge` ×16, `JSON::Tiny` ×15, `UUID`, `File::Which`, `Base64`).
Adding the [Raku Ecosystem Archive](https://github.com/Raku/REA) index as a
second source would recover most of them; that is a follow-up (§8), not a
phase-1 requirement.

## 4. The data store

```
ecosystem/
  README.md                     # what this is and how to read it (points here)
  index-snapshot.json           # fez index provenance: fetch date, sha256, dist count
  dists/<S>/<Dist--Name>.json   # ONE FILE PER DIST — the record
  summary.json                  # generated rollup: the three KPIs + per-shard counts
  summary.md                    # generated human-readable table
  history.tsv                   # append-only, one row per full sweep — the KPI time series
```

`<S>` is the uppercased first character of the dist name (`_` when it is not an
ASCII letter); `::` becomes `--` in the filename. `String::Utils` →
`ecosystem/dists/S/String--Utils.json`.

One file per dist is the merge-conflict answer (the same reasoning as
`news/YYYY-MM/<slug>.md`, and the reason `docs/dist-compat-sweep.md` moved to
one table row per dist). It also makes the issue's two re-measurement modes fall
out of the layout: one dist is one file, and "everything starting with A" is one
directory.

`summary.*` and `site/content/ecosystem.json` are **generated** from the tree and
never hand-edited — a conflict in them is resolved by regenerating, not merging.

### Record schema (`schema: 1`)

```json
{
  "schema": 1,
  "dist": "String::Utils",
  "version": "0.0.40",
  "source": { "index": "fez", "path": "…/String-Utils-0.0.40.tar.gz", "sha256": "…" },
  "measured": {
    "date": "2026-09-10",
    "mutsu_commit": "66a1e4e", "mutsu_version": "0.23.0",
    "raku_version": "2026.07", "raku_backend": "moar 2026.07",
    "host": "linux-x86_64", "sandbox": "bwrap", "harness": 1
  },
  "deps": { "mode": "flat-closure", "digest": "sha256:…",
            "resolved": ["Foo::Bar"], "unresolved": [], "bundled_would_supply": [] },
  "load": { "String::Utils": "ok" },
  "files": [
    { "path": "t/01-basic.rakutest",
      "raku":  { "verdict": "pass", "plan": 124, "ok": 124, "nok": 0, "todo": 0, "skip": 0, "secs": 3.9 },
      "mutsu": { "verdict": "fail", "plan": 124, "ok": 120, "nok": 4, "todo": 0, "skip": 0, "secs": 1.1,
                 "first_failure": "chars of ünïcödé string" },
      "cmp": "partial", "flaky": false }
  ],
  "totals": { "baseline_files": 3, "parity_files": 2, "regressed_files": 1,
              "baseline_assertions": 300, "mutsu_assertions": 296 },
  "status": "partial"
}
```

Both sides' raw TAP counts are stored deliberately: a metric definition we
change later must be recomputable from the store, never a reason to re-sweep.

**Timestamps are date-granular** (ADR-0085 D7). A re-run that reproduces the same
verdicts on the same day writes a byte-identical file and produces no diff; a
sweep on a later day rewrites what it executed with a one-line provenance delta.

`ecosystem/history.tsv` — one row per full sweep, the shape `HISTORY.tsv` already
uses for roast:

```
date	mutsu_commit	raku_version	dists	measured	blocked_dep	baseline_files	parity_files	file_parity	assertion_parity	dist_parity
```

## 5. The harness

`scripts/ecosystem-sweep.py`, sharing its index reader, tarball cache, sandbox
wrapper and TAP parser with `scripts/dist-compat-sweep.py` through a new
`scripts/ecosystem_common.py` — one implementation of the sandbox, not two
copies of security-relevant code.

```sh
# one dist (the per-module re-measurement the issue asks for)
scripts/ecosystem-sweep.py --only String::Utils

# one shard — "all modules that start with A"
scripts/ecosystem-sweep.py --prefix A --jobs 8

# everything currently red, after a fix lands
scripts/ecosystem-sweep.py --status regression

# records measured at an older mutsu commit or an older rakudo
scripts/ecosystem-sweep.py --stale

# the full corpus
scripts/ecosystem-sweep.py --all --jobs 8

# regenerate summary.json / summary.md from the tree (no runs)
scripts/ecosystem-sweep.py --rollup
```

Other flags: `--timeout` (default 120s/file, both sides), `--max-files` (per
dist, to bound a pathological suite), `--include-xt`, `--sandbox {bwrap,none}`,
`--mutsu-only` (see *Baseline caching* below), `--refresh-baseline`, `--dry-run` (resolve and print the
plan without executing), `--json -` (emit records to stdout instead of the tree).

Environment: `MUTSU_BIN` (default `target/release/mutsu`), `RAKU_BIN` (default
`raku`).

### Cost model

Calibrated 2026-09-10 on this container (4 cores) over 14 randomly-sampled
zero-dependency dists — 115 test files, 104 executed:

| quantity | measured |
|---|---|
| test files per dist | **8.2** mean (median ~2.5; one dist had 51) |
| rakudo wall time per file | **3.0 s** (startup + compile dominated) |
| files rakudo passes cleanly | **92%** (96/104) — the baseline rate for zero-dep dists |

Projected full corpus: 1623 dists ≈ **13,300 test files**; the rakudo side alone
≈ **11 CPU-hours**, and mutsu's side is at or below that (mutsu beats rakudo on
the roast whitelist). Call a full sweep **~20 CPU-hours ≈ 2.5 h wall at `-j8`**,
plus ~1600 tarball downloads (cached). One letter shard (`A` = 154 dists) is
~10-15 minutes at `-j8` — cheap enough to re-run after a fix.

These are the numbers to sanity-check phase 2 against; treat a wildly different
first full run as a harness bug, not a surprise.

### Baseline caching

The rakudo side of a file depends only on `(dist version, raku version, dep
closure digest, host)`. When those are unchanged, `--mutsu-only` reuses the
stored `raku` block instead of re-running rakudo, halving the cost of the common
case: *re-measuring after a mutsu fix*. A rakudo upgrade invalidates every
baseline at once and forces a full sweep — recorded in `history.tsv` as a
denominator change, per ADR-0085.

## 6. Publication

- **Site**: `site/ecosystem.html` + `site/content/ecosystem.json`, generated by
  `scripts/gen-ecosystem-manifest.py` and wired into `.github/workflows/pages.yml` —
  exactly the pattern `site/batteries.html` / `scripts/gen-batteries-manifest.py`
  already uses. A searchable table: dist, version, status badge, baseline files
  passed / total, and the first failure for the red ones.
- **Repo**: `ecosystem/summary.md` for readers browsing the tree; linked from
  `README.md` and PLAN.md §1 B4.
- **Raw**: `ecosystem/dists/**.json` is the machine-readable artifact the issue
  asks for — stable schema, versioned by `schema`.

## 7. Phases (one PR each)

| # | Deliverable | Done when |
|---|---|---|
| **P1** | `scripts/ecosystem_common.py` extracted from `dist-compat-sweep.py`; `scripts/ecosystem-sweep.py` with the dep resolver, sandbox, TAP compare, `--only` / `--prefix` / `--rollup`; schema v1 | the `A` shard measures end to end and its records land under `ecosystem/dists/A/` |
| **P2** | first full-corpus sweep; `ecosystem/` populated; `summary.*` + the first `history.tsv` row | `file_parity` / `assertion_parity` / `dist_parity` exist as real numbers |
| **P3** | `site/ecosystem.html` + manifest generator + `pages.yml` wiring | a user can look up a dist on the public site |
| **P4** | `.github/workflows/ecosystem.yml` — scheduled (weekly) + `workflow_dispatch` with a shard input; opens a PR with the updated records and the history row; **report-only, never gates a PR** | a sweep lands without a human running it (this also closes PLAN.md §1 B1's "working-module regression CI") |
| **P5** | root-cause grouping of `regression` records into `todo:ticket` issues, in the shape `scripts/dist-compat-tickets.py` already produces; `docs/triage.md` picks them up | the KPI feeds the work queue |

P1 and P2 are the campaign; P3-P5 make it self-sustaining. Do not start P2
before P1's `--only` round-trips a record, and do not start P4 before P2 has
produced a number worth watching.

## 8. Known limits and follow-ups

- **Network-dependent suites are invisible.** They fail on both sides and land
  in `no_baseline`. Recorded as a count so the size of the blind spot is known;
  a future opt-in `--allow-network` mode for a vetted subset is possible but is
  not part of this design.
- **`blocked_dep` residue (~15% of dists).** Adding the REA index as a second
  source is the obvious fix and the highest-value follow-up.
- **Author (`xt/`) tests, `build-depends`, and dists whose `provides` uses a
  non-convention path** (resolvable only through an installed provides map) are
  each a small, recorded population rather than a silent one.
- **`mzef compat <Dist>`** — reading the shipped data from the installed
  distribution so `mzef install` can warn before installing something known
  red. Attractive, out of scope here.
- **Timing/concurrency suites** (S17-flavoured) are the expected home of
  `flaky: true`; if the retry rule proves insufficient, quarantine them the way
  `flaky-tests.txt` quarantines local tests rather than weakening the KPI.
