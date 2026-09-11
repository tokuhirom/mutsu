# Ecosystem parity — measuring mutsu against rakudo, one distribution at a time

The operations manual for mutsu's headline compatibility KPI: run every zef
distribution's own test suite under **both rakudo and mutsu**, record both
sides, and publish the difference.

- **Why it is shaped this way**: [ADR-0085](adr/0085-ecosystem-testsuite-parity-measurement.md)
  — the decisions (rakudo as the denominator, flat dependency closures,
  one file per distribution, the sandbox and environment contract). Read it
  before changing the method; this file is the *how*.
- **Tracking issue**: [#7785](https://github.com/tokuhirom/mutsu/issues/7785).
- **Acting on a record**: this file measures; taking one distribution from red
  to green is
  [`.agents/skills/ecosystem-dist-fix/SKILL.md`](../.agents/skills/ecosystem-dist-fix/SKILL.md),
  which consumes §1-§3 below and adds the per-distribution debugging loop and the
  fix-versus-file-an-issue rule;
  [`.agents/skills/ecosystem-dist-roulette/SKILL.md`](../.agents/skills/ecosystem-dist-roulette/SKILL.md)
  wraps that in a uniform random draw over the actionable records plus a lock on
  [#7884](https://github.com/tokuhirom/mutsu/issues/7884), so parallel agents
  neither collide nor bias the sample by cherry-picking cheap records.
- **Sibling tools**: [docs/dist-compat-sweep.md](dist-compat-sweep.md) is the
  load-level (`use <module>`) diagnostic sampler that feeds root-cause tickets;
  [BATTERIES.md](../BATTERIES.md) / `scripts/battery-testsuite.sh` is the
  release gate on the ~40 *bundled* dists. This campaign is neither: it is an
  exhaustive, re-runnable ledger over the whole ecosystem.

> **Status: P1, P2 and P3 landed.** The corpus has been measured: [run
> 34566091231](https://github.com/tokuhirom/mutsu/actions/runs/34566091231)
> (2026-09-11, `scope=all`, 27/27 shards green) covers all **1624**
> distributions at mutsu `1557d41` against rakudo 2026.07, and the headline is
> **41.2%** dist parity — 53.6% file parity, 62.4% assertion parity. The first
> `history.tsv` row and the chart exist as of that sweep. A sweep runs either
> locally (§8) or from GitHub Actions (§8.1), so the numbers do not depend on
> having a many-core box to hand. **P5 has produced its first batch** — fifteen
> root-caused issues covering ~330 distribution slots, see section 9.

## 1. What gets measured

| term | meaning |
|---|---|
| **distribution** (dist) | the unit of measurement — one `Dist::Name` in the fez index, with a version, a tarball and a `META6.json` |
| **module** | one entry in a dist's `provides` |
| **test file** | the unit of comparison — one `t/**.t` / `t/**.rakutest`, run as one process |
| **baseline file** | a test file **rakudo passes cleanly** in this sweep; only these count toward the KPI |

Corpus: the fez index (`https://360.zef.pm/`, cached at
`~/.zef/store/fez/fez.json`), latest version of each dist. As of the 2026-09-10
snapshot: **1623 distributions**, 9214 provided module names. Dependencies are
resolved against fez **merged with REA** (§3), which adds ~920 more
distributions — those are dependency supply, not measurement targets.

### The three numbers

With *B* = the set of baseline files:

- **`dist_parity`** = `count(dists whose whole baseline set passes on mutsu) /
  count(dists with a non-empty baseline set)` — **the published headline**. It
  is what a user means by "does my module work?", and it weights every
  distribution equally.
- **`file_parity`** = `count(f in B where mutsu passes f) / count(B)` — **the
  number the work is steered by**: it moves smoothly enough to tell whether a
  slice helped.
- **`assertion_parity`** = `sum over B of min(mutsu_ok, raku_ok) / sum over B of
  raku_ok` — the finest signal; moves even when no file flips, so a sweep that
  halves a suite's failures is visible before the file turns green.

The roles are split because one number cannot do both jobs (ADR-0085 D3).
`dist_parity` is a step function — a distribution with 51 test files stays at
zero until the last one goes green — so steering by it would hide real progress.
`file_parity` moves continuously but weights that distribution 51× a one-file
one, so publishing it would overstate reach.

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
8. **A timeout is a claim about the budget until a longer run disproves it.**
   Re-running a timed-out file on the *same* budget is the one retry that can
   never learn anything: a hang hangs again, and a file whose own runtime sits
   at the budget is phase-locked into timing out again, because whatever sets
   its runtime also sets when the previous attempt ended. So the first timeout
   buys one retry at twice the budget and is not counted towards `flaky`; a
   file that times out at the larger budget is believed and the retries stop,
   which costs a genuine hang no more wall clock than the same-budget retries
   it replaces. `DateTime::React`'s `t/01-basic.t` is the case that forced this
   (`news/2026-09/`): it sleeps through two minute rollovers, so it runs for
   `121 - second-of-minute` seconds and therefore always *ends* at
   second-of-minute 1 — putting every following run at the one phase where it
   needs ~120.1s, just over the 120s default. It passes 8/8 on both
   interpreters; the sweep was discarding it as `flaky`.

## 3. Dependencies — a flat `-I` closure, resolved offline

Per ADR-0085 D4/D5. **The index is fez + the
[Raku Ecosystem Archive](https://github.com/Raku/REA) merged**, highest version
wins; REA is not optional (see the coverage table below).

1. Read `depends` + `test-depends` from the index entry (and from the extracted
   `META6.json`, which is authoritative when they disagree).
2. Resolve each name against the merged index: first as a dist name, then
   through the index's module→dist `provides` map. Names the compiler provides
   (`Test`, `NativeCall`, `nqp`, …) and `:from<native>` / `:from<bin>` entries
   are not dependencies.
3. Recurse to a fixed point; download and extract each member (cached under
   `~/.cache/mutsu-ecosystem/`); build one `-I <dep>/lib` list.
4. Hand that identical list to **both** interpreters.

**If the closure cannot be completed, the dist is `blocked_dep` and runs on
neither side.** Letting mutsu's bundled batteries (`modules/`, which sit below
`-I` in the precedence chain) quietly supply a dependency rakudo lacks would
raise the KPI without any compatibility improving. The record instead notes
`bundled_would_supply: [...]` — a batteries statistic, reported separately.

### Coverage — why REA is a phase-1 requirement

Measured 2026-09-10, resolving the **transitive** closure over the 1623 fez
distributions (816 of which have no dependencies at all):

| index | closures that resolve | blocked |
|---|---|---|
| fez only | 1164 (72%) | 459 |
| fez + mutsu's bundled batteries | 1260 (78%) | 363 |
| **fez + REA** (the design) | **1584 (98%)** | 39 |
| fez + REA + bundled batteries | 1584 (98%) | 39 |

A first-level-only count flatters this to 85%; **72% is the operative number**
for a resolver that recurses to a fixed point, and it is not enough to sweep on.
What fez alone cannot supply is the legacy p6c/CPAN tail that never moved:
`File::Which` (69 dependents), `Hash::Merge` (60),
`Distribution::Builder::MakeFromJSON` (58), `Getopt::Long` (52),
`Compress::Zlib` (27). REA carries all of them, and takes the corpus from
roughly a quarter unmeasurable to 2%.

The 39 that still fail are dominated by malformed `depends` entries in the
source `META6.json` — prose fragments (`has`, `path`, `as`, `shorten`) rather
than distribution names. They are recorded as `blocked_dep`, not chased.

Note the last row: **with REA in, the bundled batteries add nothing to
coverage** — which is what makes ADR-0085 D5 (skip `blocked_dep` on both sides)
cost nothing rather than being a sacrifice.

## 4. The data store

```
ecosystem/
  README.md                     # what this is and how to read it (points here)
  index-snapshot.json           # fez index provenance: fetch date, sha256, dist count
  dists/<S>/<Dist--Name>.json   # ONE FILE PER DIST — the record
  summary.json                  # generated rollup: the three KPIs + per-shard counts
  summary.md                    # generated human-readable table
  history.tsv                   # append-only, one row per full sweep — the KPI time series
  history.svg                   # generated chart of history.tsv (section 6), embeddable in README
```

`<S>` is the uppercased first character of the dist name (`_` when it is not an
ASCII letter). The filename comes from `ecosystem_common.record_filename()`:
`::` becomes `--`, anything a filesystem or a GitHub artifact upload rejects is
percent-escaped, and a `~<digest>` suffix makes the mapping injective *including
case-insensitively* — `::` → `--` alone is not (`Qwiratry::Location::HTTP` and
`Qwiratry--Location--HTTP` are both real), and an artifact upload is
case-insensitive (`CSV-AutoClass` / `CSV-Autoclass`). `String::Utils` →
`ecosystem/dists/S/String--Utils~aed281d9.json`.

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
    "host": "linux-x86_64", "sandbox": "bwrap", "harness": 2
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

The three rate columns are **percentages** (`72.0`), not fractions. One
convention, enforced by the plotter: a "0.83 means 83%" leniency cannot be told
apart from a genuine 0.83% — which is exactly what a campaign starting near
zero produces — so it would draw a near-empty corpus as near-complete.

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

# everything currently red, after a fix lands (a record `status`, so `red` /
# `partial` — `regression` is a per-FILE verdict, not a selectable status)
scripts/ecosystem-sweep.py --status partial

# records measured at an older mutsu commit or an older rakudo
scripts/ecosystem-sweep.py --stale

# the full corpus
scripts/ecosystem-sweep.py --all --jobs 8

# regenerate summary.json / summary.md / history.svg from the tree (no runs)
scripts/ecosystem-sweep.py --rollup
```

Other flags: `--timeout` (default 120s/file, both sides — a file that exhausts
it gets one retry at twice the budget, see §2.8), `--max-files` (per
dist, to bound a pathological suite), `--include-xt`, `--sandbox {bwrap,none}`,
`--no-index-snapshot` (see below),
`--mutsu-only` (see *Baseline caching* below), `--refresh-baseline`, `--dry-run` (resolve and print the
plan without executing), `--json -` (emit records to stdout instead of the tree).

Environment: `MUTSU_BIN` (default `target/release/mutsu`), `RAKU_BIN` (default
`raku`), `MUTSU_ECO_HOST` (what `measured.host` records; default
`<uname>-<machine>`, which cannot tell a hosted runner from the maintainer's box
— §8.1 sets it to `gha-*`).

**`index-snapshot.json` is corpus-level provenance and a targeted run leaves it
alone.** The file says "the records beside me were resolved against this
fez/REA snapshot"; rewriting it after re-measuring one distribution would date
the entire ledger to today's index on the strength of a single record, and puts
an unrelated file in a bug-fix PR's diff where it reads as a corpus refresh.
So `--only` never writes it (the run logs `index-snapshot.json: left
unchanged`), and `--no-index-snapshot` extends that to a `--status` / `--stale`
re-measure, which has the same problem. A corpus or shard run does write it —
that is what it is for, and §8.1's workflow pins one index across all its shards
so they agree on its content.

`scripts/ecosystem-ci.py` is the CI-side companion: `plan` turns
`.github/workflows/ecosystem-sweep.yml`'s dispatch inputs into a validated job
matrix, `apply` lands a finished sweep's records on top of whatever main has
become meanwhile (§8.2), and `provenance` groups a set of records by the
`(mutsu commit, rakudo, host)` triple that measured them. Both are useful
locally too — `--self-test` covers them, so a change to either is checked without
dispatching a run.

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

- **The KPI chart**: `ecosystem/history.svg`, rendered from `history.tsv` by
  `scripts/plot-ecosystem-history.py` (see below). A committed static SVG, so
  the same file works in `README.md`, in the GitHub repo view, and embedded in
  the site page — no JavaScript, no CDN, nothing to deploy.
- **Site**: `site/ecosystem.html` + `site/content/ecosystem.json`, generated by
  `scripts/gen-ecosystem-manifest.py` and wired into `.github/workflows/pages.yml` —
  exactly the pattern `site/batteries.html` / `scripts/gen-batteries-manifest.py`
  already uses. The chart at the top (only once a sweep has produced one — the
  manifest carries `has_chart`, so the page never requests a file that is not
  there), then a searchable, status-filtered table: dist, version, status badge,
  baseline files passed / total, and the first failure for the red ones. Rows
  are ordered **worst first**: someone opening the page is checking whether
  their module is in trouble, not admiring the green ones.
- **Repo**: `ecosystem/summary.md` for readers browsing the tree; linked from
  `README.md` and PLAN.md §1 B4.
- **Raw**: `ecosystem/dists/**.json` is the machine-readable artifact the issue
  asks for — stable schema, versioned by `schema`.

### The chart

```sh
scripts/plot-ecosystem-history.py ecosystem/history.tsv ecosystem/history.svg
```

`--rollup` runs this too, so the chart cannot drift from the TSV it is drawn
from. Three lines, all rates on a 0-100% axis: `dist_parity` heavy (the
published headline), `file_parity` and `assertion_parity` lighter (what the
work is steered by). Each line is labelled with its current value, because with
a handful of sweeps a reader wants the number rather than a position on an axis.

Two things it does that `scripts/plot_roast_history.py` deliberately does not,
which is why this is a separate script rather than a flag on that one:

- **The x axis is time, not sample index.** `HISTORY.tsv` gets a row on every
  main push, so evenly-spaced samples are honest there. Ecosystem sweeps are
  nightly *plus* whatever is dispatched in between (ADR-0085 D9's second
  amendment), so index spacing would still draw a skipped night and a same-day
  re-run the same width.
- **A rakudo change is drawn as a labelled vertical rule.** It re-bases every
  baseline, so the series is not comparable across it — the step at that point
  is the denominator moving, not news, and the chart has to say so or it lies.

The SVG carries a `prefers-color-scheme` stylesheet so it stays legible in a
dark README; browsers that ignore it get the light palette.

## 7. Phases (one PR each)

| # | Deliverable | Done when |
|---|---|---|
| ~~**P1**~~ | ~~`scripts/ecosystem_common.py` extracted from `dist-compat-sweep.py`; `scripts/ecosystem-sweep.py` with the dep resolver, sandbox, TAP compare, `--only` / `--prefix` / `--rollup`; schema v1~~ | **done** — records round-trip, `--rollup` produces summary + history + chart, and the first eight distributions surfaced real findings |
| ~~**P2**~~ | ~~first full-corpus sweep; `ecosystem/` populated; `summary.*` + the first `history.tsv` row~~ | **done** — run 34566091231 (2026-09-11) measured all 1624 dists at one commit with 27/27 shards green: 41.2% dist / 53.6% file / 62.4% assertion parity, and the first history row is appended. It took three attempts; the two that lost data did so silently, and what they cost is recorded in `news/2026-09/` |
| ~~**P3**~~ | ~~`ecosystem/history.svg` linked from `README.md`; `site/ecosystem.html` + manifest generator + `pages.yml` wiring~~ | **done** — `site/ecosystem.html` is generated from the ledger by `scripts/gen-ecosystem-manifest.py`, wired into `pages.yml` and the nav, and covered by `site/e2e.test.mjs`; the README's Status section now carries the figure and links the chart, which P2 was the thing producing |
| **P4** | the operator runbook (§8) — one entry point for a full sweep and for a shard, plus the `--rollup` + `history.tsv` append and the PR it lands as | **partly done** — `.github/workflows/ecosystem-sweep.yml` (§8.1) is that entry point for anyone with dispatch rights: it plans, builds once, measures, rolls up and opens the PR. What is left is the local `make`-level convenience wrapper |
| **P5** | root-cause grouping of the actionable records into `todo:*` issues | **first batch done** — `scripts/ecosystem-tickets.py` clusters the ledger by root cause, ordered by distributions affected, and fifteen issues were filed from the corpus at `7807eb5` (section 9). Two harness fixes came out of it: a warning can no longer be recorded as a blocker, and a parse failure keeps its location. The remaining tail is a sampling job, not a queue to drain |

P1 and P2 are the campaign; P3-P5 make it repeatable. **P5's method is section
9**, and the reading that started it still holds: of the 393 `blocked_load` records, 53
are `raku_also_fails` (rakudo does not load them either, so they are not mutsu's
to fix and belong outside the numerator), and the remaining 340 normalise to 133
distinct load errors whose top ten cover a third of them (110 of 340) —
`X::Redeclaration` on a routine, "needs parens to avoid gobbling block", `No such
method`, a handful of parse errors, `Could not find QAST in:`, and slang
activation. The 560 `red`/`partial` records have a much longer tail (513 with a
recorded first failure, 336 distinct), so the cheap grouping is on the load
axis and the file axis wants sampling rather than exhaustive triage.

**The sweep is scheduled nightly, and dispatchable on demand** — §8.1. D9
originally argued for neither (a full sweep looked like ~20 CPU-hours, too much
to pay a hosted runner for on a number that moves at the speed of interpreter
fixes); both amendments to it rest on the measured cost instead, which is 78
minutes of wall time and ~5.5 hours of job time across 27 shards. PLAN.md §1 B1's
"working-module regression CI" is still **not** closed by this: a measurement is
not a gate, and that stays a separate item.

## 8. Operator runbook

Two ways to refresh the numbers: **locally** (below) when a many-core box is at
hand, or **from GitHub Actions** (§8.1) otherwise. They produce the same records
and differ only in `measured.host`, so pick either — but do not run both at once
on overlapping selections, or the second to finish overwrites the first.

### 8.0 Locally

A machine with the cores to spare — the maintainer's 12-core box, not an
ephemeral remote container (4 cores and a fixed disk allowance; `bwrap` is no
longer the obstacle there, since `.claude/hooks/session-start.sh` installs and
verifies it, but a corpus sweep still does not fit). A single `--only`
re-measure after a fix is fine in either.

```sh
apt-get install bubblewrap          # required; the sweep refuses to run a corpus without it
touch src/main.rs && cargo build --release   # a stale binary is measured silently otherwise
scripts/ecosystem-sweep.py --prefix A --jobs 8      # ~10-15 min, to check the setup
scripts/ecosystem-sweep.py --all --jobs 8           # ~2.5 h
scripts/ecosystem-sweep.py --rollup                 # summary.json / summary.md / history.tsv / history.svg
git checkout -b ecosystem/sweep-YYYY-MM-DD && git add ecosystem/ && …   # land it as an ordinary PR
```

Before landing a sweep, check that `measured.host` is uniform across what
changed: a shard measured on a different machine is identifiable by design, but
mixing hosts inside one `history.tsv` row makes the row mean less than it looks.
`scripts/ecosystem-ci.py provenance` answers that in one command:

```sh
git status --porcelain -- ecosystem | awk '{print $NF}' \
  | grep -E '^ecosystem/dists/.*\.json$' \
  | scripts/ecosystem-ci.py provenance
```

After a **rakudo upgrade**, every baseline is invalid at once (§5) — run a full
sweep rather than a shard, and say so in the PR, because the denominator moved
and the KPI is not comparable across that boundary.

### 8.1 From GitHub Actions

**The corpus is measured for you every night**: `ecosystem-sweep.yml` carries
`schedule: - cron: '20 18 * * *'` — 03:20 JST — and a scheduled run is a full
`scope: all` sweep that rolls up and appends a `history.tsv` row (ADR-0085 D9's
second amendment; the cost that justifies it is 78 minutes of wall time and ~5.5
hours of job time). So nobody has to remember to refresh the numbers, and the
question worth asking about a stale-looking figure is "did last night's run go
red", not "when did someone last run it".

Dispatch it by hand for anything *other* than the whole corpus — one shard, one
distribution, one status — or to re-measure immediately rather than waiting for
the night: `.github/workflows/ecosystem-sweep.yml`, **Run workflow** (or
`gh workflow run ecosystem-sweep.yml -f scope=…`). One dispatch does the whole
runbook: plan, build, measure, roll up, open the pull request.

A dispatched run and the nightly one never overlap (`concurrency:
ecosystem-sweep`, queued rather than cancelled — a cancelled sweep throws away
hours of measurement), and a night that changes nothing opens no pull request.

> **Editing the workflow's inputs:** on a `schedule` event `inputs.*` are **all
> empty** — a `workflow_dispatch` default does not apply to it. Every input is
> read as `inputs.x || <default>`, and `rollup`/`history` as
> `inputs.x || github.event_name == 'schedule'`. Add a new input without its
> fallback and the nightly run silently gets the empty value.

| input | what it selects | maps to |
|---|---|---|
| `scope: stale` (default) | records measured at another mutsu commit or rakudo — "refresh the numbers" | `--stale` |
| `scope: all` | the whole corpus, fanned out across the 27 letter shards | `--prefix A` … `--prefix _` |
| `scope: prefix` + `prefix: A` | one shard | `--prefix A` |
| `scope: only` + `only: BTree, Trie` | named distributions | `--only …` |
| `scope: status` + `status: partial` | every record currently at that status | `--status partial` |

`shards` controls the fan-out (`auto` = letters for `scope: all`, one job
otherwise; `letters` forces it, which is what a large `stale` selection wants);
`jobs` / `attempts` / `file_timeout` are the harness flags; `raku_version` pins
the oracle; `rollup` regenerates `summary.*`; `history` asks for a `history.tsv`
row; `publish` chooses `pull-request` (default), `branch`, or `none`.

**The pull request auto-merges** (merge, never squash) as soon as CI passes — the
records are not reviewed and are not meant to be. A diff of thousands of
machine-generated measurements gives a reviewer nothing to act on: they cannot
tell a right number from a wrong one by reading it. Everything that makes the
numbers trustworthy runs *before* the diff exists — the vendored-`Test.rakumod`
probe, the required sandbox, one binary and one rakudo and one index per run, the
provenance check, and the guards below that withhold a summary or a history row
from a partial or mixed sweep. So CI is the gate, and the pull request is the
audit trail and the revert handle rather than a review queue. `publish: branch`
is the way to get a sweep that waits for a human.

`rollup` is on by default but will not *create* the first `summary.json` from a
partial sweep — until a `scope: all` run has produced one, a subset run skips the
rollup and says so, for the same reason `ecosystem/` has carried no summary since
P1 ([ecosystem/README.md](../ecosystem/README.md)). Once a full sweep has
established it, every later run keeps it in step with the records.

What the workflow guarantees, and why it is not simply 27 independent sweeps —
one mutsu binary, one rakudo release and one ecosystem index snapshot are
produced by the `build` job and handed to every shard, so nothing inside a single
KPI number was measured against a different denominator. `bwrap` is verified on
each runner and the job fails rather than measuring unsandboxed.

Dispatch it **from `main`**. Measuring a feature ref is supported and sometimes
what you want (it measures that ref's mutsu), but the records are then only
pushed to a branch: a pull request from a feature ref into `main` would carry
that ref's other commits alongside the records, so the workflow declines to open
one and says so.

### 8.2 What happens when the ledger moves during a sweep

A corpus sweep measures for over an hour, and the interesting case is that a
*fix* lands while it runs: a `todo:ticket` PR repairs an interpreter bug and
re-measures the one distribution it fixed, at a **newer** mutsu commit. The
sweep's own record for that distribution is then stale before it is even
committed.

So the `collect` job does not copy its records over the tree. It checks out
**main as it is now** (not the commit the sweep was dispatched from) and applies
each record through `scripts/ecosystem-ci.py apply`, whose rule is:

> **The newer mutsu commit wins.** A record says what mutsu did at one commit, so
> a record measured at a later commit is the more current answer, whoever
> measured it.

Ordering is decided by `git merge-base --is-ancestor`, so it is the real commit
graph and not a date heuristic; when git cannot order two commits (an unknown
sha) the recorded date breaks the tie, and when even that ties the record already
on the branch is kept. A record is never overwritten by one that cannot be shown
to supersede it, and the run summary reports how many were left alone.

Two consequences worth knowing:

- **This is also the merge-conflict answer.** Records are one file per
  distribution, so the only way a sweep can conflict with a sibling PR is by
  touching the same record — which is exactly the case the rule resolves, before
  the branch is created. A 1600-file data PR does not need hand resolution.
- **A locally-measured record can shadow a sweep's for a while.** A dist-fix PR
  measured on a dev box (often `"sandbox": "none"`, a different `host`) wins over
  the sandboxed `gha-*` measurement while its commit is the newer one. The record
  says so in its own provenance, and the next sweep at a newer commit replaces
  it — or `--stale` does, sooner.

Two things it will refuse to do:

- **Append a `history.tsv` row for anything but a complete, uniform sweep.** Not
  a subset (`scope` other than `all`), not a run where a shard failed, not a run
  whose records disagree about the `(mutsu commit, rakudo, host)` triple that the
  row would claim. The records still land in all three cases; only the chart
  point is withheld, and the run summary says which guard fired.
- **Measure without a sandbox.** There is no fallback flag.

Reading the result: records measured on a runner carry `measured.host` of the
form `gha-ubuntu24-4c`, so they are never silently mixed with locally-measured
ones. A hosted runner has ~4 cores, so `--timeout 120` bites earlier in
wall-clock terms than on a 12-core box — symmetrically for both interpreters, so
such a file lands in `no_baseline` rather than being charged to mutsu, but it
does make the measurable baseline slightly smaller than a local sweep's. The
escalated retry of §2.8 recovers the files that only just exhaust the budget,
which is most of what that gap was.

If the pull request opens with no CI running on it, the repository's GitHub App
credentials (`TAGPR_APP_CLIENT_ID` / `TAGPR_APP_PRIVATE_KEY`) are not configured
and it was opened with the default `GITHUB_TOKEN`, which GitHub does not let
trigger workflows; the run summary says so. Push one commit to the branch from a
clone to start CI.

## 9. From the ledger to tickets (P5)

`scripts/ecosystem-tickets.py` reads the records back and answers *why*, grouped.
Every actionable failure site — a `blocked_load` module error, and the
`first_failure` of every `regression`/`partial` test file — is normalised into a
root-cause **signature**, and signatures are clustered so that one cluster is one
interpreter fix.

```sh
scripts/ecosystem-tickets.py                      # the table, biggest first
scripts/ecosystem-tickets.py --min-dists 1 --all  # the whole tail
scripts/ecosystem-tickets.py --family no-such-method
scripts/ecosystem-tickets.py --issue b98eb9ef     # a ready-to-file issue body
scripts/ecosystem-tickets.py --json tmp/t.json    # machine-readable
scripts/ecosystem-tickets.py --self-test
```

Four decisions are worth knowing before reading its output:

- **Impact is counted in distributions, never in failure sites.** A message
  repeated across forty modules of one distribution is one bug worth one
  distribution. The raw per-module counts in the ledger say otherwise, and
  ranking by them puts `Gnome::Gtk3` at the top of every list.
- **A cluster key keeps the payload that identifies the fix and drops what is
  volatile.** `No such method '<m>' on <type>` keys on both; `nqp::<op>` keys on
  the op. Where the members share one cause by construction — an unknown
  attribute trait, an invalid typename, a stack overflow — the cluster is the
  *family*, and the payloads are listed inside it as "what varies".
- **A message that carries no shared cause is keyed per distribution**, not
  merged: `not ok 7 -` (an *unnamed* failing assertion) is the same string for
  every such test in the corpus, and merging those two dozen would produce a
  mega-ticket no single fix can close.
- **The output is filed as GitHub issues** ([issue-workflow.md](issue-workflow.md)),
  not committed as a queue file. A generated queue over ~1600 records would
  conflict on every sweep, and an issue number keeps resolving after the finding
  is fixed where a path in a generated file does not. Each body carries
  `eco-cluster: <id>`, a digest of the signature and therefore stable across
  sweeps, so "is this cluster already filed?" is a tracker search rather than
  state in the repository.

### A cluster is a hypothesis. Verify it before filing.

The first batch's clusters were right about *what* fails and wrong about *why*
often enough that this is a rule, not advice. Half an hour with the `raku` oracle
turned vague tickets into minimised ones and killed two that would have been
false:

| the cluster said | what it actually was |
|---|---|
| `unknown trait 'is' -> 'json-skip-null'` (61 dists) | not the trait — mutsu handles user attribute traits across module boundaries. `JSON::Class` **re-exports** the trait it imported with `OUR::{'&trait_mod:<is>'} := &trait_mod:<is>`, and that binding is invisible to importers ([#7989](https://github.com/tokuhirom/mutsu/issues/7989)) |
| `No such method 'AST' for invocant of type 'Str'` (13 dists) | `'say 1'.AST` works today. The L10N tests call `.AST("AF")` — the *localised slang* form, a 1-arity candidate that does not exist ([#8001](https://github.com/tokuhirom/mutsu/issues/8001)) |
| `Variable $.x used where no 'self' is available` (35 dists) | two unrelated gaps: NativeCall's `HAS` declarator ([#7991](https://github.com/tokuhirom/mutsu/issues/7991)) and `has Int ($.x, $.y)` ([#7992](https://github.com/tokuhirom/mutsu/issues/7992)) |
| `has overflowed its stack` (17 dists) | not deep recursion — *infinite* recursion: an imported `proto`/`multi` does not shadow an enclosing same-named `my sub`, so lizmat's `P5*` wrappers call themselves ([#7994](https://github.com/tokuhirom/mutsu/issues/7994)) |
| `Use of Nil in string context` (10 dists) | **nothing.** Both interpreters print that as a warning and carry on; it was the harness recording a warning as the blocker. Not filed — fixed in the harness instead (below) |

So: run the minimal case on both interpreters, and only then write the body. A
ticket that names the construct is worth ten that quote a message.

### Two harness fixes this phase produced

Both are in `first_error_line()` (`scripts/ecosystem_common.py`), and both change
what *future* sweeps record — existing records carry the old text until
re-measured:

- **A warning is no longer a cause.** `Use of Nil in string context`, `Use of
  uninitialized value`, `Potential difficulties` and friends are printed by
  rakudo too, and execution continues past them; a line matching them is used
  only when the run produced nothing else. Ten distributions had a warning
  recorded as their blocker, which hid ten real causes.
- **A parse failure keeps its location.** mutsu prints `at <path>:<line>` and a
  caret line under the reason; both were being dropped. The location is now
  appended as `... [at dist/lib/A.rakumod:50]`, which is the difference between a
  triageable record and "the parser was unhappy somewhere in this distribution"
  — 99 distributions were in the second state. The caret line stays dropped: it
  carries source text, which would fragment a cluster into one per offending
  line. Consumers strip the suffix before clustering.

### The first batch (2026-09-11)

Fifteen issues from the corpus measured at `7807eb5`, covering ~330 of the
~1200 non-green distribution slots:
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) (99 parse gaps, `todo:deep`),
[#7989](https://github.com/tokuhirom/mutsu/issues/7989) (61, `OUR::` re-export),
[#7991](https://github.com/tokuhirom/mutsu/issues/7991) (44, `HAS`),
[#7993](https://github.com/tokuhirom/mutsu/issues/7993) (21, invalid typename),
[#7995](https://github.com/tokuhirom/mutsu/issues/7995) (20, timeouts, `todo:perf`),
[#7996](https://github.com/tokuhirom/mutsu/issues/7996) (20, unknown parent type),
[#7994](https://github.com/tokuhirom/mutsu/issues/7994) (17, infinite recursion),
[#8000](https://github.com/tokuhirom/mutsu/issues/8000) (17, `CHECK` swallows its exception),
[#7999](https://github.com/tokuhirom/mutsu/issues/7999) (16, one parse gap in `Terminal::Widgets`),
[#7997](https://github.com/tokuhirom/mutsu/issues/7997) (13, `use NativeCall :TEST`),
[#8001](https://github.com/tokuhirom/mutsu/issues/8001) (13, `Str.AST($lang)`),
[#8002](https://github.com/tokuhirom/mutsu/issues/8002) (11, duplicate composed attribute),
[#8003](https://github.com/tokuhirom/mutsu/issues/8003) (9, default constructor),
[#8004](https://github.com/tokuhirom/mutsu/issues/8004) (9, `%?RESOURCES`),
[#7992](https://github.com/tokuhirom/mutsu/issues/7992) (2, parenthesised attribute list).

Three of them are **diagnostics** tickets — #8000, #7999 and the message half of
#7988 — and they are deliberately near the front: 17 distributions report only
`An exception occurred while evaluating a CHECK`, so nothing can be said about
what they need until that message carries its inner exception. Fixing a message
splits a cluster, which is progress even though it moves no KPI.

What is left after this batch is a long tail: 985 clusters in total, of which
these fifteen are the ones affecting ten or more distributions. The tail is a
**sampling** job (`--min-dists 1`), not an exhaustive-triage one.

## 10. Known limits and follow-ups

- **`--attempts` multiplies the cost of a red corpus.** The retry runs only on a
  file that did not pass, which is the right side to spend it on, but early in
  the campaign most files are that side. Lower it (`--attempts 1`) for a scouting
  run and restore it for a sweep whose numbers are going to be published.

- **Network-dependent suites are invisible.** They fail on both sides and land
  in `no_baseline`. Recorded as a count so the size of the blind spot is known;
  a future opt-in `--allow-network` mode for a vetted subset is possible but is
  not part of this design.
- **`blocked_dep` residue (39 dists, 2%).** Mostly malformed `depends` metadata
  upstream. Parsing prose-fragment dependency entries more forgivingly would
  recover a handful; it is not worth much.
- **Author (`xt/`) tests, `build-depends`, and dists whose `provides` uses a
  non-convention path** (resolvable only through an installed provides map) are
  each a small, recorded population rather than a silent one.
- **`mzef compat <Dist>`** — reading the shipped data from the installed
  distribution so `mzef install` can warn before installing something known
  red. Attractive, out of scope here.
- **Timing/concurrency suites** (S17-flavoured) are the expected home of
  `flaky: true`; if the retry rule proves insufficient, quarantine them the way
  `flaky-tests.txt` quarantines local tests rather than weakening the KPI. Rule
  out §2.8 first: a suite that is merely *slow* used to arrive here wearing the
  same label, and a `flaky: true` whose only non-passing verdict was a timeout
  is that case, not a timing bug.
