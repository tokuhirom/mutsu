# Reference `raku` baseline over the roast suite

This records how the reference **`raku`** interpreter fares on every roast test,
so that mutsu work can be prioritised against a real oracle: a test that
**raku passes but mutsu does not** is an achievable gap, whereas a test that
**raku itself cannot pass** (6.e-only syntax, removed constructs, deliberately
failing `flunk`) is not worth chasing on mutsu.

- **Data:** [`raku-baseline.tsv`](raku-baseline.tsv) — one row per roast `.t` file.
- **Generator:** [`../scripts/roast-raku-baseline.sh`](../scripts/roast-raku-baseline.sh) — re-run to refresh.
- **Captured:** 2026-07-12, against `Rakudo v2026.06` (default language **6.d**, MoarVM 2026.06).
  Previous capture: 2026-07-11 against `Rakudo v2022.12` — see "v2026.06 refresh" below
  for the diff.

## Columns

| column | meaning |
|---|---|
| `path` | roast file path |
| `plan` | `1..N` plan count raku emitted (0 if none) |
| `ok` / `notok` | number of `ok ` / `not ok ` lines |
| `todo` | number of `# TODO` markers seen |
| `sorry` | 1 if raku hit a `===SORRY===` compile error |
| `exit` | raku process exit code (124 = timeout) |
| `raku_status` | classification (below) |
| `whitelisted` | 1 if the file is in `roast-whitelist.txt` (i.e. mutsu passes it) |

`raku_status`:

- `PASS` — `plan>0 && ok==plan && notok==0`
- `FAIL` — ran to a plan but `ok<plan` or `notok>0`
- `SORRY` — compile error (usually 6.e-only syntax on this 6.d raku)
- `ABORT` — started (`ok>0`) but ran fewer than `plan` with no explicit `not ok` (mid-file die)
- `NOPLAN` — no plan line and no `ok`/`sorry`
- `TIMEOUT` — killed by the 25s per-file timeout

## ⚠️ Caveats — read before using this as a scoreboard

1. **This raku run is UNFUDGED.** roast fudge directives (`#?rakudo skip/todo`,
   `#?v6`, ...) are *not* applied, because applying them via `roast/fudge` writes
   rewritten files under `roast/`, which is read-only in this repo. mutsu's own
   runs *do* apply fudge (`MUTSU_FUDGE=1`). Therefore a **`raku FAIL`/`SORRY` on a
   whitelisted file is usually a fudge artifact, not raku being worse than
   mutsu** — the real roast harness would `skip`/`todo` those subtests. This is why
   147 whitelisted files show `raku_status=FAIL` and 80 show `SORRY` (see below);
   they are noise for the comparison, not regressions.
2. **The reference raku is now v2026.06** (default language 6.d), so the old
   "6.e-only syntax on a 2022 raku" SORRY class is mostly gone. The remaining
   `SORRY` rows are removed constructs, rakudo-NYI syntax (`::=`, regex `::`),
   or unfudged fudge-dependent lines.
3. **The reliable signal is `raku_status=PASS`** — an *unfudged* pass is a strict
   lower bound (fudge only ever skips/todos, never turns a pass into a fail), so
   every `PASS` row is a test raku genuinely passes raw.

## Summary (all 1463 roast `.t` files, unfudged raku v2026.06)

| raku_status | count | of which whitelisted | not whitelisted |
|---|---:|---:|---:|
| PASS    | 1143 | 1097 | **46** |
| FAIL    |  154 |  147 | 7 |
| SORRY   |  100 |   80 | 20 |
| ABORT   |   45 |   39 | 6 |
| TIMEOUT |   11 |   11 | 0 |
| NOPLAN  |   10 |    9 | 1 |

The 1097 `PASS ∧ whitelisted` are the healthy core (raku and mutsu both pass).
The FAIL/SORRY/ABORT/TIMEOUT columns on whitelisted rows are dominated by the
unfudged artifacts of caveat 1.

### v2026.06 refresh (2026-07-12) — diff vs the v2022.12 capture

Raku-side `PASS` grew 1063 → 1143 (+80: 54 `FAIL→PASS`, 19 `ABORT→PASS`,
7 `TIMEOUT→PASS`, 3 `SORRY→PASS`). All but three of the newly-PASS files were
already whitelisted (they were unfudged/old-raku artifacts, caveats 1–2).
The three that matter — newly oracle-verified mutsu gaps — are:

- `S06-advanced/return-prioritization.t` (raku 11/11, mutsu 9/11) — **new ★ in
  [BLOCKERS.md](BLOCKERS.md)**: `return` inside LEAVE phasers.
- `S32-str/format.t` (raku 49/49, mutsu 26/49 abort) — oracle available now, but
  still awaiting infrastructure (needs the RakuAST subsystem).
- `S02-types/generics.t` (raku 1/1, mutsu 0/1) — oracle available now, still
  awaiting infrastructure (6.e generics / `Array[T]` subclassing).

Raku-side reversals, all on whitelisted files (noise for mutsu): 
`S10-packages/precompilation.t` and `S17-procasync/stress.t` `PASS→TIMEOUT`
(25s cap, first-run precomp/load cost of the new raku), `S32-array/shift.t`
`PASS→FAIL`, `S10-packages/require-and-use--dead-file.t` `FAIL→SORRY`.

## Actionable: raku PASS but NOT whitelisted (46)

These are the tests raku passes raw that mutsu has **not** whitelisted. Running
mutsu (`MUTSU_FUDGE=1`) on each splits them:

### A. mutsu also PASSes — whitelist candidates (0)

All four candidates from the 2026-07-11 capture (`6.c/MISC/misc-6.c.t`,
`integration/advent2010-day04.t`, `advent2013-day19.t`,
`lazy-bentley-generator.t`) were whitelisted in #4423. None remain.

### B. non-integration gaps — raku PASS, mutsu FAIL/ERROR (7)

Genuine mutsu gaps outside the `integration/` bucket. `mutsu` column is
`ok/plan`. Rows closed since the 2026-07-11 capture: `S32-hash/perl.t` (#4452),
`S12-attributes/class.t`, `6.c/S14-roles/attributes.t` (#4425),
`6.c/S05-grammar/methods.t` (#4449), `6.c/S06-other/main-refactored.t`,
`6.c/S03-operators/set_precedes.t` — all whitelisted.

| file | mutsu | note |
|---|---|---|
| `S06-advanced/return-prioritization.t` | 9/11 | **new ★ (v2026.06 refresh)** — `return` inside LEAVE phaser: overwrite return value (T5), different lexical scope (T9). See BLOCKERS.md |
| `S32-str/format.t` | 26/49 abort | new oracle (v2026.06) but awaiting infrastructure — RakuAST subsystem |
| `S02-types/generics.t` | 0/1 | new oracle (v2026.06) but awaiting infrastructure — 6.e generics / `Array[T]` subclassing |
| `6.c/S14-roles/mixin-6c.t` | 16/57 | deep role mixin (6.c) features |
| `6.c/MISC/bug-coverage.t` | ERROR | error at startup |
| `APPENDICES/A01-limits/overflow.t` | TIMEOUT 0/18 | numeric overflow limits (timeout, notok 9) |
| `APPENDICES/A02-some-day-maybe/multi-no-match.t` | 3/16 | error formatting when no multi candidate matches |

### C. integration gaps — raku PASS, mutsu not-PASS (39)

Broad end-to-end programs (Advent-of-Code-style, 99-problems). Each exercises
many features at once, so they fail on the first unimplemented construct rather
than pointing at one root cause. Useful as regression targets once B is closed.
See `raku-baseline.tsv` (`raku_status=PASS && whitelisted=0 && path ~ integration/`)
for the full list; representative:

- `integration/99problems-31-to-40.t` (mutsu 44/67) — closest to passing
- `integration/advent2013-day10.t` (39/44), `advent2013-day21.t` (19/24),
  `advent2009-day20.t` (17/21) — near-misses
- several `ERROR 0/0` (fail to even start): `advent2012-day15/19`,
  `advent2013-day04`, `advent2011-day07`, `precompiled.t` (module precompilation)

## Regenerating

```bash
# ~20-30 min; writes TODO_roast/raku-baseline.tsv
scripts/roast-raku-baseline.sh
# Optional overrides:
RAKU=/path/to/raku RAKU_BASELINE_TIMEOUT=25 scripts/roast-raku-baseline.sh
```

To refresh against a **newer raku** (recommended — a 6.e raku would reclassify
many `SORRY` rows to `PASS`), point `RAKU` at it and re-run; update the
"Captured / against" line above with the new version.
