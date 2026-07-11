# Reference `raku` baseline over the roast suite

This records how the reference **`raku`** interpreter fares on every roast test,
so that mutsu work can be prioritised against a real oracle: a test that
**raku passes but mutsu does not** is an achievable gap, whereas a test that
**raku itself cannot pass** (6.e-only syntax, removed constructs, deliberately
failing `flunk`) is not worth chasing on mutsu.

- **Data:** [`raku-baseline.tsv`](raku-baseline.tsv) — one row per roast `.t` file.
- **Generator:** [`../scripts/roast-raku-baseline.sh`](../scripts/roast-raku-baseline.sh) — re-run to refresh.
- **Captured:** 2026-07-11, against `Rakudo v2022.12` (Raku **6.d**, MoarVM 2022.12).

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
   whitelisted file is usually a fudge/version artifact, not raku being worse than
   mutsu** — the real roast harness would `skip`/`todo` those subtests. This is why
   195 whitelisted files show `raku_status=FAIL` and 83 show `SORRY` (see below);
   they are noise for the comparison, not regressions.
2. **The reference raku here is 6.d (v2022.12).** `SORRY` frequently just means the
   test uses 6.e syntax this older raku cannot compile — not that the test is bad.
   A newer raku would pass more of them.
3. **The reliable signal is `raku_status=PASS`** — an *unfudged* pass is a strict
   lower bound (fudge only ever skips/todos, never turns a pass into a fail), so
   every `PASS` row is a test raku genuinely passes raw.

## Summary (all 1463 roast `.t` files, unfudged raku 6.d)

| raku_status | count | of which whitelisted | not whitelisted |
|---|---:|---:|---:|
| PASS    | 1063 | 1010 | **53** |
| FAIL    |  203 |  195 | 8 |
| SORRY   |  104 |   83 | 21 |
| ABORT   |   62 |   55 | 7 |
| TIMEOUT |   21 |   21 | 0 |
| NOPLAN  |   10 |    9 | 1 |

The 1010 `PASS ∧ whitelisted` are the healthy core (raku and mutsu both pass).
The FAIL/SORRY/ABORT/TIMEOUT columns on whitelisted rows are dominated by the
unfudged/6.d artifacts of caveat 1–2.

## Actionable: raku PASS but NOT whitelisted (53)

These are the tests raku passes raw that mutsu has **not** whitelisted. Running
mutsu (`MUTSU_FUDGE=1`) on each splits them:

### A. mutsu also PASSes — whitelist candidates (4)

mutsu already passes these; they were simply never added to the whitelist. Verify
via `MUTSU_FUDGE=1 prove -e target/debug/mutsu <file>` (and the real
`scripts/run-roast-test.sh` harness) before adding.

- ~~`roast/6.c/MISC/misc-6.c.t` (1/1)~~ — whitelisted (#4423).
- ~~`roast/integration/advent2010-day04.t` (11/11)~~ — whitelisted (#4423).
- ~~`roast/integration/advent2013-day19.t` (4/4)~~ — whitelisted (#4423).
- ~~`roast/integration/lazy-bentley-generator.t` (1/1)~~ — whitelisted (#4423).

### B. non-integration gaps — raku PASS, mutsu FAIL/ERROR (10)

Genuine mutsu gaps outside the `integration/` bucket. `mutsu` column is
`ok/plan` from an unfudged mutsu run. **DONE rows** landed 2026-07-11.

| file | mutsu | note |
|---|---|---|
| ~~`S32-hash/perl.t`~~ | 43→47/55 | per-holder hash itemization done (#4421); remaining 8 need parameterized-role type-capture binding |
| `S12-attributes/class.t` | 19/28 abort | §6 ★. Multi-feature: class-body `BEGIN EVAL` attr decl (test20), `EVAL` attr→NotFound (21), `is_run` err msgs (22-23), `where`-clause attr access `X::Syntax::NoSelf` (24-28) |
| `6.c/S14-roles/mixin-6c.t` | 16/57 | role mixin (6.c) の深い機能 |
| `6.c/S05-grammar/methods.t` | 4/8 | grammar-engine: die propagation from a `<.method>` subrule (T1/2), embedded `{...}` block not capturing the grammar's defining outer lexical (T3), duplicate-token error message (T6) |
| ~~`6.c/S14-roles/attributes.t`~~ | 2→3/3 | class attribute accessor prioritized over role method — done (#4425), whitelisted |
| `6.c/S06-other/main-refactored.t` | ERROR 0/501 | full new-MAIN interface (`ARGS-TO-CAPTURE`/`GENERATE-USAGE`/`MAIN_HELPER`/`USAGE`); aborts before test 1 |
| `6.c/MISC/bug-coverage.t` | ERROR | 起動時 error |
| `6.c/S03-operators/set_precedes.t` | ERROR | `(<)`/`(>)` set precedes 演算子 |
| `APPENDICES/A01-limits/overflow.t` | TIMEOUT 0/18 | 数値 overflow 限界（timeout, notok 9） |
| `APPENDICES/A02-some-day-maybe/multi-no-match.t` | 3/16 | multi 不一致時のエラー整形 |

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
