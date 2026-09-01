# Reference `raku` baseline over the roast suite

This records how the reference **`raku`** interpreter fares on every roast test,
so that mutsu work can be prioritised against a real oracle: a test that
**raku passes but mutsu does not** is an achievable gap, whereas a test that
**raku itself cannot pass** (6.e-only syntax, removed constructs, deliberately
failing `flunk`) is not worth chasing on mutsu.

- **Data:** [`raku-baseline.tsv`](raku-baseline.tsv) — one row per roast `.t` file.
- **Generator:** [`../scripts/roast-raku-baseline.sh`](../scripts/roast-raku-baseline.sh) — re-run to refresh.
- **Captured:** 2026-09-02, against `Rakudo v2026.07` (default language **6.d**, MoarVM 2026.07).
  Previous capture: 2026-07-12 against `Rakudo v2026.06` — see "v2026.07 refresh" below
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
   148 whitelisted files show `raku_status=FAIL` and 71 show `SORRY` (see below);
   they are noise for the comparison, not regressions.
2. **The reference raku is now v2026.07** (default language 6.d), so the old
   "6.e-only syntax on a 2022 raku" SORRY class is mostly gone. The remaining
   `SORRY` rows are removed constructs, rakudo-NYI syntax (`::=`, regex `::`),
   or unfudged fudge-dependent lines.
3. **The reliable signal is `raku_status=PASS`** — an *unfudged* pass is a strict
   lower bound (fudge only ever skips/todos, never turns a pass into a fail), so
   every `PASS` row is a test raku genuinely passes raw.

## Summary (all 1464 roast `.t` files, unfudged raku v2026.07)

| raku_status | count | of which whitelisted | not whitelisted |
|---|---:|---:|---:|
| PASS    | 1155 | 1154 | **1** |
| FAIL    |  151 |  148 | 3 |
| SORRY   |   90 |   71 | 19 |
| ABORT   |   45 |   40 | 5 |
| TIMEOUT |   13 |   13 | 0 |
| NOPLAN  |   10 |    9 | 1 |

The 1154 `PASS ∧ whitelisted` are the healthy core (raku and mutsu both pass).
The FAIL/SORRY/ABORT/TIMEOUT columns on whitelisted rows are dominated by the
unfudged artifacts of caveat 1.

### v2026.07 refresh (2026-09-02) — diff vs the v2026.06 capture

Raku-side `PASS` grew 1143 → 1155. Eleven existing files became `PASS`
(three `FAIL→PASS`, three `TIMEOUT→PASS`, and five `SORRY→PASS`); all are
already whitelisted, so they do not expose a mutsu gap. Five `sprintf` files
changed from `SORRY` to `TIMEOUT` under the unchanged 25-second cap. The
reference also now passes `S02-types/quanthash.t`, a new roast file, which
accounts for the 1,463 → 1,464 file-count increase.

## Actionable: raku PASS but NOT whitelisted (1)

| file | raku | mutsu | note |
|---|---:|---:|---|
| `S02-types/quanthash.t` | 129/129 | 4/129 abort | New roast test. mutsu raises `X::Method::NotFound` for `.new` on `Set[Str][Int(Any)]` at line 26. |

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
