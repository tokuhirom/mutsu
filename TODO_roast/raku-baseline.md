# Reference `raku` baseline over the roast suite

This records how the reference **`raku`** interpreter fares on every roast test,
so that mutsu work can be prioritised against a real oracle: a test that
**raku passes but mutsu does not** is an achievable gap, whereas a test that
**raku itself cannot pass** (6.e-only syntax, removed constructs, deliberately
failing `flunk`) is not worth chasing on mutsu.

- **Data:** [`raku-baseline.tsv`](raku-baseline.tsv) — one row per roast `.t` file.
- **Generator:** [`../scripts/roast-raku-baseline.sh`](../scripts/roast-raku-baseline.sh) — re-run to refresh.
- **Captured:** 2026-10-24, against `Rakudo v2026.09` (default language **6.d**, MoarVM 2026.09).
  Previous capture: 2026-09-02 against `Rakudo v2026.07` — see "v2026.09 refresh" below
  for the diff; the one before that (2026-07-12, `v2026.06`) is in "v2026.07 refresh".

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
2. **The reference raku is now v2026.09** (default language 6.d), so the old
   "6.e-only syntax on a 2022 raku" SORRY class is mostly gone. The remaining
   `SORRY` rows are removed constructs, rakudo-NYI syntax (`::=`, regex `::`),
   or unfudged fudge-dependent lines.
3. **The reliable signal is `raku_status=PASS`** — an *unfudged* pass is a strict
   lower bound (fudge only ever skips/todos, never turns a pass into a fail), so
   every `PASS` row is a test raku genuinely passes raw.

## Summary (all 1454 roast `.t` files, unfudged raku v2026.09)

| raku_status | count | of which whitelisted | not whitelisted |
|---|---:|---:|---:|
| PASS    | 1164 | 1163 | **1** |
| FAIL    |  135 |  133 | 2 |
| SORRY   |   82 |   63 | 19 |
| ABORT   |   48 |   45 | 3 |
| TIMEOUT |   15 |   15 | 0 |
| NOPLAN  |   10 |    9 | 1 |

The 1163 `PASS ∧ whitelisted` are the healthy core (raku and mutsu both pass).
The FAIL/SORRY/ABORT/TIMEOUT columns on whitelisted rows are dominated by the
unfudged artifacts of caveat 1.

### v2026.09 refresh (2026-10-24) — diff vs the v2026.07 capture

Re-captured so that `Interpreter::RAKUDO_COMPAT_VERSION` (ADR-0104) can move to
`2026.09`, the release the ecosystem ledger already runs (#14851). The committed
v2026.07 TSV had 1453 rows (the 1464/1155 figures in the section below were not
reproducible from it, so this diff is against the TSV, not that summary); the new
one has 1454 (`S32-str/sprintf-a.t` is new in roast). Only files whose
`raku_status` changed are listed; no mutsu behaviour changed in this refresh.

- **Raku newly passes (22):** `6.c/S04-declarations/my-6c.t`,
  `APPENDICES/A02-some-day-maybe/misc.t`, `S02-literals/{adverbs,format,pairs,version}.t`,
  `S02-magicals/sub.t`, `S03-operators/also.t`, `S04-declarations/{my-6e,smiley,state}.t`,
  `S06-signature/{code,outside-subroutine}.t`, `S06-traits/precedence.t`,
  `S12-attributes/smiley.t`, `S16-filehandles/filestat.t`,
  `S19-command-line-options/02-dash-n.t`, `S26-documentation/{07a-tables,09-configuration,block-leading-user-format,why-trailing}.t`,
  `S32-exceptions/misc2.t`. All but `A02-some-day-maybe/misc.t` (6/6 on raku) are
  already whitelisted, so they expose no mutsu gap; that one is the only
  `PASS ∧ not whitelisted` row now (`S02-types/quanthash.t` joined the whitelist).
- **Raku newly fails to compile or run further:** `S03-operators/adverbial-modifiers.t`
  and `S04-declarations/constant.t` (`FAIL→SORRY`); `S05-capture/{alias,array-alias}.t`,
  `S09-typed-arrays/arrays.t`, `S14-roles/generic-subtyping.t` (`SORRY→FAIL`);
  `S06-operator-overloading/infix.t`, `S12-methods/syntax.t` (`SORRY→ABORT`);
  `6.c/APPENDICES/A04-experimental/01-misc.t` (`ABORT→FAIL`). These move
  forward rather than backward (a compile error became a run that reaches a plan);
  all are whitelisted, i.e. fudge artifacts.
- **Environment-sensitive rows, not a raku change:** `6.c/S32-io/file-tests.t`,
  `S32-io/chdir.t`, `S32-io/indir.t` (`PASS→FAIL`) fail on "fails because of
  permissions" subtests because this capture ran as root in a container;
  `S16-io/eof.t`, `S32-io/IO-Socket-Async.t` (`PASS→ABORT`) and `S06-multi/by-trait.t`
  (`PASS→FAIL`) are the same kind of sandbox/network sensitivity; `S11-modules/versioning.t`,
  `S15-normalization/nfc-concat.t`, `S29-os/system.t` (`→TIMEOUT`) pass or nearly pass
  with a longer cap on a quiet machine. Treat these as capture-environment noise;
  re-measure on the 12-core box before drawing a conclusion.
- **Capture method:** `scripts/roast-raku-baseline.sh` now takes `RAKU_BASELINE_JOBS`
  (default 1) and sorts the rows by path, so the output does not depend on the job
  count. This run used 3 jobs on a 4-core container; the 43 files whose status
  differed from the old TSV were then re-run serially and the serial result kept
  (`S10-packages/precompilation.t` and `S17-procasync/stress.t` had timed out only
  under the parallel load and stayed `PASS`). Roast tests that rewrite files under
  `roast/` or the cwd leave the tree dirty after a run; `git checkout -- roast`
  and delete stray files before committing.

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
# ~20-30 min serial (~10 min with RAKU_BASELINE_JOBS=3); writes TODO_roast/raku-baseline.tsv
scripts/roast-raku-baseline.sh
# Optional overrides:
RAKU=/path/to/raku RAKU_BASELINE_TIMEOUT=25 scripts/roast-raku-baseline.sh
```

To refresh against a **newer raku** (recommended — a 6.e raku would reclassify
many `SORRY` rows to `PASS`), point `RAKU` at it and re-run; update the
"Captured / against" line above with the new version.
