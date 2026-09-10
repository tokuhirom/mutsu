---
name: ecosystem-dist-fix
description: Make one real zef distribution's own test suite pass under mutsu — download the dist and its dependency closure, run every test file under rakudo and mutsu, fix the interpreter where the gap is bounded, file an issue where it needs a complex feature, and land it as a PR that also updates the ecosystem/ ledger record. Use when asked to make a named distribution's tests pass ("String::Utils のテストを通るようにして", "get Trie green", "fix the JSON::Fast suite"), or to work the red/partial/blocked_load records in ecosystem/.
metadata:
  short-description: Take one zef distribution from red to green
---

# Ecosystem distribution fix

One distribution, end to end: from the `ecosystem/` ledger record to a merged PR. The unit of
success is **a test file rakudo passes and mutsu now passes too** — not "the suite looks better".

The method, the metric definitions and the fairness contract are
[docs/ecosystem-parity.md](../../../docs/ecosystem-parity.md); the decisions behind them are
[ADR-0085](../../../docs/adr/0085-ecosystem-testsuite-parity-measurement.md). This file is the
per-distribution loop that consumes them. Read the parity doc's §1-§3 once before your first
distribution; after that, this page is enough.

The `gh` commands below are the local-dev-box form. A remote container has no `gh` — translate with
the mapping table in [docs/agent-environments.md](../../../docs/agent-environments.md).

## Ground rules — these decide whether the result means anything

1. **rakudo is the denominator, and you check it first.** A file rakudo does not pass cleanly is
   `no_baseline`: it is never charged to mutsu and never worth fixing. Run `raku` on the exact same
   file with the exact same `-I` list *before* calling anything a mutsu bug. Suites that need the
   network, a database, or a `zef`-installed repository fail on both sides — that is the expected
   outcome, not a finding.
2. **`MUTSU_FUDGE` must be unset.** It is roast-only; a stray `#?rakudo skip` in a distribution
   would silently drop the next statement and hand you a fake pass.
3. **Never edit the distribution to make the test pass.** The extracted tree under `tmp/ecosystem/`
   is a *measurement subject*. Editing it to bisect which construct dies is good debugging; shipping
   anything from it is not a deliverable, and neither is a patch upstream. The fix goes in mutsu.
4. **Never special-case the distribution inside mutsu.** No hardcoded outputs, no "if the module is
   `String::Utils`" branches, no stubs. `CLAUDE.md`'s rule for roast holds here verbatim: every fix
   is a genuine, general-purpose improvement.
5. **Do not answer a hard distribution by bundling it.** Vendoring the module into `modules/` or
   reimplementing it natively is BATTERIES.md rung 3, **banned by user decision** — see `CLAUDE.md`.
   Grow the interpreter until the real, upstream module runs verbatim, or file the issue.
6. **Dependencies arrive only as `-I` paths.** Never `mzef install` them into a site repo: mutsu's
   bundled batteries would then supply something rakudo lacks and the comparison stops being one.

## 1. Read the ledger record before you download anything

```sh
cat ecosystem/dists/S/String--Utils.json      # <S> = first letter; :: becomes --
```

`status` tells you what kind of job this is, and it is worth knowing before you spend a build:

| `status` | what it means | first move |
|---|---|---|
| `blocked_load` | a provided module does not even `use` | fix the load. Nothing else in the suite can run, and this is usually one concrete parse/compile gap |
| `red` / `partial` | the suite runs, some baseline files fail | go file by file; `files[].cmp == "regression"` and `"partial"` are the actionable ones |
| `no_baseline` | rakudo does not pass it either | **stop.** Report that and pick another distribution — there is nothing here to fix |
| `blocked_dep` | the dependency closure cannot be resolved | usually malformed upstream `depends` metadata. Not a mutsu bug; not worth chasing |
| `green` | already passing | re-measure to confirm, then stop |

Also read `axis`: `pure` distributions are the ones that reward interpreter work. `guts` (`nqp::`,
Metamodel, `EXPORTHOW`) and `native` (`NativeCall`) distributions are usually the issue-filing case
of §5, not the fix case — decide that deliberately rather than discovering it after an hour.

`measured.mutsu_commit` says how old the record is. A record from before a fix you know landed is
evidence about that commit, not about `main`; re-measure rather than trusting it.

If there is no record at all, that is fine — the corpus sweep is partial (see
`ecosystem/README.md`). Go to step 2 and create one in step 7.

## 2. Check the distribution out

```sh
.agents/skills/ecosystem-dist-fix/checkout-dist.py String::Utils
```

It resolves the same flat dependency closure the sweep does, extracts everything under
`tmp/ecosystem/<Dist--Name>/` (gitignored, and reused on re-runs), and prints `DIST`, `LIBS`,
`MUTSU`, `RAKU` plus the test file list. `--json` gives the same as a machine-readable plan;
`--force` re-extracts. Set `MUTSU_BIN` first if you want a binary other than `target/debug/mutsu`.

It does **not** sandbox — you are about to execute a third-party test suite on this machine. Skim
the suite before running it, exactly as the sweep's `--sandbox none` escape hatch assumes for a
single distribution you already trust.

## 3. Load probe, then file by file

Always the load probe first: a module that does not `use` makes every file failure downstream noise.

```sh
cd "$DIST"
timeout 60 $RAKU  $LIBS -e 'use String::Utils; exit 0'
timeout 60 $MUTSU $LIBS -e 'use String::Utils; exit 0'
```

Then, for each test file, rakudo first and mutsu second:

```sh
timeout 120 $RAKU  $LIBS t/01-basic.rakutest
timeout 120 $MUTSU $LIBS t/01-basic.rakutest
```

Classify each file exactly as the ledger does — `parity`, `partial` (mutsu reaches the plan and
fails assertions), `regression` (dies, hangs, or never reaches the plan), `no_baseline`. Work
`regression` before `partial`: a file that dies at line 3 is hiding every gap after it, so one fix
there often moves more than a careful assertion hunt.

Never run the two sides with different `-I` lists, a different working directory, or a different
timeout. The distribution root is the working directory because suites reach for fixtures by
relative path.

## 4. Reduce to a repro that lives in this repo

The distribution is not in mutsu's CI and never will be, so a failure only counts once it exists as
a few lines you can run from the repo. Write the reduction to `tmp/` with the Write tool, and put
mutsu's and rakudo's output side by side:

```sh
timeout 30 target/debug/mutsu ./tmp/repro.p6
raku ./tmp/repro.p6
```

That reduction is three things at once: the thing you debug, the regression test you will commit in
step 6, and the body of the issue you file in step 5. Do not skip it and debug against the
distribution's 124-assertion test file.

`CLAUDE.md`'s debugging guidance applies unchanged — `--dump-ast`, `MUTSU_TRACE`, and
`rust-gdb -batch` before any `eprintln!`.

## 5. Decide: fix it now, or file an issue

**This decision is the point of the skill.** Make it per finding, not per distribution — one
distribution routinely produces two quick fixes and one issue, and that is a complete, successful
run.

**Fix it in this PR** when the gap is bounded and needs no new architectural decision:

- a missing or wrong method / builtin / operator, or an unhandled edge case in an existing one;
- a parser gap for syntax that has an obvious home in the existing grammar;
- a compile/VM slice that follows the existing Parser → Compiler → VM path;
- anything you can pin with a focused `t/` test and explain in one paragraph.

**File an issue instead** — this is what "複雑な機能が必要なら" means — when the fix needs:

- a new or superseding **ADR**, or a decision `CLAUDE.md` reserves for the user;
- **deep machinery**: `nqp::` ops, Metamodel/MOP, `EXPORTHOW`, slangs, precompilation, `NativeCall`
  guts — the `guts` / `native` axis;
- a **cross-cutting invariant** across execution layers (container semantics, laziness, the
  `env_dirty` dual store, concurrency ordering);
- anything that cannot be bounded as one PR, or whose right answer you cannot state confidently.

Label it by `docs/issue-workflow.md`: `todo:ticket` for a small self-contained slice you are simply
not doing now, `todo:deep` for the design-needed cases above, `todo:perf` only when mutsu is
*correct but slow* — a wrong answer is never `todo:perf`. Title it after the missing capability, not
after the distribution ("`EXPORTHOW::DECLARE` … " not "String::Utils fails"), because the next
distribution to hit it must find the issue. The body carries the §4 reduction with both
interpreters' output, the distribution and test file it came from, and why it is large. One issue
per finding; only ever in `tokuhirom/mutsu`.

Then say so in the PR: the distribution's remaining red files each name their issue number. A
distribution that goes from `red` to `partial` with the residue filed is a good outcome. What is
**not** acceptable is leaving a finding unrecorded because it was too big to fix — that is the one
way this loop loses work.

## 6. Fix, and pin it in `t/`

The change goes where `CLAUDE.md` says it goes: implement in `compiler/` and `vm/`, never a new
`runtime/methods.rs` slow-path fallback. When the spec is unclear, `raku -e` is the oracle and
`raku-doc/` is the reference.

**Every fix commits a focused regression test under `t/`** — the §4 reduction, placed by
[docs/t-directory-layout.md](../../../docs/t-directory-layout.md) (by what it would catch, not by
the syntax it uses). The ecosystem sweep is operator-run and not in CI, so this `t/` file is the
*only* thing standing between your fix and a silent regression. Name it after the capability, and
say in a comment which distribution it came from.

Then the standard gate, before publishing: `cargo fmt --all`, `make lint`, `make test`,
`make roast`, reading `tmp/make-test.log` / `tmp/make-roast.log` with the Grep tool rather than
re-running a suite. In a remote container, confirm any red `make roast` is a subset **by name** of
the environment-only failures in
[docs/agent-environments.md](../../../docs/agent-environments.md).

## 7. Re-measure and update the ledger record

The record is part of the deliverable — a fix that leaves the ledger saying `red` has not been
reported.

```sh
touch src/main.rs && cargo build --release        # a stale binary is measured silently
MUTSU_BIN=target/release/mutsu scripts/ecosystem-sweep.py --only String::Utils --sandbox none
```

- `--sandbox none` is permitted **only** with `--only`, for a distribution you have read. A shard or
  corpus sweep needs `bubblewrap`, which a remote container does not have — do not attempt one there.
- The sweep aborts unless `use Test` reaches the vendored `modules/Rakudo-Core/lib/Test.rakumod` on
  both sides. That is the fairness precondition, not a nuisance: fix it rather than working around it.
- Timestamps are date-granular, so an unchanged same-day re-run produces no diff. A record that did
  not change when you expected it to means the fix did not reach the measured binary.
- Do **not** run `--rollup --history` for one distribution: `history.tsv` takes one row per *full*
  sweep, and a row from a single dist is not comparable with the ones around it.
- `docs/ecosystem-parity.md` §5 lists `--mutsu-only`, `--refresh-baseline` and `--json -`. Those are
  design, not implementation — the harness does not accept them today. Use the flags in
  `--help`.

Commit the changed `ecosystem/dists/<S>/<Dist--Name>.json` in the same PR as the fix.

## 8. Publish

Branch off an updated `main`, commit (English, root cause in the message), push, open a
**non-draft** PR, enable auto-merge with the **merge** method (squash is rejected by this
repository), then immediately check `mergeStateStatus` is not `DIRTY` — the ledger records are
one-file-per-dist precisely so parallel PRs do not conflict, but `src/` still can. Watch CI in the
background and fix forward. The full flow, including the one-branch-per-session case, is
[`mutsu-ticket-flow`](../mutsu-ticket-flow/SKILL.md).

The PR body states: the distribution and version, baseline files before → after, what was fixed,
and the issue number for every file still red. Write the accomplishment up as
`news/YYYY-MM/<slug>.md`, and close any issue the fix resolves with `Closes #NNNN`.

## Done means

Either the distribution's record reads `green`, or every remaining non-`parity` baseline file is
explained by an open issue named in the PR. "I improved some assertions" is not a finish line;
"3 of 3 baseline files pass, and `t/02` needs #NNNN" is.
