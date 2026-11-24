# PLAN.md — mutsu implementation plan

> **This file is an outline of unfinished work — nothing more.** Each item says *what* is left and
> *where the detail lives*; it does not carry investigation notes, measurements, or progress logs.
>
> | kind of information | where it lives |
> |---|---|
> | completed work | [news/](news/) — one file per accomplishment |
> | open findings | GitHub issues on `tokuhirom/mutsu`, labelled `todo:ticket` / `todo:deep` / `todo:perf` — [docs/issue-workflow.md](docs/issue-workflow.md) |
> | which finding to pick up next | the `tier:*` labels on the issues themselves — `tier:S` first, then `tier:B`, `tier:N`; `tier:icebox` is out of the queue. Untiered `todo:*` issues are where the next triage pass starts |
> | architectural decisions | [docs/adr/](docs/adr/) |
> | non-whitelisted roast files | [docs/roast-compatibility.md](docs/roast-compatibility.md#non-whitelisted-roast-files-and-why) |
> | performance numbers | the bench CI (`bench-data` branch); how to read them: [docs/benchmarks.md](docs/benchmarks.md) |
>
> Do **not** append progress notes here. A GitHub issue or a new file under `news/` conflicts with
> nothing on merge; an append to this file conflicts with every other in-flight PR.

## Goal — a batteries-included Raku implementation

Build a **Raku interpreter where installing mutsu alone gives you a well-documented standard bundled
library, so you can write practical code immediately** — the Raku version of the positioning bun took
for JavaScript. Bundling is not new to Raku: [Rakudo Star](https://rakudo.org/star) has shipped the
compiler with a set of ecosystem modules (installed through zef, ~76 entries in its `etc/modules.txt`)
since 2010. mutsu's position is the delivery: one self-contained binary (mise, Docker, WebAssembly)
whose batteries are vendored verbatim, run against their own upstream test suites, and documented.
Four components:

1. **Fast-startup compatible interpreter** — maintain and expand raku compatibility. → §3
2. **Standard bundled libraries (batteries)**, every one documented. → §1
3. **Bundled package manager `mzef`**, vendoring the real Zef. → §1 B2
4. **Substrate quality** — GC (done, default on), JIT (done, default on), performance, error
   messages. → §2 / §4

### Standing rules

- **1 operation = 1 implementation** (user policy 2026-06-07). Write each Raku operation exactly once
  in the VM/native layer (`src/vm/` + `src/builtins/`); other call paths (EVAL, embedded regex
  blocks) **delegate** to it. When you find a duplicate, make the native one canonical and delete the
  copy.
- **Phase order is fixed by [ADR-0001](docs/adr/0001-gc-strategy-and-phasing.md)**: A (catch up) →
  A' → B (value representation + GC) → C (JIT). A, B and C have all landed; read the ADR before
  touching GC, Track B, NaN-boxing, or JIT.
- **Bundling policy is [ADR-0096](docs/adr/0096-batteries-adoption-policy.md)**, implemented by
  [BATTERIES.md](BATTERIES.md): adopt the upstream module verbatim and grow mutsu until it runs.
  Providing a module "natively" is banned going forward, and a performance measurement does not
  buy an exemption — speed justifies a transparent optimization, never a substitution under the
  real module's name. The JSON interception is retired
  ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)), and `NativeCall` is being moved to
  the vendored upstream module ([ADR-11203](docs/adr/11203-nativecall-runs-upstream-via-the-backend-neutral-path.md),
  [#11203](https://github.com/tokuhirom/mutsu/issues/11203)).

---

## 0. Architecture projects that must be finished

Each project here replaces a mechanism the interpreter outgrew, is spread over many PRs, and ends
in a **deletion** (for a coverage project, an empty missing list): it is done when the old path
is gone, not when the new one mostly works. None is a background campaign to drop when it gets
hard — keep working them alongside §1 (user decision, 2026-10-02). The list is open: a new project of this kind is added as one more item, with its ADR
and its deletion criterion.

- [ ] **RakuAST becomes the frontend IR** —
      [ADR-10723](docs/adr/10723-rakuast-is-the-frontend-ir.md) (supersedes ADR-0011); roadmap,
      metric and stage checklist: [#7564](https://github.com/tokuhirom/mutsu/issues/7564). Rakudo
      2026.09 made RakuAST its default frontend and removes the legacy grammar at 6.e, and slangs,
      L10N, RakuDoc v2 and `.DEPARSE` now build on it. The parser emits the RakuAST tree, `lower` is
      the only desugaring step, and `Expr`/`Stmt` stays as the compiler's lowered input.
      - [ ] Stage 1 — close the round trip by moving desugaring out of the parser into `lower`.
      - [ ] Stage 2 — the parser emits RakuAST, construct family by family; `convert.rs` shrinks to
            nothing. Decide source positions and the node representation first (ADR §4).
      - [ ] Stage 3 — flip the default at parity and within the frontend-cost budget; delete the
            legacy path and `convert.rs`. **This deletion is the completion criterion.**
      - [ ] Stage 4 — slangs by execution (a follow-up ADR superseding ADR-0026 §4), RakuDoc v2,
            total `.DEPARSE`.
- [ ] **Built-in methods are handler rows in the one method table** —
      [ADR-11276](docs/adr/11276-built-in-methods-are-handler-rows.md),
      [#11276](https://github.com/tokuhirom/mutsu/issues/11276): a built-in method is one
      registered row `(owner, name, shape, flags, handler)`, and calls reach it through that row
      and the call-site cache. This replaces the name-matching `match` cascades
      (`builtins/methods_0arg/`, `builtins/methods_narg/`, the slow-path arms in
      `runtime/methods*.rs`). Slices 1a/1b (the table, the `CallMethodMut` lane and its per-site
      memo) have landed; the remaining slices and their live status are in ADR §10 and the issue
      body. Each slice deletes the arms it moves. The completion criterion is ADR §6 slice 5: the cascades,
      `try_native_method_raw`'s name checks, `should_bypass_native_fastpath`,
      `native_method_row_table.rs` and the `native_call_unmodeled` counter are deleted, so rows are
      the only source of truth for both dispatch and `.^can`/`.^methods`. Progress is recorded in
      ADR-11276 §9, not here.
- [ ] **The slow-path fallbacks are retired.** AGENTS.md bans new slow-path / tree-walk
      fallbacks; the ratchet that makes the existing debt measurable landed with
      [#11510](https://github.com/tokuhirom/mutsu/issues/11510), and this item ends in its deletion.
      `make check-slow-path` counts `TODO: compile to bytecode` markers and the
      `call_method_with_values(` call sites under `src/vm/` per file
      (`scripts/slow-path-baseline.txt`; a count may fall without an edit, never rise), and
      `scripts/slow-path-survey.sh` sums the distinct `Class.method` shapes that still enter
      `run_instance_method` over `t/` and the roast whitelist. The completion
      criterion is the deletion of `run_instance_method`'s role as a fallback and of the
      `runtime/methods*.rs` slow-path cascades that serve **user-class** dispatch and the
      `eval_block_value` carriers. The built-in cascades are the previous item's (ADR-11276); this
      one covers what that ADR does not reach.
- [ ] **`Interpreter` is split into subsystems, and the lower layers stop naming the upper ones** —
      [ADR-10779](docs/adr/10779-interpreter-subsystems-and-upward-call-traits.md),
      [#10779](https://github.com/tokuhirom/mutsu/issues/10779). `struct Interpreter` keeps only the
      VM frame core and holds one type per subsystem. Each subsystem type owns its fields and states
      its own thread policy in `fork_for_thread`. The `pending_*` side channels become explicit
      parameters (D3). The AST, parser, `Value`, `opcode`, `Env` and the GC reach the runtime only
      through narrow traits (D5). The field map behind the subsystem list is
      [docs/interpreter-state-map.md](docs/interpreter-state-map.md).
      - [ ] Phase 3 — extract the subsystems, one per PR. `make check-interp-fields` blocks new
            direct fields; an extraction adds its holder name to
            `scripts/interp-fields.d/<subsystem>.txt`. Phases 1-2 (helpers moved down, the
            field × file map) are done; which subsystems remain is in ADR-10779 §Implementation
            status.
      - [ ] Phase 4 — the crate split, a separate measured decision (ADR D7).
      The completion criterion is reached when these are deleted:
      - every `Interpreter` field outside the frame core and the subsystem holders;
      - the `pending_*` handoff fields;
      - the per-field body of `clone_for_thread`, which becomes one `fork_for_thread` call per
        subsystem;
      - every entry of `scripts/layer-deps-baseline.txt`, so `check-layer-deps` reads 0.

      Progress is recorded in ADR-10779 §Implementation status, not here.
- [ ] **Every documented `nqp::` op is implemented** — tracking issue
      [#11488](https://github.com/tokuhirom/mutsu/issues/11488) (user decision 2026-10-03). This
      replaces the 2026-07 "added on demand, never as a porting campaign" rule
      (`news/2026-07/nqp-op-layer-measured-and-rejected.md`): per dist the op set is a **threshold
      function**, which is an argument for closing the whole set, not for adding one op per failing
      dist. Inventory: [docs/nqp-op-coverage.md](docs/nqp-op-coverage.md), regenerated by
      `scripts/nqp-op-coverage.py`, with one sub-issue per category group (#11490–#11505,
      #11553). Each op shares its routine with the
      Raku-level primitive (ADR-0117/0118), and each landing PR re-runs the script. The completion
      criterion is an empty "Missing" column: every op is implemented or recorded under "Not
      applicable" with its reason (never stubbed).

RakuAST and the regex engine meet at the regex source tree ([ADR-0088](docs/adr/0088-rakuast-regex-boundary-tree.md)):
a regex the parser keeps as a tree is both what RakuAST renders and what `RxVm` compiles, so the
RakuAST work may not reintroduce a regex kept only as source text.

---

## 1. 🔋 Batteries — bundled libraries and distribution (main effort)

40 libraries are vendored under `modules/` and resolved with zero configuration; the release-time
gate runs their upstream suites against the shipped copies
([docs/batteries/testsuite-gate.md](docs/batteries/testsuite-gate.md)) and is **all-green, so a drop
below the whitelisted baseline is a regression to fix, not a baseline to accept**. Adding a battery
re-opens work: whitelist what passes with `scripts/battery-testsuite.sh --update`, then close the
gaps with general interpreter fixes.

### B1. Bundle set and documentation

- [ ] **Finalize the bundle list.** Selection criterion: "a web blog can be written with the bundle
      alone". Method and per-battery selection records:
      [docs/batteries/](docs/batteries/), [BATTERIES.md](BATTERIES.md). Candidate slots ranked by
      value: [python-stdlib-comparison.md](docs/batteries/python-stdlib-comparison.md).
- [ ] **Documentation per battery** — a usage document (no install needed, API, examples) for each
      bundled library. "Well-documented" is an explicit goal requirement, so this is mandatory when
      adding a module.
- [ ] **Working-module regression CI** (report-only, on main push; user policy 2026-06-28):
      continuously detect whether modules that once worked keep working.

### B2. mzef — the bundled package manager

Installing mutsu gives you `mzef`, which drives **upstream Zef itself** (`vendor/zef/`) — also the
project's strongest compatibility north star. Install, fetch, dependency resolution, and the test
phase all work end to end.

- [ ] **Live tracker: [docs/mzef-install-pipeline.md](docs/mzef-install-pipeline.md)** — phase table,
      what each fix unblocked, and the current frontier. Read it before picking up mzef work.

### B3. Distribution and tooling

Release tarballs (4 targets), the GHCR container image, and `mise use -g github:tokuhirom/mutsu` all
work; see `docs/mzef-install-pipeline.md`. The **REPL** (`--repl`,
`src/repl.rs`, plus the in-browser one at `site/repl.html`) and the **public WASM playground**
(`site/playground.html`, deployed to GitHub Pages by `.github/workflows/pages.yml`) both ship.

- [ ] **Language server** — designed in [docs/adr/0065-language-server-targets-ai-agents.md](docs/adr/0065-language-server-targets-ai-agents.md)
      (an AI agent is the primary consumer); implementation and status in
      [docs/language-server.md](docs/language-server.md). Next is **S5b: `references`** — start
      from [#7547](https://github.com/tokuhirom/mutsu/issues/7547) (a parser side table rather than
      spans on AST variants; first measure the phantom-reference rate that parser backtracking
      causes). S2's method half is deferred on a real design question (the receiver type); see
      the ADR's S2 findings.
- [ ] Debugger.
- [ ] Native binary output.

### B4. Module-compatibility frontier (the base of batteries)

- [ ] **★Ecosystem parity: per-distribution work** — the KPI itself is **built and running**; what
      remains is turning red records green, one distribution (or one root cause) at a time. The
      corpus is measured nightly and published, with per-distribution records and the chart
      `ecosystem/history.svg` on the `ecosystem-data` branch (`scripts/ecosystem-ledger.sh pull`). Pick work one of two ways — by **root cause** from the clustered
      issues (`scripts/ecosystem-tickets.py`, see docs/ecosystem-parity.md §9; the ten largest
      clusters are filed as `todo:*` issues), or by **distribution** with
      [`ecosystem-dist-fix`](.agents/skills/ecosystem-dist-fix/SKILL.md) (named) /
      [`ecosystem-dist-queue`](.agents/skills/ecosystem-dist-queue/SKILL.md) (the next entry of
      `scripts/ecosystem-tickets.py --queue`, a fixed order over every failing distribution, each
      claimed through its tracking issue so parallel agents do not collide; what a sweep made worse
      is the "Regressed since the previous ledger" section of `ecosystem/summary.md`). Prefer a root cause when one covers several distributions; working the whole queue
      in order, never skipping an entry for looking hard, is what keeps the published figure honest.
      Method: [docs/ecosystem-parity.md](docs/ecosystem-parity.md);
      decisions: [ADR-0085](docs/adr/0085-ecosystem-testsuite-parity-measurement.md). The sweep is a measurement, not a gate, so it does **not** close B1's
      "working-module regression CI" — that stays a separate item.
- [ ] **★Real-dist compatibility sweep** — run real fez dists under mutsu and fix the general bugs
      they surface. Ledger: [docs/dist-compat-sweep.md](docs/dist-compat-sweep.md). **The `--run-tests`
      axis is the sharper frontier**: running each loading dist's own suite with raku as the baseline.
      The next step is a fresh full-corpus sample (`scripts/dist-compat-sweep.py --run-tests`). Per bug: minimal repro → general fix → `t/`
      pin → PR. **Standing rule when reading a sweep**: verify any non-`missing_dep` bucket against
      `raku -I lib` before treating it as a mutsu bug — most turn out not to be.
- [ ] Other open module-compat findings are individual `todo:ticket` / `todo:deep` issues.

---

## 2. Substrate — GC, NaN-boxing and JIT have landed

The cycle collector (ADR-0003), NaN-boxing and the Cranelift JIT (ADR-0004) are done and default
on; biased refcounting (layer 3c) is frozen until a measured trigger. Do **not** restart a "GC
campaign". What is left:

- [ ] Profile-driven GC follow-ups (clone-traffic pruning, layer-3a hardening H1–H5):
      `docs/gc-post-3a-roadmap.md`. Optimization, not correctness.

---

## 3. roast — a regression guard

1440 of the 1455 roast files are whitelisted, `integration/` included, and every remaining file is
*non-goal*, *no oracle* or *unpassable*
([docs/roast-compatibility.md](docs/roast-compatibility.md#non-whitelisted-roast-files-and-why)).
Roast is not a work source: keep the whitelist green, and pick up a remaining file only when other
work happens to unblock it.

---

## 4. perf — mostly polish, with measured losses

**Do not pick up a perf item just because the profile shows a hot symbol** — first confirm a goal
item needs it. Levers, targets and the measurement protocol:
[ADR-0006](docs/adr/0006-baseline-interpreter-optimizations.md),
[docs/perf-callpath-scouting.md](docs/perf-callpath-scouting.md); canonical numbers come from the
bench CI, never a local run.

This section used to open "mutsu beats raku on the whole roast whitelist and on every benchmark".
[ADR-0099](docs/adr/0099-regex-engine-performance-strategy.md) §2 retired that claim: it held on
one-shot wall clock, which charges rakudo its warm-up, and does not hold on steady-state or marginal
cost. Compare warm before asserting a win.

- [ ] **`JSON::Fast` at rakudo parity (it is `mzef`'s perceived speed)** — tracked by
      [#8673](https://github.com/tokuhirom/mutsu/issues/8673) (`from-json`) and
      [#14774](https://github.com/tokuhirom/mutsu/issues/14774) (`to-json`). The typed resolved IR
      of [ADR-0110](docs/adr/0110-typed-resolved-ir-for-statically-typed-routines.md) landed but
      missed its gate ([#8895](https://github.com/tokuhirom/mutsu/issues/8895)); read its findings
      before choosing the next lever. Worked solo.
- [ ] **The interpreter function-call path in hot loops** (the JIT bails at the call boundary) —
      [#7573](https://github.com/tokuhirom/mutsu/issues/7573); **read that ticket's re-diagnosis
      first**. It is the standing CI cost of running Raku's own `Test` as Raku code. Two levers
      remain, per the ticket: collapse the double `multi` resolution per call, and reduce the flat
      interpretation cost of a module body (allocation traffic and env lookups).
- Regex/grammar performance: read [ADR-0099](docs/adr/0099-regex-engine-performance-strategy.md)
  before touching it. Its Stage 0 defects and Stage 1 prefilter are done, and ADR-0135's compiled
  backtracking program replaced the tree walk; new findings are individual `todo:perf` issues.
- [ ] Opcode leftovers: [docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6.
- [ ] Biased reference counting (ADR-0001 layer 3c) — frozen; start only on a measured trigger and an
      updated ADR.

---

## 5. Concurrency and structural refactoring

Do not re-plan these — they are done: the
[shared worker pool](docs/adr/0020-shared-worker-pool.md) (ADR-0020), the whole-`locals`
clone/restore in `BlockScope` ([ADR-0018](docs/adr/0018-slot-addressed-lexical-capture-and-env-sync.md);
`docs/archive/lexical-scope-slot-campaign.md` is a historical record now), and `.^methods`/`.can` deriving
from the real dispatch table (ADR-0019 F1/F2).

- [ ] **Non-blocking `await` — ADR-0020's rejected alternative (b), kept as a standing axis.**
      mutsu's `await` is a blocking condvar wait (`SharedPromise::wait`,
      `src/value/value_async.rs`), as are `Lock`/`Semaphore` critical sections, so a blocked frame
      costs an OS thread and nested `await` of depth N materializes ~N of them.
      [ADR-0020](docs/adr/0020-shared-worker-pool.md) §2 chose the elastic pool that makes this
      correct rather than deadlocking; rakudo instead parks the frame on a continuation
      (`$*AWAITER`) and returns the worker to a *capped* pool. Doing the same here means turning
      every blocking point (`await`, channel receive, lock, sleep) into a suspension point and
      teaching the VM to unwind and restore native Rust stack frames — VM-scale, hence still
      deferred. [ADR-15967](docs/adr/15967-start-blocks-submission-order.md) §4.2 (Accepted
      2026-11-09) records the measured trigger (one OS thread per pending `await`: 303 vs rakudo's 4
      at nested depth 300) and the chosen mechanism when the axis opens (stackful tasks pinned to
      their worker, not heap frames); starting it still takes its own ADR, after ADR-15967 §4.1's
      shared-world / per-worker execution context. **No roast pressure remains** (all 99 S17 files are whitelisted); the motive is
      thread consumption under heavy concurrency, so start this only on a measured trigger and an
      updated ADR. **The [#8380](https://github.com/tokuhirom/mutsu/issues/8380) `Test::Scheduler`
      deadlock is NOT that trigger**: [ADR-0105](docs/adr/0105-promise-resolution-dispatches-through-the-promise-scheduler.md)
      (Accepted 2026-09-16, implemented 2026-09-26) measured that Rakudo's continuation is not what
      orders it, and fixed it with scheduler-routed promise resolution, a woken awaiter borrowing
      the resuming worker's slot, and no task overtaking its running submitter — leaving this axis
      untouched.
- [ ] **Improve error-message quality and bring edge-case panics to zero** — driven by roast
      pass/fail: `integration/error-reporting.t` and `weird-errors.t` for quality, and the
      deep-recursion `fatal runtime error: stack overflow` process abort for crashes. The panic
      surface itself is now ratcheted (`make check-panic-surface`,
      `scripts/check-panic-surface.py`), so it can only shrink from here — this item is the
      actual shrinking work.
- Individual concurrency bugs are individual `todo:ticket` / `todo:deep` issues.

---

## 6. QA & finalization — the compatibility gap roast no longer sees

Roast is a regression guard (§3), so the defects that remain are by definition the ones it does not exercise.
The backbone is **differential testing against the reference `raku`**: any program where mutsu and
raku disagree is a candidate defect, found objectively rather than guessed.

**Labor split (load-bearing).** Discovery, minimal-repro reduction, and triage are wide, mechanical
and parallelizable — farm them out. Interpreter **fixes stay under tighter control**: a breadth-first
agent is exactly what adds the slow-path fallbacks and test-specific hacks this repo forbids. The
deliverable of a discovery campaign is a **ranked backlog of minimal repros grouped by root cause**,
not a pile of speculative fixes.

**Align the language version.** Local raku is 6.d-default; docs may use 6.e. Prefer the stronger
signal "mutsu differs from raku **and** from the documented expectation" over a raw raku diff.

- [ ] **Doc-example differential sweep** — harness and triage rules:
      [docs/qa-doc-diff-harness.md](docs/qa-doc-diff-harness.md); backlog:
      [docs/doc-diff-backlog.md](docs/doc-diff-backlog.md). Start a resumed campaign with a fresh
      full-corpus sweep on current `main`; never trust an older survey after fixes merged.
- [ ] **Per-type method-coverage matrix** — harness landed (`scripts/method-coverage.raku`); run the
      full-corpus triage and fold the per-type hole list into the backlog.
- [ ] **Panic-zero sweep** — mutsu must never Rust-panic or process-abort on any input. Extend with
      parser fuzzing driven through the same harness with a "did it panic?" oracle. The panic-family
      surface (`unwrap`/`expect`/`panic!`/`unreachable!`/`todo!`/`unimplemented!` in `src/`, test
      scaffolding excluded) is ratcheted by `make check-panic-surface`
      ([#8186](https://github.com/tokuhirom/mutsu/issues/8186)); this item is driving it toward
      zero.
- [ ] **Error / exception parity** — differential-test that mutsu throws the right `X::` type with a
      matching message and payload, not merely that it fails. Corpus: `Type/X*.rakudoc`.

---

## Metrics

| Metric | Current | Target |
|---|---|---|
| Bundled libraries | **40 vendored**, upstream suites gated at release | 10+ bundled, all documented |
| mzef | install / fetch / resolution / test phase all work E2E | Full pipeline on the real fez index |
| Binary distribution | 4 release targets + GHCR image + mise ✅ | Achieved |
| roast whitelist | **1440 / 1455** | Achieved; every remaining file is non-goal, no oracle or unpassable |
| GC / JIT | **default on** ✅ | Achieved |
| RakuAST frontend round trip (§0) | tracked in [#7564](https://github.com/tokuhirom/mutsu/issues/7564) | parity with the ordinary frontend, then default flip |
| Built-in method rows (§0) | tracked in ADR-11276 §9 | every family migrated, then the cascades deleted |
| Slow-path fallbacks (§0) | `make check-slow-path`, `scripts/slow-path-survey.sh` ([#11510](https://github.com/tokuhirom/mutsu/issues/11510)) | marker and call-site counts at 0, survey shape count 0, then `run_instance_method` as a fallback deleted |
| Startup vs raku | **0.04×** | maintain |
| fib / method-call / bench-class vs raku | all under target (bench CI) | maintain |
