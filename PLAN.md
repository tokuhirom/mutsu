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
> | roast failure analysis | [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) |
> | performance numbers | the bench CI (`bench-data` branch), [PERFORMANCE.md](PERFORMANCE.md) |
>
> Do **not** append progress notes here. A GitHub issue or a new file under `news/` conflicts with
> nothing on merge; an append to this file conflicts with every other in-flight PR.

## Goal — a batteries-included Raku implementation

Build a **Raku interpreter where installing mutsu alone gives you a well-documented standard bundled
library, so you can write practical code immediately** — the Raku version of the positioning bun took
for JavaScript. The official Rakudo ecosystem has no batteries-included distribution, and that gap is
mutsu's unique position. Four components:

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
- **Bundling policy is [BATTERIES.md](BATTERIES.md)**: adopt the upstream module verbatim and grow
  mutsu until it runs. Providing a module "natively" is banned going forward, and a performance
  measurement does not buy an exemption — speed justifies a transparent optimization, never a
  substitution under the real module's name. The policy is not yet an ADR:
  [#8184](https://github.com/tokuhirom/mutsu/issues/8184).

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
work; see the CLAUDE.md "mzef package manager and distribution" section. The **REPL** (`--repl`,
`src/repl.rs`, plus the in-browser one at `site/repl.html`) and the **public WASM playground**
(`site/playground.html`, deployed to GitHub Pages by `.github/workflows/pages.yml`) both ship.

- [ ] **Language server** — designed in [docs/adr/0065-language-server-targets-ai-agents.md](docs/adr/0065-language-server-targets-ai-agents.md)
      (an AI agent is the primary consumer, so only the methods an agent consumes are implemented,
      and "does mutsu support this?" is a first-class diagnostic). **S0 (viability gate), S1 (server
      skeleton), S2's routine half, S3 (error recovery), S4 (symbols/definition) and S5a (hover) are done** — `crates/mutsu-lsp/`,
      `src/analysis/`, [docs/language-server.md](docs/language-server.md). Next is **S5b:
      `references`** — the one remaining method that genuinely needs per-occurrence positions,
      since a line may hold several and text scanning cannot rank them soundly. **Start from
      [#7547](https://github.com/tokuhirom/mutsu/issues/7547),
      not from D6**: reconnaissance during S5a found the parser already knows every byte offset,
      so a thread-local occurrence table behind the analysis flag is likely cheaper than spans on
      AST variants and touches neither `Expr`'s size nor the bincode cache. Its blocker is parser
      **backtracking** (phantom references from failed alternatives), so **the first step is a
      measurement of the phantom rate over `modules/`/`vendor/`/`t/`** — that number decides the
      design, and D6/S5b get amended from it. **S2's method half is deferred with a real design question**: `$x.foo` needs the
      receiver type, which the AST does not carry, and the existing `(owner, name)` catalog is
      conservative in the false-positive direction — see the ADR's S2 findings.
- [ ] Debugger.
- [ ] Native binary output.

### B4. Module-compatibility frontier (the base of batteries)

- [ ] **★Ecosystem parity: per-distribution work** — the KPI itself is **built and running**; what
      remains is turning red records green, one distribution (or one root cause) at a time. The
      corpus is measured nightly at 03:20 JST and published: **41.1%** dist parity, 53.4% file,
      63.1% assertion, with per-distribution records in `ecosystem/` and the chart in
      `ecosystem/history.svg`. Pick work one of two ways — by **root cause** from the clustered
      issues (`scripts/ecosystem-tickets.py`, see docs/ecosystem-parity.md §9; the ten largest
      clusters are filed as `todo:*` issues), or by **distribution** with
      [`ecosystem-dist-fix`](.agents/skills/ecosystem-dist-fix/SKILL.md) (named) /
      [`ecosystem-dist-roulette`](.agents/skills/ecosystem-dist-roulette/SKILL.md) (a uniform random
      draw, locked on [#7884](https://github.com/tokuhirom/mutsu/issues/7884) so parallel agents do
      not collide). Prefer a root cause when one covers several distributions; a uniform draw is what
      keeps the published figure honest.
      Method: [docs/ecosystem-parity.md](docs/ecosystem-parity.md);
      decisions: [ADR-0085](docs/adr/0085-ecosystem-testsuite-parity-measurement.md). The campaign
      that built all of this was [#7785](https://github.com/tokuhirom/mutsu/issues/7785), closed
      2026-09-12. The sweep is a measurement, not a gate, so it does **not** close B1's
      "working-module regression CI" — that stays a separate item.
- [ ] **★Real-dist compatibility sweep** — run real fez dists under mutsu and fix the general bugs
      they surface. Ledger: [docs/dist-compat-sweep.md](docs/dist-compat-sweep.md). **The `--run-tests`
      axis is the sharper frontier**: running each loading dist's own suite with raku as the baseline.
      The previous batch (`scripts/dist-compat-sweep.py --run-tests`, n=60) is now fully triaged
      (`news/2026-08/dist-test-suite-failures-batch-triaged.md`) — the next step is a fresh
      full-corpus sample, not a specific open ticket. Per bug: minimal repro → general fix → `t/`
      pin → PR. **Standing rule when reading a sweep**: verify any non-`missing_dep` bucket against
      `raku -I lib` before treating it as a mutsu bug — most turn out not to be.
- [ ] **`nqp::` ops are added on demand, never as a porting campaign** (measured 2026-07-26 —
      `news/2026-07/nqp-op-layer-measured-and-rejected.md`). The durable finding is that per dist the
      op set is a **threshold function**, so a large module is not reached by adding ops one at a
      time; the reverse-dependency weight is also dominated by modules mutsu already bundles. What
      that measurement does *not* say is "mutsu has no `nqp::` ops" — it has **111**
      (`src/runtime/nqp_ops*.rs`, `src/vm/vm_call_nqp.rs`), each added because a real dist needed it,
      starting with `nqp::sha1` for zef. Do not cite the 2026-07 record as a blanket ban.
- [ ] **NativeCall**: measured non-vendorable, stays a justified rung-3 provider
      ([#7560](https://github.com/tokuhirom/mutsu/issues/7560)) — the blockers are structural
      (`use QAST:from<NQP>`, MoarVM dispatch programs), so they do not move as the op set grows;
      native-backed `array[T]` / reference-element `CArray` are ADR-0015 P3b (done) / P3c (optional,
      pick up only when a real consumer needs it).
- [ ] Other open module-compat findings are individual `todo:ticket` / `todo:deep` issues.

---

## 2. Substrate — GC, NaN-boxing, JIT: landed; soundness tail remains

| layer | status |
|---|---|
| 3a — cycle collector on the container-kind `Gc<T>` variants | ✅ default on (ADR-0003) |
| 3b — NaN-boxing (`Value` 48→8B) | ✅ done |
| 4 — JIT (Cranelift) | ✅ default on (ADR-0004 closed) |
| 3c — biased refcount | 🧊 frozen; measured-trigger only |

Do **not** restart a "GC campaign". ADR-0013's Miri gate is closed (required CI job, landed) and the
OTF compilation-gate leftovers ticket is retired — both were stale entries here. What is left:

- [ ] Profile-driven GC follow-ups (clone-traffic pruning, layer-3a hardening H1–H5):
      `docs/gc-post-3a-roadmap.md`. Optimization, not correctness.

---

## 3. roast — at its ceiling; no cluster left to attack

The whitelist stands at **1437 / 1465**. `integration/` — the real-Raku-program files closest to the
project goal — is **fully whitelisted**. Per
[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md), nearly every remaining file is *non-goal* (rakudo
itself fails), *no oracle* (local raku SORRYs), or *awaiting infrastructure* (6.e generics).

**Implication for planning: roast is no longer the productive axis.** Prefer §1, §4, §5 or §6; pick
up a roast file only when another change happens to unblock it. (The former "remaining
language-feature gaps" ticket here — multi-line feeds, typed exceptions, `exits-ok`, `:D`/`:U`
DefiniteHow — is fully closed; no open roast-adjacent language-feature backlog remains right now.)

---

## 4. perf — de-prioritized polish

mutsu beats raku on the whole roast whitelist and on every benchmark, so **do not pick up a perf item
just because the profile shows a hot symbol** — first confirm a goal item needs it. Levers, targets
and the measurement protocol: [ADR-0006](docs/adr/0006-baseline-interpreter-optimizations.md),
[docs/perf-callpath-scouting.md](docs/perf-callpath-scouting.md); canonical numbers come from the
bench CI, never a local run.

- [ ] **The one axis where mutsu is genuinely slower than raku** — the interpreter function-call path
      in hot loops (the JIT bails at the call boundary):
      [#7573](https://github.com/tokuhirom/mutsu/issues/7573). **Read that ticket's re-diagnosis
      first**; everything this file used to say about the blocker is closed. The `&`-sigil signature
      gate, the `nqp::` by-name resolve and the 83% `interpreter_fallbacks` figure are all fixed, and
      the per-assertion cost is down an order of magnitude. Its consumer changed too: the vendored
      `Test` flip it was meant to unblock **landed 2026-09-10**, so this is no longer a rung-3
      retirement blocker but the standing CI cost of running Raku's own `Test` as Raku code
      (`t/` 2.0x, `make roast` 1.24x) — paid on every run from now on. Two concrete levers remain,
      per the ticket: collapse the double `multi` resolution per call, and reduce the flat
      interpretation cost of a module body (allocation traffic and env lookups), which is what the
      remaining ~16x against raku on an assertion actually is.
- [ ] Grammar/regex per-subrule ceremony (~25× vs raku per matched character; the exponential and
      accumulated-state halves are fixed):
      [ADR-0007](docs/adr/0007-grammar-parse-trail-matcher.md) §Implementation outcome. **This one
      has a goal-item consumer**, so it is not polish: it is the measured reason the JSON
      `to-json`/`from-json` fast path still shadows the vendored module by name
      ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)) — the real grammar decodes 200
      META-shaped documents in ~600s against 0.49s native, on a path zef walks for every metadata
      read.
- [ ] Opcode leftovers: [docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6.
- [ ] Biased reference counting (ADR-0001 layer 3c) — frozen; start only on a measured trigger and an
      updated ADR.

---

## 5. Concurrency and structural refactoring

Do not re-plan these — they are done: the
[shared worker pool](docs/adr/0020-shared-worker-pool.md) (ADR-0020), the whole-`locals`
clone/restore in `BlockScope` ([ADR-0018](docs/adr/0018-slot-addressed-lexical-capture-and-env-sync.md);
`docs/lexical-scope-slot-campaign.md` is a historical record now), and `.^methods`/`.can` deriving
from the real dispatch table (ADR-0019 F1/F2). The pool's one open follow-up: it recovered only ~10%
of per-`start` cost, so whitelisting Digest's `t/ripemd.t` still needs per-call-site compile-cache
levers — [#7571](https://github.com/tokuhirom/mutsu/issues/7571), actively worked.

- [ ] **Non-blocking `await` — ADR-0020's rejected alternative (b), kept as a standing axis.**
      mutsu's `await` is a blocking condvar wait (`SharedPromise::wait`,
      `src/value/value_async.rs`), as are `Lock`/`Semaphore` critical sections, so a blocked frame
      costs an OS thread and nested `await` of depth N materializes ~N of them.
      [ADR-0020](docs/adr/0020-shared-worker-pool.md) §2 chose the elastic pool that makes this
      correct rather than deadlocking; rakudo instead parks the frame on a continuation
      (`$*AWAITER`) and returns the worker to a *capped* pool. Doing the same here means turning
      every blocking point (`await`, channel receive, lock, sleep) into a suspension point and
      teaching the VM to unwind and restore native Rust stack frames — VM-scale, hence still
      deferred. **No roast pressure remains** (all 99 S17 files are whitelisted); the motive is
      thread consumption under heavy concurrency, so start this only on a measured trigger and an
      updated ADR.
- [ ] Propagate Supply detached-worker panics to QUIT (currently swallowed) — [#8185](https://github.com/tokuhirom/mutsu/issues/8185).
- [ ] Split out the roast fudge logic. File size (376 over 500 lines, 138 over 1000, still growing —
      `ANALYSIS.md` §6) is **not** a standalone campaign: split when a campaign opens the file and the
      ownership boundary is visible.
- [ ] **Improve error-message quality and bring edge-case panics to zero** — driven by roast
      pass/fail: `integration/error-reporting.t` and `weird-errors.t` for quality, and the
      deep-recursion `fatal runtime error: stack overflow` process abort for crashes. The panic
      surface itself is now ratcheted (`make check-panic-surface`,
      `scripts/check-panic-surface.py`), so it can only shrink from here — this item is the
      actual shrinking work.
- Individual concurrency bugs are individual `todo:ticket` / `todo:deep` issues.

---

## 6. QA & finalization — the compatibility gap roast no longer sees

roast is mined out (§3), so the defects that remain are by definition the ones it does not exercise.
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
      scaffolding excluded) is now ratcheted at 1,906 sites by `make check-panic-surface`
      ([#8186](https://github.com/tokuhirom/mutsu/issues/8186)) — it can only go down from here, so
      this item is the remaining work of actually driving it toward zero.
- [ ] **Error / exception parity** — differential-test that mutsu throws the right `X::` type with a
      matching message and payload, not merely that it fails. Corpus: `Type/X*.rakudoc`.

---

## Metrics

| Metric | Current | Target |
|---|---|---|
| Bundled libraries | **40 vendored**, upstream suites gated at release | 10+ bundled, all documented |
| mzef | install / fetch / resolution / test phase all work E2E | Full pipeline on the real fez index |
| Binary distribution | 4 release targets + GHCR image + mise ✅ | Achieved |
| roast whitelist | **1437 / 1465** | Achieved; remainder is mostly non-goal |
| GC / JIT | **default on** ✅ | Achieved |
| Startup vs raku | **0.04×** | maintain |
| fib / method-call / bench-class vs raku | all under target (bench CI) | maintain |
