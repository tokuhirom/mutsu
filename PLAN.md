# PLAN.md — mutsu implementation plan

> **This file is an outline of unfinished work — nothing more.** Each item says *what* is left and
> *where the detail lives*; it does not carry investigation notes, measurements, or progress logs.
>
> | kind of information | where it lives |
> |---|---|
> | completed work | [news/](news/) — one file per accomplishment |
> | open findings, small | [todo/tickets/](todo/tickets/) — one file per finding |
> | open findings, deep | [todo/deep/](todo/deep/) — needs design or an ADR |
> | architectural decisions | [docs/adr/](docs/adr/) |
> | roast failure analysis | [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) |
> | performance numbers | the bench CI (`bench-data` branch), [PERFORMANCE.md](PERFORMANCE.md) |
>
> Do **not** append progress notes here. A new file under `todo/` or `news/` conflicts with nothing
> on merge; an append to this file conflicts with every other in-flight PR.

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
  mutsu until it runs. Providing a module "natively" is banned going forward.

---

## 1. 🔋 Batteries — bundled libraries and distribution (main effort)

22 libraries are vendored under `modules/` and resolved with zero configuration; the release-time
gate runs their upstream suites against the shipped copies
([docs/batteries/testsuite-gate.md](docs/batteries/testsuite-gate.md)) and is **all-green, so a drop
below the whitelisted baseline is a regression to fix, not a baseline to accept**. Adding a battery
re-opens work: whitelist what passes with `scripts/battery-testsuite.sh --update`, then close the
gaps with general interpreter fixes.

### B1. Bundle set and documentation

- [ ] **Finalize the bundle list.** Selection criterion: "a web blog can be written with the bundle
      alone". Method and per-battery selection records:
      [docs/batteries/](docs/batteries/), [BATTERIES.md](BATTERIES.md).
- [ ] **Web-framework slot: make Cro run** — the campaign that fills the last hole in that criterion.
      Target, rationale, and gate order: [docs/batteries/web-framework.md](docs/batteries/web-framework.md).
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

- [ ] Debugger.
- [ ] Native binary output.

### B4. Module-compatibility frontier (the base of batteries)

- [ ] **★Real-dist compatibility sweep** — run real fez dists under mutsu and fix the general bugs
      they surface. Ledger: [docs/dist-compat-sweep.md](docs/dist-compat-sweep.md). **The `--run-tests`
      axis is the sharper frontier**: running each loading dist's own suite with raku as the baseline.
      Open batch: [todo/tickets/dist-test-suite-failures-batch.md](todo/tickets/dist-test-suite-failures-batch.md).
      Per bug: minimal repro → general fix → `t/` pin → PR.
      **Standing rule when reading a sweep**: verify any non-`missing_dep` bucket against
      `raku -I lib` before treating it as a mutsu bug — most turn out not to be.
- [ ] **Do NOT build an `nqp::` op layer** (measured 2026-07-26 — `news/2026-07/nqp-op-layer-measured-and-rejected.md`).
      The reverse-dependency weight is dominated by modules mutsu already bundles, and per dist the
      op set is a threshold function. Implement an individual op when a real dist needs it (as
      `nqp::sha1` was for zef).
- [ ] **NativeCall surface gaps** — inventory in
      [todo/tickets/nativecall-surface-gaps.md](todo/tickets/nativecall-surface-gaps.md); native-backed
      `array[T]` / reference-element `CArray` are ADR-0015 P3b/P3c.
- [ ] Other open module-compat findings are individual files under
      [todo/tickets/](todo/tickets/) and [todo/deep/](todo/deep/).

---

## 2. Substrate — GC, NaN-boxing, JIT: landed; soundness tail remains

| layer | status |
|---|---|
| 3a — cycle collector on `Arc`, type-filtered | ✅ default on (ADR-0003) |
| 3b — NaN-boxing (`Value` 48→8B) | ✅ done |
| 4 — JIT (Cranelift) | ✅ default on (ADR-0004 closed) |
| 3c — biased refcount | 🧊 frozen; measured-trigger only |

Do **not** restart a "GC campaign". What is left:

- [ ] **Close ADR-0013 — the Miri gate**:
      [todo/tickets/miri-gate-for-adr-0013.md](todo/tickets/miri-gate-for-adr-0013.md).
- [ ] Profile-driven GC follow-ups (clone-traffic pruning, layer-3a hardening H1–H5):
      `docs/gc-post-3a-roadmap.md`. Optimization, not correctness.
- [ ] OTF compilation-gate leftovers:
      [todo/tickets/otf-compilation-gate-leftovers.md](todo/tickets/otf-compilation-gate-leftovers.md).

---

## 3. roast — at its ceiling; no cluster left to attack

The whitelist stands at **1435 / 1464**. `integration/` — the real-Raku-program files closest to the
project goal — is **fully whitelisted**. Per
[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md), nearly every remaining file is *non-goal* (rakudo
itself fails), *no oracle* (local raku SORRYs), or *awaiting infrastructure* (6.e generics).

**Implication for planning: roast is no longer the productive axis.** Prefer §1, §4, §5 or §6; pick
up a roast file only when another change happens to unblock it.

- [ ] Language-feature gaps that no roast file whitelists (multi-line feeds, the remaining typed
      exceptions, `exits-ok`, `:D`/`:U` DefiniteHow):
      [todo/tickets/remaining-language-feature-gaps.md](todo/tickets/remaining-language-feature-gaps.md).

---

## 4. perf — de-prioritized polish

mutsu beats raku on the whole roast whitelist and on every benchmark, so **do not pick up a perf item
just because the profile shows a hot symbol** — first confirm a goal item needs it. Levers, targets
and the measurement protocol: [ADR-0006](docs/adr/0006-baseline-interpreter-optimizations.md),
[docs/perf-callpath-scouting.md](docs/perf-callpath-scouting.md); canonical numbers come from the
bench CI, never a local run.

- [ ] **The one axis where mutsu is genuinely slower than raku** — the interpreter function-call path
      in hot loops (the JIT bails at the call boundary):
      [todo/deep/interpreter-call-path-in-hot-loops.md](todo/deep/interpreter-call-path-in-hot-loops.md).
- [ ] **The `needs_env_sync` blanket** — the last structural piece of dual-store decoupling, a fused
      campaign: [todo/deep/needs-env-sync-blanket-removal.md](todo/deep/needs-env-sync-blanket-removal.md).
- [ ] Grammar/regex per-subrule ceremony (~25× vs raku per matched character; the exponential and
      accumulated-state halves are fixed):
      [ADR-0007](docs/adr/0007-grammar-parse-trail-matcher.md) §Implementation outcome.
- [ ] Opcode leftovers: [docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6.
- [ ] Biased reference counting (ADR-0001 layer 3c) — frozen; start only on a measured trigger and an
      updated ADR.

---

## 5. Concurrency and structural refactoring

- [ ] **Write a Proposed ADR for a shared worker pool** — mutsu spawns a thread per task at all 19
      `spawn_user_thread` sites; the design fork is what `await` does to a pooled worker:
      [todo/deep/shared-worker-pool-adr.md](todo/deep/shared-worker-pool-adr.md).
- [ ] **Remove the full locals clone/restore in `BlockScope`** — the final move of the lexical-scope
      slot campaign and the perf core: [docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md).
      A load-bearing refactor entangled with `$OUTER::`, GC roots, and env resync; suited to a
      dedicated session.
- [ ] Semaphore / non-blocking await / lock contention (S17; hard; separate axis).
- [ ] Propagate Supply detached-worker panics to QUIT (currently swallowed).
- [ ] Derive `.^methods` / `.can` from the real dispatch table; split out the roast fudge logic; split
      files over 500 lines.
- [ ] **Improve error-message quality and bring edge-case panics to zero** — driven by roast
      pass/fail: `integration/error-reporting.t` and `weird-errors.t` for quality, and the
      deep-recursion `fatal runtime error: stack overflow` process abort for crashes.
- Individual concurrency bugs are files under [todo/tickets/](todo/tickets/) and
  [todo/deep/](todo/deep/).

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
      parser fuzzing driven through the same harness with a "did it panic?" oracle.
- [ ] **Error / exception parity** — differential-test that mutsu throws the right `X::` type with a
      matching message and payload, not merely that it fails. Corpus: `Type/X*.rakudoc`.

---

## Metrics

| Metric | Current | Target |
|---|---|---|
| Bundled libraries | **22 vendored**, upstream suites gated at release | 10+ bundled, all documented |
| mzef | install / fetch / resolution / test phase all work E2E | Full pipeline on the real fez index |
| Binary distribution | 4 release targets + GHCR image + mise ✅ | Achieved |
| roast whitelist | **1435 / 1464** | Achieved; remainder is mostly non-goal |
| GC / JIT | **default on** ✅ | Achieved |
| Startup vs raku | **0.04×** | maintain |
| fib / method-call / bench-class vs raku | all under target (bench CI) | maintain |
