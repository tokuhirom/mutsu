# PLAN.md — mutsu implementation plan

> This file lists **only unfinished work**. Completed work moves to [news/](news/).
> See [news/](news/) for past logs, [PERFORMANCE.md](PERFORMANCE.md) for performance details,
> and [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) for roast failure analysis.
>
> **Last updated 2026-07-14**:
> §4 was rewritten based on full measurement. The items listed in the old §4 (negation meta / hyper assignment /
> `augment class` / `A::B.new` / file test / PRE·POST / signature type checks / lazy-seq ④) are
> **all already implemented**; instead, it turned out that **the 41 `integration/` files (real-program
> compatibility, all perfect-score under raku) had been missing from the BLOCKERS table**.
> §5 reflects the work consumed by #4492–#4495.
> The canonical record of completed work is [news/2026-07.md](news/2026-07.md). The redefinition of the goal
> ("**a batteries-included Raku implementation**") and the full restructuring happened on 2026-07-05
> (the old completion notes are in the [news/2026-06.md](news/2026-06.md) archive section and news/2026-07.md).

## Goal — a batteries-included Raku implementation

Build a **Raku language interpreter where installing mutsu alone gives you a well-documented
standard bundled library, so you can write practical code immediately**.

The Raku version of the positioning bun took for JavaScript (runtime + package manager + standard
tooling in a single fast binary). The official Rakudo ecosystem has no batteries-included
distribution, and that gap is mutsu's unique position. Four components:

1. **Fast-startup compatible interpreter** — startup 0.04x vs raku; roast whitelist 1384. With CLI
   tools and script execution as the main battleground, maintain and expand raku compatibility. → §3 / §4
2. **Standard bundled libraries (batteries)** — bundle JSON / HTTP / templates / DB / file utilities
   etc., so they work with a plain `use` right after install. **Every library gets documentation**. → §1
3. **Bundled package manager `mzef`** (vendoring the real Zef) — anything not covered by the bundle
   can be fetched from the fez ecosystem. → §1 B2
4. **Substrate quality** — GC (table stakes; **done, default on**), performance, error messages. → §2 / §5

## How to read this document

- **§1 Batteries** is the main effort, directly tied to the goal: bundle selection, vendoring,
  documentation, mzef, distribution.
- **§2 Phase B** is fully complete: layer 3a (GC), layer 3b (NaN-boxing), and layer 4 JIT
  (J1–J5 + all J4d slices, **default on**, ADR-0004 closed 2026-07-15).
- **§3 substrate / §4 roast / §5 perf / §6 concurrency & structure** are the remaining substrate-quality items.
- **The roast frontier in §4 is `integration/` (real-program compatibility; all 41 files perfect-score under raku)**
  — established by the 2026-07-14 full measurement. For individual files and root-cause clusters see
  [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md).

### Phase structure (ADR-0001)

Beyond catching up with raku on performance and compatibility, **GC and JIT are the next big jumps**.
An interpreter without GC is considered "defective" and nobody will use it — GC is table stakes.
The order and approach are decided in
[docs/adr/0001-gc-strategy-and-phasing.md](docs/adr/0001-gc-strategy-and-phasing.md).
**On 2026-07-03, Phase A completion (roast goal met) was confirmed and the GC start was decided**
([ADR-0002](docs/adr/0002-phase-a-gate-reassessment.md)). Key points:

| Phase | Content | Where in this document |
|---|---|---|
| **A. Catch up** | Match raku on compatibility + speed (**done — ADR-0002**) | Only leftovers in §3 / §4 / §5 |
| **B. Value-representation rework + GC** | Layer 3a (Track B + cycle collector, integrated) and layer 3b NaN-boxing **both done** (2026-07-12 #4469; `Value` 48→8B) | §2 |
| **C. JIT** | Unique advantage (**done** — J1–J5 + J4d, default on, ADR-0004 closed 2026-07-15) | §5 Lever 4 |

- **GC comes before JIT** (the JIT is built on top of a GC-ready foundation).
- **Approach = cycle collector on Arc (non-moving + refcount; level 1 adopted)**. Scalar variants are
  excluded from GC by a type filter — numeric/string hot paths pay zero cost. Performance comes from
  the JIT, not from GC.
- **Track B is fused with GC (layer 3a). Do NOT start it standalone.** NaN-boxing is groundwork for
  the JIT (layer 3b); biased refcounting is layer 3c.
- Batteries (§1) can proceed in parallel with GC (compatibility, module, and distribution work does
  not depend on the Value representation).

### 🚫 Standing rule: keep "1 operation = 1 implementation" (user policy 2026-06-07)

The execution engine is unified into a single `Interpreter` struct (= the bytecode VM).
Do **not** implement the same Raku operation in multiple places:

1. Write new implementations and fixes **exactly once**, in the VM/native layer (`src/vm/` plus pure
   native `src/builtins/`).
2. When another call path (EVAL / embedded `{}` blocks in regexes, etc.) needs the same processing,
   **delegate** to the single native implementation.
3. When you find a duplicate, make the native implementation canonical and delete the duplicated copy.

---

## 1. 🔋 Batteries — standard bundled libraries and distribution (goal-critical, main effort)

Modules with a proven working record (details in [news/2026-06.md](news/2026-06.md)): JSON native
(`to-json`/`from-json` #3402) / Template::Mustache / File::Temp / File::Directory::Tree /
HTTP::Parser / MIME::Base64 / HTTP::Server::Tiny (end-to-end HTTP serving) / Tubu (homegrown
synchronous web framework, `t/lib`) / DBDishLite (homegrown SQLite layer, `t/lib`) / NativeCall MVP
(real SQLite CRUD round-trip) / zef CLI.

Right now these merely "work" — the three pillars of **bundling, documentation, and continuity
guarantees** are all missing. To call ourselves batteries-included we need all three — that is this
section.

### B1. Finalize the bundle set, vendoring, documentation

- [ ] **Finalize the bundle list**. First candidates (based on working record):
      JSON (native built-in) / Template::Mustache / File::Temp / File::Directory::Tree / HTTP::Parser /
      MIME::Base64 / HTTP::Server::Tiny / Tubu (sync WAF) / DBDishLite (SQLite) / NativeCall.
      Use "a web blog can be written with the bundle alone" as the selection criterion (the HTTP
      client gap needs investigation).
- [ ] **Vendoring mechanism**: vendor the bundled modules into the source tree (e.g. `modules/`) so
      an installed mutsu resolves them with no extra configuration (make `MUTSULIB` have a built-in
      default, or register a standard lib path in `Interpreter::new()` — same pattern as
      `add_default_site_repo()`). Also verify consistency with the precomp cache.
- [ ] **Promote the `t/lib` homegrown libraries**: move Tubu / DBDishLite out of the test-helper
      location into proper bundled modules; clean up naming, API, and standalone tests.
- [ ] **Documentation**: a usage document per bundled library (overview; the fact that no install is
      needed; API reference; example code). Location: `docs/batteries/` (or starting from a top-level
      `BATTERIES.md`). "Well-documented" is an explicit goal requirement, so documentation is
      mandatory when adding a module.
- [ ] **Working-module regression CI** (non-blocking for PRs; detects on main push; user policy
      2026-06-28): continuously detect whether modules that once "worked" keep working. A harness
      that `use`s each known module plus a smoke test (load, representative method, `zef --help`
      output match). External dists are either `zef fetch`ed in CI or vendored. Failures are
      report-only (a red run does not stop main). Once the bundle set is fixed, its smoke tests
      become the battery quality gate as-is.

### B2. mzef — an `mzef` package manager bundling the real Zef (north-star; user policy 2026-06-28)

Vision: **installing mutsu gives you the `mzef` command**. The implementation does **not reimplement
Zef — it uses Zef itself** (upstream). Since zef is a huge real Raku program, it is also the
**strongest compatibility north star** (many general bug fixes originating from zef have already
landed — see news).

Current state (details in news/2026-06.md and news/2026-07.md): ✅ CLI load + command dispatch
(`zef --help`/`--version` work) / ✅ the CompUnit::Repository install→use bridge
(`repository-for-name` well-known names, automatic default site-repo registration, pinned by
`t/compunit-repository-for-name.t`). Remaining:

- [ ] **Blockers to end-to-end execution of the real zef binary (major progress in the 2026-07-12 session)**:
      the old 2 bugs are resolved ((a) %-sigil Associative bind = #4452 / (b) parser error = no longer
      reproduces). Landed the same day: #4457 (classify pair-iteration / hash-init contained-Pair /
      IO::Path.child concatenation),
      #4460 (grammar token static fold — `REQUIRE.parse` went from ~70x vs raku → **1.1x**),
      #4462 (`Version.parts/.plus/.whatever` — the true cause of candidate-version matching),
      **#4466 (★root fix of the former biggest blocker: on worker threads, append/prepend/pop/shift/
      splice on shared arrays bypassed the `__mutsu_atomic_arr::` store and were silently lost — the
      true cause of the "%-hash attribute push loss" was that populate's
      `append @short-names-to-index` was wiped out entirely. See news/2026-07.md)**.
      → **`zef info Zef` works completely — unmodified upstream, hyper enabled, GC default on, real
      fez index (7648 dists) — all the way to Identity/Provides/Depends output** (stable across 2
      release runs; pin t/hyper-array-mutators.t).
      Current frontier:
      1. populate performance: the former "3-5 min" was dominated by an MRO dispatch bug, fixed
         2026-07-15 (accessor-MRO-shadowing PR): `Zef::Distribution.name` dispatched to the parent
         DependencySpecification's `method name` (a full REQUIRE identity-grammar parse per call,
         ~4ms) instead of the child's `has $.name` accessor. Second fix same day
         (definite-return-eval PR): every return from a `--> Nil` routine (e.g.
         Zef::Distribution's TWEAK) paid a full string EVAL whose setup clones the entire class
         registry twice; constant specs (Nil/True/False/Empty/pi/int) are now constructed
         directly. Third fix (wrap-chain-prefilter PR): every slow-path instance call
         (bless/TWEAK) Debug-traversed method-body ASTs looking for `.wrap` chains that don't
         exist; gated behind `has_any_wrap_chains()` + `Arc::ptr_eq` candidate matching.
         Combined with #4559 (subrule memoization): full fez populate = **6.5s** release
         (raku ~1-2.8s). Fourth fix (native-bless PR): `bless` no longer routes through the
         interpreter's generic method-dispatch scan — the VM forks it straight to
         `dispatch_bless`, which now reuses the cached per-class `NativeCtorPlan` (attribute
         defs + BUILD/TWEAK probes, incl. 6.e role submethods) instead of re-collecting the
         class shape per call; ctor microbench (benchmarks/bench-ctor.raku) 0.35 → 0.31s release,
         **2.9x → 2.2x** vs raku. Fifth round (2026-07-15, #4571/#4573/#4575/#4576 —
         see news/2026-07.md): single-visible-candidate fast return in method
         resolution (skips the speculative match for the BUILD/TWEAK dispatch shape),
         and the construction phases thread the constructed instance's shared
         attribute cell (no more per-step AttrMap clones / phantom intermediate
         instances; also a raku-compat fix — `self` inside BUILD/TWEAK IS the returned
         object). bench-ctor 0.341 → 0.299s local. Remaining (per-dist): the
         attributive-named-param full env path of TWEAK (`:%!meta` forces
         `call_compiled_method`'s full env setup + merge — §5 item 0 / §1.5 territory),
         MakePair/named-arg re-materialization (`|%_` slip), Symbol intern/as_str
         traffic in dispatch signatures.
      2. Nested `.raku` rendering: an Instance inside a collection renders as `Sp()` (type-object
         style) (`(C.new,).raku` → raku gives `(C.new(...),)`). The value itself is fine
         (semantically harmless; display only).
      3. `zef list --installed` runs to exit 0 with no output (reasonable while the mutsu-side site
         repo is empty).
      4. ~~Small index-name-count difference~~ RESOLVED 2026-07-15: the 3 missing keys were the same
         accessor bug (`.name` returned the identity-grammar parse result instead of the attribute;
         dists whose name fails to parse were dropped). mutsu now indexes 9259/9259 keys, exactly
         matching raku.
      5. (Watch) the old observation "with GC on, the 2nd Ecosystems reads an empty `$!name`" did not
         reproduce in 2 release runs after #4466. If it recurs, investigate independently as
         GC × thread state corruption.
- [ ] Known small difference: coercion of CLI numeric strings to `Int $n` is more eager than raku
      (`7` matches `MAIN(Int $n,…)`; raku falls back to slurpy). In practice the mutsu behavior is
      more intuitive.
- [ ] **network fetch**: fetching from the fez ecosystem (`https://360.zef.pm/`). Robust async TLS is
      a prerequisite.
- [ ] **Real install + build/test execution**, an `mzef` binary shim + vendoring of zef itself +
      dependencies + config (debian's zef lacks `resources/bin/zef`; a known-good vendoring is needed).

Split: **"keep running the real Zef as a test target" has high immediate value — continue it**. For
real installs as the bundled installer, network fetch (TLS) is the biggest prerequisite work.

### B3. Distribution and tooling

- [ ] **Binary distribution**: verify installation via the mise GitHub backend / automate GitHub
      Releases. This is the entry point of "everything present just by installing", so design it
      together with B1 vendoring (packaging of the binary + bundled module tree).
- [ ] REPL / Debugger / native binary output / public WASM playground.

### B4. Remaining module-compatibility blockers (the base of batteries)

- [ ] **NativeCall remainder**: ① `CArray[uint8]`, `CArray[Str]` ② `is repr('CStruct')` structs
      ③ callbacks (generic C callbacks). Everything from the MVP up to real SQLite CRUD is done
      (news/2026-06.md archive section).
- [ ] **Remaining 2 blockers for full Humming-Bird serving** (LOAD + LISTEN + accept + decode works;
      #3549): **B1** = leakage of `var_type_constraint` from typed parameters to same-named caller
      lexicals (the proper fix scopes the global name-keyed HashMap at call boundaries;
      env-authoritative-ization is not possible because it breaks subset-6e). **B2** = a detached
      `start{react{whenever $chan{}}}` is not driven unless awaited = the concurrency-scheduling
      campaign. Details = memory `session-24-humming-bird-loads`.
- [ ] **Remainder from the HTTP::Server::Tiny deep dive**: keep-alive consecutive requests, chunked
      request bodies, and `done`/`last` control signals inside `whenever $conn.Supply(...)` (the tap
      callback runs on a worker thread, disconnected from the react control-flow frame). These do not
      fire in the default configuration, so basic serving is unaffected.
- [ ] **Template::Mustache remainder** (91/92 specs): delimiter persistence / inheritable partials /
      a lambda + first-spec-only `+$spec.value`=0 subtest/Seq-consumption bug.
- [ ] Stored Regex `<$var>` lexical capture loss (found via Tubu; separate axis).
- 📌 The off-the-shelf `DBDish::SQLite` depends on `MoarVM::Guts::REPRs` (direct emulation of MoarVM
  internal representations) and cannot work in principle = a de-facto wall. Practical SQLite goes
  through DBDishLite + NativeCall (investigation conclusion = news/2026-06.md).
  A general parse bug found as a side effect (unfixed): greediness of the ternary then-branch in
  `constant NAME is export = <cond> ?? <Type> !! <Type>`.

---

## 2. ★ Phase B: GC → NaN-boxing → JIT (layers 3a, 3b, and layer-4 JIT all complete)

**GC (cycle collector on Arc, layer 3a) is done and default on** (2026-07-05 ADR-0003).
**NaN-boxing (layer 3b, = §5 Lever 2) is also done** (2026-07-12 #4467 B-guards / #4469 B-flip;
`size_of::<Value>()` 48→8B; GC counters match main; all benches 5–9% faster = gate met).
History and details in [news/2026-07.md](news/2026-07.md). Remaining:

- **Layer 4 JIT (Cranelift, = §5 Lever 4) = done**: following J1–J5 (default on, 2026-07-13),
      **all 6 J4d slices are complete, the gate was re-judged, and ADR-0004 is closed** (2026-07-15 —
      #4527/#4528/#4529/#4534/#4537/#4540; bench CI: fib+jit ratio 0.34→0.28;
      history = [news/2026-07.md](news/2026-07.md); judgment =
      [ADR-0004](docs/adr/0004-jit-strategy.md) 2026-07-15 addendum).
      The root fix for the remaining interpreter/JIT shared fixed costs (SetLocal env-mirror etc.)
      belongs to the §6 lexical-slot campaign. The canonical source of bench numbers is the bench CI
      (`bench-data` branch; the `+jit` series starts at #4480; from J5 onward the plain series pins
      `MUTSU_JIT=off` explicitly as the interpreter baseline).
- [ ] **3b-2 traffic pruning** ([docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §3.3):
      now that clone/drop is an 8B copy, reduce needless clones themselves (overlaps with the
      inventory of the 9022 `.clone()` calls). Lower priority than the JIT; profile-driven.
- [ ] Layer 3a hardening (H1 continuous measurement through the H5 background-collect start trigger) =
      see [docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md). Suppressing the candidate pushes
      from grammar parsing themselves (~510k/200-parse) is not started (the real harm — memory
      retention — was already fixed via `Weak`).
- Layer 3c biased refcount = frozen (start trigger in gc-post-3a-roadmap §4).
  Layer 4 JIT = [ADR-0004 (Accepted 2026-07-06)](docs/adr/0004-jit-strategy.md);
  start condition = layer 3b gate met (§5 Lever 4).

---

## 3. 🔴 substrate — removing the multi-dispatch tree-walk fallback (leftovers)

The major campaigns (single-store unification, tree-walk interpreter removal, first-class containers,
state ownership, multi-dispatch VM-ization, **module-sub OTF gate relaxation #4427→#4429→#4431→#4437**)
are complete ([news/2026-06.md](news/2026-06.md) / [news/2026-07.md](news/2026-07.md)). The gate
itself = `def_is_otf_compilable_module_single` (`vm/vm_call_func_ops.rs`). The frontier of
"just remove the gate and experiment" is exhausted; every remaining task requires **mechanism work**:

- [ ] **OTF for `start` = per-call capture cells**: when a recursive sub's start closure captures a
      param, the re-bind of the recursive call clobbers the captured value, so this is excluded
      wholesale (regression pin = `t/start-block-return-value.t` test 3; proof of infeasibility and
      history = news/2026-07.md).
- [ ] **OTF for sigilless scalar (`\x`) params**: caller writeback of raw aliases across EVAL needs a
      mechanism equivalent to #4091 (`is rw` compile-time caller slot)
      (FAIL pin = `t/sigilless-params.t` "sigilless aliases are writable through EVAL calls").
- Intentionally excluded (decided not to do): default-param builtin-shadow single candidate
  (name-cache pollution risk; user policy) / `is encoded(...)` (NativeCall; zero practical harm) /
  `state` sharing across signature alternates (kept as an interpreter boundary).

---

## 4. 🟢 roast backlog — the frontier is `integration/` (real-program compatibility)

The whitelist currently stands at **1384 / 1463** (2026-07-14) = **79 files** not whitelisted. The
authoritative detailed table is [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md).

**What the 2026-07-14 full measurement of the 79 non-whitelisted files revealed** (the old §4 list was
stale — mostly already-implemented items. Items eliminated by measurement = negation meta / hyper
assignment / `augment class` / `A::B.new` / file test / PRE·POST / signature type checks /
lazy-seq ④ — history in [news/2026-07.md](news/2026-07.md)):

- **The S\* series (synopsis feature tests) is genuinely exhausted**. All 24 remaining files are
  non-goal / oracle-impossible / waiting on infrastructure.
- **The bulk of the non-whitelisted files is the 41 `integration/` files** (plus 7 in `6.c/` and 4
  APPENDICES), and **nearly every one is perfect-score under raku = by definition all ★achievable**.
  They are **real Raku programs** — Advent Calendar and 99-problems — the compatibility metric
  closest to the goal of "you can write practical code". They were entirely missing from the
  BLOCKERS.md table (= they had never once been picked up as work).

The root causes collapse into a handful (counts, symptoms, and files in BLOCKERS.md §integration):

- [ ] **① Deep recursion overflows the Rust stack and aborts the whole process (4 files)**:
      `99problems-41-to-50.t` / `99problems-51-to-60.t` / `man-or-boy.t` /
      `deep-recursion-initing-native-array.t` die with `fatal runtime error: stack overflow`.
      The root is that Raku-level recursion consumes the Rust call stack = a mechanism issue
      (heap-allocated frames / stack growth / depth control).
      **Highest impact, and the same target as "zero panics/crashes on edge cases" (§6).**
- [ ] **② 10 files fail to parse** (`===SORRY!===`): `q | … |` delimiters, user-defined postfix
      (`4.7k`), heredoc indentation, `do {…} … *` sequence series, `subtest … => {}`, etc. Each
      construct is independent = likely contains some cheap ★ wins.
- [ ] **③ 5 hangs/timeouts**: `gather-with-loops.t` and others.
- [ ] **④ 2 error-message-quality files**: `error-reporting.t` (**28/33**, originally 4/33) and
      `weird-errors.t` (**31/36**, originally 26/36). #4539 implemented backtraces on all runtime
      errors, is_run seeing the same stderr as the CLI, Backtrace.new/.full, etc. Compile-time
      undeclared-routine detection is now implemented (breakdown of the remainder in the
      BLOCKERS.md inventory rows).
- [ ] **⑤ Individual feature gaps**: derived grammar extension / `nextsame` with inheritance and
      mixins / `Rat`'s `$!numerator`/`$!denominator` / `Metamodel::GrammarHOW` inheritance /
      `--doc` and `DOC INIT {}` / signature introspection / parameterized-role mixins / operator
      adverbs (`:round`) / precomp.
- [ ] **Shortcut**: `6.c/S04-declarations/my-6c.t` is at **111/112** (the only failure =
      the `OUTER::<$x>` pseudo-package).

The only real feature gaps left in the S\* series (they do not directly lead to whitelisting):

- [ ] Multi-line feed: feeds spanning lines with a leading `==>` (blocked by the
      `!ws_before.contains('\n')` guard in `parse_list_infix_loop`). `ff`/`fff` and single-line feeds
      are done. `==>>`/`<<==` and `~<`/`~>` are unimplemented/unspecified in rakudo itself = cannot
      be started.
- [ ] Remaining typed-exception gaps: strict-mode undeclared-variable detection / cross-EVAL
      detection of class redeclaration / X::Redeclaration::Outer (compile-time scope analysis). All
      non-trivial, and none whitelists a roast file on its own.

---

## 5. perf — execution speed (measurement-driven; MUTSU_VM_STATS / timed roast)

**Completed levers** (details = news/2026-06.md / news/2026-07.md): method-call hot path round 1
(#3853/#3857/#3859/#3867/#3870) / removal of the per-call env deep clone via single-store
unification / the malloc clusters from `Value` clone/drop and attribute materialization
(#4447 / #4451 / #4494 — attributes became `AttrMap` = `FxHashMap<Symbol, Value>`, and the
`__memcmp_avx2` 5.2% in the profile disappeared) / **Lever 2 NaN-boxing** (ADR-0001 layer
3b, #4467/#4469; `Value` 48→8B). Remaining levers:

- **Lever 3: threaded dispatch — frozen** (user-approved 2026-07-06;
      [ADR-0004](docs/adr/0004-jit-strategy.md) §2.5 J0): JIT Tier A takes the same gain — larger —
      by removing the dispatch loop, so avoid double investment. Revive only if the JIT fails.
- **Lever 4: JIT (Cranelift) = ADR-0001 layer 4 — done** (J1–J5 + all 6 J4d slices; default on;
      gate judgment included — **ADR-0004 closed** (see the 2026-07-15 addendum)).
      Lever 5: skipping type-constraint checks in tight loops was mostly recovered by J3's
      `type_matches_value` fast accept. Canonical bench numbers = the bench-data branch
      (`+jit` series).
- [ ] **Lever 6: biased reference counting = ADR-0001 layer 3c (independent perf work, post-GC)**.
      Frozen — the only start trigger is "atomic inc/dec remains near the top of the profile after
      JIT J4 completes" (gc-post-3a-roadmap §4).
- [ ] **Lever 7: baseline (classical) bytecode optimizations = [ADR-0006](docs/adr/0006-baseline-interpreter-optimizations.md)**.
      Orthogonal to the JIT (it shortens the executed opcode sequence itself).
      **§2.1 constant folding (#4485), §2.4 constant-pool dedup (#4486), §2.2 `constant` inlining +
      constant-condition DCE (#4487), §2.3-a declaration-sequence fusion (#4488), and §2.3-b
      `SetSourceLine` removal (#4489) are done** (contents and numbers per slice = news/2026-07.md).
      Remaining:
      - **★Lesson learned here = reducing opcode count ≠ reducing time** (measurement protocol =
        [ADR-0006 §"Measurement protocol for implementation slices"](docs/adr/0006-baseline-interpreter-optimizations.md)).
        `SetSourceLine` was 21% of executed opcodes (fib) but the cheapest single-store op, so the
        time saving was an order of magnitude smaller (**-3.4% instructions**; JIT path ±0). Worse,
        the implementation that added a refresh to every instruction was a **+7.8% instruction**
        deficit. Before touching the remaining administrative ops (`SetVarDynamic` 500k,
        `CheckReadOnly` 100k), **first confirm with perf retired instructions (`instructions:u` +
        `taskset` core pinning — otherwise it wobbles 8%) that they actually consume time**. If it's
        a miss, don't chase it.
      - [ ] **§2.3-c remaining administrative ops (`SetVarDynamic`, `CheckReadOnly`)** — start only
            after passing the pre-measurement gate above.
- [ ] **★The next perf target is what the profile shows — "allocation, hashing, env" (not the opcode
      histogram)**. **Order of attack, root causes, and gates are already researched in
      [docs/perf-callpath-scouting.md](docs/perf-callpath-scouting.md).**
      The table below is from `perf record -e cycles:u` on release, JIT on (default configuration),
      pinned to a P-core (2026-07-13, after #4489), and is **the evidence that drove #4492–#4495**.
      Now that those have landed (readonly deep clone, sigilless-key `format!`, and attribute
      `String` keys are removed), **the table is stale = re-profile before starting the next item**:

      | bench-fib (call-heavy) | % | bench-class (object-heavy) | % |
      |---|---|---|---|
      | `call_compiled_function_positional_light` | 10.9 | `malloc`+`_int_malloc`+`_int_free`+`malloc_consolidate` | **19.5** |
      | `_int_free`+`_int_malloc` | **11.8** | `__memcmp_avx2` (attribute-name `String` key comparison) | 5.2 |
      | `Env::scoped_child` | 5.4 | `nanbox::gc_op`+`Gc::drop` | 7.7 |
      | **JIT native code itself** | *5.7* | `exec_call_method_mut_op` | 2.7 |
      | `Env::get_sym` | 4.4 | `AttrReadGuard::drop` | 2.3 |
      | **SipHash `Hasher::write`** | 4.1 | | |
      | `hashbrown RawTable::clone` (env table duplication) | 3.7 | | |

      Reading: in fib, **the JIT-generated native code only runs 5.7% of the time — allocation
      (11.8%) + hashing / env table duplication (12%+) + the call path (10.9%) dominate**. In
      bench-class **the allocator alone is ~20%**, plus attribute-name `String` key comparison
      (memcmp 5.2%).

      **Consumed by 2026-07-14 (details = news/2026-07.md)**: allocation-free per-call readonly
      snapshots and return merge (#4492 — **bench-fib -32.3% / bench-tak -23.9%**) / pre-interning of
      sigilless-alias and readonly env keys (#4493 — num-arith -21.6% / bench-mandelbrot -14.9%) /
      `Symbol`-keyed attributes (#4494 — ✅ above) / stopping per-declaration/per-store metadata key
      rebuilding (#4495 — time-parts -37.2% / bench-mandelbrot -33.7%) /
      **removal of intern, SipHash, and COW from the declaration path (`my $x = ...`)**
      (#4506/#4507/#4508 — time-parts went from **1.17 → 0.62 vs raku with JIT on**, and even
      interpreter-only from 1.46 → 0.93, beating raku. Breakdown = latching the placeholder `^name`
      probe, using the pre-interned Symbol that `flush_local_to_env` was discarding, removing the
      re-intern/String allocation in `SetVarDynamic`, avoiding `Arc::make_mut` on absent-key
      deletion, removing the SipHash probe on empty maps, and `Symbol`-keying the
      declaration-tracking set. Along the way, **~310 lines of the unreachable `simple_locals` fast
      path were deleted** (scalar locals are stored without sigils, so `name.starts_with('$')` was
      always false = it had never executed)).

      **★Re-baseline 2026-07-15 (roast-wide raku-vs-mutsu wall-clock, `scripts/roast-speed-diff.sh`
      over the 1358 runnable whitelisted tests):** mutsu beats raku (Rakudo 2022.12) on ~1348/1358
      tests, usually 2–30× (fast startup + JIT). The **only** files where mutsu is meaningfully
      *slower* (clean single-run ratios, release):
      `S04-declarations/state.t` **4.2×** (`for ^2000000 { $ = foo }`), `S06-signature/named-parameters.t`
      **2.6×** (`for ^1000000 { foo(:color($_)) }`), `S07-iterators/range-iterator.t` **1.7×**,
      `S12-methods/private.t` **1.6×**. Isolated micro-repro: a 1M-iteration loop calling a sub is
      **1.85× slower** than raku positional / **2.6× slower** with a named arg. **All of these converge
      on one root: the interpreter function-call path in hot loops.** The JIT *bails at the call
      boundary* (proven: positional 1M loop is `MUTSU_JIT=on` 0.74s ≈ `off` 0.72s — JIT does nothing),
      so any loop that calls a sub runs the interpreter call path, whose profile is **~15% malloc/free
      churn per call** (frame + named-args structure + `Env::cow_mut` + `Arc::drop_slow`) plus per-call
      `current_package`/`Env::get_sym`/param-binding (`exec_set_local_op_inner`). Named args add
      disproportionate cost (mutsu **+46%** vs raku **+6%**) = a `String`-keyed named-args structure
      rebuilt every call. **This is the highest-value perf target** — it hits real spec tests and the
      most common real-world shape (a loop that calls a sub), unlike the two items below which polish
      benchmarks mutsu already wins. (`bench-grammar-parse`'s synthetic 6.4× gap does NOT surface in
      the roast whitelist — the whitelisted grammar tests are not pathological.) See memory
      `project_roast_speed_measurement`.

      **Remaining (in order of attack)**:
      -1. **★Function-call path in hot loops (NEW top priority, from the re-baseline above)** — reduce
         the per-call allocation churn: reuse the call frame / named-args storage, cut `Arc::drop_slow`
         on teardown, cache `current_package`, and avoid rebuilding the `String`-keyed named-args
         structure each call (intern the param names, bind by `Symbol`/slot). Because the JIT bails at
         calls, this is interpreter-path work that no amount of JIT progress will subsume. Pins:
         `roast/S04-declarations/state.t`, `roast/S06-signature/named-parameters.t`; micro-repro in the
         re-baseline note.
      0. **★Removing the `needs_env_sync` blanket (a dedicated-session fused campaign; NOT a
         standalone change — see the four-mechanism breakage below)** — currently `captures_env_by_name`
         (true if the frame contains even one
         `ForLoop`/`BlockScope`/`MakeGather`/`WheneverScope`) **makes every local in the frame an env
         mirror target**, so locals never read by name — like a loop body's `my $ts` — are written to
         env on every store. **Update 2026-07-15 (probed, then clean-reverted):** the actual per-store
         cost is the *unconditional* env write at the tail of `exec_set_local_op_inner`
         (`vm_var_assign_set_local.rs`, `set_env_plain_lexical`/`set_env_with_main_alias`) — NOT
         `flush_local_to_env`, which is already gated on `needs_env_sync` (so the `env_flushes`
         counter reads 0 and does not surface this; measure by wall-clock). Gating that tail write on
         `needs_env_sync || reflective` won ~7% on a JIT-bailed `time-parts` loop but **deterministically
         broke four independent mechanisms** (each pinned by an existing test), confirming this is a
         fused campaign, not a standalone change:
         (a) **block-scope restore** — `exec_block_scope_op` reverts `self.locals` to the pre-block
             snapshot then **re-pulls every local from env by name**, so an outer var mutated inside a
             bare `{ }` reverts to its pre-block value without the env seed (`my $x=1;{$x=2};say $x`
             printed `(Any)`). `BlockScope`/`BlockLocalScope` frames therefore still need the blanket;
             note a loop-body `if { }` stays *inline* (no `BlockScope`) unless the branch declares its
             own `my` (`BlockLocalScope`), so most hot loops are unaffected.
         (b) **cross-thread closure capture / `cas`** — a `%h` captured by a `Thread.start` body and
             mutated via `cas` needs its shared-var cell established through env by name; the gate lost
             it (`tests/gc_stress.rs::dead_sweep_bounds_threaded_mutation_memory`, sum=2 vs 800).
             Folding `closure_compiled_codes` free vars + `op_arg_sources_idx` (rw-arg sinks) +
             `op_container_mutate_const_idx` into `needs_env_sync` fixes this axis.
         (c) **★JIT outer-lexical read (the decider)** — the JIT reads an *outer* lexical referenced
             inside a hot loop (`my $c=…; for ^30 { $c.bump() }`) **from env by name**, so eliding the
             env seed makes the JIT see `Any` (`tests/jit_diff.rs::hot_method_body_compiles_and_matches`).
             Escape-folding cannot fix this; the JIT is default-on and compiles exactly the hot loops
             the gate targets. A "gate only slots *declared inside* a `ForLoop` body (JIT-register
             temps)" restriction might be safe but is unverified.
         (d) **currying/priming capture** — `roast/S06-currying/positional.t` aborts at test 157.
         So it is **a campaign fused with §1.3/§1.5, §6 (`BlockScope`'s `self.locals.clone()`), and the
         JIT's lexical-access path** (memory: a standalone change has a track record of breaking 5
         mechanisms — 4 reproduced here). See memory `project_needs_env_sync_blanket_removal`.
      1. **Remove SipHash from `compiled_fns`** (scouting §2.1 — the function table is still a
         `HashMap<String, CompiledFunction>` (`vm.rs:280`), so **even calls that hit the light-call
         cache SipHash + memcmp the function name every time**). Order: FxHashMap → `Symbol` keys →
         store the callee itself in the cache to eliminate the lookup. Mechanical; effect
         predictable.
      2. **Remove the callsite-line marker** (scouting §2.3 — `peek_callsite_line`
         (`runtime/call_helpers.rs:194`) scans args on every call. With the #4489 line table in
         place, deriving it from the call op's ip makes it unnecessary).
      3. **Lexical-scope slot campaign** (scouting §2.2; §6's "remove the full locals clone/restore
         in `BlockScope`") — the core fix that eliminates per-call env materialization itself.
         Suited to a dedicated session.
- [ ] **Opcode leftovers ([docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6;
      continuation of #4279)**: move the inline `Option<String>` payloads (labels etc. —
      `Last`/`Next`/`Redo`/loop family/`SmartMatchExpr.lhs_var`) to constant-pool `Option<u32>`
      (bring `OpCode` under 48B) / measured reduction of per-instruction fixed costs
      (`current_code` raw-pointer store, `trace_log!` check) / fix the encoding where `Jump(i32)`
      carries an absolute index / merge specialized ops driven by a per-opcode histogram
      (`ContainerEq`×4, `IndexAssign*`×6 — driven by data, not aesthetics).
- [ ] Regexes: reduce `RegexCaptures.clone()` per quantifier iteration. **Update 2026-07-15:
      the exponential half of this item is FIXED** — ratcheted (`token`/`rule`) separated
      quantifiers (`* %`) are now possessive (single greedy chain, Rakudo semantics), and
      ratcheted quantifiers over alternation atoms skip the bounded backtracking expansion:
      the `S04-exceptions/exceptions-alternatives.t` JSON parse went **12.6s → ~0.9s**
      (was ~3.6×/pair exponential, now linear), and the capture-heavy alternation case
      `[ (<[ac]>) | (<[bc]>) ]*` over 40 chars went 372ms → 3.5ms. Pinned by
      `t/regex-sep-quantifier-ratchet.t`; tracked by `benchmarks/bench-grammar-parse.raku`
      (mutsu ~5.9s vs raku ~0.5s, **~12×**). **Update 2026-07-15 (2): the Named-subrule
      per-call ceremony is also FIXED** — subrule resolution+parse is memoized per
      (pkg, name) in `PARSED_TOKEN_CANDIDATES` (invalidated by `TOKEN_DEFS_GEN`, same
      discipline as `REGEX_PARSE_CACHE`): the per-reference registry walk
      (`collect_token_patterns_for_scope` scanned every `token_defs` key), the per-candidate
      `parse_regex` probe, the singular matcher's scratch sub-interpreter + tail-text
      `String` copy, and the all-path's `tail.to_vec()` are all gone; the singular arm now
      matches in place and wraps via the shared `build_named_candidates_from_inner`
      (capture markers / silent-subrule action channel now behave identically on both
      paths). bench-grammar-parse **5.9s → ~2.2s (~2.7×)**; nested `matrix` case 3.0s →
      1.0s. Remaining (why still ~4-5×): allocation churn — profile is ~50%
      malloc/memmove/free with `RegexCaptures::clone`+drop and `String::clone` on top =
      the capture representation itself (String-keyed maps, full clones per DFS stack
      push and per non-ratchet backtrack step). That is the original item of this bullet.
- Targets (numbers from bench CI, main `c8955d2e`, 2026-07-13; parentheses = JIT-on series):
  method-call <1.5x (✅ 1.19x / jit 1.16x), bench-class <1.5x (✅ 1.02x / jit 1.00x),
  fib <10x (✅ **0.82x / jit 0.65x**), bench-fib (with type constraints) <2x
  (✅ **1.78x / jit 1.39x**), int-arith **0.47x / jit 0.43x**.

---

## 6. Concurrency (Track C leftovers) and structural refactoring (independent; mid-to-long term)

- [ ] **Remainder of true sharing for state/lexical aggregates**: only the lost-update on
      high-contention concurrent "structural" inserts (real rakudo crashes with a MoarVM oops on the
      same shape = outside the language guarantee. mutsu doesn't break, which is an advantage — keep
      as out-of-spec = effectively a decision not to do it). The cell-ification itself was completed
      in Track B slices 2+3 and T6 (news/2026-07.md; pin = the 18 tests in
      t/state-aggregate-shared-cell.t).
- [ ] Semaphore / nonblocking await / lock contention (S17; hard; separate axis).
- [ ] **Running a recursive start/await sub twice in a row hangs the second one (found 2026-07-11;
      reproduces on main ef5cd62e)**: after running
      `sub f($n){ start { $n<=0 ?? "b" !! await(f($n-1)) ~ "|$n" } }; await(f(3));`, awaiting a
      start sub with 2-branch recursion (`await(fib($n-2)) + await(fib($n-1))`) hangs
      deterministically (fib alone, or two consecutive single-branch recursions, are OK). Suspicion:
      the preceding start chain does not release thread-pool workers. raku returns both 4/8.
- [ ] Eliminate raw-pointer aliased writes: the old `arc_contents_mut` is dead code now, and the
      production path moved to `gc::gc_contents_mut` / `Gc::{get,make}_mut` (the unsoundness was
      moved, not resolved — ANALYSIS rev8 §2.1). With Track B T4–T6 done (news/2026-07.md), start
      from an inventory of what actually remains.
- [ ] **★Remove the full locals clone/restore in `BlockScope`** (the final move of the lexical-scope
      slot campaign [docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md);
      **the perf core** — the root of the malloc/free churn and `Env::get_sym` shown by the #4489
      profile; see §5): remove the `self.locals.clone()` in `exec_block_scope_op`. A load-bearing
      refactor entangled with the `$OUTER::` runtime snapshot, GC roots, and env resync — suited to a
      dedicated session. The preliminaries (S1–S17 slot burn-in + shadow-slot default ON) are done
      (news/2026-07.md).
- [ ] Separating the error/control channels: consolidating the bool flags into `enum Control` and
      shrinking `RuntimeError` (cold Box-ing; `result_large_err` 23→0) is done. The remaining
      structural separation of "carrying control flow via `Result::Err`" is low priority now that
      the practical harm is gone (ANALYSIS rev8 §2.2).
- [ ] Propagate Supply detached-worker panics to QUIT (currently swallowed; ANALYSIS §5).
- [ ] Derive `.^methods`/`.can` from the real dispatch table / split out the roast fudge logic /
      split files over 500 lines.
- [ ] **Hygiene-trend inventory (ANALYSIS rev8 §5/§6)**: re-slim the re-bloated `runtime/mod.rs`
      (1932→2118 lines) / review the increases from GC and Track B churn: `.clone()` 9022 (+1322),
      `unwrap` family 1643 (+167), `#[allow(` 157 (+19).
- [ ] **Improve error-message quality / bring edge-case panics and crashes to 0** — not an abstract
      goal: it turned out (2026-07-14) that it **can be driven by roast pass/fail**: quality =
      `integration/error-reporting.t` (mutsu 4/33, raku 33/33) and `weird-errors.t`;
      crashes = **the deep-recursion `fatal runtime error: stack overflow` (process abort; 4 files)**
      is the concrete target. → §4 / BLOCKERS.md §integration.

---

## Metrics

| Metric | Current | Target |
|------|------|------|
| **Bundled libraries (vendored + documented)** | **0** (10+ with working record still in t/lib or fetched externally) | **10+ bundled, all documented** |
| mzef | CLI startup + dispatch ✅ / install→use bridge ✅ / **`zef info` works fully on the real fez index ✅** (#4466) | Real install with the real zef binary (remaining = populate performance, network fetch (TLS), vendoring) |
| Binary distribution | none | Single-command install via mise / GitHub Releases |
| Whitelist | **1384** (of 1463 total .t files; 79 remaining) | 1300+ ✅ achieved. Next target: the 41 `integration/` files (§4) |
| GC | **default on ✅** (2026-07-05; ADR-0003) | Achieved (remaining perf goes to layer 3b) |
| JIT | **default on ✅** (2026-07-13); J4d complete = **ADR-0004 closed** (2026-07-15) | Achieved |
| fib(25) vs raku | **0.82x / jit 0.65x** (bench CI `c8955d2e`, 2026-07-13) | <10x ✅ |
| method-call vs raku | **1.19x / jit 1.16x** (same) | <1.5x ✅ |
| bench-class vs raku | **1.02x / jit 1.00x** (same) | <1.5x ✅ |
| bench-fib (with type constraints) vs raku | **1.78x / jit 1.39x** (same) | <2x ✅ |
| Startup time vs raku | **0.04x** | 0.04x ✅ maintain |
| Tree-walk fallback (methods/functions) | **~1% / ~18.6% (mostly carrier)** | 0% (excluding carrier) |
