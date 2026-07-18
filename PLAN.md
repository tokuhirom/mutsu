# PLAN.md — mutsu implementation plan

> This file lists **only unfinished work**. Completed work moves to [news/](news/).
> See [news/](news/) for past logs, [PERFORMANCE.md](PERFORMANCE.md) for performance details,
> and [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) for roast failure analysis.
>
> **Last updated 2026-07-16 — priority reset: performance is NOT the mzef blocker.**
> mzef already works functionally: `zef info Zef` runs to completion against the real fez index
> (7648 dists), populate is ~6.5s, and the constructor microbench already **beats** raku (0.57×).
> Perf tuning (§5) is therefore **polish, not a blocker** — it had been getting picked up
> autonomously session after session past the point of usefulness. **The active mzef frontier is
> functional, not performance:** (A) network fetch over robust async TLS from `https://360.zef.pm/`,
> and (B) the real install + build/test pipeline (`mzef` binary shim + vendoring zef itself and its
> deps + config). See §1 B2. §5 stays as a reference of levers/measurements but is **de-prioritized**;
> before starting any §5 item, ask "does mzef need this to work?" — if not, don't.
>
> **Earlier (2026-07-14)**:
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
- [x] **network fetch** — done. **No native TLS was needed**: zef shells out to the system
      `curl`/`wget` and mutsu drives that via `Proc::Async`, so the old "robust async TLS is the
      biggest prerequisite" assumption was simply wrong. Concurrent multi-candidate fetch works as of
      #4658 (ADR-0010).
- [x] **Real install** — done for a dependency-free dist: a real fez dist downloads, extracts,
      installs into the site repo and is then `use`-able (#4655). Verify with a **non-bundled** dist
      (`JSON::OptIn`); `use JSON::Fast` succeeds without any install because mutsu bundles it.
- [ ] **A dist WITH dependencies** — the remaining gap. Resolution (#4650) and fetch (#4658) are done;
      `zef install Test::META` now dies in **extract**. See `docs/mzef-install-pipeline.md`
      ("Current frontier") for the lead and the next steps.
- [ ] **build/test execution**, an `mzef` binary shim + vendoring of zef itself + dependencies +
      config (debian's zef lacks `resources/bin/zef`; a known-good vendoring is needed). The test
      phase (6) has not been exercised yet — runs have used `--/test`.

**`docs/mzef-install-pipeline.md` is the live tracker** — phase table, what each fix unblocked, and
the current frontier with its next steps. Read it before picking up mzef work.

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

## 4. 🟢 roast backlog — `integration/` is DONE; what remains is 33 mostly-unachievable files

The whitelist stands at **1430 / 1463** (2026-07-17) = **33 files** not whitelisted. The
authoritative detailed table is [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md).

**`integration/` is fully whitelisted (0 remaining).** That was this section's headline
target — "the bulk of the non-whitelisted files is the 41 `integration/` files", the real
Raku programs closest to the project goal. The last one, `advent2013-day18.t`, landed
2026-07-17 ([ADR-0009](docs/adr/0009-regex-code-assertion-execution-model.md)). The former
①(stack overflow) / ②(unparseable) / ③(hang) / ④(error-message) clusters are all cleared —
history in [news/2026-07.md](news/2026-07.md).

**Re-measured 2026-07-17** — the 33 remaining, by area: `6.c/` 4, `S02-names` (pseudo-package)
3, `S12-*` 6, `S05-*` 5, `S32-str` 2, `S10-packages` 2, `S06-advanced` 2, `APPENDICES` 2,
`roast/t/` 2 (roast's own Perl 5 tooling — non-goal by definition), and 5 singletons.

**There is no cluster left to attack.** Per the BLOCKERS.md classification nearly all of
these are *non-goal* (rakudo itself fails), *no oracle* (local raku SORRYs, so the correct
answer cannot be confirmed), or *awaiting infrastructure* (RakuAST for `S32-str/format.t`,
6.e generics for `S02-types/generics.t`). The genuinely ★achievable ones are few and each
needs its own unrelated feature:

- [ ] `6.c/S04-declarations/my-6c.t` — **111/112**. The one failure (`OUTER::<$x>`) needs
      **lexical hoisting**, not the OUTER pseudo-package alone; same family as
      mixin-6c / my-6e hoisting, so it has leverage beyond this file.
- [ ] `APPENDICES/A02-some-day-maybe/multi-no-match.t` — **11/16**. Error-message quality for
      multi no-match across ~10 builtins (`.splice`, `Lock.protect`, `Proc::Async.new`, …).
      No single lever; steady per-builtin work.
- [ ] `6.c/APPENDICES/A04-experimental/01-misc.t` — 16/19. `:D`/`:U` DefiniteHow coercion.

**Implication for planning: roast is no longer the productive axis.** Prefer §1 (Batteries /
mzef), §5 (perf) or §6 (concurrency / structural refactoring). Pick up a roast file only when
a §1/§5/§6 change happens to unblock it.

The only real feature gaps left in the S\* series (they do not directly lead to whitelisting):

- [ ] Multi-line feed: feeds spanning lines with a leading `==>` (blocked by the
      `!ws_before.contains('\n')` guard in `parse_list_infix_loop`). `ff`/`fff` and single-line feeds
      are done. `==>>`/`<<==` and `~<`/`~>` are unimplemented/unspecified in rakudo itself = cannot
      be started.
- [ ] Remaining typed-exception gaps: strict-mode undeclared-variable detection / cross-EVAL
      detection of class redeclaration / X::Redeclaration::Outer (compile-time scope analysis). All
      non-trivial, and none whitelists a roast file on its own.

### Gaps surfaced by the `raku-doc` update to `468a767f` (2026-07-07)

Audited the whole `raku-doc` diff `6c879bc7..468a767f` (see `vendor.lock` / `docs/vendoring.md`)
for newly-documented language features. Everything documented in the update is **already
implemented** — parameterised regexes (`my regex r ($x) {…}` called as `<r: arg>` and `<r(arg)>`),
and `Instant.from-posix` / `.to-posix` / `.Date` / `.DateTime` all match raku — **except one**:

- [ ] `exits-ok($code, $exit, $reason)` — new `Test` routine (`Type/Test.rakudoc`): passes if the
      code exits with the given exit code. Implement alongside the sibling `dies-ok` / `lives-ok`
      Test routines (Test-module handler, not a core builtin — it is not in `perl-func.rakudoc`).
      Note: no roast file uses it (not in upstream roast HEAD either), so this is Test-completeness /
      batteries polish, **not** a roast-whitelisting lever.

### Vendored `roast` bumped `7f2d7508` → `b2cbe8a4` (2026-06-12) — DONE 2026-07-18

The roast bump landed. All four prerequisite feature clusters were implemented first (plan A,
no whitelist regression), then `scripts/update-vendor.sh roast b2cbe8a4` re-vendored the tree
(33 files: 1 new, 32 modified). All 28 whitelisted-and-changed files still pass; no whitelist
edits were needed. The clusters, now merged:

- ✅ **① List-of-overlapping-needles `index`** — #4710.
- ✅ **② Immutable QuantHash RO errors** — `:delete`/`DELETE-KEY`/`ASSIGN-KEY` on immutable
      Bag/Mix throw `X::Assignment::RO`. Shipped in the bump PR (behavior change).
- ✅ **③ 6.e sprintf flag combinations** — sign-before-prefix, radix-flag rules, `#` forced
      decimal point, string zero-pad. `v6e_active()`-gated so 6.d is byte-identical. Shipped in
      the bump PR (behavior change).
- ✅ **④ `.assuming` priming signature gist** — #4722.

**Remaining after the bump:** the new `S02-types/quanthash.t` stays non-whitelisted — it needs
`.^parameterize` on Set/Bag/Mix (parameterized QuantHash types), tracked in
[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md).

---

## 5. perf — execution speed (measurement-driven; MUTSU_VM_STATS / timed roast)

> **De-prioritized 2026-07-16 (see header).** mutsu already beats raku on the whole roast whitelist
> and on every benchmark here, and mzef's ctor path is faster than raku — **performance is not what is
> blocking mzef.** This section is kept as a reference of levers and measurements, but do **not** pick
> up a §5 item just because the profile shows a hot symbol: first confirm mzef (or another goal item)
> actually needs it. The remaining big lever is biased reference counting (§ Lever 6 / ADR-0001 layer
> 3c) — a multi-week, high-risk campaign that would shave the inherent per-clone atomic-refcount cost;
> it is explicitly **not** required for mzef and should only be started as a deliberate, ADR-updating
> decision, not autonomously. (2026-07-16 investigation: the nanbox ~20% is dominated by the two
> atomic refcount ops per `Gc` clone / two per drop, inherent to the dual-count Bacon-Rajan-on-Arc
> design; the drop side is already optimized; cheap wins are exhausted.)

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
         (c) **★method-call caller-local coherence × JIT inline `GetLocal` (the decider; diagnosis
             CORRECTED 2026-07-15)** — `tests/jit_diff.rs::hot_method_body_compiles_and_matches`
             (`my $c=…; for ^30 { $c.bump() }`). The earlier "the JIT reads the outer lexical from env"
             claim was **wrong** — proven by unconditionally skipping *only* `$c`'s env write: the JIT
             run still prints the right answer, because `$c` is read via `GetLocal(0)` from its slot,
             not from env. The real gatekeeper is **method-call specific** (a positional *sub* call in
             the same JIT-hot loop is fine): the method path keeps caller-local coherence through env
             (`vm_call_method_ops.rs` `drain_and_reconcile_after_cached_call`), so once the gate makes
             `$c` env-absent, the first `bump()` leaves `$c`'s slot in a state the JIT's Tier-B **inline**
             `GetLocal` (which bypasses `exec_get_local_op`) reads as `Any`, while the interpreter's
             `exec_get_local_op` still reads it correctly — so it only surfaces under `MUTSU_JIT_THRESHOLD=1`
             (JIT off / default threshold pass). Fixing it is load-bearing method-dispatch work.
         (d) **currying/priming capture** — `roast/S06-currying/positional.t` aborts at test 157
             (cause not yet isolated; likely the same method/dispatch env-reconcile as (c)).
         So it is **a campaign fused with §1.3/§1.5, §6 (`BlockScope`'s `self.locals.clone()`), and the
         method-dispatch env-based caller-local reconcile** (memory: a standalone change has a track
         record of breaking 5 mechanisms). Note scalar locals are stored **sigil-less** (`"c"`, not
         `"$c"`) — relevant when instrumenting. See memory `project_needs_env_sync_blanket_removal`.
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
      1.0s. **Update 2026-07-15 (3): nested sub-captures are now shared behind `Arc`
      (#4586)** — `named_subcaps` / `positional_subcaps` / quantified-capture entries hold
      `Arc<RegexCaptures>` instead of an owned `RegexCaptures`, so cloning a parent caps at
      each DFS stack push (and every separated/repetition-quantifier fold) is a refcount
      bump rather than a recursive deep copy of the whole sub-match tree. Profile:
      `RegexCaptures::clone`+drop ~10% → ~4% of samples; bench-grammar-parse ~9% faster
      (1.51s → 1.37s), a deep nested-JSON doc ~4-10% faster. Pinned by
      `t/regex-nested-subcaps-sharing.t`.
      **Update 2026-07-16 (4): the trail matcher (ADR-0007, Accepted) is LANDED (#4591)** —
      the engine walks tokens depth-first over ONE mutable `RegexCaptures` store per
      pattern level with an undo-log (`regex_trail.rs`: mark/apply-delta/rewind); atom
      producers return deltas relative to an empty baseline instead of cloning the
      accumulated caps per candidate, and quantifier chains grow/shrink on the store with
      a mark per iteration. Per-step capture cost is O(delta), never O(accumulated).
      Measured (local release A/B): deep bench ~×1.25 (memmove 15.4%→7.5% of samples,
      `RegexCaptures::clone` 2.1%→1.0%), shallow ~×1.2.
      **Remaining: per-subrule ceremony — still ~25× vs raku per matched character.**
      The residual profile (~36% allocator + spread) is a constant cost per subrule
      invocation, NOT accumulated-state churn: candidate `Vec`s + captured-text `String`s
      + `Arc<RegexCaptures>` subcap allocs, HashMap+SipHash traffic on the caps maps
      (~3%), a **runtime regex re-parse path** visible in-profile
      (`parse_regex_uncached` + LTM expansion ~4% — bypasses `PARSED_TOKEN_CANDIDATES` /
      `REGEX_PARSE_CACHE` somewhere; find it first, likely the cheapest big win),
      `RegexCaptures::default` zeroing (~2%), one `snapshot()` per complete inner end.
      Next slices: memoize the residual re-parse; FxHash (or a small-vec map) for the
      caps maps; box cold `RegexCaptures` fields (shrinks default/memmove); intern trail
      undo-record keys. Details in
      **[docs/adr/0007-grammar-parse-trail-matcher.md](docs/adr/0007-grammar-parse-trail-matcher.md)**
      §Implementation outcome. Bench: `benchmarks/bench-grammar-parse.raku` (shallow) +
      `benchmarks/bench-grammar-parse-deep.raku` (deep).
- Targets (numbers from bench CI, main `c8955d2e`, 2026-07-13; parentheses = JIT-on series):
  method-call <1.5x (✅ 1.19x / jit 1.16x), bench-class <1.5x (✅ 1.02x / jit 1.00x),
  fib <10x (✅ **0.82x / jit 0.65x**), bench-fib (with type constraints) <2x
  (✅ **1.78x / jit 1.39x**), int-arith **0.47x / jit 0.43x**.

---

## 6. Concurrency (Track C leftovers) and structural refactoring (independent; mid-to-long term)

- [ ] **ADR-0008 push-delivery follow-ups** (the core landed in #4636 and the first two follow-up
      slices in #4638 / #4639, 2026-07-17; see docs/adr/0008-push-based-supply-event-delivery.md
      and news/2026-07.md). What is left:
  - [ ] **Write a Proposed ADR for a shared worker pool** (groundwork done 2026-07-17; all figures
        below are release builds on main 159a30cb0, 12 cores, raku 2026.06 on the same host).
        mutsu has no pool at all: it spawns a thread per task at each of the **19
        `spawn_user_thread` sites** (`ThreadPoolScheduler` is a bare type name in
        `runtime_init.rs:67` with nothing behind it). What that costs:

        | probe | mutsu | raku |
        |---|---|---|
        | 500 × trivial `start {}` | 0.232–0.262s | 0.051–0.07s |
        | 50 idle `cue(:every(60))` | RSS +20.7 MB, **VmSize +16.4 GB**, threads 2→**52** | RSS +4.3 MB, VmSize +25 MB, threads 2→**5** |
        | 200 × `start { sleep 2 }` | 2.09s (unbounded concurrency) | 6.1s (bounded, 3 batches) |
        | nested `start`+`await`, depth 500 | 0.99s (500 real OS threads) | **0.12s** |

        **The ADR's central question is not pool sizing — it is what `await` does to a pooled
        worker.** raku's `max_threads` defaults to 96 here (8 × cpu-cores) and genuinely-blocking
        tasks *do* serialize against it (200 × `sleep 2` takes 6.1s, not 2s), yet nested `await` at
        depth 500 does **not** deadlock on those 96 workers: Rakudo's `await` yields a MoarVM
        continuation (`$*AWAITER`) and hands the worker back. mutsu has no continuations, so a
        **bounded pool + blocking `await` deadlocks** (depth-500 pins every worker). The ADR must
        choose between (a) an **elastic** pool that grows on starvation, Rakudo-supervisor-style —
        which still re-explodes to ~500 threads on that shape, so it wins for idle `cue`/short tasks
        but not there — and (b) continuation-ifying `await`, a VM-scale project.

        Other decisions the ADR must record:
        - **Stack tiering.** `spawn_user_thread` reserves 256 MiB (`builtins_system.rs:9`) for
          deep-recursion headroom. Five sites (`Proc::Async` ×4, `signal_watcher.rs:47`) take that
          stack while running **no user VM code** — they only need GC registration, so
          reclassifying them to `spawn_gc_helper_thread` is free. Conversely 256 MiB *reserved* per
          pooled worker makes the steady-state pool size an address-space decision.
        - **Task-boundary invariants.** `clone_for_thread` (`runtime_thread.rs:8`) is per-*task*,
          not per-thread — a pooled worker cannot reuse the previous task's `Interpreter`. Likewise
          `drop_thread_local_gc_state` (`value/mod.rs:553`) must run **between tasks**, or task N's
          pending DESTROYs leak into task N+1 while the thread stays registered. `WorkerGuard`'s
          drop order (drain → `mark_thread_registered(false)` → `exit_mutator_worker`,
          `builtins_system.rs:65-80`) becomes a task-boundary rule rather than a thread-exit one.
        - **★The biggest correctness risk**: an idle pooled worker parked on a raw `recv()` is
          permanently non-quiescent and would defeat **every** STW in the process — strictly worse
          than today. The task-queue wait must use `stw_aware_wait` / `block_quiescent`.
        - **An argument in favour**: `preregister_worker_quiescent` and `notify_worker_exit`
          (`stw.rs:141/195`) exist *only* to survive spawn/exit churn; a pool makes
          `mutator_worker_count()` near-constant and both near-moot.
        - The shape to mirror is `interval_timer.rs` (leaked `OnceLock` state + one long-lived
          registered driver + actions run with the heap lock released). Its stated contract
          (`:13-14`, `:160`) is that actions must never run user VM code on the driver thread, and
          its escape hatch is "spawn a worker" — exactly where the pool slots in.

        Only once that lands does `cue(:every)` become a timer entry that enqueues onto the pool
        (skipping a tick while the previous iteration still runs). Today an `:every` cue owns a
        thread for its whole lifetime (`scheduler.rs:286` → `scheduler_run_every_loop`,
        `:415-446`), which is what the 16.4 GB / 52-thread row above measures; `:in`/`:at` delays
        already moved onto the deadline heap in #4638. Moving `:every` onto the timer *without* a
        pool would be a regression: every iteration runs user VM code, so the heap would have to
        spawn a fresh 256 MiB worker plus a `clone_for_thread` per tick.
  - [ ] Watch CI for the residual under-load syntax.t flake (1 notok in 18 loaded runs locally,
        unreproduced in 14 follow-ups; raku's own fixed-sleep tests also wobble at that load).
- [ ] **Remainder of true sharing for state/lexical aggregates**: only the lost-update on
      high-contention concurrent "structural" inserts (real rakudo crashes with a MoarVM oops on the
      same shape = outside the language guarantee. mutsu doesn't break, which is an advantage — keep
      as out-of-spec = effectively a decision not to do it). The cell-ification itself was completed
      in Track B slices 2+3 and T6 (news/2026-07.md; pin = the 18 tests in
      t/state-aggregate-shared-cell.t).
- [ ] Semaphore / nonblocking await / lock contention (S17; hard; separate axis).
- [ ] **★Recursion through a `start` block silently returns the wrong answer** (re-characterized
      2026-07-17 on release main 159a30cb0; this entry previously claimed "running a recursive
      start/await sub twice in a row *hangs the second one*", which understates it — the hang is a
      later symptom, the first one is silent corruption):

      ```raku
      sub f($n) { start { $n <= 0 ?? "b" !! await(f($n-1)) ~ "|$n" } }
      say await f(3);   # mutsu: b|1|2|3  (raku: same)
      say await f(3);   # mutsu: b|3|3|3  -- WRONG, no error   (raku: b|1|2|3)
      say await f(2);   # mutsu: b|2|2    -- WRONG             (raku: b|1|2)

      sub k($n) { $n <= 0 ?? "b" !! (await start { k($n-1) }) ~ "|$n" }
      say k(3);         # mutsu: b|1|1|1  -- WRONG ON THE FIRST CALL (raku: b|1|2|3)
      ```

      Every frame reads a single `$n` (collapsing to the innermost value for `k`, the outermost for
      `f`'s second call). A two-branch `fib` (`await(fib($n-2)) + await(fib($n-1))` inside `start`)
      then hangs deterministically — that is the previously-recorded symptom. Plain recursion
      without `start`, and non-recursive `start`, are both correct.

      **Root cause**: `clone_for_thread` seeds `shared_vars` — a flat **name-keyed**
      `HashMap<String, Value>` keyed by the bare name (`runtime_thread.rs:18-53`) — so it cannot
      represent **two concurrently-live bindings of the same name**, which is exactly what a
      recursive frame chain is. It is *not* a naive name collision: sequential same-name frames are
      fine (`sub a($x)` / `sub b($x)` interleaved, and `p(1); p(2); p(3)`, all match raku) — the
      corruption needs same-name frames simultaneously live across a thread boundary.

      Routing the value through a `my` instead of a parameter is **worse, not better**:
      `sub g($m) { my $n = $m; start { $n <= 0 ?? "b" !! await(g($n-1)) ~ "|$n" } }` gives
      `b|0|0|0` on the *first* call. `thread_redeclared_vars` (the `my` mask) is interpreter-global
      and cleared at every spawn (`runtime_thread.rs:76`), so a recursive chain force-`insert`s the
      innermost `n` and unmasks the parent — it is the same name-keyed disease, not a lever.

      **Mechanism** (traced 2026-07-17). Each recursive frame *does* get a correct distinct slot and
      env; the flat map and the writeback that feeds it back into slots are what corrupt them:
      - **write**: every plain lexical store runs `flush_local_to_env` (`vm_env_helpers.rs:963`) →
        `set_env_plain_lexical` → `set_shared_var_sym` (`runtime_shared_vars.rs:306-370`), so each
        frame's `$n` bind overwrites `shared_vars["n"]` and marks it dirty.
      - **read-back**: `await` calls `sync_shared_vars_to_env` (`runtime_shared_vars.rs:402-486`),
        which pulls every dirty key into `env` *and* queues it in `pending_caller_var_writeback`;
        `apply_pending_caller_var_writeback` (`vm_env_helpers.rs:1055-1074`) is retain-on-miss and
        deliberately **walks up the frame chain** until some frame owns a slot named `"n"`.
        Innermost write → every outer frame's slot. `k`/`g` are the `insert`-overwrite direction;
        `f`'s 2nd call is the `or_insert_with` direction (the stale `3` from run 1 survives).

      **Fix direction — narrower than it first looked; NOT the full §1.3 campaign.** `start` already
      compiles its block argument as escaping (`compiler/expr_call.rs`, `escaping_args = name ==
      "start"`), so `box_captured_lexicals` (`vm/vm_register_ops.rs:570-695`) already gives it
      correct per-binding cells, and `$n` (read-only in the block) is correctly captured by value
      per frame. `shared_vars`' plain-scalar lane runs **in parallel with a mechanism that already
      works, and overwrites its correct answer** — precisely the "1 operation = 1 implementation"
      violation. So the fix is to **retire the plain-scalar lane of `shared_vars`** and keep the
      `@`/`%`, `__mutsu_*` atomic, and `state` lanes:
      1. stop seeding bare-name scalar keys in `clone_for_thread` (`runtime_thread.rs:21-53`);
      2. drop the scalar mirror in `set_shared_var_sym` (its `sv.contains_key` guard may make this a
         no-op once seeding stops — verify);
      3. `sync_shared_vars_to_env` then never marks a scalar param dirty, so the frame-walking
         writeback stops force-feeding `"n"` into ancestor frames;
      4. `thread_redeclared_vars` becomes scalar-only dead code (set only at
         `vm_var_assign_set_local.rs:1723`) — delete it, retiring a *second* name-keyed mechanism.

      Realistic edit surface ~5 files (the 49 `shared_vars` accesses across 9 files are mostly the
      `@`/`%`/atomic/`state`/CAS lanes, which are untouched). **Pin these before touching the
      seeding** — they are the shapes `box_captured_lexicals` declines to box, i.e. where
      cross-thread scalar *mutation* visibility would regress: type-constrained scalars
      (`my Int $c = 0; await start { $c = 5 }`, skipped at `vm_register_ops.rs:650-656`), scalars
      holding `Instance`/`Proxy`/`Package` (`:669-679`), and the empty-`needs_cell_locals` early
      return (`:601-607`). If the typed case regresses, the smallest honest patch is to relax the
      `var_type_constraint` skip for the escaping-to-thread case, re-checking the constraint at the
      `ContainerRef` write-through chokepoint.

      Worth doing before the worker-pool ADR above: a silent wrong answer outranks a
      perf/footprint change, and the fix retires name-keyed mechanisms rather than building on them.
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

## 7. RakuAST — user-facing AST (new capability track)

Design and phasing are fixed in **[docs/adr/0011](docs/adr/0011-rakuast-model-layer-and-phasing.md)**.
RakuAST is a **reflection/model layer bidirectionally convertible with the internal `Expr`/`Stmt`
AST** — NOT a frontend rewrite. Near-zero roast payoff today (this is a new-capability direction),
so it is tracked separately from the roast backlog.

- [ ] **Phase 1 — introspection MVP**: `Value::RakuAst` + `RakuAstClass` enum + class-metadata table;
      `Q|...|.AST` on `Str` for the literal + say-call cluster; `.gist`/`.raku`/`.^name` renderer
      matching raku exactly. Tests in `t/rakuast-ast.t`.
- **Phase 2 (in progress)** — read-coverage expansion.
  - [x] Slice 1: Var::Lexical (all sigils), VarDeclaration::Simple + Initializer::Assign (plain
        `my $x [= expr]`), ApplyInfix/ApplyPrefix/ApplyPostfix + Infix/Prefix/Postfix. Tests in
        `t/rakuast-expr.t`.
  - [x] Slice 2: `=` assignment (`Assignment` node, `:item` for scalar targets) and method
        calls (`Call::Method` as an `ApplyPostfix`). Tests in `t/rakuast-calls-assign.t`.
  - [x] Slice 3: bare blocks (`Block`/`Blockoid`) and pointy blocks (`PointyBlock`/`Signature`/
        `Parameter`/`ParameterTarget::Var`, plain positional params only). Tests in
        `t/rakuast-blocks.t`. (PR #4684.)
  - [x] Slice 4: `if`/`if...else`, `while`, and bare `loop` (`Statement::If`, `Statement::Loop::
        While`, `Statement::Loop`), condition + `then`/`else`/`body` = `Block`. Tests in
        `t/rakuast-control.t`. (mutsu desugars `unless`→`if !` and `until`→`while !`, so those
        render with a negated condition — documented divergence, ADR-0011.)
  - [x] Slice 5: `elsif` chains — mutsu nests each `elsif` as a single `if` in the else-branch;
        flattened into raku's `elsifs` list (`Statement::Elsif`) with any trailing `else` block.
        Tests in `t/rakuast-elsif.t`.
  - [x] Slice 6: implicit-topic `for` (`Statement::For` with topic-taking `Block` marked
        `implicit-topic`/`required-topic`). Tests in `t/rakuast-for.t`. (`with`/`without` are
        desugared by mutsu into temp-var + `.defined` + `if`, so no `Statement::With` node to
        recover — deferred, documented in ADR-0011.)
  - [x] Slice 7: named sub declarations (`RakuAST::Sub`, reusing `Signature`/`Parameter`; sub
        params carry an implicit `type => RakuAST::Type::Setting(Any)`). Tests in `t/rakuast-sub.t`.
  - [x] Slice 8: C-style loops (`Statement::Loop` with setup/condition/increment) and `repeat`
        loops (`Statement::Loop::RepeatWhile`). Tests in `t/rakuast-loop.t`.
  - [x] Slice 9: bare comma lists (`ApplyListInfix`) and `:=` binding (`ApplyInfix` over a plain
        `Infix(":=")`). Tests in `t/rakuast-listinfix-bind.t`. (Parenthesised lists and compound
        `+=` are desugared/dropped by mutsu — deferred, documented in ADR-0011.)
  - [x] Slice 10: scoped/typed declarations — `my`/`our`/`state` with an optional simple type
        (`VarDeclaration::Simple` gains `scope` + `type => Type::Simple`). Tests in
        `t/rakuast-vardecl-scoped.t`.
  - [x] Slice 11: anonymous parameter-less `sub { }` (`RakuAST::Sub` with no name). Tests in
        `t/rakuast-anon-sub.t`. (`sub ($x)` shares `AnonSubParams` with pointy blocks, still
        deferred.)
  - [x] Slice 12: explicit-signature `for @a -> $x` (body becomes a `PointyBlock` carrying the
        signature). Tests in `t/rakuast-for-signature.t`.
  - [x] Slice 13: class and method declarations (`RakuAST::Class`, `RakuAST::Method`; class body is
        an ordinary `Block`, methods reuse the `Sub` routine helper). Tests in `t/rakuast-class.t`.
        (Attributes `has $.x`, inheritance, roles/grammars deferred.)
  - [x] Slice 14: class attributes `has [Type] $.x` (`VarDeclaration::Simple` with `scope => "has"`
        and a `twigil`). Tests in `t/rakuast-attribute.t`. (Explicit defaults → `Trait::WillBuild`,
        deferred.)
  - [x] Slice 15: method-call modifiers `.?`/`.+`/`.*` (`Call::Method` gains a `dispatch` field).
        Tests in `t/rakuast-method-modifier.t`.
  - [x] Slice 16: role declarations (`RakuAST::Role` with a distinct `RoleBody`). Tests in
        `t/rakuast-role.t`. (Grammars are `ClassDecl`+`parents=["Grammar"]` in mutsu — no distinct
        node — so they defer.)
  - [x] Slice 17: loop labels (`LABEL: while/for/loop` → `labels => (Label(...),)`). Tests in
        `t/rakuast-loop-label.t`. (Labelled `repeat`/C-style loops use a separate `Stmt::Label`
        node, deferred.)
  - [x] Slice 18: given / when / default (`Statement::Given`/`When`/`Default`). Tests in
        `t/rakuast-given-when.t`.
  - [x] Slice 19: the ternary `?? !!` operator (`RakuAST::Ternary`). Tests in `t/rakuast-ternary.t`.
  - [x] Slice 20: definite types `Int:D`/`Int:U` (`Type::Definedness` over a `Type::Simple` base).
        Tests in `t/rakuast-type-definite.t`.
  - [x] Slice 21: parameterised types `Array[Int]`/`Hash[Str, Int]` (`Type::Parameterized` with an
        `ArgList` of type args). Tests in `t/rakuast-type-parameterized.t`.
  - [x] Slice 22: positional subscripts `@x[EXPR]` (`Postcircumfix::ArrayIndex` + `SemiList`). Tests
        in `t/rakuast-subscript.t`. (Associative `%h{...}`/`%h<...>` deferred — indistinguishable.)
  - [x] Slice 23: quoted method names `$x."foo"()` (`Call::QuotedMethod`). Tests in
        `t/rakuast-quoted-method.t`.
  - [x] Slice 24: list-associative infixes `andthen`/`orelse`/`notandthen` (flat `ApplyListInfix`).
        Tests in `t/rakuast-list-infix.t`.
  - [x] Slice 25: reduction metaoperator `[+]`/`[\+]` (`Term::Reduce`). Tests in
        `t/rakuast-reduction.t`.
  - [x] Slice 26: hyper method calls `@a>>.method` (`MetaPostfix::Hyper`). Tests in
        `t/rakuast-hyper-method.t`.
  - [x] Slice 27: attribute build-time defaults `has $.x = 5` (`Trait::WillBuild` + initializer).
        Tests in `t/rakuast-attribute-default.t`.
  - [x] Slice 28: labelled `repeat` loops (`Stmt::Label`-wrapped). Tests in
        `t/rakuast-labelled-repeat.t`.
  - [x] Slice 29: coercion types `Int()` (`Type::Coercion`). Tests in `t/rakuast-type-coercion.t`.
  - Phase 2 read-coverage is complete (slices 1–29); user chose the Phase 3 pivot (2026-07-18).
    Remaining Phase 2 constructs (`.=`, hyper `<<.m`/`>>+<<`, signature return types) carry mutsu
    desugar divergences and are deferred.
- **Phase 3 (in progress)** — `RakuAST::*` type-object registry + introspection dispatch.
  - [x] Slice 1: node accessors — `.condition`/`.expression`/`.args`/… return field values, and
        `.statements` returns a `StatementList`'s children as a `List`, so the tree is walkable.
        Tests in `t/rakuast-accessors.t`.
  - [x] Slice 2: `~~ RakuAST::Node` / `.isa` hierarchy — base `RakuAST::Node` + `::`-namespace
        ancestors (`Statement::If isa RakuAST::Statement`). Tests in `t/rakuast-smartmatch.t`.
        (`use experimental :rakuast` already parsed as a no-op.)
  - [x] Slice 3: positional-leaf accessors (`IntLiteral.value`, `Var::Lexical.name`). Tests in
        `t/rakuast-leaf-accessors.t`.
  - [x] Slice 4: semantic Expression/Term hierarchy (`IntLiteral isa RakuAST::Expression`/`Term`,
        `ApplyInfix isa Expression`). Tests in `t/rakuast-semantic-hierarchy.t`.
  - [ ] Slice 5+: registering `RakuAST::*` as first-class type objects; `.WHAT`.
- **Phase 4 (in progress)** — construction (`.new`).
  - [x] Slice 1: literal constructors (`RakuAST::IntLiteral.new(42)`, `StrLiteral`, `RatLiteral`).
        Tests in `t/rakuast-construct.t`.
  - [x] Slice 2: `RakuAST::Name.from-identifier("x")`. Tests in `t/rakuast-construct-name.t`.
  - [ ] Slice 3+: multi-field constructors (`ApplyInfix.new(:left, :infix, :right)`,
        `Statement::Expression.new(:expression)`, `StatementList.new(...)`) — these take **named**
        args (Pairs) per a per-class field schema; then Phase 5 (EVAL: lower RakuAST → internal AST →
        existing compiler).
- [ ] **Phase 4** — construction (`.new`, `.from-identifier`, …).
- [ ] **Phase 5** — EVAL: lower RakuAST → internal AST → existing compiler (no new engine).
- [ ] **Phase 6** — macros / `quasi` / unquoting (built on 4+5; may defer indefinitely).

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
