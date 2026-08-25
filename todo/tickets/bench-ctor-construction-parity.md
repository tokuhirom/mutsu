# bench-ctor: close the remaining gap vs rakudo (construction residual costs)

bench-ctor is the only benchmark where mutsu is slower than raku on the bench CI
(ratio 1.17-1.35x across 2026-07-30 rows; every other bench is at or below
parity). The bench is the Zef::Distribution shape: a 20-attribute class with a
parent, `method new(*%_)` delegating to `self.bless(|%_, :meta(%_))`, and TWEAK
submethods at two MRO levels. History: 35x → ~3x (#4557/#4558/#4561) → ~1.3x
today.

## Where the time goes (measured 2026-07-30)

A perf profile (profiling build, 40k iterations) is completely flat — no
dominant function. The cost is the sum of many small per-construction heap and
hash operations: malloc/free family ~17%, NaN-box payload/arc/gc ops ~16%,
`Gc::drop` + cycle-collector `buffer_candidate` ~7%, hashbrown ~5%,
thread-local access ~5%.

Wall-clock A/B with bench variants (min of 5, startup subtracted, per
construction, local machine):

| variant                          | mutsu  | raku   |
|----------------------------------|--------|--------|
| baseline (new + bless + TWEAK x2)| 58.6us | 61.8us |
| TWEAK submethods removed         | 28.4us | 51.2us |
| plain default ctor only          | 13.0us | 50.2us |

So the plain constructor is already ~4x faster than raku; the entire deficit
lives in the phases layered on top:

1. **TWEAK phase: ~30us/construction — half the total.**
   `run_tweak_phase` (src/runtime/methods_dispatch_new.rs) re-derives
   everything on every construction: full MRO walk with per-level registry
   probes, `ordered_role_submethods_for_class`, language-revision metadata
   string building, `cell.to_map()` probe materialization (21 attrs), and then
   `run_instance_method_celled` does a full uncached
   `resolve_method_with_owner_invocant` (multi-candidate matching) per TWEAK
   call. `MUTSU_VM_STATS` confirms 2 resolver-path dispatches per construction
   (TWEAK=10000 for 5000 iterations). None of this is cached, although it is a
   pure function of the class shape — the same shape data that
   `NativeCtorPlan` already memoizes for the default-ctor path.
2. **Custom `new` -> `bless` plumbing: ~15us/construction.**
   `*%_` slurpy -> `|%_, :meta(%_)` -> bless rebuilds pairs/hashes each time
   (MakePair x7 + MakeHash + MakeSlip + MakeRealArray per construction), plus
   3 O(env) `env_deep_copies` per construction (dual-store `cow_mut` — the
   known Slice-F debt) on the dispatch frames.
3. **GC candidate buffer churn:** candidate_pushes=105670,
   dedup_hits=1384390 for 5000 constructions — ~280 dedup hits per object.
   Container temporaries (the `%_` hash, the `:meta` copy, @/% attribute
   defaults, dispatch frames) hammer the cycle-collector buffer thread-local.

Rakudo, by contrast, compiles construction to a BUILDPLAN over flat P6opaque
slots, passes named args as a capture without rebuilding hashes, and spesh
specializes the whole new -> bless -> BUILDALL chain.

## Slices

- [x] **S1: cache a per-class construction phase plan.** Extend
  `NativeCtorPlan` (or add a sibling cache with the same invalidation sites)
  with the ordered, already-resolved BUILD/TWEAK submethod list: (owner class,
  role origin, method def Arc, 6.c/6.e skip decisions). `run_build_phase` /
  `run_tweak_phase` then iterate the cached list instead of re-walking
  MRO/registry/metadata, and skip `resolve_method_with_owner_invocant` when
  the plan already pins the candidate (single non-multi TWEAK is the common
  case). Also avoid the unconditional `cell.to_map()` probe when no alias
  refresh is needed and the resolved candidate doesn't consume it.
  Expected: removes most of the 30us/construction TWEAK overhead.
- [ ] **S2: trim the custom-new -> bless argument plumbing** — avoid
  rebuilding the named-arg hash/pair vector twice (`%_` slurpy then
  `:meta(%_)` slip flatten), and chase the 3 env_deep_copies/construction on
  this path. **2026-08-14 investigation (this session) root-caused the
  env_deep_copies split — see "S2/S3 investigation" below. No safe narrow fix
  found; the real cost is gated behind the closure-upvalue-cell prerequisite
  in `docs/vm-single-store.md` §3, not something to chase as an independent
  bench-ctor slice.** (Note: the doc formerly referenced here as "the Slice F
  campaign, PLAN §6" no longer exists under that name — PLAN.md §6 is now "QA
  & finalization"; the live tracking doc for this exact mechanism is
  `docs/vm-single-store.md`.)
- [x] **S3: GC candidate churn** — ~280 dedup hits per construction is pure
  overhead; investigate suppressing candidate buffering for
  freshly-constructed containers whose refcount never exceeded 1, or batching
  the thread-local lookups. **2026-08-14: both suggested directions are
  already implemented — see "S2/S3 investigation" below. 2026-08-25 (round
  3): closed — `MUTSU_GC=off` release A/B shows ~zero wall-clock delta on
  this bench; see the round-3 update.**
- [ ] **S4 (ADR territory, long term):** flat attribute slots instead of the
  per-instance hash map (the ADR-0016 "span + shared subject" analog for
  objects). Only if S1-S3 leave a measurable gap. **2026-08-25 (round 3):
  reassessed — see below. Flat slots alone buy only ~5-10% on this bench
  (AttrMap is already `FxHashMap<Symbol, Value>`, no string keys); the real
  long-term lever is a construction-pipeline campaign (flat slots + a
  compiled BUILDALL that skips per-phase full-path dispatch + capture-style
  named args). Blast radius measured: 88 files / ~800 `to_map()`/`as_map()`
  sites.**
- [x] **S6 (NEW, 2026-08-25 — the actionable residual): extend the
  compiled-method fast path to simple named/attributive parameter shapes.**
  **Implemented 2026-08-25 (round 4, see below): −15.9% instructions /
  −21% wall clock on this bench; the `fast_method_cache` tier and the
  TWEAK(Dist) closure-body residual remain follow-ups.**
  Instruction-count decomposition (see round 3 below) shows the two
  near-empty TWEAK submethods cost ~240k instructions/construction — 51% of
  the bench — and virtually all of it is full-path dispatch overhead, not
  body work. They can never take `call_compiled_method_fast` today because
  `has_complex_params` returns true for ANY named (`pd.named`) or attributive
  (`:$!x`) parameter, and `new(*%_)` is excluded by its slurpy. A fast-path
  extension that handles (a) named-scalar/array/hash params matched by Pair
  key, (b) attributive params via the single-key cell write S2-partial
  already built for the full path, and (c) the implicit `*%_` slurpy (built
  only when the body references `%_`, which `cc` knows), with conservative
  bail-to-full-path gates for defaults/constraints/aliases/rw, would remove
  most of that overhead. Caveats found in round 3: TWEAK(Dist) has
  `has_calls` (the `.map`) so it cannot `can_skip_merge` — the win there is
  the env-setup/param-bind half, not the merge; and this is param-binding
  surgery with many semantic edges (`:name(:$alias)`, boolean shorthand,
  type coercion), so it needs its own focused session with the binding roast
  set (`S06-signature/named-*`, `S12-construction/*`) as the gate, not a
  drive-by.
- [x] **S5 (deep, general):** closure env capture was O(program symbols) per
  lambda creation — `*.flat` in TWEAK cost ~10% of this bench, and every
  hot-loop `.map({...})` paid it. Fixed by #5571 (`Env::filtered_flat` never
  walks/materializes the shared `GLOBAL_BASE` tier); see
  `news/2026-08/closure-env-capture-cost-resolved.md`. Re-profiled
  2026-08-14: closure capture is now ~2-3% of this bench, not the dominant
  cost.

Progress: S1 landed (#5569) — resolver-path TWEAK dispatches 2/construction
-> 0, phase re-derivation gone, probe skeleton replaces the per-construction
`to_map()`. S2 (partial) — `bind_param_value` attributive-param whole-map
`to_map()`+`commit_attrs` round-trip replaced with a single-key cell write
(3 whole-map clones/construction removed), `is Type` container-trait lookup
plan-cached, `run_resolved_method_celled` borrows the def instead of cloning
per call. S5 landed (#5571, see above). A 2026-08-14 re-profile (40k
iterations, profiling build) shows no single dominant function anymore: the
remaining cost is spread across malloc/free (~10%), string formatting
(~5-8%), hashing (~2-3%), and NaN-box payload/GC bookkeeping — S2 (arg
plumbing) and S3 (GC candidate churn) territory.

## S2/S3 investigation (2026-08-14, this session)

Picked between S2's remainder and S3 by investigating both before writing any
code, per the "measure before assuming" rule. Debug-build `MUTSU_VM_STATS=1`
on current `main` (`benchmarks/bench-ctor.raku`, 5000 iterations) reproduces
numbers close to the ticket's last snapshot:

```
dual-store: clone_env=15000 env_deep_copies=15002 env_flushes=0 slots_flushed=0
gc: candidate_pushes=103124 dedup_hits=1201934
```

i.e. 3.0 env_deep_copies/construction (unchanged from the last measurement)
and ~20.6 candidate_pushes / ~240 dedup_hits per construction (down slightly
from 21.1 / 277, likely incidental drift from unrelated PRs since 2026-08-14).

**env_deep_copies root cause (S2).** Used `rust-gdb -batch` breakpoints on
`Env::cow_mut` (`src/env.rs:602`) rather than guessing — see CLAUDE.md's
debugging guidance. Per construction, exactly 3 call sites hit `cow_mut`, all
via `call_compiled_method`'s `env_mut().insert("?CLASS", ...)`
(`src/vm/vm_method_dispatch.rs:280`), for the 3 method dispatches
`new()` -> `TWEAK(Spec)` -> `TWEAK(Dist)`. The dispatching function installs a
scoped-overlay env (Slice 6, `docs/vm-dual-store.md`/`docs/vm-single-store.md`
§3) *unless* the callee's compiled body contains a nested closure
(`cc.closure_compiled_codes.is_empty()`, `vm_method_dispatch.rs:254`) — gdb
confirmed `closure_compiled_codes` is empty for `new()` and `TWEAK(Spec)`, but
has exactly one entry for `TWEAK(Dist)` (the `*.flat` WhateverCode inside
`@!resources.map(*.flat)`).

- `new()` and `TWEAK(Spec)`: scoped-overlay path taken -> the "deep copy"
  `cow_mut` performs is of the fresh, still-**empty** overlay map (shared with
  a process-wide singleton, so *any* first write reads `strong_count > 1` and
  trips the counter) — an O(1) clone of an empty `FxHashMap`, not the O(env)
  cost the counter name implies. 2 of the 3 counted "deep copies" are
  therefore near-free.
- `TWEAK(Dist)`: the closure disables the scoped-overlay optimization, so
  `env_mut()` operates directly on the live, Arc-shared env. Confirmed with
  gdb (`print self.inner` at the `cow_mut` breakpoint): the map being cloned
  has **39 entries** (`strong=2`) — a genuine O(env) `HashMap` clone + rehash
  on every single construction. This is the one real cost in the 3.

**Why no fix landed this session.** The gate exists precisely because "no
closure/thread body runs under [the overlay]" (Slice 6, #2650) — extending it
requires the same closure-upvalue-cell rework that
`docs/vm-single-store.md` §3 explicitly defers as the campaign's
**"highest-blast-radius change"**, sequenced *after* R1/R2 land. Considered
and rejected a narrower alternative — reusing the already-computed
`closure_escapes[i]` static flag (true only for a closure in a
stored/returned/bound position; `*.flat` here is an immediately-invoked call
argument, so it would read `false`) to re-enable the overlay when no nested
closure escapes the frame. Rejected because `closure_escapes` was built and
vetted for a *different* consumer (deciding which mutated captured locals
need a `ContainerRef` cell), and this codebase has a documented history of
exactly this failure mode: extending a closure-escape classifier past its
vetted case previously caused a real bind-alias regression (see the
`trap-closure-escape-detection-positional-vs-named` lesson). Reusing it here
without a dedicated audit of every escape-position case risks a subtle,
CI-invisible correctness bug rather than a clean red build — the class of
risk this ticket's own instructions call out as *not* worth forcing through.
**Conclusion: S2's env_deep_copies remainder stays open, but is not an
independently-actionable bench-ctor slice — it is a downstream consequence of
the deferred closure-upvalue-cell prerequisite. Revisit once that prerequisite
lands, not before.**

**GC candidate churn (S3) — both proposed directions already implemented.**
Read `src/gc/gc_ptr.rs`'s `Gc::drop`/`buffer_candidate` (and
`docs/adr/0001-gc-strategy-and-phasing.md` for the Bacon-Rajan invariants
before drawing conclusions):

- *"Suppress candidate buffering for freshly-constructed containers whose
  refcount never exceeded 1"* — already true. `buffer_candidate` is called
  only in the `prev > 1` arm of `Gc::drop`; a node whose refcount never rose
  above 1 takes the `else` arm (`finalize()` only, never buffered). A
  candidate is offered *only* for nodes that genuinely had more than one live
  handle at some point — exactly the class that could be part of a cycle.
- *"Batch/cheapen the thread-local lookups"* — already true for the
  `dedup_hits` counted here. The code comment on `Gc::drop` documents that
  this was the actual historical overhead (+30%/+61% on bench-fib/bench-class
  from the old "Arc clone + unsize + swap + drop" round-trip per
  already-buffered node) and that it was fixed: a `dedup_hit` today is a
  single `Relaxed` atomic load on `header.buffered`, not a round-trip.
  `dedup_hits=1201934` for this bench is therefore ~240 cheap loads per
  construction, not "pure overhead" in the sense the ticket's earlier
  phrasing implied.

The real remaining cost is `candidate_pushes` (~20.6/construction), each a
genuine `Arc` clone + push into the candidate buffer for a node that *did*
have `strong > 1` at drop time — i.e. a temporary that was legitimately
shared for part of its life (a saved dispatch-frame env copy, an attribute
cell referenced by both the instance and a local, etc.). Suppressing these
further without a specific per-shape proof that no cycle is reachable would
risk violating Bacon-Rajan soundness (a missed real cycle leaks memory
permanently) — precisely the GC-soundness risk this ticket's own instructions
flag as needing "real understanding," not a drive-by change.
**Conclusion: S3 as literally specified is done; no further safe action
without a GC-level redesign (out of scope here).**

No code change landed from this investigation — both leads converge on either
an already-completed optimization or a deliberately-deferred, high-blast-
radius architectural prerequisite. Landing this write-up (rather than forcing
either change through) follows this ticket's own guidance.

## Update (2026-08-25, round 3 — independent re-examination of the round-2 conclusions)

A fresh session re-derived rounds 1-2 from scratch rather than trusting them.
Verdict up front: **round 2's "no safe independent fix" conclusion stands, and
is now stronger** — but the *reason* for the S2 dead end is different from the
one round 2 gave, and the decomposition surfaced a new actionable slice (S6
above) that rounds 1-2 missed.

**1. The closure-upvalue-cell prerequisite has NOT landed.**
`docs/vm-single-store.md` §3 (indexed upvalue cells) is untouched since
2026-07-15 and no commit on `main` since round 2 implements it. What DID land
nearby — ADR-0025 (captured scalar cells, slices 1+2), ADR-0032 (WrapVarRef
container capture on every closure kind), the `GetUpvalue` read-only-scalar
rewrite — is capture-edge machinery, not the §3 rework; the overlay gate at
`vm_method_dispatch.rs` (`cc.closure_compiled_codes.is_empty()`) is unchanged.

**2. S2 via overlay-gate relaxation is a dead end for a REASON ROUND 2 DID NOT
STATE: even a fully correct relaxation buys ~nothing.** Round 2 rejected
reusing `closure_escapes[i]` on classifier-vetting grounds. Round 3 examined
the narrower purpose-built-gate idea and found it moot on cost grounds alone:
closure *capture* is already scoped-env-safe (`capture_closure_env` walks env
tiers via `filtered_flat` with shadow/tombstone handling, and closure dispatch
itself installs its own `scoped_child` unconditionally), so a relaxed gate
would likely be *correct* — but TWEAK(Dist)'s body calls `.map`, and any
non-accessor nested dispatch runs `flatten_scoped_env()`
(`vm_call_method_ops.rs`, placed after the accessor fast path), which
materializes parent+overlay into a fresh flat map. The O(env) clone the
overlay would save at `?CLASS`-insert time reappears as the flatten at the
`.map` — the cost *relocates*, it does not disappear. Any method body with a
closure AND a genuine nested dispatch (the only shape that hits this gate in
practice) pays the clone either way. The env_deep_copies residual is
therefore structurally tied to §3 (upvalue cells make the flatten
unnecessary), confirming round 2's sequencing conclusion via a different
route.

**3. S3 re-verified independently, now with a release wall-clock A/B: GC
contributes ~zero to this bench.** `MUTSU_GC=off` vs default on the fresh
release build: 0.26-0.29s vs 0.25-0.28s — indistinguishable from noise.
`Gc::drop` re-read: candidate buffering fires only on `prev > 1`, dedup is a
single relaxed load — as round 2 said. New detail: all 5 collections on this
bench reclaim **zero** nodes (the candidates are short-lived temporaries whose
Weak buffer entries fail to upgrade at drain time), and the ADR-0003 adaptive
threshold correctly stays at BASE because `2 x revived < 16384`. Release
`pause_ns_total` is ~10ms across the whole run. A "provably acyclic by
construction" type-level suppression is not expressible here — every Gc'd
container kind can hold arbitrary Values — and would buy nothing measurable
anyway. S3 is closed.

**4. Fresh measurements (2026-08-25, release build == origin/main).**
- Bench CI (last 30 rows of `bench-history.tsv`): ratio is **bimodal** —
  fast-mode rows ~1.02-1.13, slow-mode rows ~1.30-1.46, median ~1.09. The
  ticket's "1.17-1.35" framing undersells the variance; single rows are not
  meaningful, compare distributions.
- Debug `MUTSU_VM_STATS` replicates round 2 exactly (env_deep_copies=15002,
  candidate_pushes≈20.6/ctor, dedup≈220/ctor).
- **The local raku moved: v2026.06** (bench CI pins Rakudo v2022.12). Against
  modern rakudo, startup-subtracted per-construction cost is mutsu ~50us vs
  raku ~9us on a 40k-iteration run — a **~5.6x** real gap that the CI ratio
  (~1.1) masks via old-rakudo slowness plus startup amortization. The
  architectural headroom is much larger than the CI number suggests.
- **Instruction-count phase decomposition** (perf stat, core-pinned, ±0.07%,
  5000 ctors): baseline 474.7k insns/ctor = TWEAK phase 240.5k (**51%**) +
  custom-new→bless 122.4k (26%) + plain-ctor-and-loop-body 111.8k (24%).
  S1 cached the phase *plan*, but each TWEAK invocation still costs ~120k
  instructions of pure full-path dispatch overhead (frame + env setup +
  named/attributive param binding + merge) for a near-empty body — that is
  the S6 finding above, and it is the largest remaining slice by far.
- A 40k-iteration flat profile (2095 samples) confirms round 2's "no dominant
  function": NaN-box ops ~16%, malloc/free family ~16%, thread-locals ~5%,
  hashbrown ~6%, `Symbol::intern`+sip+memcmp ~4% (interning already has a
  thread-local memo; pre-interned statics for the fixed per-dispatch env keys
  would buy ~1% — not worth the churn alone), `cow_mut` ~1%. The earlier
  "string formatting 5-8%" bucket did not reappear as a distinct signal.

**Recommended order: S6 (fast-path named/attributive binding — a focused
session with the binding roast set as gate) first; the S4-class
construction-pipeline campaign (ADR) only after S6 lands and is measured, as
S6 may take the CI ratio to parity by itself. S2's env_deep_copies remainder
stays gated on `docs/vm-single-store.md` §3; S3 stays closed.**

## Update (2026-08-25, round 4 — S6 implemented)

S6 landed: the compiled-method fast path (`call_compiled_method` gate +
`call_compiled_method_fast` binding, src/vm/vm_method_dispatch.rs) now accepts
**simple named and attributive parameter shapes**, and calls carrying named
(string-keyed Pair) arguments. What the fast path binds directly:

- plain readonly scalar named params (`:$x`) — Pair args matched by key,
  rightmost wins, itemized via `itemize_plain_scalar_param` like the binder;
- attributive named params (`:$!x` / `:$.x` / `:@!a` / `:%!h`) — bound via the
  same single-key live-cell write `bind_param_value` performs (`@`-sigiled
  values re-homed through `normalize_positional_param_value`); an unsupplied
  one binds its type-object default into the attribute (rakudo semantics,
  verified against v2026.06 and pinned by `t/tweak-named-fast-path.t`);
- the implicit/explicit `*%_` slurpy — leftover named args, with
  `implicit_method_named_slurpy` fixed to strip sigil+twigil when computing
  consumed keys (an attributive `:$!x` now keeps caller key "x" out of `%_`,
  matching the slow path's `explicit_named_keys` and rakudo);
- `method new(*%_)`-shaped methods called with all-named args (the custom-new
  half of the bench) — no eligible-param change needed, only the arg-side gate.

Conservative bails to the full path (all pinned by the new test): named params
with traits / type constraints / where / sub-signature aliases (`:a(:$b)`) /
defaults-when-unsupplied / required-when-missing; non-attributive `@`/`%`/`&`
named params (container-alias semantics); non-string-keyed `ValuePair` args;
internal `__mutsu_*` pairs; a named container-variable arg feeding a plain
named scalar param (new `method_shares_container_into_named_scalar_param`,
mirroring the sub side — `$n.push` writeback stays correct).
`method_shares_container_into_scalar_param`'s positional alignment was also
made Pair-aware (it could misalign once Pair args reach the gate). The
`fast_method_cache` gate (vm_call_method_compiled_cache.rs) is deliberately
unchanged — extending the *cached* tier to named shapes is a follow-up.

One subtlety the full `make test` run caught: an attributive bind during the
BUILD phase must be recorded via `record_build_attr_write`, or the post-BUILD
initializer pass re-applies `has $.a = 5` over an *unpassed* `:$!a` bind — the
seed comparison alone cannot tell, because an untyped attribute's seed IS the
same `Any` type object the bind writes (t/build-attr-default-order.t test 8).
The slow path records this through its exit attr-local sync
(`write_attr_cell_by_key`); the fast path now records it at bind time.

**Measured (release, core-pinned `perf stat -e instructions`, 5000 ctors,
local box):**

| slice                     | before (insns/ctor) | after | delta |
|---------------------------|--------------------:|------:|------:|
| full bench                | 474.4k | 399.2k | −75.2k (−15.9%) |
| custom-new→bless plumbing | 233.8k (no-TWEAK variant) | 211.1k | −22.7k |
| TWEAK(Spec) (empty body)  | 59.5k | 34.1k | −43% |
| TWEAK(Dist) (`.map` body) | 181.1k | 154.0k | −15% |

Wall clock (min of 5, `taskset -c 3`): 0.28s → 0.22s whole run, ≈52µs →
≈40µs/ctor startup-subtracted (−21%). Confirm on bench CI after merge, per the
measurement notes below.

**Remaining headroom after S6:** TWEAK(Dist) still costs ~154k insns/ctor —
dominated not by param binding but by its body's closure creation (`*.flat`
MakeLambda per call), the non-scoped env path (its nested closure disables the
overlay → the one real O(env) deep copy per ctor survives, still 15002
env_deep_copies for the run), `flatten_scoped_env` on the nested `.map`
dispatch, and the exit merge/reconcile. That is the `docs/vm-single-store.md`
§3 territory round 3 already gated S2 on, plus the S4-class construction
pipeline. Next actionable, if wanted: extend the `fast_method_cache` tier to
the same named shapes, and look at per-call closure-literal reuse for
immediately-invoked WhateverCode.

## Measurement notes

- Iterate counters with the debug build (`MUTSU_VM_STATS=1`, identical to
  release); wall-clock with release. Document numbers come from the bench CI
  (`bench-history.tsv` on `bench-data`), not local runs.
- Local A/B on this machine flatters mutsu (P-core, large caches): local
  baseline already beats raku while the 4c CI runner shows 1.35x — the
  allocation-heavy construction path degrades more on the smaller runner, so
  always confirm on bench CI.
