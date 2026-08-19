# ADR-0035: Caller-frame observation from method bodies — chain-aware dynamics enumeration, plus `uses_callframe`-gated frame pushing at the two compiled-method chokepoints

- Status: Accepted (Slices 1-3 implemented; see "Implementation status" below)
- Date: 2026-08-19
- Origin: `todo/deep/method-calls-never-push-caller-frame.md` (reclassified from
  `todo/tickets/log-timeline-task-event-recording-empty.md`)

## 1. Context

Four mechanisms let a running body observe its *caller*:

- `CALLER::<$*x>` / `$CALLER::x` — read/write a caller frame's lexical,
- `callframe(N)` / `callframes()` — frame introspection (line, file, code),
- the `PROCESS::` pseudo-stash — process-level dynamics, resolved across the
  whole dynamic scope,
- the `DYNAMIC::` pseudo-stash — all `$*x`-style dynamics visible in the
  dynamic scope.

All four are backed by `src/runtime/runtime_caller_env.rs`: `push_caller_env`
(-`_with_code`) pushes the calling frame's env onto `self.caller_env_stack` and
a `CallFrameEntry` (file/line/code/env) onto `self.callframe_stack`;
`get_caller_var`, `get_caller_line`, and `dynamic_pseudo_stash_entries` walk
those stacks. `dynamic_pseudo_stash_entries` additionally chains
`self.env` (the current frame) after the stacked caller envs; the `PROCESS::`
stash read (`src/runtime/accessors_stash.rs`, `package_stash_value`) is built
from exactly that walk.

**Method calls never feed this machinery, on any dispatch path.** The three
repros from the ticket (all verified again on `main` @ 6e6f53569, against raku
as oracle):

```raku
class C { method reader() { say PROCESS::<$X> } }
PROCESS::<$X> = 42;
C.new.reader();           # raku: 42   mutsu: Nil

class C { method reader() { say CALLER::<$*y> } }
sub outer() { my $*y = 42; C.new.reader(); }
outer();                  # raku: 42   mutsu: Nil

class C { method reader() { say callframe(1).line } }
sub outer() { C.new.reader(); }   # <- this line
outer();                  # raku: the call-site line   mutsu: the method's own line
```

This blocks real bundled-battery code: `Log::Timeline`
(`modules/Log-Timeline/`) decides where to record by reading
`PROCESS::<$LOG-TIMELINE-OUTPUT>` from inside role-composed methods
(`t/logging.rakutest` tests 10-30 fail silently through the module's
no-op-when-unset fallback).

### 1.1 How the sub side actually works today (the load-bearing prerequisite)

The ticket left unresolved *which* mechanism makes the working sub-chain repro
work. Traced precisely, there are two independent mechanisms, and they split
cleanly along the read-kind boundary:

**(a) `CALLER::` / `callframe()` work via `uses_callframe` + the full path's
unconditional push.** `CompiledCode::uses_callframe` (`src/opcode.rs`, set
during `emit()`) latches on `OpCode::GetCallerVar`/`SetCallerVar`/
`BindCallerVar`/`GetCallerOuterVar` and on `CallFunc`/`CallFuncNamed` of
`callframe`/`callframes`. All three frameless sub paths exclude flag-bearing
bodies (`is_fast_call_eligible` / `is_light_call_eligible` /
`is_positional_light_call_eligible`, `src/vm/vm_call_eligibility.rs`), which
routes them to `call_compiled_function_named_inner`
(`src/vm/vm_call_named_inner.rs`), and *that* path calls `push_caller_env()`
unconditionally (line ~51), before installing its scoped-overlay env. So the
push is a property of the **callee's** dispatch: only the frame that *reads*
its caller needs a frame-pushing path. (Depth->2 `CALLER::CALLER::` chains
through frameless intermediates are approximated today — an accepted,
pre-existing limitation this ADR inherits rather than fixes.)

Note the stash-subscript spelling compiles to the same ops: `CALLER::<$*y>`
emits `GetCallerVar { depth: 1 }` (verified via `--dump-bytecode`), so method
bodies using it already carry `uses_callframe = true` — the flag is computed
correctly for methods by the shared compiler; it is simply **never read** by
any method-dispatch path.

**(b) `PROCESS::` / `DYNAMIC::` work via env *visibility*, not via frames.**
`PROCESS::<$X>` compiles to `GetPseudoStash` + `Index`. `GetPseudoStash`
does NOT set `uses_callframe`; instead it latches the process-global
`REFLECTIVE_NAME_ACCESS_SEEN` flag (`CompiledCode::scan_reflective_name_access`,
`src/opcode.rs`), and the 0-arg sub fast path
(`src/vm/vm_call_fast.rs`, `use_scoped = has_locals && !cf.has_inner_subs &&
!reflective_name_access_possible()`) then **skips the scoped-overlay install
entirely**, running the body directly in the live caller env. Since
`dynamic_pseudo_stash_entries` chains `self.env`, the mainline's `*X` entry is
visible. The full named path works differently but equivalently: its
unconditional `push_caller_env()` puts the (flat) caller env on the stack.
The `vm_call_light.rs` "skip push_caller_env for speed" comment (~line 237)
is therefore *not* load-bearing for the working repro — the 0-arg repro subs
take the fast path, whose overlay skip is what compensates.

### 1.2 The bug is broader than methods: `Env::iter()` does not agree with `Env::get()`

`Env::iter()` (`src/env.rs`) iterates **only the top overlay tier**
(`self.inner`), while `Env::get()` traverses the whole parent chain (with
tombstones). `dynamic_pseudo_stash_entries` enumerates each frame env with
`iter()`. Any frame that (i) runs under a scoped overlay and (ii) did not push
a caller-env entry is therefore invisible-through: dynamics living in its
parent tiers are unreachable from the stash walk.

That predicts — and testing confirms — two **latent sub-side gaps** beyond the
method repros, plus one shape that unexpectedly works:

```raku
# BROKEN (mutsu Nil / raku 42): a sub WITH a positional param takes the
# positional-light path — scoped overlay, no push:
sub reader($n) { say PROCESS::<$X> }
PROCESS::<$X> = 42; reader(1);

# BROKEN (mutsu Nil / raku 42): frameless overlay intermediate breaks the
# chain even for a reader shape that works when called directly:
sub reader() { say PROCESS::<$X> }
sub mid($n) { reader() }
PROCESS::<$X> = 42; mid(1);

# WORKS (42): a method WITH an inner closure — because both method executors
# gate their overlay on cc.closure_compiled_codes.is_empty(), a
# closure-bearing method body runs in the live caller env:
class C { method reader() { my $f = { 1 }; say PROCESS::<$X> } }
PROCESS::<$X> = 42; C.new.reader();
```

The last probe is the smoking gun: the method-side `PROCESS::` failure is
purely an *env-visibility* problem (overlay + top-tier-only enumeration), not
a frame-stack problem. Only `CALLER::`/`callframe()` genuinely need the frame
stack.

### 1.3 Method dispatch has exactly two compiled-execution chokepoints

The five files the ticket lists (`vm_call_method_compiled_cache.rs`,
`_interpret.rs`, `_mut.rs`, `vm_method_dispatch.rs`, `vm_dispatch_helpers.rs`)
are resolution/routing layers. Every compiled user-method body — plain,
multi, private, submethod, wrapped, `callsame`-redispatched
(`runtime/builtins_dispatch_next.rs`), and the interpreter-bridge slow path
(`runtime/class_dispatch.rs`, `run_resolved_method_celled`) — ultimately
executes through **`call_compiled_method`** or **`call_compiled_method_fast`**
(`src/vm/vm_method_dispatch.rs`). This is an established invariant: `monitor`
serialization already keys on exactly these two functions
(`class_dispatch.rs` comment, "the two compiled-execution chokepoints").
Neither pushes a caller-env frame; both install a scoped overlay (gated only
on `cc.closure_compiled_codes.is_empty()`, *not* on the reflective latch —
the second asymmetry vs. the sub fast path).

The only executor outside the chokepoints is the tree-walk residue for
methods with no `compiled_code` (`run_resolved_method_compiled_or_treewalk`'s
tree-walk arm), which is rare (`populate_uncompiled_method` compiles on
demand) and covered by Slice 3.

## 2. Decision

Two orthogonal mechanisms, matching the two read kinds:

### 2.1 Mechanism 1 — dynamics enumeration follows `get` semantics (fixes `PROCESS::`/`DYNAMIC::` everywhere)

Make `dynamic_pseudo_stash_entries` enumerate each frame env **through its
overlay parent chain** — outermost tier first, nearer tiers overwriting,
tombstoned keys suppressed — i.e. make whole-env enumeration observe exactly
the same entries single-key `Env::get()` observes. Implemented as a
chain-aware iteration helper in `src/env.rs` (the tier walk already exists in
`Env::flattened()`; the helper either reuses it or visits tiers into the
destination map without materializing an intermediate env). Applied to every
env the walk touches: each `caller_env_stack` entry *and* `self.env`.

This is a pure coherence fix — `iter()` disagreeing with `get()` *is* the
bug — and it fixes the method `PROCESS::` repro, both latent sub-side gaps
(§1.2), and the closure-bearing-method shape uniformly, with **zero dispatch
changes and zero hot-path cost** (the walk only runs inside a
`PROCESS::`/`DYNAMIC::` stash read). It needs no frame push because in every
overlay-without-push shape, `self.env`'s parent chain reaches the caller's
env by construction; frames that *replace* env instead of overlaying it
(closure dispatch with a captured env, `vm_closure_dispatch.rs`) already push
a caller-env entry, so the stacked-entry side of the walk covers them.

`GetPseudoStash` is deliberately **not** added to `uses_callframe` detection:
dynamics are name resolution through env chains, not frame observation, and
routing every `PROCESS::`-reading sub off the fast paths would tax working
code for no correctness gain. (The ticket's experiment already showed
detection-side extension alone is inert; with Mechanism 1 it is also
unnecessary.)

### 2.2 Mechanism 2 — the method chokepoints consult `uses_callframe` (fixes `CALLER::`/`callframe()` in methods)

Extend the *consultation* of the existing flag — not its detection — to the
two method chokepoints. In both `call_compiled_method` and
`call_compiled_method_fast`:

- prologue: `let pushed_caller = cc.uses_callframe; if pushed_caller {
  self.push_caller_env(); }` — placed **before** the scoped-overlay install,
  so the pushed entry captures the caller's env, mirroring
  `call_compiled_function_named_inner`'s ordering. Plain `push_caller_env()`
  (code = `None`) is correct: the `CallFrameEntry` describes the frame the
  call is made *from*, and `push_caller_env_with_code` already defaults to
  the caller's `block_stack` top.
- every frame-exit site: `if pushed_caller { self.pop_caller_env(); }`,
  paired with the existing `pop_call_frame` sites (slow path: the early
  invocant-concreteness/typecheck returns at ~455/602 and the two normal
  exits at ~866/957; fast path: the type-mismatch bail and the normal exit).
  Plain `pop_caller_env()` (not `_with_writeback`) — dynamic-write
  propagation on method return is already owned by `merge_method_env` /
  the can-skip-merge gating, and this ADR does not move it.

The sub side keeps its current shape (flag drives *path eligibility*, full
path pushes unconditionally); the method side makes the push *conditional
inside* the chokepoints instead of rerouting, because method routing is
decided by parameter/caching shape and there is no frame-pushing method path
to reroute *to* — the chokepoints are the paths.

Method bodies already carry the flag correctly (§1.1); redefinition /
monkey-patching needs no new invalidation, because the flag lives on the
`CompiledCode` of the *resolved* `MethodDef`, and resolution already goes
through generation-invalidated caches (`refresh_method_caches_for_generation`;
`FastMethodCacheEntry` carries `compiled_code`, so the cached fast path reads
the same flag). JIT is unaffected: `vm_jit::try_enter` runs inside the
chokepoints' op loop, after the prologue.

One detection-parity audit rides along: bareword `callframe`/`callframes`
(no parens) may compile as `GetBareWord` rather than `CallFunc` (the
`uses_dispatcher` detection already handles this pair-wise for
`callsame`/`nextsame`); if the runtime dispatches those barewords, `emit()`
detection must cover `GetBareWord` for the two names too.

## 3. Alternatives considered and rejected

**(a) Unconditionally push a caller frame on every method call.** Simplest
and closest to what rakudo semantically guarantees, but a real tax on the
hottest code path in the interpreter: per call it costs two `Env` clones (the
stack entry and the `CallFrameEntry.env`), a `String` for the entry's file,
and — the real cost — bumping the caller env's `Arc` strong count for the
call's duration, which turns the body-side env writes back into
`Arc::make_mut` COW forks. The scoped-overlay campaign
(docs/vm-dual-store.md Slice 6) exists precisely to keep per-method-call cost
free of "the ~12μs Arc::make_mut deep clone" (comment in
`call_compiled_method_fast`); an unconditional push re-introduces that
pressure on every method call to fix a mechanism the overwhelming majority
of methods never use. Rejected.

**(b) Extend `uses_callframe` detection to `GetPseudoStash` and gate the push
on it (frames for everything).** Works for the method repros, but (i) taxes
every `PROCESS::`-reading *sub* by demoting it from the fast/light paths,
(ii) still leaves the §1.2 sub-side gaps in any shape where a frameless
overlay intermediate sits between writer and reader (the pushed entry's
top-tier-only `iter()` is the same bug one level up — verified by the `mid`
repro), and (iii) conflates two different semantics (name resolution vs.
frame introspection) into one flag. Mechanism 1 fixes the dynamics side
soundly at the enumeration primitive instead. Rejected as the *dynamics*
fix; the flag stays scoped to genuine frame observation.

**(c) Lazily materialize caller frames from `call_frames` instead of an eager
push.** `push_call_frame` already saves the caller env into
`frame.saved_env`, so in principle `caller_env_stack` duplicates state the
call-frame stack already holds, and `CALLER::`/`callframe()` could walk
`call_frames` + `routine_stack` on demand — eliminating the parallel stacks
entirely. Attractive as architecture, but it is a dual-mechanism-unification
campaign, not a fix: the frameless sub paths push no call frame at all
(`call_compiled_function_fast` / positional-light), `push_light_call_frame`
differs from `push_call_frame`, and every consumer of
`caller_env_stack`/`callframe_stack` (EVAL's synthetic frames, the writeback
lists, `CALLERS::` cascading) would need re-plumbing. Rejected for this ADR;
recorded as the natural follow-up direction if the parallel stacks ever
become a maintenance burden.

**(d) Make method dispatch reuse the sub fast path's reflective-latch overlay
skip** (`!reflective_name_access_possible()` gating, so flag-latched programs
run method bodies in the live caller env). This is how the sub fast path
"accidentally" works, but it is the *worst* of the existing mechanisms: a
process-global monotonic latch that de-optimizes every method call in any
program containing a single `EVAL` or pseudo-stash op anywhere, and it still
would not fix `CALLER::`/`callframe()` (which need the stacks, not env
visibility). Mechanism 1 makes the enumeration correct *without* giving up
the overlay. Rejected.

## 4. Performance

- **Majority case (method never observes its caller): near-zero.**
  Mechanism 1 adds nothing to dispatch. Mechanism 2 adds one boolean test of
  an already-resolved `CompiledCode` field per chokepoint entry/exit —
  the same cost class as the existing `cc.uses_dispatcher` gate that sits
  right next to it.
- **Flag-bearing methods** pay the same push cost flag-bearing subs already
  pay on the full named path (two env Arc clones + one String + two Vec
  pushes). The entry is popped before the caller resumes, so no lasting COW
  pressure on the caller env beyond the call's duration (during which
  `frame.saved_env` references it anyway).
- **`PROCESS::`/`DYNAMIC::` read sites** pay a bounded tier walk per stacked
  env (chain depth is capped by `MAX_OVERLAY_DEPTH`, envs flatten beyond it).
  These are reflective reads, not hot loops; the cost lands where the feature
  is used.
- Guard: watch the bench CI (`bench-history.tsv`) class-heavy rows
  (`bench-class`) across the Slice 2 landing; the design predicts no movement
  because the majority path adds only the boolean test.

## 5. Implementation plan (independently mergeable slices)

### Slice 1 — chain-aware dynamics enumeration (env + runtime layer)

- `src/env.rs`: add a chain-aware enumeration helper (visit tiers
  outermost-first with tombstone suppression, or reuse `flattened()`'s walk).
  Unit-test tombstone/shadowing ordering directly (`#[test]`).
- `src/runtime/runtime_caller_env.rs`: `dynamic_pseudo_stash_entries` uses it
  for each `caller_env_stack` entry and for `self.env`.
- Regression tests (`t/`): the flat-method `PROCESS::` repro, the
  closure-bearing-method shape, the positional-light sub gap, and the
  frameless-intermediate gap (§1.2) — all expecting 42. Existing pins that
  must stay green: `t/process-stash-visible-across-sub-boundary.t`,
  `t/pseudo-dynamic-stash.t`, `t/process-register-dynamic.t`,
  `t/process-dynamic-nil-decay.t`, `t/pseudo-callers.t`.

### Slice 2 — `uses_callframe`-gated push at the method chokepoints (VM layer)

- `src/vm/vm_method_dispatch.rs`: prologue push + per-exit pop in
  `call_compiled_method` and `call_compiled_method_fast` as specified in
  §2.2. Audit that every early return between push and normal exit pops
  (pair each with its existing `pop_call_frame`).
- Detection-parity audit in `src/opcode.rs` `emit()`: bareword
  `callframe`/`callframes` via `GetBareWord` (§2.2).
- Regression tests (`t/`): the `CALLER::<$*y>` method repro (42) and the
  `callframe(1).line` method repro (call-site line); a depth-0/1 sanity check
  from inside a method. Must-stay-green pins: `t/callframe-file-line-same-frame.t`,
  `t/callframe-for-block-frame.t`, `t/callframe-annotations-map.t`,
  `t/callframe-setting-frame.t`, `t/module-file-var-and-callframe.t`,
  `t/eval-caller-frames.t`, `t/caller-not-dynamic.t`,
  `t/caller-frame-write-slot-coherence.t`, `t/backtrace-block-frames.t`, and
  whitelisted `roast/S06-advanced/callframe.t`.

### Slice 3 — residue + end-to-end acceptance

- Tree-walk residue: audit `run_resolved_method_compiled_or_treewalk`'s
  non-compiled arm; if a caller-observing body can reach it, push
  unconditionally there (cold path — eligibility analysis is not worth it).
- `Log::Timeline` end-to-end: `t/logging.rakutest` tests 10-30 (the ticket's
  blocked battery) as the acceptance gate; then retire the ticket file to
  `news/` per the todo/ lifecycle.
- Document (in the ticket retirement / news entry) the inherited, unchanged
  approximation: deep `CALLER::CALLER::` / `callframes()` chains through
  *frameless* intermediate subs remain gappy, exactly as on the sub side
  today.

## 6. Verification burden

The ticket flags this correctly: `CALLER::`/`callframe()`/`PROCESS::`/
`DYNAMIC::` are load-bearing across many currently-passing tests in
sub/block contexts. A sufficient sweep before calling any slice done:

- the `t/` pins listed per-slice above (grep basis: `callframe`, `CALLER::`,
  `PROCESS::`, `DYNAMIC::` under `t/` — ~20 files), run on the debug binary;
- roast: `S06-advanced/callframe.t` (whitelisted — must stay green),
  `S06-advanced/caller.t` (not whitelisted — check for newly-passing),
  `S02-names-vars/pseudo*.t`, `S04-statements/goto.t`;
- full CI (`make test` + `make roast` + gc-stress/jit-stress) as the
  comprehensive net — Mechanism 2 touches the hottest dispatch functions, so
  the jit-stress job is the one most likely to surface an ordering mistake
  (push before overlay, pop before env restore).

## 7. Implementation status (2026-08-20)

All three slices landed:

- **Slice 1** (chain-aware dynamics enumeration): PR #6703. The existing
  `Env::filtered_flat` chain-aware tier-walk primitive already implemented
  exactly what §2.1 specified — no new primitive was needed, only wiring
  `dynamic_pseudo_stash_entries` to use it instead of `Env::iter()`. Fixes
  `PROCESS::`/`DYNAMIC::` reads from methods and both latent sub-side gaps
  (§1.2). Regression test: `t/adr0035-dynamics-chain-aware-enumeration.t`.
- **Slice 2** (`uses_callframe`-gated push at the method chokepoints): PR
  #6704. Implemented as specified in §2.2, with two corrections found during
  implementation: (a) the fast path (`call_compiled_method_fast`) has two
  distinct normal-exit sites, not one as the ADR's line-derived description
  implied — handled with a single `pop_caller_env()` hoisted immediately
  before the branch that splits between them; (b) the detection-parity audit
  (§2.2's last paragraph) turned out to be a non-issue — `--dump-bytecode`
  confirms bareword `callframe`/`callframes` always compile as
  `CallFuncNamed`, already covered by existing `uses_callframe` detection, so
  no `emit()` change was needed. Fixes `CALLER::`/`callframe()` reads from
  methods. Regression test: `t/method-caller-frame-push.t`.
- **Slice 3** (tree-walk residue + Log::Timeline end-to-end acceptance): the
  "tree-walk residue" concern in §1.3 turned out to already be moot, resolved
  by an earlier, unrelated refactor (#3658) that predates this ADR:
  `run_resolved_method_celled` (`src/runtime/class_dispatch.rs`) compiles any
  candidate with no `compiled_code` on demand (`compile_method_def_in_place`)
  *before* reaching its own dispatch, then unconditionally calls
  `call_compiled_method` — the same chokepoint Slice 2 already patched. The
  only executor that does NOT go through the two chokepoints is
  `forward_resolved_delegation`, which handles `handles`-delegation
  forwarders (plumbing that redirects to another method call, not a
  caller-observing user body) — confirmed by reading the function's own
  doc comment, which states the former tree-walk method-execution arm "has
  been deleted." No code change was needed for this slice.

  The Log::Timeline end-to-end acceptance gate (`t/logging.rakutest` tests
  10-30) does NOT fully pass, but not because of anything this ADR's
  mechanism owns: mechanism-wise, tests 1-8 now pass (previously silently
  no-op'd via the `PROCESS::<$LOG-TIMELINE-OUTPUT>` visibility bug this ADR
  fixes). Test 9 onward hits a distinct, unrelated bug — a class-level `my
  atomicint` counter read via `⚛++` inside an attribute's default-value
  expression is wrong for the first-ever instance of the class (and
  off-by-one after) — filed separately as
  `todo/tickets/class-level-atomicint-attribute-default-first-instance-wrong.md`.
  This is the `Log::Timeline::Ongoing::Logged` task-ID counter, unrelated to
  caller-frame observation; it surfaced only because Slices 1-2 let execution
  reach that far for the first time. The caller-frame mechanism itself is
  considered fully verified by the smaller, targeted regression tests listed
  under Slices 1-2 above, plus the roast/`t/` pin sweep (all pins green,
  `roast/S06-advanced/callframe.t` whitelisted and green).

The inherited, unchanged approximation flagged in §5 Slice 3 stands: deep
`CALLER::CALLER::` / `callframes()` chains through *frameless* intermediate
subs remain gappy, exactly as on the sub side before this ADR.
