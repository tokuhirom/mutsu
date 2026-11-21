# ADR-18393: A call frame is slots plus a slim record — no `Env` overlay, readonly scope or fat record per call; the name view is materialised on demand

- **Status**: Accepted (maintainer approved, 2026-11-21; proposed 2026-11-21)
- **Issue**: [#18393](https://github.com/tokuhirom/mutsu/issues/18393), a child of
  [#12529](https://github.com/tokuhirom/mutsu/issues/12529). Goal: the ecosystem ledger at
  100%. The two files still short are AttrX::Mooish `t/090-concurrent.custom` (240 s
  SWEEP-TIMEOUT on the runner) and MCP::Server::Tool::Web `t/12` (`html-to-text`).
- **Related**:
  - [ADR-12529](12529-frames-resolve-names-through-the-static-link.md) §2.3: a call builds no
    name-keyed env. This ADR decides *how* for method, closure and sub frames.
  - [ADR-14633](14633-call-frame-materializes-no-name-keyed-env.md): fixtures shipped; §2.3 side
    state. Its Amendment A1 deferred the lazy `Env` because, alone, it is worth ~400 Ir. This ADR
    supersedes A1's *deferral* only, not its finding.
  - [ADR-15737](15737-raku-level-call-path-footprint.md) D1/D2 and
    [ADR-18244](18244-wide-raku-code-floor-staged-structural-plan.md) O1/O2: the per-site plan
    and the call-site layers stay O2's (#18267). Static addressing of free variables stays O1's
    (#18274).
  - [ADR-17335](17335-mooish-concurrent-wall-time-is-per-round-cpu.md) O1.
  - [ADR-18356](18356-the-instance-cell-is-the-only-attribute-store.md): attributes live only in
    the instance cell.
  - [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md),
    [ADR-0035](0035-method-calls-observe-caller-frames.md),
    [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md).

## 1. Context

### 1.1 Where a method call's instructions go

Measured on `main` @ `41be14d5` with a profiling build. The figure is the callgrind slope between
N = 1,000 and 3,000 iterations of `$t = $o.m0` (`method m0() { 1 }`), from the second run of each
binary. The loop skeleton (`$t = 1`) is 1,061 Ir per iteration.

| | Ir per call |
| --- | ---: |
| whole call (`call_method_mut`, from the JIT helper down) | 8,752 |
| call-site layers (`exec_call_method_mut_site` → … → `run_method_call_plan`; O2's, #18267) | ~3,840 |
| the frame (`call_compiled_method_lean_in_unit`, inclusive) | 4,819 |

Inside the frame, by inclusive cost:

| component | Ir | kind |
| --- | ---: | --- |
| `pop_call_frame` + `push_call_frame` (`VmCallFrame`, 18 fields, six moved-out containers) | 869 + 320 | frame record |
| `RoutineStack::pop` (incl. the `FrameFixtures` drop) + `push_method_routine_with_location` | 258 + 150 | frame record |
| `Env::scoped_child` + the caller handle's clone + the two `Env` drops | ~212 + ~200 + ~220 | env overlay |
| readonly scope: `replay_readonly_undo`, the topic unmark, the parameter marks | ~300 | readonly scope |
| `method_body_package` (a function of (code, class), recomputed per call) | 265 | per-call fact |
| `$CALLER::` alias save/restore, class-body statics and parameters copied in by name, `state` scope, captured-readonly reconcile | ~170 | env overlay |
| the body (`run_compiled_method_body`, a constant) | 179 | body |
| bind, slot seeding, result finalisation, the rest inlined | ~1,800 | bind / misc |

On this box rakudo inlines `m0` away entirely. A one-parameter method that also reads an
attribute costs rakudo about 120 ns per call.

### 1.2 What it is on the two workloads

**AttrX::Mooish.** Probe: subtest 1 of `t/090-concurrent.custom`, run single-threaded. Slope over
R = 120 → 360 rounds: **4,627,514 Ir per round**.

Self cost by family:

| family | share of the round |
| --- | ---: |
| name-keyed `Env` (tiers, overlay inserts and drops, capture, class-body statics) | 12.6% |
| `Symbol` interning and `as_str` | 16.1% |
| alloc / free | 8.8% |
| readonly | 1.0% |
| call-frame record | 0.6% |

Frame-attributable work per round, inclusive (the rows overlap):

| item | Ir |
| --- | ---: |
| closure creation (`build_closure`), of which env capture (`capture_closure_env` → `layered_capture`) | 140k, 114k |
| `inject_class_body_statics` (+ `_saving_except`) | 87k + 25k |
| `Env` / `Tier` drops | ~110k / ~98k |
| `push_call_frame` + `pop_call_frame` | 97k |
| readonly | ~48k |
| `Env::cow_mut` | 46k |
| `insert_fast_param_values_by_name` | 31k |

The by-name *reads* are O1's: `package_chain_var_fallback` 225k and `Env::get_sym` 122k.

About 55 user method calls and 12 closure calls run per round. Most of the method calls go through
the general fast binder (`call_compiled_method_fast_in_unit`), whose frame does the same work as
the lean frame's and more.

**MCP::Server::Tool::Web `html-to-text`.** Probe: t/12's pathological document, scaled by S.
Slope over S = 0.02 → 0.06: **Δ = 3.147 G Ir**.

| family | share of Δ |
| --- | ---: |
| name-keyed `Env` | 8.0% |
| `Symbol` | 8.4% |
| readonly | 0.9% |
| record | 0.5% |

Frame-attributable work, inclusive:

| item | share of Δ |
| --- | ---: |
| `Env` drops | 2.0% |
| `pop_call_frame` | 1.0% |
| `cow_mut` | 0.7% |
| readonly marks | ~0.7% |
| `scoped_child` | 0.4% |
| closure capture | ~1.2% |
| declaration bookkeeping (`SetVarDynamic` 3.3%, `env_free_slot` 0.8%) | 4.1% |
| by-name reads (`get_env_with_main_alias_sym` 2.0%, `get_sym` 1.8%, `walk_with_fallback` 1.1%; O1) | ~5% |

Subs (`call_compiled_function_positional_light_at`, 33.7% inclusive) and methods
(`call_compiled_method_fast_in_unit`, 42.8%) carry most of Δ.

### 1.3 Why slices have stopped paying

Every slice since ADR-14633 has shaved one layer: fixtures, the lean frame, plans, the late
deferral frame, the bind lanes. Each made one component cheaper and kept the frame's *contract*:
a frame is an `Env` overlay over the caller's `Env`, plus a readonly scope keyed by bare name,
plus a record that saves and restores every side register. The remaining cost is that contract,
spread thinly:

- No single component of §1.1 is above ~900 Ir.
- No family of §1.2 that a frame owns is above ~3% on its own.

ADR-14633 A1 measured the lazy `Env` *alone* at ~400 Ir and correctly declined to build it.

Removing the contract is a different proposition, for two reasons:

- It deletes every frame-owned component at once: overlay, readonly scope, side-register record and
  per-call facts. Together they are ~3.3k of the 4.8k frame.
- It is a precondition for the larger name-keyed families: closure capture, class-body statics and
  declaration bookkeeping. Each of those exists *because* a frame might be read by name.

### 1.4 Who consumes the overlay, the readonly scope and the record

Counted with `grep` over `src/` on `41be14d5`. Each row gives call sites, then files.

| consumer | sites / files | what it needs from the frame |
| --- | ---: | --- |
| `.env()` reads | 785 / 195 | a name lookup that starts at the current frame |
| `.env_mut()` writes | 610 / 128 | a tier to write a name into |
| `clone_env()` (closures, END phasers, threads) | 27 / 12 | a flattenable view of the frame's names |
| `Env::scoped_child` (frame constructors) | 49 / 24 | none: they *build* overlays |
| `saved_env` (restore after a call) | 244 / 64 | the caller handle to restore |
| `call_frames.last/iter/len` | 44 / 25 | the record: caller code, locals base, stack depth (CALLER::, panic recovery, closures) |
| `push_caller_env` / `caller_env_stack` | 80 / 21 | `(caller_code, locals base)`; slot-based already (`flush_calling_frame_slots`, ADR-0035) |
| readonly checks (`is_readonly_sym`, `check_readonly_for_*`) | 38 / 20 | the name-keyed readonly set, so a param or `is rw` bind is refused |
| `enter_readonly_frame` | 24 / 14 | the scope that undoes a frame's marks |
| `inject_class_body_statics` | 22 / 12 | an overlay to copy class-body `my`s into |
| fixtures (`FrameFixtures`, `frame_invocant`) | 37 / 11 | the routine frame, not the env (ADR-14633 §2.1) |

The ~1,400 `env()`/`env_mut()` sites cannot be swept one by one, and this design does not need
them to be. A site runs in one of two places:

- **inside an op of the frame's own body.** D2's closed op list admits no op whose execution
  reaches such a site.
- **inside a callee.** The callee has its own frame, and that frame keeps today's contract unless
  it is itself a slot frame.

The only consumers that must learn about slot frames are therefore the ones that read *another*
frame's state: `CALLER::`, `callframe` and panic recovery. They already read the call-frame record
rather than the overlay.

### 1.5 The stage 1 prototype, measured

Branch `perf/frame-without-env-overlay`, `src/vm/vm_method_slot_frame.rs`. Measured with a
profiling build, as the callgrind slope over N = 1,000 → 3,000 calls (second run). `$o.m3(2)` is
`method m3($a) { $a + 1 }`.

| `MUTSU_SLOT_FRAME` | what the frame skips | `$o.m0` | `$o.m3(2)` |
| --- | --- | ---: | ---: |
| unset (today's lean frame) | nothing | 10,029 | 11,626 |
| `1` | env overlay, `$CALLER::` save/restore, statics, by-name parameter copies, readonly marks/scope, `method_body_package`, `state` scope | 8,551 (−1,478) | 9,581 (−2,045) |
| `2` | the above, plus the `VmCallFrame` record (slot window and stack depth only) | **7,727 (−2,302, −23%)** | **8,742 (−2,884, −25%)** |

In the `2` run the frame itself (`call_compiled_method_slot_in_unit`) is 2,531 Ir, down from
4,819. What is left in it:

| item | Ir |
| --- | ---: |
| routine frame with fixtures (push + pop + `FrameFixtures` drop) | ~650 |
| the body | 179 |
| attribute-cell guard drops | ~400 |
| slot window truncate / refill | ~400 |
| finalisation | the rest |

The call-site layers are unchanged at ~3.8k. Of those, `push_method_dispatch_frame_named` (445)
and `push_method_samewith_context_named` (179) are pushed even for a body that cannot observe them.

The prototype admits only leaf bodies: slot reads, constants and arithmetic, with no calls and no
declarations. On the two workloads it therefore moves nothing:

- The Mooish probe measures 4,627,514 vs 4,627,912 Ir per round.
- Every hot Mooish and Web method runs through the general fast binder
  (`call_compiled_method_fast_in_unit`; about 89 entries per Mooish round, ~88k over Web's Δ) or
  the general path. None runs through the lean frame.

That is why stage 2 below (calls and declarations, the fast binder's frame) is the first stage
that can move the workloads.

## 2. Decision

**A call frame whose code never needs a name view is a *slot frame*.** It consists of its slot
window and a slim record, and nothing else:

- it builds no `Env` overlay;
- it opens no readonly scope;
- it moves no side registers;
- it recomputes no per-(code, class) fact.

Whether a body needs a name view is a property of its compiled code, decided by a closed list of
ops (D2), never by an analysis that can be incomplete. A body that needs one keeps today's frame.
A runtime request for a slot frame's names is answered by materialising a view from the record
(D3).

### D1. The frame shape

A slot frame pushes:

- **the slot window**: `locals.push_frame`, as now;
- **a slim record** on the existing call-frame stack. Its fields are:
  - `caller_code`;
  - the caller's locals base;
  - `saved_stack_depth`;
  - `saved_cur_line`;
  - one scope-stack base (ADR-14633 §2.3's single base for the four loop/block scope stacks);
  - a `kind: Slot` tag.

  It holds no `saved_env`, no moved-out side containers and no readonly mark;
- **the routine frame and its fixtures** (`self`, `::?CLASS`), as ADR-14633 §2.1 shipped them.

The `Env` handle is not touched: the slot frame runs on the caller's handle.

This is sound only because of D2. No op the body executes reads or writes a name, so no name
lookup can start in the slot frame and see the caller's tier. That is exactly the hazard ADR-14633
A1 §8.1 found for a lazy frame that reuses the caller's handle.

The per-(code, class) facts the frame needs are computed once, when the call plan is installed
(O2's plan, guarded by the registry generation), and replayed:

- the method body package;
- the frame plan's slot seeds;
- the `state` scope key.

Return:

- truncate the value stack;
- pop the routine frame and the slim record;
- restore the caller's locals base.

Nothing is merged and nothing is dropped beyond the slot window.

### D2. Admission: the code's frame needs

`CompiledCode` gains a `frame_needs: FrameNeeds` bitset. The compiler computes it once, from the
emitted ops and the code's existing flags. An op the classifier does not list sets `NAME_VIEW`, so
a new op can only make slot frames rarer, never wrong.

| bit | set by | why the overlay or readonly scope was needed |
| --- | --- | --- |
| `NAME_VIEW` | any by-name read or write: `GetGlobal`/`SetGlobal` and kin, `EVAL`, `::()`, `MY::`/`OUTER::`/`LEXICAL::`, `callframe`, `$CALLER::x :=`, `static_name_lookup`, `needs_reflective_capture`, a `LocalInit::Outer` slot seed | the op resolves a name starting at the current frame |
| `ROUTINE_STATE` | `reads_topic`/`writes_topic`/`uses_routine_state`/`sets_routine_state` while `$_`/`$/`/`$!` are env-owned | the frame root answers them |
| `DECL_NAMES` | a declaration whose name bookkeeping is observable: a dynamic, `our`, a sigilless/`is rw` alias, a `:=` bind declaration, `state` | `SetVarDynamic`'s dynamic map, readonly unmark and env-free-slot check |
| `READONLY_OBS` | a parameter slot that is written, bound, or passed as a call argument | the name-keyed readonly set is how a write and an `is rw` bind are refused today |
| `CLOSURES` | `MakeAnonSub`/closure creation | creation captures the frame's env (`layered_capture`) |
| `PHASERS` | `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`FIRST`/`NEXT`/`LAST`/`END` capture, `let`/`temp` | phasers and let-saves restore by name |
| `DISPATCH_CTX` | `callsame`/`nextsame`/`samewith`/`callwith`/`nextwith`, `uses_capture`, `may_observe_named_slurpy` | the deferral context and capture |

**Per-call guards.** A slot frame is entered only when:

- the plan says the code's `frame_needs` is clear for the frame kind;
- the process-wide latches the slot fast paths rely on are clear. These are `local_read_unspoiled`
  (no atomic variable, no `$CALLER::` alias, no sigilless attribute alias ever created) and an
  empty `var_bindings`.

A refusal runs today's frame. Both guards are monotonic or O(1).

**The stages lift bits one at a time** (§4):

- Stage 2 lifts `READONLY_OBS` and `DECL_NAMES`. Readonly becomes a slot property: the binder's
  rw check asks the argument's source code for `readonly_code_params`, through the
  `(code, slot)` the call site already records in `pending_call_arg_sources`. `SetVarDynamic`
  compiles to a slot-only declaration when the code has no `NAME_VIEW`.
- Stage 3 lifts `CLOSURES`: a closure created in a slot frame captures the ADR-0018 cells of the
  slots it closes over, plus the static link. It never captures a layered env.

`NAME_VIEW`, `PHASERS` and `DISPATCH_CTX` stay permanent exclusions until a later ADR. Those frames
keep today's shape.

### D3. The materialisation contract

A name view of a slot frame is built only on request, from its record, never kept:

| request | answered by |
| --- | --- |
| `CALLER::` / `callframe(n)` from a callee | the record's `(caller_code, locals base)`. This is what `push_caller_env_of_frame` / `flush_calling_frame_slots` already do (ADR-0035): a scoped view over the slots, built O(locals), only when `caller_frame_read_possible()` |
| dynamic variable lookup (`$*x`) from a callee | the caller's `Env` chain, which the slot frame did not replace. The slot frame declares no dynamic (`DECL_NAMES`) |
| `self`, `::?CLASS`, `::?ROLE` from runtime helpers | the routine frame's fixtures (ADR-14633 §2.1) |
| backtraces, `&?ROUTINE` | the routine frame |
| `EVAL`, symbolic lookup, pseudo-packages, by-name writes *in the frame's own body* | impossible: `NAME_VIEW` refuses admission |
| a panic unwound through the frame (`recover_call_frames`) | the record's locals base. No env to restore |
| JIT entry at ip 0 | slots only. No helper may assume an overlay exists (the `try_enter` audit of ADR-14633 §5 applies) |

**Invariant (checked in debug builds).** While a slot frame is the innermost frame:

- `self.env()` is pointer-equal to the handle the caller had at entry;
- no `insert`/`remove`/`cow_mut` reaches its tier.

A violation is a missing `frame_needs` bit. It is fixed by adding the bit, never by an env write
in the frame.

### D4. Ir targets per component (`$o.m0`, per call)

| component | now | target |
| --- | ---: | ---: |
| env overlay (§1.1 rows) | ~800 | 0 |
| readonly scope | ~300 | 0 |
| frame record (call frame + routine frame) | ~1,600 | ≤ 400 |
| per-call facts | ~450 | ≤ 50 |
| the frame in total | 4,819 | **≤ 1,500** |
| whole `$o.m0` (with O2's call-site layers unchanged) | 8,752 | ≤ 5,500 |

## 3. Risks

| risk | handling |
| --- | --- |
| **roles / mixins** | A role method composed into a class or mixin runs with `?ROLE` and role bindings. The lean gate already refuses `role_param_bindings`, `original_role`, `role_origin` and `is_role` owners. Stage 1 inherits that. A later stage moves `?ROLE` to the fixture (already there) and keys the plan on the mixin type (O2 stage 4 did this for the lane). |
| **multi** | Stage 1 refuses `is_multi` (the plan already does). A multi winner's frame is the same frame once the dispatcher has chosen. The candidate cache is O2's, so a later stage only lifts the refusal. |
| **`callsame`/`nextsame`/`samewith`/`callwith`** | `DISPATCH_CTX` refuses. The outer deferral frame stays late-built (#18361) and is not part of the slot frame. |
| **`is rw` / `is raw` / sigilless parameters** | Stage 1 refuses them (`lean_param_ok`). Stage 2's readonly-as-slot-property must reproduce raku's three refusals exactly: assign to a readonly parameter; bind a readonly parameter to `is rw`; `is raw` relay. Tests are pinned against `raku` before the change. |
| **`self` captures, closures in methods** | `CLOSURES` refuses until stage 3. Stage 3's capture is cell-based (ADR-0018), so a closure sees later writes to the slot. That is the "cell, not snapshot" rule of AGENTS.md. |
| **phasers, `leave`, `return`** | `PHASERS` refuses phasers and `let`/`temp`. `return`/`leave` are control exceptions matched by callable id in `run_compiled_method_body` and do not touch the env. Unchanged. |
| **`CALLER::`** | It reads the record (D3). Callees' `CALLER::` writes target only dynamics, and a slot frame declares none. |
| **`Proxy`** | A Proxy argument is refused per call (`lean_args_ok`). A Proxy-valued slot is a value like any other. The FETCH/STORE closures run in their own frames. |
| **A missed name reader** | D2's closed op list fails safe, and D3's debug invariant catches a runtime helper that writes the env during a slot frame. Roast, `t/` and the ledger parity run are the net. |
| **Threads** | Frames are per-thread state (ADR-17216). No new shared state, no `unsafe`. |

## 4. Stages

Each stage is one PR, measured per the `perf-tuning` skill and gated by a parity check of
CSS::Properties, Red, AttrX::Mooish, Monad and MCP::Server::Tool::Web against ledger run #110
before auto-merge.

1. **Prototype (this ADR's measurement; not for merge).** Plain planned lean methods on Instance
   receivers whose bodies use only slot/constant/arithmetic ops, behind `MUTSU_SLOT_FRAME=1`. It
   measures the overlay, readonly and per-call-fact share with the existing record.
2. **Slim record and `FrameNeeds`.** This stage covers:
   - the `frame_needs` bitset and the slim record (D1). This is ADR-14633 §2.3's side state,
     coordinated with O2's frame-record stage (#18267 stage 3);
   - readonly as a slot property, and slot-only declarations;
   - **both** the lean frame and the general fast binder's frame
     (`call_compiled_method_fast_in_unit`). The fast binder's frame is where the workloads' method
     calls actually run (§1.5);
   - a plan whose code has no `DISPATCH_CTX` pushes no dispatch frame and no samewith context
     (~0.6k per call, §1.5).

   It admits bodies with calls and `my` declarations: Web's renderer methods, and Mooish's
   `has-foo`/`clear-foo`/accessors.
3. **Closures.** Cell capture plus the static link; the closure call path enters a slot frame.
   This is Mooish's FETCH/STORE closures and `build_closure`.
4. **Subs.** `call_compiled_function_positional_light_at` (Web's `is-name-char`, `find`, …) enters
   the same frame.
5. **Class-body statics** resolve through the static link to the class body's cells: retire
   `inject_class_body_statics` per call (with O1, #18274).
6. **Retire** the overlay-building paths that no admitted frame uses any more.

## 5. Expected gain

These are estimates from §1.2's buckets. Each stage re-measures them.

**AttrX::Mooish probe, Ir per round:**

| stages | gain |
| --- | ---: |
| 1-2: ~55 method frames × ~3.3k | ≈ −3.9% |
| 3: closures, ~12 calls; creation capture 114k plus closure-frame env | ≈ −4 to −5% |
| 5: statics | ≈ −2.4% |
| **total** | **≈ −10 to −12% (1.11-1.14x)** |

Together with O1's by-name reads (≈ −7%), that makes the ~1.2x the worst case of #18267's target
note needs (4.78M → ≤ 4.0M Ir per round).

**Web `html-to-text`:**

| item | gain |
| --- | ---: |
| frame shape for methods and subs | ≈ −5 to −6% |
| slot-only declarations | ≈ −4% |
| **total** | **≈ −9 to −10%** |

With O1's by-name reads (≈ −5%), that is ~1.15x toward the 27 s → < 21 s local target. That
target also needs the per-op floor work (ADR-18244 O3).

## 6. Alternatives rejected

- **ADR-14633 A1's lazy `FrameView` alone.** It removes ~400 Ir and keeps the readonly scope,
  the fat record and the name-keyed declaration bookkeeping. That is the slice-by-slice shape §1.3
  describes.
- **A cached per-method frame `Env`** (`scoped_child` built once, copy-on-write on first insert).
  It would also change the env's parent from the caller to the method's static scope. That breaks
  dynamic-variable lookup from callees, which still walk the env chain. It also keeps the drop and
  clone per call.
- **A static "never reads a name" proof over the whole program** (an interprocedural analysis, or
  a process-wide `EVAL` flag). This is the #14337 failure mode and ADR-0110's incomplete-analysis
  hazard. The per-code closed op list cannot be wrong, only conservative.

## 7. Open questions

- Is O2's call-site layer (~3.8k) the next ceiling once the frame is ≤ 1.5k? The per-site inline
  cache that enters a slot frame directly is ADR-15737 D1's end state and is not decided here.
- Whether stage 2's readonly-as-slot-property can retire the name-keyed readonly set for *all*
  frames, or only slot frames, is decided by stage 2's measurement and roast run.
