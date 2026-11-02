# ADR-14633: A routine or method call frame materializes no name-keyed `Env` unless a write by name asks for one

- **Status**: Accepted (user decision 2026-10-23; proposed 2026-10-22). Accepted with two
  amendments made on review: the fixtures live on a parallel stack, not in `RoutineFrame`
  (§2.1), and the new frame state is frame-core state under ADR-10779 (§2.3).
- **Date**: 2026-10-22
- **Issue**: [#14633](https://github.com/tokuhirom/mutsu/issues/14633), a child of
  [#12529](https://github.com/tokuhirom/mutsu/issues/12529). It carries the close
  conditions of [#8880](https://github.com/tokuhirom/mutsu/issues/8880) (`$o.m()` at most
  10x rakudo) and [#8888](https://github.com/tokuhirom/mutsu/issues/8888) (`@a.elems` at
  most 10x rakudo), which this ADR does not move.
- **Related**:
  [ADR-12529](12529-frames-resolve-names-through-the-static-link.md) (static link vs.
  dynamic link; §2.3 says a call builds no name-keyed env and defers *how* to this ADR),
  [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md) (program names leave
  the frame `Env`; #7817),
  [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md) (typed, resolved IR),
  [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md) (slots and cells),
  [ADR-0035](0035-method-calls-observe-caller-frames.md) (caller-frame observation),
  [#14337](https://github.com/tokuhirom/mutsu/issues/14337) (an `EVAL` anywhere forces env
  deep copies), [#9291](https://github.com/tokuhirom/mutsu/issues/9291) (attribute slots),
  [#7573](https://github.com/tokuhirom/mutsu/issues/7573) (the call path).

## 1. Context

ADR-12529 §2.3 decided that "a call builds no name-keyed env" and that a frame
"materializes a name view only when its code asks for one". It did not say how a method
call, whose frame today writes six fixed keys, gets there. #14633 asks four questions;
§2 answers them, §3 records what the measurement says about how far the answers reach.

### 1.1 What a `$o.m()` call costs now

`main` @ `d6a90701`, `cargo build --profile profiling`, callgrind (`--cache-sim=no`), the
repro of #8880 (100,000 iterations of `$t = $o.m()`, a zero-argument method returning `1`)
minus the same loop with `$t = 1`; second run of each, 4-core container:

| loop | Ir total | Ir per iteration, loop skeleton subtracted |
| --- | ---: | ---: |
| `$t = 1` (skeleton) | 222,232,947 | - |
| `$t = f()` (`sub f() { 1 }`) | 611,071,811 | 3,888 |
| `$t = $o.m()` | 1,675,181,073 | 14,530 |

Inclusive cost per call of the method path (`callgrind_annotate --inclusive=yes`, each
figure the function's inclusive Ir divided by 100,000):

| layer | Ir per call |
| --- | ---: |
| `exec_call_method_mut_site` (the whole call, from the JIT helper down) | 14,333 |
| `run_plain_method_lane` | 11,187 |
| `call_compiled_method` | 8,859 |
| `call_compiled_method_fast_in_unit` (the frame: bind, env, body, return) | 7,326 |

Callees of `call_compiled_method_fast_in_unit` that belong to the frame's shape
(per call, inclusive): `pop_call_frame` 774; `Env::insert_sym` x3 678 (`self`,
`__ANON_STATE#`, `?CLASS`); `drop_glue::<Env>` 801 (the callee overlay and the saved
caller handle); `Env::scoped_child` 210; `Env::remove_sym` (`?ROLE`) 181; `Env::clone` 81;
`push_method_routine_with_location` 77. That is about **2.8k Ir**. Callees that are *not*
the frame's shape: `reconcile_attrs` 548, `method_body_package` 327,
`seed_cstruct_fields_for_method` 455 (called from `vm_method_dispatch.rs:137`, which looks
up the string key `address` on every call of every instance method), `has_alias_meta`
282, `class_role_param_bindings` 80, `method_attr_cells_for` 89. The rest of the 7.3k is the
function's own inlined body, not separated further here.

Two facts follow, and the rest of this ADR is shaped by them:

1. **The frame's name-keyed `Env` and saved registers are about 2.8k of 14.5k Ir.** Removing
   them entirely takes `$o.m()` from 14.5k to about 11.7k Ir. #8880 puts 10x rakudo at about
   600 Ir per call, so the frame is a prerequisite, about a fifth of the distance. **This ADR alone cannot reach
   #8880's close condition**; it is a prerequisite that removes the frame's share and makes
   the rest (§3) measurable on its own.
2. About 5.4k Ir (14.3k - 8.9k) are spent *before* `call_compiled_method` is entered:
   `exec_call_method_mut_site` -> `call_method_mut_site_around` ->
   `exec_call_method_mut_op` -> `run_plain_method_lane` ->
   `dispatch_compiled_method_mut_with_raw_invocant` ->
   `try_compiled_method_mut_or_interpret_sym` -> `compiled_mut_resolved_dispatch`. That is the
   call-site protocol, not the frame.

Already done and not part of the problem: the samewith/deferral push is gated on
`CompiledCode::uses_dispatcher` (methods) and `uses_samewith` (functions), and no
`push_method_samewith_context` appears in this profile. The 2026-10-02 estimate of ~500 Ir
for it (#14633) is stale for this shape; §2.4 is about what remains.

### 1.2 What the frame writes today

On entry a compiled method writes into the callee overlay: `self`, `__ANON_STATE#` (an
alias of the invocant that the `$.foo` desugaring reads), `?CLASS`, `?ROLE` (inserted or
removed), and for the non-fast shape also the callable id and `%_`
(`vm_method_dispatch.rs`, `call_compiled_method_fast_in_unit`, the `skip_env_setup` and
else branches). The comment above the `skip_env_setup` branch states the premise this ADR
generalizes: "skip self, params, attrs, and other method-local vars since all reads go
through `GetLocal`". `$_`, `$/` and `$!` already left the env: the rooted frame answers
them (`src/env/routine_owned.rs`), and a call inserts none of them.

Who still reads these keys by name (non-parser, non-compiler source; each is a call that
must keep working):

- **`self`**: about 35 Rust runtime sites, almost all slow-path helpers: class and role
  registration (`registration.rs`, `registration_private_access.rs`,
  `registration_class_body*.rs`), role and mixin application (`types/roles.rs`,
  `types/role_mixin_class.rs`, `types/mod.rs`, `methods_mixin_dispatch.rs`), grammar
  (`methods_grammar.rs`), deferral (`dispatch_proto.rs`, `builtins_dispatch_next.rs`),
  attribute defaults (`attr_build_defaults.rs`), the closure call path
  (`vm_closure_dispatch.rs`, `resolution_call_sub.rs`), signature binding
  (`types/binding_signature.rs`), plus `get_env_self` (`vm_env_helpers.rs`),
  `vm_var_get_ops.rs` (a block nested in a method reads `self` through the env) and the
  rw-attribute and element-proxy helpers. A compiled method body does **not** read it by
  name: `self` is a local slot.
- **`?CLASS` / `?ROLE`**: `::?CLASS` / `::?ROLE` compile to a constant when the class is
  static (`compiler/expr_helpers.rs`), else to `GetGlobal` of the key; class-body
  execution, role composition and attribute-default evaluation *write* `?CLASS` into the
  env temporarily (`registration_class_body.rs`, `registration_class_compose_body.rs`,
  `attr_build_defaults.rs`).
- **`__ANON_STATE#`**: the compiler maps it to the invocant (`cc.locals` entry), the VM
  binds both `self` and it from `base`.
- **`%_` / `@_`**: written only when the body can observe them
  (`may_observe_named_slurpy`); the env entry exists because the slot name `%_` cannot be
  looked up bare.

Closures created in a method capture `self`, `?CLASS` and `__ANON_STATE#` into their own
capture tier (ADR-12529 phase 1 notes); a closure call re-inserts `self` into the callee
overlay (`vm_closure_dispatch.rs`).

### 1.3 The saved-register frame

`push_call_frame` / `push_light_call_frame` build a `VmCallFrame` of 18 fields; `pop_call_frame`
restores them. Four are scope-stack bases (`saved_loop_local_vars`,
`saved_loop_local_saved_env`, `saved_block_declared_vars`, the two `active_loop_*` stacks,
ADR-0077/0078: an integer each), one is the slot-frame base, and the rest are *moved-out
containers that are empty for nearly every call*: `saved_upvalues`,
`saved_local_bind_pairs`, `saved_frame_authoritative`, `saved_frame_owned`,
`saved_pending_caller_var_writeback` (an `FxHashSet<String>`), `saved_rw_param_rebinds`.
Each is `mem::take`n on push and written back on pop; the pop is 774 Ir per call.
`saved_env` is a full `Env` handle clone (an `Arc` bump plus a drop of the callee's).

### 1.4 The counter-example: #14337

`sub ev($c) { EVAL $c }` never called, `sub f($n) { $n + 1 }` called 1000 times:
`env_deep_copies=1000`. A process-wide fact ("an `EVAL` exists somewhere") selects a more
expensive frame shape for every call. Any design that decides a frame's shape from a global
or interprocedural analysis has this failure mode, and also the failure mode ADR-0110
records for incomplete static analyses (a mutation or name use the analysis misses).

## 2. Decision

**A call frame is a frame record plus slots. It carries no name-keyed `Env` of its own. An
`Env` overlay appears the first time something writes a name into the frame, by
copy-on-first-write at the `Env` API, not by a static prediction of who will.**

### 2.1 Where the fixed keys live, and how readers find them (question 1)

| key | home | readers |
| --- | --- | --- |
| `self`, `__ANON_STATE#` | the frame's fixture record: `invocant: Value` (one value for both names) | compiled body: its slot, as now. Runtime helpers: `Interpreter::frame_invocant()`. Name lookup (`Env::get_sym(wk::self_())` and `GetGlobal self`): answered from the record by the fixture rule below |
| `?CLASS` | `class: Symbol` on the fixture record | `::?CLASS` constant or slot, as now; helpers: `frame_class()` |
| `?ROLE` | `role: Option<Symbol>` on the fixture record (no insert, no remove) | helpers: `frame_role()` |
| `$_`, `$/`, `$!` | unchanged: the frame root answers them (`env::routine_owned`) | unchanged |
| `%_`, `@_` | a slot when `may_observe_named_slurpy` / the body mentions `@_`, never an env key; the bare-name lookup special case for `%_` goes | compiled body: slot |

**Where the fixture record lives (amended on acceptance, 2026-10-23).** Not in
`RoutineFrame` itself. `RoutineFrame` (`src/runtime/mod.rs`) is `#[derive(Clone, Copy)]`:
every field is a `Symbol`, an integer or a flag, and readers snapshot it by value
(`runtime_caller_env.rs` copies it into each `CallFrameEntry`, `system_introspect.rs` into
caller views). `Value` is `Clone` but not `Copy`, so an `invocant: Value` field would end
`Copy` and turn every one of those snapshots into a reference-count increment and a drop.

Instead the records form a parallel stack beside `routine_stack`:

```
struct FrameFixtures { invocant: Value, class: Symbol, role: Option<Symbol> }
frame_fixtures: Vec<FrameFixtures>      // frame-core state, see §2.3
```

`RoutineFrame` gains one `Copy` field, `fixtures: u32`: the index of its entry plus one, or `0`
for a frame without a record (sub frames, inlined blocks, class bodies). A method call pushes
one entry next to the `RoutineFrame` push it already makes
(`push_method_routine_with_location`, 77 Ir) and pops it in the same place. The cost per
call is one `Vec` push of three words and one reference-count increment for the invocant;
a sub call pays nothing. "The innermost routine frame that has a record" (the fixture rule
below) is the top of `frame_fixtures`, an O(1) read.

- **GC.** The entry's `Value` is a strong reference held from Rust, like the slot that holds
  the same invocant today. Under the Bacon-Rajan collector (ADR-0001) such a reference is
  external to every cycle, so it keeps the invocant alive for exactly the frame's lifetime
  and needs no trace hook. Nothing else in the record is a `Value`.
- **Snapshots do not hold indices past their frame.** An index is valid only while its
  frame is on the stack. A reader that keeps a frame view beyond that (a `callframe`
  object, a `CALLER::` view handed to Raku code, a thread spawn) clones the
  `FrameFixtures` entry into the view when it builds the view, never the index.
  `CallFrameEntry`, which is pushed and popped with its frame, may keep the index.

**Fixture rule.** The four fixed names (`self`, `__ANON_STATE#`, `?CLASS`, `?ROLE`) are marked `Symbol::is_frame_fixture`, the same latch mechanism
`Symbol::is_routine_owned` uses: the `Env` lookup learns from one flag read on its miss path
whether the key needs the record. A lookup of a fixture symbol is answered by the
**innermost routine frame that has a record**, then falls back to the env chain (so a
class-body frame, which keeps env keys, still answers). The ~35 name-based readers therefore
keep working unmodified from slice 1; each is then converted to the typed accessor in a
mechanical follow-up, which also lets the fixture rule be deleted for the fixed names. A
reader that *writes* a fixture (the three class-body and role-composition sites that set
`?CLASS` temporarily, `attr_build_defaults.rs` for `self`) converts to a scoped helper that
pushes and pops a record, not an env insert.

**Closures.** A closure created in a frame with a record captures `invocant`, `class` and
`role` as one field of `SubData` (a `Value` clone and two symbols) instead of three env
entries in its capture tier, and a call of it pushes a record built from that field. This is
sound without analysis: capturing is unconditional when a record exists, so a helper called
from inside the closure body that reads `self` sees the closure's lexical `self`, never the
dynamically current method's. (ADR-12529 §2.2: `self` is a slot of the routine that owns it,
reached through the outer link by a nested block.)

### 2.2 When an `Env` exists, and how it stays sound under ADR-12529 (question 2)

A call frame starts **lazy**: its `Env` handle is the caller's handle (`Arc` clone, no new
tier, no allocation) with the frame bits set on the handle (`frame_root`, `outer_is_unit`,
`outer_is_foreign_unit`, `owns_routine_state`, `chain_has_*`; these are already handle
fields, packed in the padding by ADR-12529 slice 6, not stored on the tier). The first
operation that would modify the frame's overlay (`Env::cow_mut`, `insert_sym`, `remove_sym`,
`env_mut()` through any by-name store, the `EVAL`/symbolic/pseudo-package name view) calls
`materialize_frame()`: it does what `scoped_child(parent)` does today, once, and clears the
lazy mark.

- **Reads need no trigger.** A lookup on a lazy frame walks the same chain as it would with
  an empty overlay. The one place a missing tier changes the answer is the routine-owned
  walk (`$/`, `$!`, `$_`, which stop at the owning frame root): a lazy frame that owns
  routine state answers them as *unset* (`Nil`, `Any` for the topic) from its handle bits
  without walking, which is exactly what an empty root tier answers today (#12398, #12902).
  A fixture symbol is answered from the record (§2.1). The static link is unaffected: the
  link is the `outer_is_unit` bit on the handle, and the walk that follows it starts from
  the frame the lookup started in.
- **The trigger is the write itself.** `EVAL`, `::('$x')`, `MY::`, `OUTER::`, `callframe`
  introspection and `CALLER::` writes are by-name writes or reads of the frame's name view;
  the code that implements them already goes through `env_mut()` or builds the view
  (ADR-12529 §2.3), so the first one that runs materializes the frame it runs in, and only
  that one. No compile-time flag *decides* correctness. The existing per-code flags
  (`static_name_lookup`, `eval_scope_names`, `uses_callframe`, `needs_cell_locals`) stay
  what they are: they pre-box cells so a materialized view sees current values (ADR-0018),
  and they may additionally *pre-materialize* a frame whose code is certain to ask, as a
  cost hint only.
- **#14337 cannot recur in this design.** What it records is a process-wide fact that
  forces a copy on frames that never use the name view. Here no global or cross-unit fact
  selects a frame's shape: a frame pays for an overlay iff it executes a by-name write. The
  light-call parameter bind that #14337 found disabled by a global `EVAL` flag is a slot
  bind in this design and has no env to copy. #14337's own close condition (0 deep copies,
  within 5%) is therefore a *test* of this ADR, and its root-cause fix may land earlier on
  its own.
- **Return.** A lazy frame that stayed lazy needs no merge and no restore: the env handle
  is unchanged. A materialized frame's caller env is its `parent`, so the call frame stops
  keeping its own `saved_env` clone: pop restores `parent` (or does nothing for a lazy
  frame), and the frame bits of the caller handle, which push saved in one word, are put
  back.
- **Frames that keep an eager overlay.** Package/class-body frames, `EVAL` bodies, thread
  entry, `gather`/`react` bodies and anything reached through `call_sub_value`'s slow path
  keep materializing at entry: they are not the hot path and their contracts (ADR-0035,
  `&?BLOCK`) read the overlay.

### 2.3 What the saved-register frame shrinks to (question 3)

The empty-for-nearly-every-call containers become **one** lazily created side state:

```
struct FrameSideState {          // frame_side: Option<Box<..>>, see below
    upvalues, local_bind_pairs, frame_authoritative, frame_owned,
    pending_caller_var_writeback, rw_param_rebinds,
}
```

**Where the new state lives (amended on acceptance, 2026-10-23).** `frame_side` and
`frame_fixtures` (§2.1) are `Interpreter` fields of the `frame` subsystem. ADR-10779 D1 keeps
the frame core (`env`, `locals`, `call_frames`, `routine_stack`, ...) on `Interpreter`
itself, so they are not new subsystems' state. `make check-interp-fields` still applies.
The slice that adds a field adds its name to `scripts/interp-fields.d/frame.txt`. Slice 5
removes the six fields it absorbs from the files that list them today: `upvalues`,
`frame_authoritative`, `frame_owned` and `rw_param_rebinds` from `frame.txt`,
`local_bind_pairs` and `pending_caller_var_writeback` from `handoff.txt`. The net change is
five fewer fields. Two of the six are `handoff` channels, values a caller leaves for a callee.
The side state only changes where they are saved and restored across a call. Turning them
into parameters (ADR-10779 D3, AGENTS.md's "a value passed from a caller to a callee is a
parameter") is separate work that this ADR neither blocks nor does.

`push_call_frame` stores the current `Option<Box<FrameSideState>>` in the call frame and
installs `None`. A callee that needs any of the six allocates a fresh state on its first
write (a `side_mut()` accessor, the only mutation path); `pop_call_frame` drops the
callee's state and restores the caller's with one pointer move. The call frame becomes:
slot-frame base, one scope-stack base for the four loop/block scope stacks (one stack of
frames instead of four parallel ones, which is a prerequisite refactor of ADR-0078's
`ScopeStack` users), `saved_stack_depth`, `readonly_mark`, `cur_source_line`, `caller_code`,
`call_capture`, the frame bits word and the side-state pointer: eight to ten words, and the
push/pop is moves and integer stores. The `saved_env` clone and the callee overlay drop
(together ~900 Ir) go with §2.2.

**Samewith and deferral context.** `uses_dispatcher` / `uses_samewith` already gate the
push per body (§1.1), so the per-call push is already absent for a method whose body never
reaches `callsame`/`samewith`/`nextsame`. What this ADR decides is the other half: the
dispatch frame the *outer* call path builds for multi/MRO deferral (the "empty boundary" of
`vm_method_dispatch.rs`) is built only when the resolved entry says a next candidate exists
or the callee body can observe it. That fact is a property of the resolution result
(candidate list and wrappers), so it is stored in the call-site resolution cache entry
(#8880's per-site cache, keyed on the receiver type id and invalidated by the registry
generation) and read from it, not recomputed or pushed per call. No cache exists for
methods yet; until it does, the gate stays per body.

### 2.4 Composition with ADR-0084 and ADR-0110 (question 4)

- **ADR-0084 / #7817** removes the program's names (types, packages, constants, markers)
  from the frame `Env`. It is a precondition of §2.2, not an alternative: a frame whose
  overlay is empty is only cheap to *materialize later* if the chain below it is small, and a
  `by-name` read that falls through a lazy frame must not land in a 400-entry map. This ADR
  adds nothing to ADR-0084's work list and does not wait for its remainder: the remaining
  items (qualified names below a module's top level, #11913, the main program's own routine
  markers) make a *materialized* frame cheaper, not a lazy one correct.
- **ADR-0110 (TRIR)** compiles a routine to typed, resolved ops only if no construct in its
  scope can read or write its lexicals by name (its §4 condition 2). That predicate is
  exactly "this routine never triggers `materialize_frame()` by itself". The two share one
  compile-time bit, `CompiledCode::needs_name_view` (already approximated by
  `static_name_lookup` || `eval_scope_names` non-empty || `uses_callframe`), used by TRIR as
  an eligibility *proof obligation* (it must hold, or the routine declines) and by this ADR
  only as a pre-materialization *hint* (it may be wrong in the cheap direction without
  changing an answer). TRIR's `CallDirect` slot windows are this ADR's end state for typed
  routines: no overlay, no record beyond the invocant, bind straight into slots. This ADR
  extends that shape to untyped methods and subs without requiring TRIR eligibility.
- **#9291 (attribute slots)** is orthogonal: it changes what `invocant` points at, not where
  it lives.

## 3. What this does and does not reach

- It removes the frame's share of #8880: the 2.8k Ir of §1.1 (about 19% of the call),
  minus the cost of the record fields and the fixture rule (a few dozen Ir).
- It does **not** reach 10x on its own (§1.1 fact 1). #8880's close condition stays; the
  rest of the distance is the call-site protocol (the 5.4k Ir before `call_compiled_method`)
  and the non-shape per-call work (`reconcile_attrs`, `method_body_package`,
  `seed_cstruct_fields_for_method`, `has_alias_meta`), each of which is its own issue
  against #8880, not a part of this decision. The ADR's contribution to them is only that,
  once the frame is a record, a per-site inline cache can enter the body without building
  anything first.
- **#8888** (`@a.elems`): a builtin receiver already builds no frame; its 1.9k Ir is the
  site protocol (`exec_call_method_mut_site`, local-slot get/set). The per-site guarded
  inline that skips `CallMethodMut` for a plain-Array receiver, and JIT support for it, rest
  on the same "a call reads slots and a record, never an overlay" property but are not
  decided here; they need their own measurement and, if the guard shape is contested, their
  own ADR.
- Cost contract after the change (ADR-12529 §2.5 restated for methods): call setup is
  O(parameters) with no `Env` allocation; a fixture read is O(1); the first by-name write in a
  frame is O(1) (one tier, no copy of the parent); return is O(1) for a lazy frame.

## 4. Slices (an order, not a progress log)

Each slice passes the gate and is A/B measured per the `perf-tuning` skill on the §1.1
repro (callgrind Ir, second run) plus the bench rows ADR-12529 §3 names; a slice that
regresses a call row is fixed on its branch.

1. **Records and the fixture rule.** Add the `frame_fixtures` stack and
   `RoutineFrame::fixtures` (§2.1), listing the field in `frame.txt`; keep writing the env keys; make the fixture rule answer from the record first
   and assert equality in debug builds. No behavior change; proves the 35 readers.
2. **Stop writing the keys.** The fast method path writes only the record; convert closure
   capture to the one `SubData` field; convert the class-body/role-composition writers to
   the scoped helper. The measurable win: ~1k Ir (inserts, remove, clone).
3. **Convert readers to accessors** and delete the fixture rule for the fixed names.
4. **Lazy frame env** (§2.2): `materialize_frame()`, the handle-level routine-owned answer,
   the parent-based restore. The measurable win: ~1.3k Ir (`scoped_child`, `drop_glue<Env>`,
   `saved_env`).
5. **`FrameSideState` and the single scope-stack base** (§2.3): `pop_call_frame` 774 Ir to a
   small constant.
6. **Plain subs and closures** adopt the same record and lazy frame (`f()` at 3.9k Ir is the
   same shape); the method-specific parts of 2 and 4 are generalized.

## 5. Risks and open questions

- **A reader of a fixture symbol that the inventory in §1.2 missed.** The fixture rule
  (§2.1) is the guard: during slices 1-2 an env read of `self`/`?CLASS`/`?ROLE` is answered
  from the record, and the debug assertion compares it with the env value, so a missed
  reader shows up as a roast or `t/` failure, not as a silent wrong answer. A name built
  dynamically (`::('self')`, `EVAL 'self'`) goes through the name view and the fixture rule.
- **A lazy frame inside a materialized caller** shares the caller's handle; frame bits are
  per-frame state that two nested lazy frames both set. The save/restore of the bits word on
  push/pop (§2.2) is what keeps them apart; a unit test over nesting depth 1..40 with a
  by-name write at each level is part of slice 4.
- **The fixture stack and its index.** Settled on acceptance (§2.1): the records are a
  parallel `frame_fixtures` stack, so `RoutineFrame` stays `Copy` and grows by one `u32`. The
  remaining risk is an index read after its frame popped. A debug assertion in the accessor
  checks that the index is below the stack length. The snapshot rule of §2.1 is what keeps a
  long-lived view from holding an index at all.
- **Cross-thread.** `Interpreter::clone_for_thread` copies a materialized env; the fixture
  records of the spawning frame must be copied with it, since a `start`/`Thread` body
  closure reads its lexical `self`. Closures carry theirs (§2.1), so only a thread entry
  that is not a closure (none today) would need it.
- **Does a lazy frame break `call_capture` or the writeback of `is rw` arguments?** Both
  are keyed on call-frame state, not on the overlay (`call_capture` is a frame field since
  the writeback fix; the rw rebinds are the `rw_param_rebinds` list). To be confirmed by
  slice 4's focused tests, not assumed.
- **JIT.** Native code that enters a method body at ip 0 reads slots; it must not rely on an
  env overlay existing at entry. `try_enter` is called after the frame setup today; slice 4
  checks every helper in `vm_jit_helpers.rs` that touches `env_mut()`.

## 6. Alternatives rejected

- **Static analysis chooses lazy vs. eager per routine** (a compile-time "writes no names"
  proof, then an unchecked fast path). This is the ADR-0110 hazard: correct only while the
  analysis is complete, and mutsu's mutation analysis has already missed writes from
  separately registered methods and `cas` sinks (`S12-construction/roles-6e.t`). The
  copy-on-first-write rule needs no proof, so it cannot be wrong, only slow.
- **A process-global "EVAL exists" flag** that disables the lazy frame. This is #14337.
- **Keep the keys in the overlay and make the overlay cheaper** (arena-allocate the tier,
  intern the inserts). Treats the 2.8k as a constant to shrink instead of removing it;
  repeats the "cost spread over every layer" outcome ADR-12529 §1.4 describes.
- **Store the fixtures in `locals` only** (no record). A slot is per compiled body; a
  runtime helper called from deep in the runtime (registration, deferral, grammar) has no
  body and no slot to read, and closures that never mention `self` would still need a way to
  pass it to such a helper.
- **Do the site protocol first.** Possible and independent, but the frame cost is the part
  that blocks the site cache from entering the body cheaply; the order is a preference, and
  the two tracks can run in parallel (they touch different files).

## 7. Reproduction

```sh
cargo build --profile profiling
# method: #8880's repro (class C { has $.v; method m() { 1 } }; $o = C.new(v => 1);
#   `my int $i = 0; while $i < 100000 { $t = $o.m(); $i = $i + 1 }`; say $t)
# skeleton: the same loop with `$t = 1`; sub: `sub f() { 1 }` with `$t = f()`.
valgrind --tool=callgrind --cache-sim=no --branch-sim=no \
  --callgrind-out-file=tmp/cg_method.out ./target/profiling/mutsu tmp/m14633_method.raku
callgrind_annotate --inclusive=yes --tree=calling --threshold=99.9 tmp/cg_method.out
```

Run each workload twice and use the second (the module cache is cold after a rebuild,
`perf-tuning` §0).

## 8. Amendment A1 (Proposed, 2026-11-02): the lazy frame needs its own handle, and is not worth it alone

Status of this amendment: **Proposed**. It supersedes §2.2's *mechanism* ("the frame's `Env`
handle is the caller's handle with the frame bits set") and records the decision on whether
to build §2.2 now. §2.1 (fixtures), §2.3 (saved registers) and §2.4 are unchanged and
shipped; this concerns only the lazy `Env`.

### 8.1 What the re-analysis found

`frame_root`, `owns_routine_state` and `outer_is_unit` are per-handle bits that today
describe the **frame's own tier**. A lazy frame that reuses the caller's handle would have
to set them to describe the frame while the same handle still has to answer for the
**caller's tier** (the caller keeps running on that handle after the call returns). Two
consequences:

- `Env::get_sym`'s own-overlay-first hit path would answer the *caller's* `$_`, `$/`, `$!`
  for a frame that owns them: routine-owned names must be answered from the frame record
  before the overlay probe.
- Every consumer that enumerates the overlay (`iter`, `keys`, `len`, `overlay_iter`,
  `frame_writes`, `merge_method_env`, about 80 sites) must learn that a lazy frame's overlay
  is *empty*, not the caller's.

So the §2.2 wording ("same handle, bits on the handle") is unsound as written. The sound
form is a **separate virtual-tier handle**: the frame record holds `FrameView { parent:
Env, bits }` (the frame's identity and bits) and only `materialize_frame()` produces a real
`Env` from it. Reads on a lazy frame consult the `FrameView` for routine-owned names and
fall through to `parent`; the handle the caller keeps is never mutated.

### 8.2 What it is worth

Measured on `main` @ `e2a70e8d`, `for ^N { $t = $o.m() }` (zero-argument method returning
`1`), callgrind, profiling build, slope between N = 1,000 and 3,000: **10.8k Ir per call**
(14.5k when this ADR was written). The frame's `Env` share is `Env::scoped_child` 211 +
`Env::clone` ~84 + the `Env` drops ~220-380, i.e. 500-650 Ir. A lazy frame still pays a
clone of `parent` and its drop, so the realistic gain is **~400 Ir, about 3.7%** of the call.
No other single item of the plain method call is above ~8% (`pop_call_frame` 876 is the
largest and is mostly `Vec::truncate`, `FrameFixtures` drop and the dispatch-frame pop).

### 8.3 Decision

1. **Do not build the lazy `Env` now.** By the arithmetic in the `perf-tuning` skill (5a), a
   3.7% slice against a 10x-rakudo goal is not the structure that moves the ratio, and it
   carries a contract change over ~80 overlay consumers with a compatibility risk
   (`CALLER::`, `OUTER::`, `callframe`, `EVAL`) for the gain.
2. **If it is built later, it is built as §8.1's `FrameView`**, never as bits on the
   caller's handle, in one PR that also removes the saved-caller-handle clone and drop
   (`saved_env`), so the ~400 Ir is paid for by deleting the caller-handle bookkeeping and
   not by adding a second mechanism beside it.
3. **The trigger to revisit** is the call-site protocol work (the ~5.4k Ir before
   `call_compiled_method`, §3): once that lands, 400 Ir is ~7% of the remaining call and the
   change is worth its consumer sweep.

Nothing else in this ADR changes, and no code depends on this amendment.
