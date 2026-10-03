# ADR-11142: Readonly-ness is a property of the binding, not of a dynamically scoped name

- **Status**: Accepted (user decision 2026-10-03). Partly implemented; see §7.
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11142](https://github.com/tokuhirom/mutsu/issues/11142)
- **Related**: [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md) (a binding's metadata
  lives on a slot-addressed descriptor; this ADR applies it to readonly-ness and settles the part
  ADR-0097 §2 left as "a readonly-by-alias parameter" in the runtime half),
  [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) (the same move for
  type constraints), [ADR-0055](0055-closure-free-vars-resolve-to-their-own-binding.md) (a closure's
  free variable resolves to its own binding), ADR-0097 §14 (the binding cell)

## 1. Context

### 1.1 The mechanism today

Whether `$x = 1` is allowed is answered by one process-wide registry,
`Interpreter::readonly_vars` (`ReadonlySet`, `src/runtime/mod.rs`). It maps a **bare name** to a
`ReadonlyKind` (`Alias`, `Immutable`, `ImmutableValue`, `ImmutableDeep`, `TypeObject`):

- Every routine and method call opens a scope (`enter_readonly_frame`) and marks each non-`is rw`
  scalar parameter by name. Exit replays an undo journal (`ReadonlyUndo`, `exit_readonly_frame`,
  `ReadonlyFrameGuard` for panic unwinds).
- `for`/`given`/`with` topics, `for` aliases, `my $x := 42`, `constant`s and sigilless terms mark
  and unmark their names the same way.
- `OpCode::CheckReadOnly` and about twenty other consumers ask the registry by name: increments,
  `.VAR`/`=:=` (`scalar_name_has_no_container`), method-lvalue refusals, `is rw` argument checks.

About 205 call sites outside the registry's own two files use this API (2026-10-03 count).

### 1.2 Why it is wrong

A name is not a binding. The registry follows the **dynamic** call stack, but Raku decides
readonly-ness **lexically**: a write refers to the binding its name resolves to in the writer's
scope. Whenever the writer and the binding's frame differ, the registry answers with whatever
frame happens to be on the stack under that name. Each instance found so far was patched
separately:

| issue | shape | patch |
| --- | --- | --- |
| #10389 | closure / nested `my sub` writes its captured variable; caller has a same-named readonly param | record readonly state at code-object creation, reconcile on entry (`readonly_capture.rs`) |
| #10400 | nested sub called by name from a closure | fold `nested_sub_written_free` into the record |
| #11054 | class method writes an outer variable | snapshot the declaring frame onto `MethodDef`, reconcile on method entry |
| #11070 | top-level sub called through a code value | snapshot onto `FunctionDef`, reconcile when the code value is called |
| #11142 | caller's param shares its name with an outer `my $x := 42` | **open**: the caller's `Alias` mark re-kinds the `Immutable` one, and the reconcile drops it, so the write goes through |

Every patch adds a record (`SubData`/`MethodDef`/`FunctionDef::captured_readonly`) that tries to
reconstruct, at call time, a fact the binding already had. #11142 shows the limit of that
approach. The registry holds one entry per name, so when a caller's parameter shadows an outer
immutable binding, the outer kind is overwritten. No later reconcile can bring it back.
Declaration-time snapshots are also taken at hoist time, before later binds in the same scope
have run. That is why the reconcile already has two rules (creation-time vs. declaration-time,
`ReadonlySnapshot::at_declaration`).

### 1.3 It also costs on every call

Every call marks every scalar parameter and unmarks it on return. The registry grew a 64-slot
direct-mapped cache because "`bench-fib` spent ~6% of its cycles there" (comment on
`ReadonlySet::cache`), plus a scope sentinel and a cancellation peephole to keep the journal
flat. Most of these marks are facts the compiler already knows: a parameter without
`is rw`/`is copy`/`is raw` is readonly, by declaration.

### 1.4 How rakudo models it

In rakudo, a lexical slot holds either a `Scalar` container or a bare value. A non-rw parameter
is bound to the decontainerized value, `my $x := 42` binds the slot to `42`, and assignment fails
because no writable container is there. Readonly-ness *is* what the slot holds, so it travels
with the binding by construction: into closures, through `OUTER::`/`CALLER::`, and into methods
that reach the variable lexically.

mutsu cannot copy the "bare value means readonly" rule directly. Its plain locals hold unboxed
values with no `Scalar` behind them (NaN-boxing, ADR-0001), so a bare value is the normal
*writable* case. The kind has to be recorded explicitly, but on the binding, as rakudo
effectively does.

## 2. Decision

**A binding's readonly kind is stored with the binding, and every check resolves the binding
first and then asks it. No process-wide or dynamically scoped name-keyed readonly state
remains.** `readonly_vars`, `ReadonlySet`, `ReadonlyUndo`, the readonly part of
`ReadonlyFrameGuard`, `enter_readonly_frame`/`exit_readonly_frame`, `readonly_capture.rs`
(`capture_readonly_state`, `capture_declaring_readonly_state`, `reconcile_captured_readonly`) and
the three `captured_readonly` fields are deleted at the end of the migration.

Where the kind lives depends on when it is known and on who can reach the binding. This follows
ADR-0097's two halves:

1. **Compile-time kinds go on the declaring chunk's `BindingDesc`.** A scalar parameter without
   `is rw`/`is copy`/`is raw`, a `constant`, and a sigilless term are readonly by declaration.
   A write through such a local slot needs no runtime probe: the compiler emits the refusal, or
   omits `CheckReadOnly` for a slot that is statically writable.
2. **Runtime kinds go in the runtime half: a per-frame, slot-parallel kind array in `Locals`.**
   This covers kinds decided when the bind executes: `:=` to a value or a type object,
   `for`/`given`/`with` aliases and the topic, and a `my $y := $x` that copies `$x`'s kind.
   Entering a call does nothing to the readonly state of any other frame.
3. **A binding reachable from another frame carries its kind in its shared home.** Such a
   binding is captured by a closure, reached by a method, sub or `EVAL` through the unit or outer
   scope, or addressed by `OUTER::`/`CALLER::`. Its home is the **binding cell** (ADR-0097 §14):
   the per-binding indirection that the frame slot, its env entry and every capturer already
   share. The cell gains an optional `ReadonlyKind`. A writer from another frame resolves the
   name to that cell, exactly as it already resolves the value, and the cell answers.
   The kind belongs to the binding cell, not the `ContainerCell`. Two names may share one
   container and still differ in readonly-ness (`sub f($x)` called as `f($v)`, ADR-0097 §8). The
   existing `ContainerCell::readonly` flag stays what it is today: a property of an element
   bound to a bare value.
4. **A consumer that only has a name** (a by-name `SetGlobal`, `.VAR`, an `is rw` argument check
   driven by `arg_sources`, symbolic `::("$name")`) resolves the name in the *current* frame's
   scope chain (local slot, then env/binding cell, as reads already do) and asks the binding it
   found. It never asks a set shared across frames.

The invariant, stated for review:

> **A readonly decision is a function of the binding the write resolves to, never of which
> frames are on the call stack.**

## 3. Consequences

- **The bug class goes away by construction.** In #10389, #10400, #11054, #11070 and #11142 the
  writer reached the right binding but asked the wrong question. Once the answer lives on the
  binding, a caller's parameter cannot affect it, and nothing needs to be recorded or reconciled
  on entry.
- **Calls get cheaper.** Parameter readonly-ness becomes a compile-time fact, so the per-call
  mark/unmark, the journal, the scope sentinel and the 64-slot cache all go. Expect a measurable
  drop on call-heavy benches (`bench-fib` attributed ~6% to the mark path before the cache).
  `scripts/bench-det.sh` is the measure, not wall clock (ADR-0097 §1.4).
- **The JIT gets a static answer.** A statically writable slot needs no `CheckReadOnly` at all.
- **Cost moves to binding cells.** A readonly binding that is captured, or otherwise reachable
  from another frame, needs a binding cell with a kind. Uncaptured bindings (the common case)
  pay nothing beyond a slot-array entry.
- **The topic's save/override/restore dance** (`restore_topic_readonly`,
  `unmark_readonly_topic` on every call) becomes per-frame slot state. It no longer needs to
  undo a mark that leaked in from the caller.

## 4. What must keep working

These tests pin behaviour that the registry got right and the migration must keep:

- `t/routines/signature/closure-captured-var-ignores-callers-readonly-param.t` (#10389/#10400)
- `t/oo/method/method-outer-var-ignores-callers-readonly-param.t` (#11054)
- `t/routines/signature/routine-code-value-outer-var-ignores-callers-readonly-param.t` (#11070)
- `t/io/copy-param-shadows-caller-readonly.t` (an `is copy`/`is rw` param shadows a caller's
  readonly name)
- `t/oo/given-readonly-topic-does-not-leak-into-routine.t`
- the `ImmutableDeep` loop-topic cases (ADR-0097 §12)
- `ReadonlyKind`'s four error wordings: `X::AdHoc` "readonly variable", "immutable value",
  `X::Assignment::RO` naming the value, and the type-object wording (#9730)

The #11142 repro must start refusing the write with "Cannot assign to an immutable value", as
rakudo does.

## 5. Alternatives rejected

- **Keep the registry and keep patching** (the #10389 → #11070 line). Each fix reconstructs a
  binding's fact from a snapshot, and #11142 shows a case no snapshot can recover: the shadowed
  kind is gone from the registry. The reconcile already has two rules for two snapshot origins.
  A third origin (EVAL, `CALLER::`) would add a third.
- **Narrow fix for #11142**: when the reconcile drops an `Alias` mark, restore the kind that the
  journal's `Rekinded` entry shadowed. This makes the specific repro pass. It couples the
  reconcile to the journal's internal shape, leaves the per-call cost, and fixes nothing for the
  next frame-crossing writer.
- **Put the kind on the `ContainerCell`** (ADR-0042 generalized). This is wrong for this
  property: readonly-ness belongs to the name's binding, and two bindings can share one
  container (§2.3).
- **Wrap every readonly parameter value in a readonly cell at bind time** (rakudo's model taken
  literally). This adds an allocation or an extra decode to every parameter of every call, the
  hottest path in the VM. A compile-time kind costs nothing per call for the same parameters.
- **Make the registry per-frame instead of process-wide.** This fixes nothing on its own: a
  frame-crossing writer would still need to find the *owning* frame, and that is binding
  resolution. Once the binding has been resolved, it might as well answer the question itself.

## 6. Slices

Each slice keeps `make test`/`make roast` green. During the transition the registry stays the
source of truth, and the binding-side answer is cross-checked against it under `debug_assert!`
(ADR-0097 §15's verification pattern). The known-divergent shapes are the bugs in §1.2 and are
exempted by name.

1. **Representation.** Add the `ReadonlyKind` slot to `BindingDesc`, the runtime-half kind array
   in `Locals` (grown in lockstep by `push_frame`/`pop_frame`/`refill_slots`/`resize_slots`,
   ADR-0097 §13.2), and the optional kind on the binding cell. No behaviour change.
2. **Writers record on the binding.** Parameter binding (static kind; the binder writes nothing),
   `for`/`given`/`with`/topic aliasing, `:=` binds, `constant`, sigilless terms. A readonly
   binding that the compiler marks captured (`needs_cell_ref_capture_slots`, the
   `needs_cell_*` sets) gets a binding cell carrying its kind. The registry is still written too,
   and debug builds assert agreement.
3. **Readers ask the binding.** `CheckReadOnly` (static fast path for local slots), the by-name
   checks, increments, `.VAR`/`=:=`, method-lvalue refusals, `is rw` argument checks. The
   #11142 repro passes here.
4. **Delete.** The registry, the journal, `ReadonlyFrameGuard`'s readonly half,
   `readonly_capture.rs`, the `captured_readonly` fields on `SubData`/`MethodDef`/`FunctionDef`,
   and the per-call marking in the light, fast and general call paths and in method dispatch.
   Measure with `scripts/bench-det.sh`.

Slice 2's capture rule depends on ADR-0097 §11.5/§13.2's open question: does the compiler's
capture bookkeeping identify every slot that another frame can reach? Answer that by
investigation, not by assumption, before slice 2 lands. An unaudited path then shows up as a
debug assertion rather than a wrong answer.

## 7. Implementation status

The decision was taken on 2026-10-03 after #11142. The interim patches
(#11085, #11134) stay in place until slice 4 deletes them.

### 7.1 First slice: immutable `:=` bindings carry their kind (#11142, 2026-10-03)

This covers slices 1-3 for one writer, the runtime immutable bind:

- **Representation.** `ContainerCell`'s `readonly` flag is now an encoded
  `ReadonlyKind` (`ContainerCell::readonly_kind`). The element bind
  (`BIND-KEY`) keeps the `Immutable` kind it always meant.
- **Writer.** A `$` variable bound straight to a value or a type object
  (`my $x := 42`, `my $t := Int`, and the same rebinds) seats that value in a
  binding cell carrying `Immutable` or `TypeObject`
  (`ContainerCell::new_readonly_binding`, in `exec_set_local_op_inner`). The
  cell goes into the slot and the env entry, so every holder of the binding
  shares it: the in-sequence sub registration that captures it into
  `unit_lexicals`, a closure capture and a `my $y := $x` alias. A value that
  reads differently behind a cell (a `Range`, `Seq`, `Slip`, lazy or immutable
  `List`) keeps a bare slot and only the registry mark.
- **Reader.** `CheckReadOnly` on a free variable (a name with no local slot in
  the writing code) resolves the name to its raw binding in the order a read
  does (the running routine's unit lexical, then the env chain), follows the
  binding-cell chain, and, when a cell carries a kind, raises that kind's error
  worded from the bound value. The registry is not asked then. Gated on
  `readonly_binding_cells_possible()`, so a program with no such bind pays
  nothing.

Not covered yet: every other readonly writer (parameters, `for`/`given`
aliases and the topic, `constant`, sigilless terms, `my $y := $ro-param`)
still records only in the registry. So a free-variable write that resolves to a
binding without a kind still falls back to the registry. #11165 is the case
where that fallback is wrong for a *writable* binding. The binding's answer can
only become final for free variables once slice 2 covers every readonly writer.
