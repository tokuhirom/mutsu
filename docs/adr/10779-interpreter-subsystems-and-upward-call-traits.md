# ADR-10779: `Interpreter` subsystems and traits for upward calls

- **Status**: Accepted (2026-10-03, by the maintainer)
- **Issue**: [#10779](https://github.com/tokuhirom/mutsu/issues/10779)
- **Input**: [docs/interpreter-state-map.md](../interpreter-state-map.md) (phase 2),
  `make check-layer-deps` (phase 1)

## Context

`struct Interpreter` (`src/runtime/mod.rs`) has 438 fields. Its methods are spread over about 690
files under `src/runtime/`, `src/vm/` and `src/trir/`, and every method can read and write
every field. The lower layers (AST, parser, `Value`, `opcode`, `Env`, GC) also name the layers
above them. Together these make the module graph a mesh: no subsystem has an interface, and the
crate cannot be split.

Phase 1 of #10779 moved misplaced pure helpers down and added the `check-layer-deps` ratchet.
Over seven slices (#10809 … #11130) the upward references from the lower layers fell from 208
to 71. Phase 2 (#11141) mapped which files touch each field and grouped the fields into
subsystems. That analysis found:

- Only 12 fields are touched from 30 or more files (`env` 334 files, `registry` 184, `stack` 115,
  `locals` 91, ...). 53% of the fields are touched from at most 3 files.
- Every field falls into one of 15 subsystems. Apart from `frame` and `types`, they overlap
  little: excluding `frame`, `types` and `handoff`, no two subsystems share more than 39 files.
- 39 `pending_*`-style fields are not state at all. They are arguments that a caller sets and
  the callee takes (`set_pending_call_arg_sources` / `take_pending_call_arg_sources`).
- Co-occurrence clustering does not recover the subsystems, because accessor files link
  unrelated fields. The grouping has to be decided by meaning.

The remaining 71 upward references are the ones that a move cannot fix:

- **parser (24)**: mostly pure catalog lookups (`is_known_type_constraint`,
  `is_builtin_enum_value`, `CType::from_type_name`, `rakudo_declares_infix`,
  `validate_regex_structurally`, `source_has_no_precompilation`, ...), which can still move down.
  Two are essential: `run_slang_activation` and `probe_module_exports`. Both run user code at
  parse time, and both are instance-free: each spawns a thread with a fresh `Interpreter`.
- **value (14)**: the promise/await/wake path calls the worker pool (`submit`,
  `keeper_mark`, `defer_until_yield`), `vm_poll::poll` and the Supply serialize tickets.
  `seq_body` names `MapGrepPlanSlot`, and `SubData` names `vm::LightDefParam`.
- **gc (2)**: `stw` calls `worker_pool::enter_blocking`.
- **opcode (31)**: `CompiledCode` embeds compiler, VM and TRIR types (`LexScopeChain`,
  `CodeSnapshot`, `TrChunk`, `TrCallSite`, `ScalarParamBind`) and calls compiler scans while it
  is built.

## Decision

### D1. Target shape: `Interpreter` = the frame core plus one field per subsystem

`Interpreter` keeps the execution core (the `frame` subsystem: `env`, `stack`, `locals`,
`call_frames`, `routine_stack`, `current_code`, ...), because that is what the VM is. Each other
subsystem becomes its own type, which:

- owns its fields privately and exposes methods, as `OutputSink`, `TapState`, `IoHandleTable`,
  `Registry`, `RoutineStack`, `SharedStore`, `OnceStore`, `CurRepoState` and `MarkContextState`
  already do;
- receives the `Interpreter` methods that touch only its own state, as its own methods;
- states its thread policy (share, copy, or start fresh) in a `fork_for_thread(&self) -> Self`
  method next to its definition. `clone_for_thread`'s 438-field literal becomes one line per
  subsystem.

`Interpreter` then holds `self.caches`, `self.modules`, `self.dispatch` and so on. A method that
needs two subsystems stays on `Interpreter` and borrows the two fields separately.

### D2. The subsystems and their order

The 15 groups are the `SUBSYSTEMS` rules in `scripts/interp-field-matrix.py`. That script is the
source of truth for which field belongs where. A new field that matches no rule is reported as
unclassified and has to be placed deliberately. Extraction order, smallest and best bounded
first:

1. `guards` (4 fields), then `caches` (51 fields, 44 files; derived state already started fresh
   in a spawned thread, and mostly invalidated by `GenCache` generations: one
   `ResolutionCaches` type with a single invalidation entry point);
2. `regex`, `eval`, `threads`, `async`;
3. `io` (part of it already extracted), `control`, `topic`, `dispatch`, `lexicals`;
4. `module` (71 fields, the largest bounded one);
5. `types`: `registry` is already its own type. The question to settle when this step is
   reached is whether `current_package`/`current_package_sym` belong to `frame` (they are the
   running code's lexical package) or to `types`.

One subsystem per PR. A PR moves fields and methods without changing behavior.

### D3. `handoff` fields become explicit parameters, not a struct

The 39 `handoff` fields are removed one at a time: each set/take pair becomes a function
parameter or a return value along the call path that carries it. Collecting them in a
`HandoffState` struct is rejected (see Alternatives). This is also a correctness gain: a field
that is set and then not taken (an early return, an error path) leaks into the next call.

### D4. No new direct fields: a field-count ratchet

The number of direct fields of `Interpreter` may only fall. Concretely: a check (a `--check`
mode of `scripts/interp-field-matrix.py`, wired into `make checks` as
`make check-interp-fields`) fails when the count grows
or when a field matches no `SUBSYSTEMS` rule. New state goes into the subsystem type it belongs
to. This is what keeps the extraction from being undone by later features, as the
`check-layer-deps` ratchet does for module edges.

*Amendment (2026-10-03).* The baseline is a list of allowed field **names**, not a count, and it
is never rewritten. A count was one line that every extraction PR had to rewrite, so any two
PRs in flight (and every rebase of a stacked one) conflicted on it. `scripts/interp-fields-baseline.txt`
now holds the names the struct had when it was cut and is frozen; a PR that extracts a subsystem
allows its new holder field by adding its own file `scripts/interp-fields.d/<subsystem>.txt`.
A field allowed by neither fails the check, with its name; an allowed name that is no longer a
field is ignored. No shared file is edited, so parallel PRs cannot conflict. The decision — no
new direct fields — is unchanged.

### D5. Upward calls go through traits defined below and implemented above

A lower layer that needs a service from the runtime declares a narrow trait for it. The runtime
implements the trait. Which of two forms to use depends on whether the service needs a specific
interpreter:

- **Instance-bound** (the service reads one interpreter's state): the trait object is a
  parameter. The precedent is `value::signature::SubsetBases` (#11130): building a `Parameter`
  needs a subset's base type, so the builders take `Option<&dyn SubsetBases>`, and
  `Some(self)` call sites coerce unchanged.
- **Process-global** (the service does not depend on an instance): the lower layer holds a
  `OnceLock<&'static dyn Trait>` (or a `fn` pointer) that the runtime registers once at
  startup. The precedent is `wasm_sched::set_timer_source` (#10909). This form covers:
  - the parser's **compile-time host**: `run_slang_activation` and `probe_module_exports`,
    both instance-free (each spawns a thread with a fresh `Interpreter`);
  - the **scheduler** used by `value`'s promise path and by `gc::stw` (`submit`,
    `keeper_mark`, `defer_until_yield`, `enter_blocking`, `vm_poll::poll`).

  An unregistered host is a programming error (`expect` with a message naming the missing
  registration), not a silent no-op. Only a test that drives the parser without a runtime can
  hit it.

Traits are for cold edges: parse-time execution, promise scheduling, error construction. A
hot VM path never goes through a trait object. If a hot edge appears, the data it needs moves
down instead.

### D6. Pure catalog lookups keep moving down

The phase-1 rule still holds: a pure helper or table that a lower layer calls moves down, into
`value/` or a leaf module listed in `check-layer-deps`'s `LOWER`, and its old path stays as a
re-export. The parser's catalog lookups and `source_has_no_precompilation` are in this class.
A trait is only for an edge that really calls back into the runtime.

### D7. The crate split is not decided here

Phase 4 (an `ast` + `value` + `parser` + `env` + `gc` lower crate) is a separate decision with
these preconditions:

- `check-layer-deps` reaches 0 for those modules;
- the `opcode` placement question below is resolved;
- a measurement shows the split pays for itself. A release build is not incremental, and
  `cargo build --timings` / `-Z time-passes` should show how the lib's time divides between the
  frontend and codegen. The expected build gain is modest (roughly 20-30% at best), so the
  goal of phases 1-3 is maintainability. A faster build would be a bonus, not the justification.

## Alternatives considered

- **Extract subsystems by co-occurrence clustering.** Rejected: phase 2 tried it, and the 90
  clusters follow file layout (the accessor files) instead of meaning.
- **Move the VM out of `Interpreter` first.** Rejected: `frame` is the VM, and 402 files touch
  it. Moving the *other* subsystems out shrinks `Interpreter` to the VM without touching the
  hottest code.
- **A `HandoffState` struct for the `pending_*` fields.** Rejected: it would give an
  implicit-argument smell a name and a type, and leave the leak-on-early-return hazard in place.
- **Thread-locals or process globals per subsystem.** Rejected: they hide state, make
  `clone_for_thread`'s per-thread policy implicit, and break the "one interpreter per thread"
  model.
- **`&mut Interpreter` passed down to the parser (or a `dyn` on hot paths).** Rejected: the
  lower layer would still name the runtime, and a `dyn` on a hot path costs an indirect call per
  operation.
- **Split the crate first and sort out the cycles inside it.** Rejected: Rust permits inherent
  `impl`s only in the defining crate, so the cycles have to go before the split, not after.

## Open questions

- **`opcode` / `CompiledCode` placement.** `value::SubData` owns `Arc<CompiledCode>`, so if
  `value` is in the lower crate, `CompiledCode` must be too. But `CompiledCode` embeds compiler,
  VM and TRIR types. There are two options: (a) move those types down with it, so the lower
  crate grows to include the bytecode representation; or (b) make `SubData` hold the compiled
  body through an opaque handle that the upper crate downcasts. A downcast on every call costs
  something, so this needs measuring before phase 4.
- **`current_package`'s home** (D2 step 5).
- **Thread policy of each cache.** `clone_for_thread` starts all caches empty today. Whether some
  (the resolution caches of a large loaded program) should be shared should be measured on a
  spawn-heavy benchmark, not assumed.

## Implementation status

- Phase 1 (move misplaced helpers down, `check-layer-deps` ratchet): 208 → 71 upward references
  in #10809, #10837, #10871, #10909, #10973, #11120, #11130. Ongoing under D6.
- Phase 2 (state map): done in #11141.
- Phase 3 (this ADR): D4 ratchet landed (`make check-interp-fields`, baseline 439).
  - `guards`: done. `RakuCycleGuards` (`src/runtime/raku_cycle_guards.rs`) holds the two
    `.raku` render guards as one generic `CycleGuard<K>`; 439 → 436 fields.
  - `caches`: done. `ResolutionCaches` (`src/runtime/resolution_caches.rs`) holds the 51
    cache fields, all started fresh in a spawned thread; 436 → 386 fields. A single
    invalidation entry point is a follow-up: this step only moved the fields.
  - `regex`: done. `RegexGrammarState` (`src/runtime/regex_grammar_state.rs`) holds the 10
    regex/grammar/slang fields, all started fresh in a spawned thread; 386 → 377 fields.
  - `module` visibility (out of order, from ADR-11136's needs): `ModuleVisibility`
    (`src/runtime/module_merge.rs`) took the seven ADR-11136/#7797 tables instead of adding
    them to `Interpreter`.
  - `async`: done. `AsyncState` (`src/runtime/async_state.rs`) holds the 23
    gather/lazy-pull/supply/react fields, all started fresh in a spawned thread; 375 → 353.
  - `eval` is deferred: its rule mixes the MAIN fields (copied into a spawned thread) with
    `pending_eval_*`/`pending_supply_*`, which are set-then-taken side channels and so fall
    under D3 (explicit parameters), not into a struct. The rules need splitting first.
  - `threads`: done. `ThreadSharing` (`src/runtime/thread_sharing.rs`) holds the 13
    shared-store/masking/lock fields. Its policy is the first non-uniform one, and it is now
    one method: `fork_for_thread(captured_scalars)` makes the store a child lineage, seeds the
    redeclared set from the block's captured scalars and the parent's parameter shadows,
    shares the two dirty sets and starts the rest fresh; `root()` is the main interpreter's.
    353 → 341 fields.
  - `topic`: done. `TopicState` (`src/runtime/topic_state.rs`) holds the 22 `$_`-source,
    given/when, smartmatch-context and per-loop scope fields, all started fresh in a spawned
    thread; 340 → 319 fields.
  - Next: `io`, `control`, `dispatch`, `lexicals`.
