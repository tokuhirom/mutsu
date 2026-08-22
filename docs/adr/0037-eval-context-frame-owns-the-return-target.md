# ADR-0037: `EVAL ..., context => $frame` — the context frame owns the return target, and the routine chain must be dispatch-path-independent

- Status: Implemented — all five slices landed (see "Implementation status"
  below)
- Date: 2026-08-20
- Origin: `todo/deep/eval-context-frame-owns-the-return-target.md`
- Related: [ADR-0035](0035-method-calls-observe-caller-frames.md) (same family —
  frame observation that a frameless dispatch path silently elides; its
  rejected alternative (c) named the parallel-stacks unification this ADR
  takes a first step toward);
  [ADR-0050](0050-block-routine-ness-is-a-definition-site-property.md) (the
  *other branch* of §1.2's `in_routine` derivation — this ADR owns the
  `is_eval_unit == true` branch, ADR-0050 owns the `else` branch every
  re-compiled closure body takes)

## 1. Context

`EVAL $code, context => $ctx` compiles `$code` **as if it stood at `$ctx`'s
frame**. mutsu takes the *package* from the context value
(`news/2026-08/eval-context-argument.md`) and nothing else, so everything else
about the snippet — including where its `return` goes — is still resolved
against the frame that happens to call `EVAL`.

This is the last systemic blocker named in
`todo/deep/vendor-real-test-module.md`'s residue table for
`t/throws-like-gather-sink.t` and part of `t/emit-done-controlflow.t`, because
rakudo's real `Test.rakumod` writes exactly this shape:

```raku
my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::;
subtest {
    ...
    EVAL $code, context => $caller-context;
    ...
    CATCH { default { pass $msg; ... } }
}
```

so every `throws-like '<code that returns>', X::ControlFlow::Return` reports
the code as not having died.

### 1.1 What raku actually does (measured, not assumed)

Three probes against `raku` on this machine, 2026-08-20:

**(a) Context frame is a mainline (no enclosing routine) — throw at the
`return` site, do not unwind.**

```raku
sub thrower() {
    my $ctx = CALLER::;              # names the mainline
    my $r = 'no-throw';
    { EVAL 'return 1', context => $ctx; CATCH { default { $r = .^name } } }
    say "thrower saw: $r";
    return 'thrower-end';
}
say thrower();
say "still alive";
```

```
raku                                    mutsu (main @ e13d278ff)
thrower saw: X::ControlFlow::Return     1
thrower-end                             still alive
still alive
```

The exception is raised **where the `return` executes**, so the `CATCH` that
lexically surrounds the `EVAL` catches it and `thrower` runs to completion.
mutsu instead emits a raw CX::Return signal that unwinds out of `thrower`, and
the snippet's value (`1`) becomes `thrower`'s return value.

**(b) Context frame is a live routine — the return targets *that* frame, past
any intervening ones.**

```raku
sub thrower($code) { my $ctx = CALLER::; EVAL $code, context => $ctx; ... }
sub caller-is-a-routine() { my $x = thrower('return 1'); say "got: $x"; return 'car-end' }
say caller-is-a-routine();
say "still alive";
```

```
raku            mutsu
1               caller-is-a-routine got: 1
still alive     car-end
                still alive
```

raku's `return` unwinds **past `thrower`** and returns `1` from
`caller-is-a-routine` — the frame the context names. mutsu returns from
`thrower`, the frame that called `EVAL`. So the fix is not merely "throw when
the context has no routine": the context frame genuinely *owns* the return
target.

**(c) Context frame's routine has already exited — `X::ControlFlow::Return`
with out-of-dynamic-scope.** Both agree on the class today; only the message
differs (raku: "Attempt to return outside of immediately-enclosing Routine
(i.e. `return` execution is outside the dynamic scope of the Routine where
`return` was used)"; mutsu: "Attempt to return outside of any Routine").

And the *un*contextualised behaviour is correct and must stay:
`sub f() { EVAL 'return 1'; return 2 }` answers `1` in both.

### 1.2 How mutsu decides today

`return` is classified at **compile time**. `Compiler::is_routine` selects
between two opcodes (`src/compiler/stmt.rs`, `Stmt::Return`):

- `OpCode::Return` → `RuntimeError::return_signal(val)`, a control signal that
  unwinds to a routine boundary;
- `OpCode::ReturnFromNonRoutine(lexically_in_routine)` → either the same signal
  (when a routine lexically encloses) or, with no lexical routine at all,
  `RuntimeError::controlflow_return(false)` — a real `X::ControlFlow::Return`
  thrown right there (`src/vm/vm_exec_dispatch.rs:4100`).

For an EVAL'd compilation unit that flag comes from the **ambient** interpreter
state at EVAL time (`compile_block_value_opts`, `src/runtime/resolution_eval.rs`):

```rust
let in_routine = if is_eval_unit { self.enclosing_routine_exists() }
                 else            { !self.routine_stack.is_empty() };
```

`enclosing_routine_exists()` (`src/runtime/accessors_stack.rs:26`) answers "does
any live `RoutineFrame` have `is_block == false`". The context argument never
enters this decision — it is consulted only for the package
(`builtins_eval_misc.rs:322`, `eval_context_package`).

The targeting half already exists and is unused here: `RuntimeError` carries
`return_target_callable_id` (`src/value/error.rs:105`), a bare block's `return`
inherits the lexically enclosing routine's id out of the closure's captured
`__mutsu_callable_id` (`src/vm/vm_closure_dispatch.rs:862-895`), and every
routine boundary declines a signal whose target is not itself
(`src/vm/vm_call_named_inner.rs:327-336`). That is exactly the machinery probe
(b) needs; nothing points it at the context frame.

### 1.3 The ambient answer is itself unsound — the frameless light paths

Verifying the finding surfaced a second, independent defect that had been
*masking* it. `push_routine_with_location` is called from exactly two places —
`src/vm/vm_call_fast.rs:168` (the 0-arg fast path) and
`src/vm/vm_call_named_inner.rs:90` (the full named path). The two **light**
sub paths — `call_compiled_function_positional_light` and its named/mixed
sibling (`src/vm/vm_call_light.rs`, `vm_call_light_typed.rs`) — push no routine
frame at all. So inside a sub that qualifies for either of them,
`enclosing_routine_exists()` answers **false** even though a routine plainly
encloses.

Measured matrix (`tmp/eval-path-matrix.p6`), one sub shape per dispatch path,
each body `{ EVAL 'return 1'; return 2 }`, raku answers `1` for all six:

| declaration | path | mutsu |
| --- | --- | --- |
| `sub zero()` | fast (pushes a frame) | `1` — correct |
| `sub pos1($x)` | **positional-light** | `X::ControlFlow::Return` — wrong |
| `sub named1(:$x)` | **light** | `X::ControlFlow::Return` — wrong |
| `sub arr(@x)` | full named (excluded from light) | `1` — correct |
| `sub opt($x?)` | full named | `1` — correct |
| `sub slurp(*@x)` | full named | `1` — correct |

Confirmed under `rust-gdb -batch` by breaking on
`call_compiled_function_positional_light` for the `sub d($x)` case (the
breakpoint fires; the frame push never happens). Methods are unaffected — both
method chokepoints push (`vm_method_dispatch.rs:675`/`:1660`) — as is any sub
reached through a frame-pushing path.

**This is why the origin ticket's headline repro appears to pass on `main`
today.** Its `sub thrower($code)` takes a parameter, so it runs frameless, so
`in_routine` is `false`, so the snippet's `return` throws — the right answer
for the wrong reason. Rewriting the same repro with a 0-arg `thrower` (§1.1(a))
reproduces the documented wrong output exactly. Any fix for the context
mechanism that is validated against a param-bearing repro will therefore
validate against noise.

The same gap explains sibling defects with the same origin:
`caller_frame_package()` (`accessors_stack.rs:129`) reads
`routine_stack[len - 2]` and answers `GLOBAL` when the caller ran frameless —
which is the very value §2.2's stamp site records; `executing_source_file()`
and backtrace rendering walk the same stack.

## 2. Decision

Three mechanisms, in dependency order. The first is a prerequisite for the
other two, and is a correctness fix in its own right.

### 2.1 Mechanism 1 — the dynamic routine chain does not depend on which dispatch path a callee qualified for

`call_compiled_function_positional_light` and `call_compiled_function_light`
push a `RoutineFrame` (`push_routine_with_location`, `is_block: false`) in
their prologue and pop it at every exit, mirroring `vm_call_fast.rs`'s existing
push/pop pairing — including the early type-mismatch/arity bails, each paired
with its existing cleanup. `enclosing_routine_exists()` and every other
`routine_stack` consumer then describe the real chain on all paths.

This is deliberately **not** the `uses_callframe` treatment ADR-0035 applied to
the method chokepoints, and the difference matters: `uses_callframe` gates a
push on a property of the *body that observes*, which works for `callframe()`
because the observer's own frame is what it needs. `enclosing_routine_exists()`
asks about the **chain**, so a frameless *intermediate* breaks it no matter
what the observer's flag says — the same shape ADR-0035 §1.2 recorded as a
latent sub-side gap and could not fix with a flag either.

Cost: a `Vec` push/pop of a `RoutineFrame` (eight `Copy`/`Symbol` fields, no
allocation, no `Env` clone, no `Arc` COW pressure) plus one relaxed atomic
increment for `invocation_id`. This is a different cost class from the
unconditional caller-env push ADR-0035 §3(a) rejected, which cost two `Env`
`Arc` clones and a `String` per call and re-introduced `Arc::make_mut` forks on
the caller env. If the bench CI's `bench-fib`/`bench-class` rows do move, the
fallback is to source `invocation_id` from a thread-local counter rather than
the process-global atomic — not to reintroduce the frameless hole.

### 2.2 Mechanism 2 — a pseudo-stash records its frame's control-flow identity, not just its package

The `CALLER::`/`CALLERS::` stamp site (`src/vm/vm_var_assign_local.rs:447-458`)
already attaches one hidden attribute to the `Stash` value
(`STASH_ORIGIN_PACKAGE_ATTR`, `src/runtime/accessors_stash.rs:41`) precisely
because "which frame was this taken from" is unrecoverable later. Extend that
stamp with the frame's control-flow identity:

- **`__mutsu_origin_routine`** — the callable id of the innermost non-block
  routine enclosing the captured frame, or absent when the captured frame is a
  mainline (no enclosing non-block routine). Resolved at capture time from the
  captured frame's `RoutineFrame { package, name }` via the existing
  `__mutsu_callable_id::{package}::{name}` env key — the same identity space
  `return_target_callable_id` already compares against, so no new one is
  invented.

Same invisible-attribute convention as the package stamp: an attribute, not a
`symbols` member, so `.keys`/`.gist` are unaffected. Zero per-call cost — this
runs only where a pseudo-stash is captured, which is already a reflective
operation.

The predicate half ("was there an enclosing non-block routine at all") is what
Slice 1 of §2.1 makes trustworthy: at the moment `CALLER::` is evaluated inside
`throws-like`, `throws-like`'s own frame is on the stack, so "the caller has an
enclosing routine" is `routine_stack` containing a non-block frame *below the
top one* — a question the frameless paths currently answer wrongly.

### 2.3 Mechanism 3 — `EVAL`'s `context` drives the return classification and the return target

`builtin_eval` (`src/runtime/builtins_eval_misc.rs`) reads the recorded
identity alongside the package it already reads, and threads it into the EVAL
unit's compilation through a new `pending_eval_context_frame` field — the same
family as `pending_eval_sigilless` / `pending_eval_placeholder_params`, and
**it must join `carrier_compile_ctx_key`** (`resolution_eval.rs:216`) or the
carrier-compile cache would serve a unit compiled under a different
classification.

`compile_block_value_opts`'s `in_routine` derivation for an EVAL unit becomes:

| context | classification | runtime effect |
| --- | --- | --- |
| absent (no `context` argument) | ambient `enclosing_routine_exists()` — now sound (§2.1) | unchanged; `sub f() { EVAL 'return 1' }` still returns from `f` |
| present, names a mainline | `is_routine = false`, `lexically_in_routine = false` | `ReturnFromNonRoutine(false)` → `X::ControlFlow::Return` thrown **at the `return`**, catchable by the `CATCH` around the `EVAL` (§1.1(a)) |
| present, names a routine that is live | `is_routine = true`, and the emitted `Return` carries `return_target_callable_id = <recorded id>` | the signal unwinds past intervening routines to the context frame (§1.1(b)), using the existing decline-if-not-my-target logic |
| present, names a routine that is not live | `is_routine = false` | `X::ControlFlow::Return` with `out-of-dynamic-scope` set, and rakudo's wording (§1.1(c)) |

**Liveness is decided once, at EVAL entry, not at the `return`.** The snippet
runs synchronously inside the `EVAL` call, so no frame below the EVAL can
disappear between the two points; deciding at entry keeps the whole mechanism a
compile-time classification and avoids adding a per-`return` liveness query to
the hot path. The check itself is a walk of `routine_stack` comparing
`(package, name)` `Symbol`s — sound only after §2.1, which is why that slice
comes first. A re-entrant same-named routine is indistinguishable by that key
and resolves to the innermost frame; that matches what `return`'s own lexical
semantics do anyway.

Carrying the target id onto the emitted `Return` needs a per-unit constant
rather than a new opcode variant: the EVAL unit is compiled fresh for this call
site, so the compiler can emit `Return` with the id already resolved (an
`OpCode::Return`-with-target form, or a unit-level field the `Return` arm
reads). Prefer the unit-level field — `size_of::<OpCode>()` is pinned at
<= 48 bytes and `Return` is currently payload-free.

## 3. Alternatives considered and rejected

**(a) Convert the escaping CX::Return at the top-level run loop when no routine
frame exists at or below the context depth** (the origin ticket's own proposal,
generalising `vm_run_loop.rs:175`'s `routine_stack().is_empty() &&
nested_run_depth == 0` conversion). Rejected on the measurement in §1.1(a):
raku raises the exception **at the `return`**, so a `CATCH` lexically around the
`EVAL` sees it and the enclosing routine runs to completion. Converting at the
top loop means the signal has already unwound out of every intervening frame
before it becomes an exception — `thrower`'s `CATCH` never fires and its
remaining statements never run, which is the wrong output even though the
exception class is right. It also cannot express §1.1(b) at all.

**(b) Extend `uses_callframe` detection to `EVAL`/`EVALFILE` and route
EVAL-bearing bodies off the light paths, instead of §2.1's unconditional push.**
Cheaper on paper and it is the pattern ADR-0035 established. Rejected: the
predicate is a property of the *chain*, not of the asking body, so a frameless
intermediate still breaks it (ADR-0035 §1.2 recorded exactly this as an
unfixable-by-flag residue on the sub side); and the flag would have to
enumerate every routine-stack consumer — `EVAL`'s `in_routine`, `&?ROUTINE`,
`caller_frame_package`, `executing_source_file`, backtrace rendering — which is
the "correct only under an incomplete static analysis" shape CLAUDE.md's
gain/risk doctrine names as the *risky* route. A missed consumer becomes a
silent wrong answer, not a loud failure.

**(c) Make `EVAL`'s `context` a first-class frame object (a real `PseudoStash`
bound to a live frame) rather than a `Stash` value carrying stamped
attributes.** Architecturally the honest model, and it would also fix deep
`CALLER::CALLER::` chains. Rejected for this ADR as a much larger campaign:
`CALLER::` is captured and then used several frames deeper (`Test.rakumod`
stores it in `$caller-context` and uses it inside a `subtest` block), so the
frame it names is not on the stack in any recoverable form by then — which is
precisely why the stamped-attribute convention exists. Revisit together with
ADR-0035 §3(c)'s parallel-stack unification.

**(d) Do nothing on the grounds that the origin repro passes on `main`.**
Rejected: it passes only because of the §1.3 defect (a param-bearing `thrower`
runs frameless). Both the corrected repro and the real `Test.rakumod` path fail.

## 4. Performance

- **§2.1 is the only per-call cost**, and only on the two light sub paths: one
  `Vec<RoutineFrame>` push/pop (no allocation) plus one relaxed atomic
  increment. Guard: the bench CI `bench-history.tsv` rows that exercise
  param-bearing sub calls (`bench-fib` and friends), across the Slice 1
  landing. No `Env` clone, no `String`, no `Arc` COW — the costs ADR-0035 §3(a)
  rejected are not incurred.
- **§2.2 and §2.3 cost nothing on any hot path.** The stamp runs only where a
  pseudo-stash is captured; the classification runs once per `EVAL` call, which
  already compiles source.

## 5. Implementation plan (independently mergeable slices)

### Slice 1 — routine frames on the light sub paths (prerequisite)

- `src/vm/vm_call_light.rs`, `src/vm/vm_call_light_typed.rs`: prologue
  `push_routine_with_location` + a pop at every exit, mirroring
  `vm_call_fast.rs`'s pairing. Audit every early return between push and normal
  exit.
- Regression pin: the §1.3 dispatch-path matrix as `t/eval-return-across-dispatch-paths.t`
  (one sub shape per path, each `{ EVAL 'return 1'; return 2 }`, all answering `1`).
- Must stay green: `roast/S04-statements/return.t` (whitelisted; **its test 15
  is the pin for the `enclosing_routine_exists()` narrowing this slice
  perturbs**), `roast/S06-advanced/{return,callframe}.t`, `t/eval-caller-frames.t`,
  `t/caller-not-dynamic.t`, `t/callframe-*.t` (5 files),
  `t/module-file-var-and-callframe.t`, `t/backtrace-block-frames.t`.
- Bench guard as in §4.

### Slice 2 — stamp the frame's control-flow identity onto `CALLER::`

- `src/vm/vm_var_assign_local.rs` (the `CALLER`/`CALLERS` arm) +
  `src/runtime/accessors_stash.rs`: add `__mutsu_origin_routine` beside
  `__mutsu_origin_package`, and a reader beside `eval_context_package`.
- Pin: a `t/` test asserting the attribute stays invisible to `.keys`/`.gist`
  (the existing convention's guarantee) and that
  `t/eval-context-package.t` is unaffected.

### Slice 3 — `context` classifies the EVAL unit's `return`

- `builtins_eval_misc.rs` reads the identity; `resolution_eval.rs` threads it
  into `compile_block_value_opts` **and** into `carrier_compile_ctx_key`.
- Covers the mainline-context and dead-routine-context rows of §2.3's table,
  plus rakudo's out-of-dynamic-scope wording.
- Acceptance: `MUTSU_REAL_TEST=1 prove -e target/debug/mutsu t/throws-like-gather-sink.t`
  reaches 4/4 (today it aborts after subtest 1, the remaining subtests nesting
  inside it because the EVAL'd `return;` unwinds out of `subtest`'s block chain).

### Slice 4 — target a live context routine

- Emit the resolved `return_target_callable_id` on the EVAL unit's `Return`
  (unit-level field, not an `OpCode` payload — the size guard).
- Pin: §1.1(b)'s two-deep repro.

### Slice 5 — residue and end-to-end

- `$*THROWS-LIKE-CONTEXT`, `CALLERS::`, `EVALFILE`; the `t/emit-done-controlflow.t`
  half named in the origin ticket; re-run `scripts/test-module-sweep.sh` and
  update `todo/deep/vendor-real-test-module.md`'s residue table; retire the
  origin ticket to `news/` per the `todo/` lifecycle.

## 6. Verification burden

Slice 1 changes the answer to a predicate that gates `return`'s *compilation*
in every carrier body, so it is the high-blast-radius one. Before calling it
done:

- the `t/` pins listed under Slice 1, on the debug binary;
- `roast/S04-statements/return.t` specifically — its test 15 is the recorded
  reason `enclosing_routine_exists()` exists at all
  (`accessors_stack.rs:19-25`), and a frame push that miscounts a block frame
  would flip it;
- full CI (`make test` + `make roast` + gc-stress/jit-stress) as the
  comprehensive net.

Slices 2-4 are narrow by construction (a reflective capture site, an EVAL-only
classification), and their own pins plus the real-`Test` acceptance gate are
sufficient.

## 7. Out of scope

Deep `CALLER::CALLER::` / `callframes()` chains through frameless intermediate
subs — ADR-0035's inherited approximation. Slice 1 makes the *routine* stack
faithful on the light paths; the `caller_env_stack`/`callframe_stack` pair is a
separate mechanism and is not touched here.

## 8. Implementation status (2026-08-20)

**Slice 1 landed.** `call_compiled_function_positional_light`
(`src/vm/vm_call_light.rs`) and `call_compiled_function_light_spec`
(`src/vm/vm_call_light_typed.rs`, the implementation behind
`call_compiled_function_light`) now push a `RoutineFrame` via
`push_routine_with_location` right after entering the routine's declaring
package/pragma state, and pop it immediately after the body-execution loop —
mirroring `call_compiled_function_fast`'s (`src/vm/vm_call_fast.rs`) existing
push/pop pairing exactly. Every early exit between the two functions' entry
and the push (the arity-mismatch and type-mismatch bails) runs before any
frame is pushed, so needs no matching pop; every exit after the push (the
body's `return`/`fail`/error/natural-completion arms, and the post-loop
return-type-check failure) funnels through the single pop site, so the pairing
holds unconditionally.

The delegate path in `call_compiled_function_light` for a hand-built chunk
with no precomputed named-call plan (`cf.named_call_plan.is_none()`) falls
through entirely to `call_compiled_function_named`, which already pushes its
own frame — untouched.

This fixes `enclosing_routine_exists()` (and every other `routine_stack`
consumer — `caller_frame_package()`, `executing_source_file()`, backtrace
rendering) for every sub shape that qualifies for either light path:
mandatory-positional-only signatures and named-only signatures. Confirmed
against the §1.3 measured matrix (`t/eval-return-across-dispatch-paths.t`,
new): all six dispatch-path shapes now answer `1` for
`EVAL 'return 1'; return 2`, matching raku, where `pos1($x)` and `named1(:$x)`
previously escaped as an uncaught `X::ControlFlow::Return`. Also re-verified
the closest pre-existing pin for this predicate,
`t/eval-return-target-needs-a-real-routine.t`, and the roast pin the design
called out by name, `roast/S04-statements/return.t` (test 15 specifically),
both still green.

Cost: confirmed via `MUTSU_VM_STATS=1` on a recursive positional-arg body
(the exact path this slice touches) that the dual-store counters
(`clone_env`, `env_deep_copies`, `env_flushes`) are unchanged at `0` —
the push/pop is a plain `Vec<RoutineFrame>` operation with no `Env` clone, as
§4 predicted. A bench-CI-measured wall-clock verdict (the §4 guard) is
pending the next `bench-history.tsv` row past this change's merge commit.

**Slices 2-3 landed (2026-08-21, PR #6828).** Slice 2:
`caller_frame_enclosing_routine()` (`src/runtime/accessors_stack.rs`) walks
`routine_stack` from `caller_frame_package`'s same frame down past any block
frames to the nearest actual routine, and the `CALLER::`/`CALLERS::` stamp
site (`src/vm/vm_var_assign_local.rs`) now stamps its `package::name` onto the
pseudo-stash as `__mutsu_origin_routine`
(`Interpreter::stamp_stash_origin_routine`, `src/runtime/accessors_stash.rs`)
beside the existing `__mutsu_origin_package` — same invisible-attribute
convention, absent entirely when the captured frame is a mainline. A reader,
`eval_context_routine`, sits beside `eval_context_package`.

Slice 3: `builtin_eval` (`src/runtime/builtins_eval_misc.rs`) reads that
identity via a new `classify_eval_context_routine` helper into an
`EvalContextRoutineState` (`Mainline` / `Live` / `Dead`, `src/runtime/mod.rs`),
walking `routine_stack` for a `package::name` match to decide liveness —
sound only because Slice 1 made every dispatch path push a frame. The
classification is threaded through a new `pending_eval_context_routine` field
(save/restore around the `EVAL` call, mirroring `pending_eval_sigilless`)
into `compile_block_value_opts`'s `in_routine` derivation (via a shared
`eval_unit_in_routine()` helper) **and** into `carrier_compile_ctx_key`, so
the carrier-compile cache cannot serve a unit compiled under a different
classification. `Live` is treated exactly like the ambient
"an enclosing routine exists" answer (Slice 4's job is to additionally target
that specific frame — not implemented yet, so a `Live` classification today
just keeps the pre-ADR-0037 non-local-return behavior). `Mainline` and `Dead`
both compile `is_routine = false`, but `Dead` additionally sets a new
`Compiler::eval_context_dead_routine` flag, which rides a second `bool` added
to `OpCode::ReturnFromNonRoutine` so the thrown `X::ControlFlow::Return`
carries `out-of-dynamic-scope: True`.

That flag also drove a small, independently-useful correction to
`RuntimeError::controlflow_return()`: its message was hardcoded to "Attempt
to return outside of any Routine" regardless of `out_of_dynamic_scope`, but
`raku` uses a second, fuller wording ("Attempt to return outside of
immediately-enclosing Routine (i.e. `return` execution is outside the dynamic
scope of the Routine where `return` was used)") whenever a routine genuinely
was the target at some point and is no longer live — verified directly
against `raku` for both the plain "no routine at all" case and a `-> { return
5 }` closure called after its defining routine returned. The message now
branches on the flag, which also correctly changes the wording for the two
pre-existing `out_of_dynamic_scope: true` call sites (`io_env.rs`'s
`render_gist_value`, `vm_run_loop.rs`'s top-level escaping-signal
conversion) — checked against the tests that pin the old wording
(`t/exception-messages.t`, `t/try-catch-tail-statement-sink.t`) and confirmed
none of them exercise an `out_of_dynamic_scope: true` path.

Verified directly against `raku -e` for all three rows the ADR's §2.3 table
assigns to these slices: §1.1(a)'s mainline-context probe (`thrower saw:
X::ControlFlow::Return`, `thrower-end`, `still alive`) and a dead-routine
probe modeled on §1.1(c) (`Attempt to return outside of
immediately-enclosing Routine ...`) both match mutsu's output exactly, and
the uncontextualized `sub f() { EVAL 'return 1'; return 2 }` still answers
`1`. Pins: `t/eval-context-caller-routine-attr.t` (new — the hidden-attribute
invisibility pin for Slice 2, and confirms `t/eval-context-package.t` is
unaffected). The ADR's own acceptance gate,
`MUTSU_REAL_TEST=1 prove -e target/debug/mutsu t/throws-like-gather-sink.t`,
now reaches 4/4 (previously aborted after subtest 1). Full local `t/` suite
(30847 tests) green; targeted roast re-runs on both debug and release
binaries (`roast/S04-statements/return.t` test 15 specifically,
`roast/S32-exceptions/misc.t`, `roast/S02-types/WHICH.t`,
`roast/6.c/S02-names/SETTING-6c.t`, `roast/S02-names/{SETTING-6e,caller}.t`,
`roast/S03-binding/scalars.t`, `roast/S04-statements/goto.t`,
`roast/S06-signature/definite-return.t`) all green; `cargo clippy -- -D
warnings` and `cargo fmt` clean.

**Slices 4 and 5 landed (2026-08-22).** Slice 4:
`EvalContextRoutineState::Live` grew a payload (`Live(Option<u64>)`), the
target routine's registration clone id resolved right where liveness itself
is decided — `classify_eval_context_routine`
(`src/runtime/builtins_eval_misc.rs`) now, on a live match, calls
`registration_clone_id` (`src/runtime/accessors_resolve.rs`, widened from
private to `pub(crate)` for this) on the matched `RoutineFrame`'s
`package`/`name`, the same `__mutsu_callable_id::{package}::{name}` lookup
`RegisterSub` maintains and `RuntimeError::return_target_callable_id`
compares against. `None` for a live frame with no resolvable id (an
anonymous routine, which never registers one) — falls back to the pre-Slice-4
first-boundary-catches behavior rather than mistargeting.

Per the ADR's own guidance, the id is **not** an `OpCode::Return` payload.
`compile_block_value_opts` (`src/runtime/resolution_eval.rs`) bakes it onto
the freshly-compiled EVAL unit's own `CompiledCode` as a new field,
`eval_context_target_callable_id: Option<u64>` (`src/opcode.rs`), set only
when `pending_eval_context_routine` is `Live(Some(id))` — so it only affects
`return` written directly in the EVAL unit's own mainline; a nested
closure/sub the snippet declares compiles through
`compile_closure_body_with_routine_flag`, which builds a fresh `Compiler`/
`CompiledCode` that never sees the field, so this cannot leak into a routine
the snippet defines. Already covered by `carrier_compile_ctx_key`'s existing
`eval_context_routine: Option<EvalContextRoutineState>` field, since the
`Live` payload is part of that same enum — a cache hit only serves a compile
made under the identical (state, id) pair. `OpCode::Return`'s exec arm
(`src/vm/vm_exec_dispatch.rs`) reads `code.eval_context_target_callable_id`
and stamps it onto the raised `RuntimeError::return_signal` via the existing
`set_return_target_callable_id`, which the decline-if-not-my-target check
every routine boundary already runs
(`vm_call_named_inner.rs`/`vm_closure_dispatch.rs`/`vm_method_dispatch.rs`)
then does the rest: the signal sails past every intervening routine frame
whose id does not match, exactly the existing mechanism a bare block's
`return` already used to reach its lexically enclosing routine.

**A second, independent defect surfaced while verifying Slice 4 against the
ADR's own §1.1(b) two-deep repro**, and had to be fixed for the repro to pass
at all: the two **light** call dispatch paths
(`call_compiled_function_positional_light` in `vm_call_light.rs`,
`call_compiled_function_light`/`call_compiled_function_light_spec` in
`vm_call_light_typed.rs`) catch *any* signal carrying `return_value.is_some()`
unconditionally — unlike `call_compiled_function_named`
(`vm_call_named_inner.rs`) and the closure dispatcher
(`vm_closure_dispatch.rs`), neither light path ever checks
`return_target_callable_id()` before swallowing the signal. Slice 1 (this
ADR) made both paths push a `RoutineFrame`, which made `enclosing_routine_exists()`
sound on them, but never taught them to *decline* a signal aimed at a
different frame — so a positional-light or named-light routine sitting
between the EVAL call and its live-context target caught the signal itself
instead of letting it pass through, which is exactly the shape the ADR's
`sub thrower($code) { ... }` repro takes (a mandatory-positional signature is
positional-light-eligible). Fixed by adding the same decline check the named
path already has to both light paths' `return_value.is_some()` arm, gated
behind `if let Some(target_id) = e.return_target_callable_id()` so the
(overwhelmingly common) untargeted-return case pays no extra cost at all —
`registration_clone_id` is only called, and only then, when a signal actually
carries a target. This is a genuine, general-purpose fix (not scoped to
EVAL): any non-local return whose target is a specific callable id — which
already included a bare block's `return` reaching for its enclosing routine
before this ADR existed — was silently mis-caught by an intervening
light-dispatched routine. Confirmed with `rust-gdb -batch` breakpoints that,
pre-fix, the two-deep repro's `return_target_callable_id` was correctly
resolved and stamped (`Some(18)`, matching the target routine's registration
id) but was being swallowed one frame too early, inside `thrower` itself —
the positional-light path's own decline arm was simply missing.

Pin: `t/eval-context-live-target.t` (5 assertions) — the ADR's own §1.1(b)
two-deep repro verified byte-for-byte against `raku`, plus the same shape
through the named-light dispatch path (`named-thrower(:$code)`) to pin the
`vm_call_light_typed.rs` twin of the decline check, plus a three-deep chain
where the context is captured one frame below the actual `EVAL` call site (so
neither the immediate `EVAL` caller nor its immediate caller is the target).
All five assertions verified first against `raku`, then against mutsu.
Re-verified the §1.1(a) mainline-context and uncontextualized probes (§1.1(a),
`sub f() { EVAL 'return 1'; return 2 }`, and the §1.3 dispatch-path matrix
across `pos1($x)`/`named1(:$x)`) are all still correct after both changes.

Slice 5: swept the three residue items the ADR names by name.
`CALLERS::` (plural) is stamped identically to `CALLER::` at the same site
(`vm_var_assign_local.rs`'s combined `CALLER`/`CALLERS` arm, unchanged by
this ADR), so `context => CALLERS::` already targets a live frame the same
way — verified directly (`raku` and mutsu agree on the two-deep repro with
`CALLERS::` substituted for `CALLER::`) and pinned in
`t/eval-context-slice5-residue.t`. `$*THROWS-LIKE-CONTEXT`
(`Test::Util`'s `no-fatal-throws-like`, `roast/packages/Test-Helpers/lib/Test/Util.rakumod`)
stores its captured `CALLER::` in a dynamic variable that the real
`Test.rakumod`'s `throws-like` reads back several frames deeper, inside its
own `subtest { ... }` — exactly the capture-now/use-later shape §2.3's
rejected alternative (c) named as the reason the mechanism is a stamped
attribute on a `Stash` value rather than a live frame reference; verified
that the stamp survives being threaded through a dynamic variable (not just
a lexical) and pinned in the same file. `EVALFILE`'s signature
(`raku-doc/doc/Type/independent-routines.rakudoc`:
`EVALFILE($filename where Blob|Cool, :$lang = 'Raku', :$check)`) has no
`context` parameter at all, so it was never in scope for the targeting
machinery; confirmed its plain, uncontextualized `return` semantics
(unwinding to whichever routine dynamically encloses the `EVALFILE()` call,
or throwing `X::ControlFlow::Return` with none) are unaffected by any of
this ADR's changes and pinned the same way, so a future EVALFILE change
cannot silently regress it.

The ADR's own acceptance gate for Slice 3
(`MUTSU_REAL_TEST=1 prove -e target/debug/mutsu t/throws-like-gather-sink.t`
reaching 4/4) still holds; the file's remaining 3 subtests (the ones needing
the actual targeting, not just the sink-forcing fix) now pass for the right
reason rather than by the pre-Slice-4 first-boundary coincidence.
`t/emit-done-controlflow.t` (the other file the origin ticket named) passes
5/5 under `MUTSU_REAL_TEST=1` — it was already closed by an unrelated,
earlier fix (`news/2026-08/emit-done-controlflow-illegal-control.md`,
2026-08-18) before this ADR's targeting work began; re-verified here as part
of Slice 5's sweep, not newly fixed by it.

Verification: full local `t/` suite green; `cargo clippy -- -D warnings` and
`cargo fmt` clean; `roast/S04-statements/return.t` (test 15 specifically),
`roast/S06-advanced/{return,callframe}.t`, and the full Slice 1-3 pin set
(`t/eval-return-across-dispatch-paths.t`,
`t/eval-return-target-needs-a-real-routine.t`,
`t/eval-context-caller-routine-attr.t`, `t/eval-context-package.t`,
`t/eval-caller-frames.t`, `t/caller-not-dynamic.t`,
`t/backtrace-block-frames.t`, `t/module-file-var-and-callframe.t`,
`t/callframe-*.t`) all still green. The origin ticket,
`todo/deep/eval-context-frame-owns-the-return-target.md`, is retired to
`news/2026-08/eval-context-frame-owns-the-return-target.md`.
