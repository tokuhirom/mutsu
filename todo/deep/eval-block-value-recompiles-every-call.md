# `eval_block_value` recompiles a closure's body from AST on every call

## Summary

`Interpreter::call_sub_value`'s general closure branch — the path that runs any
block/closure Value invoked as a first-class callable (`.()`, a callback, a
`lives-ok { ... }` argument, etc.) that is not itself a named-routine-derived
`compiled_routine` — executes the body via `eval_block_value(&data.body)`, and
`eval_block_value_inner` **recompiles the whole body from its AST every single
call** (`compile_block_value_opts` runs the full `Compiler::compile` pipeline:
parsing is already done, but semantic analysis — `needs_env_sync`, free-var
analysis, name interning, opcode fusion — reruns from scratch). This is true
even when the `SubData` already carries pre-built bytecode from closure
creation (`data.compiled_code`, populated by `exec_make_anon_sub_op` for every
block literal).

Two costs follow:

1. **Wasted compile work** proportional to the block's size, paid on every
   invocation instead of once at closure-creation time.
2. **The JIT never gets a chance** in some shapes. A `CompiledCode` built by
   `compile_block_value_opts` is a brand-new object every call, so any
   hotness/JIT-cache keyed on that code object's identity starts from zero
   each time.

**A fix was attempted (reuse `data.compiled_code` instead of recompiling) and
then reverted** after measurement showed it regresses the primary target file
2.4x. See "Attempted fix and why it was reverted" below — read this before
trying again, it is not a free win.

## How this was found

Investigating `todo/deep/interpreter-call-path-in-hot-loops.md` (measured
13.8x mutsu-vs-raku regression on a hot function-call loop). Re-measuring in
2026-08 found the *named*-function-call path (`call_compiled_function_
positional_light` etc.) already well-tuned by prior J4d work — an isolated
`for ^1_000_000 { $n = outer-fn($n) } ` loop is now only ~2x raku, not 13.8x.

But the actual roast file the ticket names,
`roast/S04-declarations/state.t`, was still catastrophically slow (~8.5s vs
raku's ~0.7s, ~12x). The slow subtest is:

```raku
lives-ok { sub foo () {$ = 42}; for ^2000000 { $ = foo } },
    'Intensive use of state variable in inline-friendly sub does not hit problems';
```

Isolating the shape with the inner `sub` declared at *file scope* instead
(called from inside a block passed to `lives-ok`, sub declaration itself
outside the block) reproduced a **~20s** run for a mere 2,000,000-iteration
loop that runs in under a second when NOT wrapped in `lives-ok { ... }`.
`MUTSU_VM_STATS=1` on that wrapped version showed:

```
function-call opcodes=2000002 interpreter_fallbacks=1000000 (50.0% of opcodes)
jit: compiles=0 entries=0 bailouts=1
```

Every single call to the sub fell through to `record_function_fallback`'s
full tree-walk dispatch (`call_function_fallback`), and the JIT never
compiled anything. A block passed to a plain *user-defined* Raku sub
(`sub call-it(&blk) { blk() }`) does **not** show this — it JIT-compiles fine
(`jit: compiles=2 entries=1999802`). The difference is that
`lives-ok`/`dies-ok`/etc. are native Rust functions that invoke their block
argument via `eval_test_callable_body` → `eval_test_block_value` →
`eval_block_value`, the interpreter carrier described above — while a block
passed to a *user* sub is bound as a normal `&`-parameter and invoked through
the ordinary compiled closure-call machinery (`call_compiled_closure`).

Tracing further up the stack, `call_sub_value`'s own general closure branch
(used by `.()`, callback invocation, map/grep callbacks not already routed
through the pre-compiled fast path, etc.) has the *same* property: for a
`Sub` with no `compiled_routine` (i.e. an ordinary block/closure, not a
registry-routine code object), it falls through to
`self.eval_block_value(&data.body)` around line 758 of
`src/runtime/resolution_call_sub.rs`, regardless of whether `data.compiled_code`
is populated. So this is not a Test-module-specific quirk — it is
`call_sub_value`'s **general** mechanism for invoking a first-class
block/closure value.

## Attempted fix and why it was reverted

Changed `eval_test_callable_body` / `eval_test_block_value` (used by
`lives-ok`, `dies-ok`, `throws-like`, `fails-like`, `warns-like`) to reuse
`data.compiled_code`/`data.compiled_fns` instead of recompiling, via a `reuse`
parameter threaded through `eval_block_value_inner`
(`eval_block_value_inner_opts`), keeping every existing wrapper invariant
(block-scoped registration restore, topic isolation, leaked-lexical cleanup,
`use fatal` scoping) untouched around the swapped-out compile step.

**Correctness held** across `make test` (29079 tests), a 21-file roast sample
including `state.t` itself, and targeted repros for leaked-lexicals,
`our`-decls, fatal-mode scoping, and topic isolation (two pre-existing
failures reproduced identically with and without the change — not
regressions).

**Performance was a genuine, measured win for blocks with no nested `sub`
declaration**: a 100,000-iteration loop each calling `lives-ok { ... }` with a
trivial body dropped from 13.95G to 10.38G retired instructions (-25.6%,
`perf stat instructions:u`, taskset-pinned) and ~1.36s → ~0.97s wall clock
(-29%, 3-run average, release build).

**But performance got WORSE — 2.4x — for the actual `state.t` repro**, which
declares its `sub foo` *inside* the `lives-ok { ... }` block:

```
baseline (recompile every call):  8.3-8.5s
"fixed" (reuse compiled_code):   20.1-20.4s   <- regression
```

`MUTSU_VM_STATS=1` isolated the cause precisely:

```
baseline: function-call opcodes=4000002 interpreter_fallbacks=0       (0.0%)
"fixed":  function-call opcodes=4000002 interpreter_fallbacks=2000000 (50.0%)
```

Both runs execute the *identical* opcode counts for every op EXCEPT one: the
"fixed" run has an extra `RoutineScope=1` opcode the baseline does not. That
is the tell — the reused `data.compiled_code` is not byte-identical to what a
fresh `compile_block_value_opts` call produces for the same AST, because
`compile_block_value_opts` derives `compiler.is_routine` /
`compiler.lexically_in_routine` from **call-site** state
(`!self.routine_stack.is_empty()` *at the moment `lives-ok`'s block runs*),
not from the state that was live when the block was originally compiled at
`MakeAnonSub` time (program load / mainline compile). When those two contexts
disagree, the nested `sub foo` compiles differently — into a shape that ADR-
0024's "mainline lexical" machinery treats as needing the frame-pushing path
(`light_call_blocked_by_mainline_capture`), which is categorically slower and,
in this exact shape, funnels every call through
`record_function_fallback`'s full tree-walk dispatch instead of the compiled
fast paths.

Confirmed the fault line is specifically "nested `sub` declared inside the
block": the file-scope-`foo` variant (sub declared *outside* `lives-ok`, only
the loop inside) showed **zero** difference between baseline and the reuse
fix — both hit `interpreter_fallbacks=0`, `jit: compiles=2 entries=1999802`.

**The change was reverted** (`git checkout -- src/runtime/resolution_eval.rs
src/runtime/test_functions/fails_like.rs
src/runtime/test_functions/throws_like.rs`) rather than shipped with an
AST-based "skip reuse if the block declares a nested sub" gate, because:

- The single concrete counterexample found (`RoutineScope` / mainline-lexical
  treatment) does not obviously bound the *complete* set of constructs whose
  compiled shape depends on call-site-vs-creation-site context. `is_routine`/
  `lexically_in_routine` also affects `return` semantics, and
  `compile_block_value_opts` separately seeds
  `pending_eval_sigilless`/`pending_eval_placeholder_params` from caller
  state. A narrow "no nested sub" gate would silence the one failure found by
  hand, not necessarily every failure mode this mismatch can produce.
- A perf regression is invisible to `make test`/`make roast` — CI is not a
  safety net here the way it is for correctness. Landing a change whose
  failure mode is "some other roast file gets slower," discovered only by
  someone noticing a timeout months later, is worse than not landing it.
- The win (real, but so far only demonstrated on a synthetic microbenchmark)
  needs to be weighed against the discovered risk class before a second
  attempt — see the recommended next step below.

## The larger fix (still not attempted — needs its own design pass)

Make `call_sub_value`'s general `ValueView::Sub(data)` branch prefer
`call_compiled_closure(&data, cc, args, fns)` when `data.compiled_code` is
`Some`, the same way it already does for `data.compiled_routine` (see line
~417-431 of `resolution_call_sub.rs`). This remains a substantial,
not-yet-scoped change, and now carries the additional caution above:

- The current tree-walk branch (lines ~433-830 of `resolution_call_sub.rs`,
  ~400 lines) does its own hand-rolled free-var/env merge
  (`closure_base_env`, `auth_free_vars`/`is_authoritative`, `merge_all`
  caller-priority logic), package switching, supply-block flag threading,
  and post-body `let`/registry/topic restoration — built up incrementally
  over many bug fixes (see the inline comments citing specific roast
  regressions each piece prevents: `S14-roles/anonymous.t`,
  `S04-declarations/my-6e.t`, Cro's `whenever`/`self` capture, etc.).
  `call_compiled_closure` (`vm/vm_closure_dispatch.rs`) is a **different,
  independently-evolved** implementation of conceptually the same job. It is
  unclear without a careful line-by-line audit whether it is a strict
  superset of the tree-walk branch's behavior.
- **Confirmed** (not just suspected, see above): `compile_block_value_opts`'s
  call-site-derived compilation context can produce a *functionally
  different* (not just differently-shaped) compiled body than the closure's
  original creation-time compile, specifically for a nested `sub` declaration
  interacting with ADR-0024 mainline-lexical detection. Any fix that reuses
  `data.compiled_code` needs to either prove this class of mismatch cannot
  arise for its target call sites, or reconcile the two compilation contexts
  so they agree.
- Blast radius: this branch is reached from *many* callers across the whole
  interpreter (any code holding a `Value` known to be `Sub` and wanting to
  invoke it) — a wrong step is not confined to the Test module.

Recommended next step: rather than patching the symptom (skip reuse when a
gate fires), investigate *why* `compile_block_value_opts` needs call-site
context at all for a plain block literal with a fixed, already-known creation
site. If a block Value's `data.compiled_code` is supposed to be a complete,
self-sufficient compiled representation of the closure (which the
`SubData.compiled_fns` doc comment implies it should be), the call-site
context-sensitivity may itself be the bug — worth a design pass answering
"what does a block's compiled shape actually need to depend on the calling
context for, and can that be captured explicitly (e.g. as an argument) rather
than inferred from ambient interpreter state at call time?" `call_protect_block`
(`resolution_eval.rs`, used by `Lock::Async.protect`) and
`eval_precompiled_block_fast` (used by some map/grep fast paths) are two
existing, narrower precedents that already reuse `data.compiled_code` for
their specific carrier shape — worth checking whether either has quietly
hit (or avoided) the same trap.

## Verification protocol for a future attempt

- `MUTSU_VM_STATS=1` before/after on a loop-in-block-argument repro: confirm
  `jit: compiles>0 entries>0` where it was `compiles=0`, AND confirm
  `interpreter_fallbacks` does not increase for a repro with a nested `sub`
  declaration inside the block (the specific trap this document reverted).
- `perf stat instructions:u` (taskset-pinned) before/after, on BOTH a
  no-nested-sub repro and a nested-sub repro (`state.t`'s own subtest) — a
  win on one shape and a loss on the other is exactly what slipped through
  this attempt's first round of testing.
- Full `make test` + `make roast` (CI) for correctness — but remember CI does
  NOT catch a performance regression, so the instruction-count comparison
  above is not optional polish, it is the actual safety net for this class of
  change.
