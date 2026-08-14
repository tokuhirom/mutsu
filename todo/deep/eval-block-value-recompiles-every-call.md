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

## 2026-08-14 re-investigation: the "larger fix" audited, not yet attempted

Re-verified the reverted attempt's numbers on the exact doc repro (`sub foo ()
{$ = 42}; for ^2_000_000 { $ = foo }` wrapped in `lives-ok { ... }`, release
build): baseline recompile-every-call runs in ~7.6s with
`interpreter_fallbacks=0`, `jit: compiles=0 bailouts=2
(StateVarInitGuard)` — matches the numbers already recorded above.

Pursued the "recommended next step" above: is `compile_block_value_opts`'s
call-site-context-sensitivity itself the bug, fixable by capturing the right
context explicitly at `exec_make_anon_sub_op` time instead of re-deriving it
from ambient state? **No — this framing undersold the problem.**
`compile_block_value_opts` (`resolution_eval.rs:99-154`) is not "almost the
same compile with one flag wrong": the real per-closure compile
(`compile_closure_body_with_routine_flag`,
`compiler/helpers_sub_body.rs:855-913`) inherits the *entire* parent-compiler
state into the child compiler — `fold_ctx` (constant-folding context),
`outer_code_var_names`, `enclosing_scopes`/`enclosing_sigilless`/
`enclosing_local_names` (the full lexical scope chain),
`user_listop_shadows`, `outer_constant_names`, `lexically_in_method`. None of
that exists anywhere at `eval_block_value` call time — the original
`Compiler` instance that held it is long gone. `compile_block_value_opts`
re-derives only a handful of scalar flags (`is_routine`, package scope,
sigilless/placeholder seeds) from ambient interpreter state and otherwise
starts from a bare `Compiler::new()`. So a block's compiled shape is NOT
"context-free apart from a couple of flags" — reusing `data.compiled_code`
(the properly-context-inherited compile) is therefore not just a different
recompile, it can be a *more complete* one, which is consistent with why the
reverted attempt's "fixed" version differed in shape (an extra `RoutineScope`
opcode) even though `self.routine_stack` was independently confirmed (via
`rust-gdb` breakpoints, no rebuild) to be empty in both the ambient recompile
AND the real compile at the point `sub foo` is declared — i.e. the divergence
is not a simple `is_routine` flag mismatch as originally suspected; it comes
from the missing scope-chain/fold-context inheritance, which the ambient path
cannot reconstruct at call time even in principle (the information doesn't
exist to reconstruct — it was never generated). **Conclusion: a narrow "make
`compile_block_value_opts` context-aware enough" fix is not tractable.** The
tractable direction really is the "larger fix" below (prefer
`data.compiled_code` via `call_compiled_closure`, not a smarter ambient
recompile).

Tried to pin down *why* the reused-`compiled_code` version regressed 2.4x
(the extra `RoutineScope` opcode / `light_call_blocked_by_mainline_capture`
categorization) by checking whether `sub foo`'s `RegisterSub` op was getting
wrongly added to `self.mainline_lexical_subs` (`vm_register_sub_ops.rs:429`,
gated on `self.block_scope_depth() == 0 && self.routine_stack().is_empty() &&
...`) — the theory being that `call_compiled_closure` never calls
`push_block_scope_depth`/`pop_block_scope_depth` (confirmed: no match for
`block_scope_depth` anywhere in `vm_closure_dispatch.rs`), so running the
reused block through it instead of through `eval_block_value_inner` (which
does `self.block_scope_depth += 1` around every nested block, including this
one) could make `RegisterSub(foo)` see `block_scope_depth() == 0` and
wrongly qualify as a mainline-lexical sub. **This hypothesis was tested and
ruled out**: a `rust-gdb` breakpoint at `vm_register_sub_ops.rs:430` never
fired at all for this exact repro shape (`sub foo () {$ = 42}`) — the state
variable `$` is an anonymous `state`, and `state`-declared names are
excluded from `code.my_declared_sym` (`vm_register_sub_ops.rs:465-469`,
"`our`/`state`/`dynamic`-declared names are excluded"), so `foo` has no
`my`-declared free var to trigger this boxing path in the first place. **The
actual mechanism behind the `RoutineScope`/`light_call_blocked_by_mainline_
capture` categorization for this specific repro remains unidentified** —
worth checking next time before attempting another reuse: instrument
`light_call_blocked_by_mainline_capture`'s call sites
(`vm_call_func_ops.rs:19,589,717,1168,1213`) directly to see which one
actually rejects `foo`'s calls, rather than assuming the
`mainline_lexical_subs` path from first principles again.

Separately audited whether `call_sub_value`'s general
`ValueView::Sub(data)` branch (the actual "larger fix" target — not just
the Test-module `eval_test_callable_body` entry point the reverted attempt
touched) could safely be routed through `call_compiled_closure` whenever
`data.compiled_code.is_some()`, the same fork that already exists for
`data.compiled_routine` (`resolution_call_sub.rs:417-431`). **Not a strict
superset relationship** — two structural gaps and two smaller concrete bugs
were found, filed separately so they can be fixed/attempted independently of
this ticket's larger question:

- `todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md`
  — `call_compiled_closure` has no equivalent of `call_sub_value`'s
  `merge_all` parameter (which ~97 call sites depend on), and per-closure
  persisted state lives in two independent stores (`closure_env_overrides`
  vs `closure_captured_state`) depending on which path a given closure
  instance is invoked through. This is the main blocker for an unconditional
  general fork.
- `todo/tickets/call-compiled-closure-underscore-arg-binding-bug.md` — a
  live, already-present bug: `call_compiled_closure` binds `$_` for a bare
  block even when the body reads `@_` instead (confirmed via `.()` vs
  `Promise.then` giving different, and differently-correct, results today).
- `todo/tickets/call-compiled-closure-missing-rw-lazylist-tail.md` — the
  tree-walk branch's `is rw`/`LazyList` return-value post-processing has no
  equivalent in `call_compiled_closure` (already affects the existing
  `compiled_routine` fork, not just a hypothetical future one).

None of these are fundamental blockers on their own, but together they mean
the general fork is not a small patch — it needs `call_compiled_closure` to
grow a `merge_all`-equivalent mode (or the two persistence stores unified)
before it can be attempted safely across all ~265 call sites the fork would
affect (97 `merge_all: true` + the larger `merge_all: false` population).

## 2026-08-14 Fable design consultation: recommended sequencing

Consulted for advice given the two blockers found in the same-day
re-investigation above. Full reasoning is in the session; key conclusions
recorded here for whoever picks this up next:

1. **The "cache `compile_block_value_opts`'s result" option (option C, a
   `HashMap<(u64, CompileCtxFingerprint), (Arc<CompiledCode>,
   Arc<CompiledFns>)>` keyed by `data.id`) is NOT a lesser option that only
   fixes "wasted compile work" while leaving "JIT never gets a chance"
   unaddressed** — both costs are the same underlying cause. `JitCodeState`
   (`opcode.rs:3798`) is embedded per-`CompiledCode` object; its own doc
   comment says cloning a chunk resets hotness tracking because "a clone is a
   distinct compilation identity." A cache that returns the same `Arc` on
   every call lets the hotness counter accumulate across calls and JIT-compile
   — for free, as a side effect of caching. This makes C worth doing
   regardless of what happens with the larger `call_sub_value` fork.
2. **The cache-key soundness worry (pointer/GC-address reuse) is moot**:
   `SubData.id` is a monotonic `u64` from `next_instance_id()`
   (`value/mod.rs:730`), never reused, and is already the established key for
   exactly this shape of cache — `protect_block_cache`
   (`resolution_eval.rs:559-670`, used by `Lock::Async.protect`) already
   caches compiled code per `data.id`. C is a third instance of an existing
   pattern, not a novel mechanism.
3. **`state.t`'s 12x-vs-raku gap is caused by neither of this ticket's two
   costs.** See `todo/tickets/state-var-init-guard-jit-bailout-blocks-hot-loop.md`
   — `lives-ok` runs its block once, so recompilation cost is negligible, and
   `interpreter_fallbacks=0` at baseline. The actual cause is the JIT bailing
   on `StateVarInitGuard` and running the `for` loop interpreted. **Stop
   measuring this ticket's success against `state.t`** — use a
   repeatedly-invoked carrier block instead (e.g. 100,000x `lives-ok { ... }`
   in a loop).
4. **Reframing the 2.4x regression**: the creation-time compile
   (`data.compiled_code`) is the MORE correct one — it emits `RoutineScope`
   (`compiler/helpers_sub_body.rs:962`) because a `sub` declared inside a
   block is genuinely block-lexical, and needs a registry save/restore
   bracket. The ambient recompile skips `RoutineScope` only because
   `routine_stack` happens to be non-empty at invocation time, and gets
   correct block-lexicality *for free* from the carrier's own registry
   snapshot/restore in `eval_block_value_inner`. So baseline being fast is
   "borrowing" correctness from the carrier rather than the compile itself.
   The durable fix is a **separate, standalone perf bug**: make calls to a
   `RoutineScope`-registered sub eligible for fast compiled dispatch, instead
   of falling to `record_function_fallback` ~50% of the time. Next debugging
   step: break on `vm_stats::record_function_fallback` (`vm_stats.rs:782`,
   4 call sites: `vm_call_dispatch.rs:131`,
   `vm_call_func_ops.rs:1345/1408/1513`) with `bt`, NOT on
   `light_call_blocked_by_mainline_capture`'s 5 call sites (already proven
   irrelevant — `mainline_lexical_subs` stays empty for this repro). Prime
   suspect: `RoutineScope`'s `snapshot_routine_registry`/
   `restore_routine_registry` (`vm_misc_scope.rs:218-234`) bumping a
   registry generation that invalidates a resolution cache
   (`fn_resolve_gen`/`otf_call_cache_gen`, see the eligibility chain at
   `vm_call_func_ops.rs:704-719`) — unverified, check with the breakpoint
   before trusting it.
5. **Recommended sequencing**:
   - Now: land `todo/tickets/call-compiled-closure-underscore-arg-binding-bug.md`
     and `todo/tickets/call-compiled-closure-missing-rw-lazylist-tail.md`
     (independent, small), and implement option C (the compile-result cache)
     per the design above — safe, mechanical, reuses the `protect_block_cache`
     pattern.
   - Next: the `record_function_fallback` breakpoint session; fix the
     `RoutineScope` dispatch-eligibility bug. Standalone perf win, and a
     prerequisite for the larger fork (without it, the fork re-imports the
     2.4x cliff for any nested-sub-in-block shape).
   - Then: attempt `todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md`'s
     fork, under a `Proposed` ADR written first. Recommended ADR framing:
     "`call_compiled_closure` is the canonical closure-invocation mechanism;
     the `call_sub_value` tree-walk branch is transitional debt, retired via:
     bug-parity fixes → `CapturePriority` mode (see that ticket) →
     `RoutineScope` dispatch-eligibility fix → unconditional fork on
     `compiled_code.is_some()` → delete the branch and
     `closure_env_overrides`." Key simplification: gate the fork ONLY on
     `data.compiled_code.is_some()` (a stable per-instance property), never on
     `merge_all` — that avoids the dual-persistence-store split hazard
     entirely, since a given closure instance then always uses the same store
     regardless of which call site invokes it.
   - The C cache remains useful even after the fork lands — it stays the
     carrier-level mechanism for `eval_block_value`'s surviving callers with
     no `SubData` in hand (EVAL, regex code blocks, phasers, class/role
     bodies).

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
