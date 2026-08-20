# `eval_block_value` no longer recompiles a closure body on every call

The deep finding `todo/deep/eval-block-value-recompiles-every-call.md` recorded
two costs of `Interpreter::call_sub_value`'s general closure branch: it ran a
first-class block/closure `Value` by handing its AST to `eval_block_value`,
which re-ran the whole `Compiler::compile` pipeline on **every** invocation
(cost 1, wasted compile work), and because each call produced a brand-new
`CompiledCode` object the JIT's per-object hotness counter restarted from zero
every time, so such blocks were never JIT-compiled at all (cost 2).

Both costs are closed, and the ticket's remaining "larger fix" now has a proper
architectural home. This note records the close-out and the one concrete,
newly-root-caused perf defect that came out of re-verifying the ticket against
`main` on 2026-08-20.

## Cost 1 and cost 2 were fixed together by the carrier compile cache

`f6a6eb780` ("perf(carrier-compile-cache): reuse `eval_block_value`'s carrier
compile across repeated calls to the same block") added
`Interpreter::carrier_compile_cache`, a `HashMap` keyed by
`(SubData.id, ambient compile context)` holding
`(Arc<CompiledCode>, Arc<CompiledFns>)`. `call_sub_value`'s general branch now
calls `eval_block_value_cached(&data.body, data.id)`
(`src/runtime/resolution_call_sub.rs:780`), so a repeated invocation of the same
closure instance reuses the previous compile instead of redoing it. `SubData.id`
is a monotonic, never-reused `u64`, which is why the key is sound; the same
pattern was already established by `protect_block_cache`.

Because the cache hands back the *same* `Arc<CompiledCode>`, the `JitCodeState`
embedded in that object accumulates across calls, so cost 2 fell out for free —
a clone resets hotness tracking, an `Arc` bump does not. The cache is
deliberately bypassed whenever the chunk would need a per-call mutation
(supply-block / whenever-emitter flags), so it can never serve a wrong chunk.

Re-verified on `main` at `991b55ffa` (release build). A
`my &blk = {...}; for ^100_000 { lives-ok &blk, 'ok' }` repro — the exact shape
the ticket named — now reports:

```
function-call opcodes=1 interpreter_fallbacks=0 (0.0% of opcodes)
jit: compiles=2 entries=2099802 bailouts=0
```

`compiles=0` was the ticket's headline symptom; it is `compiles=2` with 2.1M JIT
entries now.

## The `state.t` framing in the original ticket was a red herring, and it is also fixed

The ticket used `roast/S04-declarations/state.t`'s
`lives-ok { sub foo () {$ = 42}; for ^2000000 { $ = foo } }` subtest as its
motivating case, then later corrected itself: `lives-ok` runs its block *once*,
so recompilation cost was never the issue there — the real cause was a JIT
bailout on `StateVarInitGuard`, filed separately as
`todo/tickets/state-var-init-guard-jit-bailout-blocks-hot-loop.md`. That ticket
has since been fixed and retired. The same repro on `main` today reports
`jit: compiles=2 entries=3999802 bailouts=0`, `interpreter_fallbacks=0`, and
runs in ~5.0s against raku's ~0.35s — still slower than raku, but for reasons
that belong to general dispatch throughput, not to this ticket.

## The "larger fix" is now ADR-0055 slice 4, not a loose ticket

The ticket's open-ended part was "make `call_sub_value`'s general
`ValueView::Sub(data)` branch prefer `call_compiled_closure` when
`data.compiled_code` is `Some`". That question was taken up in its own right by
[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md),
whose slice 4 *is* this fork, gated exactly as the ticket's own conclusion
recommended (on `data.compiled_code.is_some()`, a per-instance-stable property,
never on `merge_all` or on the call site). The two structural blockers the
ticket had identified — `call_compiled_closure` having no `merge_all`
equivalent, and per-closure state living in two disjoint stores — are ADR-0055's
§1.1 gaps, and its §5 explicitly rejects the "give `call_compiled_closure` a
`merge_all` knob" shape the ticket had sketched. There is nothing left for this
ticket to carry that the ADR does not own better.

## The one live residue: a block-lexical `sub` is OTF-recompiled per call on the compiled-closure path

The ticket had warned that a naive fork regressed the `state.t` shape 2.4x, and
left the mechanism unidentified after two failed hypotheses (`mainline_lexical_subs`
boxing, then `light_call_blocked_by_mainline_capture`). Re-investigating on
2026-08-20 reproduced the cliff **on plain `main`, with no patch at all**, by
invoking the block through `.()` (which already routes to
`call_compiled_closure`) instead of through a tree-walk carrier — and a
`rust-gdb` breakpoint sweep over the four `record_function_fallback` sites plus
`compile_and_call_function_def` named the real mechanism.

For a `sub` declared *inside* a block, the compiled-closure path does not find
that sub in the `compiled_fns` table it consults, so every call falls through
`dispatch_func_call_inner` to `compile_and_call_function_def`
(`src/vm/vm_call_func_ops.rs:1413`) — i.e. the sub's body is **recompiled on
every single call**. The carrier path never does this: `eval_block_value`'s own
compile of the block body places the nested sub into the carrier chunk's
`compiled_fns`, so the call hits the stable compiled-function fast path. If the
nested sub also declares `state` (the `state.t` shape), it degrades one step
further: `def_is_otf_compilable_module_single`'s `declares_state` exclusion
rejects it, and it drops to full tree-walk dispatch via
`vm_call_function_fallback`.

Measured on the same 2,000,000-iteration loop, release build:

| nested `sub` | invoked via | `interpreter_fallbacks` | wall |
|---|---|---|---|
| `sub foo() { 42 }` inside the block | tree-walk carrier (`lives-ok`) | 0 | 2.00s |
| `sub foo() { 42 }` inside the block | `call_compiled_closure` (`.()`) | 0 | 12.40s |
| `sub foo() {$ = 42}` inside the block | tree-walk carrier (`lives-ok`) | 0 | 5.04s |
| `sub foo() {$ = 42}` inside the block | `call_compiled_closure` (`.()`) | 2,000,000 (50%) | 14.81s |
| `sub foo() {$ = 42}` at file scope | `call_compiled_closure` (`.()`) | 0 | 3.83s |

JIT stats are identical across all rows (`compiles=2 entries=3999802
bailouts=0`), so this is neither a JIT nor a hotness problem — and note that the
6.2x OTF-recompile cliff is entirely invisible in `interpreter_fallbacks`, which
is why the ticket's earlier hypotheses, all framed around that counter, could
not find it.

That defect is now filed on its own as
`todo/tickets/nested-sub-in-block-otf-recompiles-per-call.md`. It is a
standalone perf win today (any `.()`-invoked block that declares a helper sub
pays it), and it is the named prerequisite for ADR-0055 slice 4 — without it,
the fork re-imports this cliff for every nested-sub-in-block shape.
