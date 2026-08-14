# `state.t`'s 12x-vs-raku slowdown is a JIT bailout on `StateVarInitGuard`, not a recompilation problem

## Summary

`roast/S04-declarations/state.t`'s "Intensive use of state variable in
inline-friendly sub does not hit problems" subtest (`sub foo () {$ = 42}; for
^2_000_000 { $ = foo }`, wrapped in `lives-ok { ... }`) was the original
motivating repro for `todo/deep/eval-block-value-recompiles-every-call.md`.
Re-measured on 2026-08-14 (release build, `MUTSU_VM_STATS=1`):

```
baseline: ~7.6-8.5s, function-call opcodes=4000000 interpreter_fallbacks=0 (0.0%)
jit: compiles=0 entries=0 bailouts=2 (StateVarInitGuard)
```

`lives-ok`'s block argument runs exactly **once** — its own recompilation
cost (what `eval_block_value_inner`/`compile_block_value_opts` pays per
invocation) is therefore negligible here regardless of whether it is fixed.
`interpreter_fallbacks=0` confirms `foo()`'s 2,000,000 calls are NOT falling
back to tree-walk dispatch either. The actual bottleneck is that the JIT
bails out on the `StateVarInitGuard` opcode (`compiler/stmt.rs:1320`) and the
whole loop runs interpreted instead of JIT-compiled — that is where the
12x-vs-raku gap comes from.

**This means none of the fixes discussed in
`eval-block-value-recompiles-every-call.md` (compile-result caching, or the
larger `call_sub_value` → `call_compiled_closure` fork) will move this
specific benchmark.** That ticket's own success should be measured against a
carrier block invoked *repeatedly* (e.g. `lives-ok { ... }` called 100,000
times in a loop, or any block passed many times to a native Test/comparator
function), not against `state.t`, which calls its `lives-ok` block once.

## Fix direction (not yet investigated)

Teach the JIT to handle the state-var-init guard shape, or restructure how a
`state` variable's one-time initialization check compiles so it doesn't
require a guard the JIT tier can't yet compile. Start by reading the JIT's
existing bailout handling around `StateVarInitGuard` (`vm/vm_jit.rs` or
wherever `try_enter`/JIT tier-A compilation lives — grep
`StateVarInitGuard` across `src/vm/vm_jit*.rs`) to see what makes this
opcode currently JIT-incompatible, and whether it's a fundamental limitation
or a missing case.

## How this was found

Investigating `todo/deep/eval-block-value-recompiles-every-call.md`. A
design-consultation (Fable, 2026-08-14) pointed out that the ticket's own
motivating benchmark had never actually been re-measured against the
dispatch-cost theory being discussed — `MUTSU_VM_STATS` for the baseline
already shows `interpreter_fallbacks=0`, which rules out both "wasted
recompile work" and "falls back to tree-walk dispatch" as the cause for this
specific file. The `bailouts=2 (StateVarInitGuard)` line was there in the
stats output the whole time.
