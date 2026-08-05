# JIT Tier A bitwise coverage + shared OTF bodies — the RIPEMD hot loop now compiles (but the gate did not move)

The `todo/tickets/jit-bitwise-tier-a-coverage.md` ticket asked for the bitwise
opcode family in the Tier A support tables, on the theory that `BitShiftLeft`
bailouts (3126 per small repro, zero compiles) were the last lever for
`t/ripemd.t`'s 120s batteries-gate budget. The coverage was added — and it
surfaced a chain of deeper defects that had to be fixed before the JIT could
even keep a compiled body across `start` tasks. All of them are fixed; the
honest headline result is that the RIPEMD gate itself is **flat** (295.3s →
299.0s local, within noise), because opcode dispatch was not the dominant
per-round cost. The mechanism wins are real and measured by counters:

1. **Bitwise Tier A shims** (`BitAnd`/`BitOr`/`BitXor`/`BitShiftLeft`/
   `BitShiftRight`/`IntBitNeg`): payload-free fallible shims mirroring
   `helpers::add`, in `vm_jit_helpers.rs` + `vm_jit_support.rs`. The repro's
   bailout histogram went `BitShiftLeft=3126` → `bailouts=0`.

2. **Per-task registry COW clones eliminated** (`registry-cow: clones=64` →
   `0` on the repro): `Interpreter::class_mro` unconditionally took
   `registry_mut()`, whose first mutable deref after a spawn share deep-clones
   the whole registry — and every RIPEMD task resolved `Blob[uint32]`'s MRO.
   The deep clone also reset every `CompiledFunction`'s `JitCodeState` (a
   clone is a fresh compile identity). `Registry::class_mro_readonly` now
   resolves every no-cache-write shape (builtin table, parametrized names,
   cached MROs) under a read guard; only a registered class with an uncached
   MRO falls to the write side.

3. **Process-global L2 for OTF-compiled bodies** (`global_otf_cache` in
   `vm_call_dispatch.rs`): a spawned task starts with an empty per-interpreter
   `otf_compile_cache`, so a spawn-heavy loop re-OTF-compiled the same sub per
   task — a fresh `CompiledCode` identity per task, which reset JIT hotness,
   re-paid the 100-call warmup plus a Cranelift compile per task (jit
   `compiles=158` for 79 blocks), and re-stamped `BEGIN` site memos. Bodies
   are now shared process-wide keyed by `(body fingerprint ^ package)`;
   `state`-declaring defs stay per-interpreter (body identity is their
   semantic key). `compiles` dropped to one per distinct body.

4. **Closure bodies can enter the JIT at all**: `call_compiled_closure_with_topic`
   ran its body through a plain `exec_one` loop with no `try_enter` hook, so a
   closure-shaped hot loop (the 80-round `reduce -> $A, $j {...}`) never went
   native. Same hook shape as `vm_call_named_inner.rs`.

5. **Per-call test-assertion resolution removed**: `call_compiled_function_named_inner`
   ran `routine_is_test_assertion_by_name` — a FULL name resolution cloning an
   `Arc<FunctionDef>`'s AST, plus a fingerprint re-hash over a Debug-format
   walk — on every compiled named call, just to read one bool. A monotonic
   process-global name set (the `USER_INFIX_DECLS` pattern) recorded at
   registration now answers `false` for free unless an `is test-assertion`
   routine was ever declared under that bare name. rotl full-resolves on the
   repro: 10624 → 192.

## Why the gate did not move

With `bailouts=0` and `entries` tracking every hot call, `t/ripemd.t` still
takes ~299s and JIT on/off A/B at `rmd160("a" x 20_000)` is flat (~6.0s both).
Tier A is subroutine threading: the native body calls the same interpreter
helper per opcode, so it removes only dispatch overhead — and the RIPEMD
per-round cost lives inside the helpers. The flat perf profile (release,
`--profile profiling`) is ~20% malloc/free (per-round closure-call setup:
scoped env overlay, captured-env merge, args), `Index`/`AT-POS` dispatch that
probes `class_mro("Blob[uint32]")` per element access, and `GetGlobal` env
reads for the captured free vars (`@words`, `@K`, `$r`, `$s`). Those are the
next levers; findings recorded in
`todo/tickets/digest-ripemd-start-per-block-overhead.md`.
