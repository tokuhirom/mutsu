# The JIT no longer permanently bails out on `state`-variable init opcodes

`roast/S04-declarations/state.t`'s "Intensive use of state variable in
inline-friendly sub does not hit problems" subtest was ~12x slower than
`raku`. Investigation (`MUTSU_VM_STATS=1`, release build) traced this to a
JIT bailout, not a recompilation problem: any routine body containing a
`state` declaration compiled to `StateVarInitGuard` (the once-only-init
check) and `StateVarInit` (the store), neither of which was in the Tier A
opcode set (`src/vm/vm_jit_compile.rs`/`vm_jit_support.rs`). The whole chunk
was rejected up front and permanently left to the interpreter — `state.t`'s
2,000,000-iteration hot loop never ran natively at all.

## Fix

- `StateVarInitGuard(key_idx, jump_to)` gets a dedicated conditional-branch
  codegen arm in `vm_jit_compile.rs::build`, mirroring the existing
  `JumpIfFalse`/`JumpIfTrue`/`JumpIfNotNil` arm: a new
  `helpers::state_var_init_guard_cond` shim reads the state var's
  initialized-or-not status (keyed on the opcode's own `key_idx`, not a
  stack value) and returns whether to jump — pushing the `NIL` placeholder
  `StateVarInit` discards on that path, exactly like the interpreter arm.
  `compile_range`'s static support scan now collects its jump target the
  same way it already does for `Jump`/`JumpIf*`.
- `StateVarInit(slot, key_idx)` (the actual once-only store — infallible,
  straight-line, never touches `ip`) is added to the generic step-shim
  whitelist (`step_supported`), so it runs through the existing
  `helpers::step` shim like dozens of other opcodes already do.

Verified via `MUTSU_VM_STATS=1`: a `state`-heavy hot routine (2,000,000
calls) went from `bailouts=2 (StateVarInitGuard)` to `bailouts=0`, with the
JIT fully compiling and entering the routine's body natively.

## Tests

`t/jit-state-var-init-guard.t` (new) — a `state` scalar counter, a `state`
scalar accumulator, a `state` array, and the exact `$ = foo` bare-topic-
assignment shape `state.t` uses, each called well above the default
`MUTSU_JIT_THRESHOLD=100` hotness bar, plus a cold (below-threshold) sanity
case. `MUTSU_VM_STATS=1` on this file confirms zero bailouts.

PR [#TBD](https://github.com/tokuhirom/mutsu/pull/TBD).
