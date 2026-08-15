# The `state` store is keyed by `(Symbol, Option<u64>)`, and the JIT publishes too

`news/2026-08/state-vars-belong-to-the-block-clone.md` made a write to a `state`
slot publish straight to the state store (`Interpreter::publish_state_local`), so
a re-entrant call observes a mutation the outer frame has already made. That
publish ran from the interpreter's `SetLocal`/`SetLocalDecl`/increment dispatch
arms, but the JIT lowers `SetLocal`/`SetLocalDecl` to dedicated
`vm_jit_helpers::{set_local, set_local_decl}` shims that call
`exec_set_local_op` directly and skipped it — a residue from before the
write-through existed, reachable when a hot loop's write range JIT-compiles
while its enclosing `state` declaration does not (`StateVarInit` is outside the
JIT's Tier A set).

The fix that was previously deferred (both obvious ones cost too much on the
`roast/S04-declarations/state.t` 2,000,000-iteration microbench — see the
original ticket's numbers) is now done: `state_vars` is keyed by
`(Symbol, Option<u64>)` — an interned base-key symbol plus the optional
closure-scope id — instead of a `format!`ed `String`. `scoped_state_key` is now
a `Copy` tuple construction, not an allocation, so `publish_state_local` is
free enough to call unconditionally from the JIT shims too.

## What changed

- `Interpreter::state_vars: HashMap<(Symbol, Option<u64>), Value>` (was
  `HashMap<String, Value>`).
- `CompiledCode::state_locals: Vec<(usize, Symbol)>` (was `Vec<(usize, String)>`).
- `OpCode::StateVarInit`/`StateVarInitGuard`'s second operand is now an interned
  `Symbol` id (`Symbol::from_id`/`Symbol::id`), not a constant-pool index — the
  compiler already produced the key string once per declaration site, so
  interning it there instead of only adding it to the constant pool is free.
- `vm_jit_helpers::{set_local, set_local_decl}` now call
  `Interpreter::publish_state_local` after a successful
  `exec_set_local_op`, exactly mirroring the interpreter's own `SetLocal`/
  `SetLocalDecl` dispatch arms.
- A handful of call sites that built a raw state-store key directly (flip-flop
  `ff`/`fff` state, smart-match's flip-flop matcher, the anonymous
  `__ANON_STATE_*` store, and the `__mutsu_state_key::*` closure free-var
  writeback bridge) now intern their key into the same `(Symbol, Option<u64>)`
  shape instead of using a `String` key.

## Measured effect

Release build, idle box (same methodology as the original ticket, which
measured main at 8.28s/7.1s before the write-through existed):

| variant | state.t | 2M-loop microbench |
| --- | --- | --- |
| as merged (JIT range skipped the publish) | 8.4s | 7.1s |
| this change (rekeyed + JIT shims publish) | 7.45s | 5.7s |

Both got *faster*, not just even — publishing from the JIT shims no longer
costs anything measurable, and removing the `format!`/`HashMap<String, _>`
overhead from every other `state` access (load, sync, init) is a net win on
its own.

## Verification

No deterministic repro existed for the residue itself (recorded as such in the
original ticket), because a plain `state $n` scalar has been cell-boxed since
`state-vars-belong-to-the-block-clone.md` (2026-08-06) — a `ContainerRef`
mutation is visible to every holder of the same cell regardless of the
store-level publish, so a same-shape recursive repro built for this change
turned out insensitive to the bug either way. The full local `t/` suite
(29,577 tests) and `roast/S04-declarations/state.t` pass unchanged, and
`MUTSU_JIT=off` vs the default (and vs `MUTSU_JIT_THRESHOLD=1`, which forces
compilation almost immediately) agree on every state/flip-flop/smart-match
test — the verification method the original ticket prescribed.
