# Concurrent `for` loops sharing a bare loop-param name no longer collide on the cross-thread lane

Two concurrent `for LIST -> $x { start {...} }` sibling iterations whose
loop item was not a "plain" scalar type (an `Instance`, e.g. a
`Cro::HTTP::Client`, was the case that triggered this) used to converge on
whichever iteration's value most recently won a last-writer-wins race on a
single bare-name shared-store slot, instead of each spawned thread keeping
its own binding. `t/http-session-inmemory.rakutest` /
`t/http-session-persistent.rakutest` subtests 8-9 ("No session confusion
with concurrent clients") exercised exactly this shape.

Root cause: `block_captured_scalars`'s "plain" allow-list
(`Int/Num/Str/Bool/Rat/.../ContainerRef`) decided which of a spawned `start
{}` block's free variables the closure machinery boxed into its own
per-binding cell versus which stayed visible only through the cross-thread
bare-name shared-store lane. A name-keyed store cannot hold two
concurrently-live bindings of one bare name, so any non-"plain" for-loop
parameter shared by two live sibling iterations was unsafe regardless of
which of the store's two write branches (force-overwrite vs.
seed-once-then-ignore) a spawn took.

The fix ([ADR-0023](../../docs/adr/0023-binding-provenance-spawn-capture.md))
is a third path, not either of the two originally sketched: the true
decision axis is **binding provenance**, not value type. A `for`-loop
parameter is a readonly, per-iteration fresh binding, so it is always safe
to treat as closure-owned at spawn time regardless of its type — no boxing
or `SharedStore` changes needed. `Interpreter::active_loop_param_names`
tracks which bare names are currently bound as loop parameters; while a
name is on that stack, `block_captured_scalars` captures it unconditionally
(type-blind), so each spawn's already-correct per-iteration env clone is
never seeded into or overwritten by the bare-name lane.

Implementation touched the loop lifecycle (`push_loop_local_scope` /
`pop_loop_local_scope` in `vm_control_ops.rs`, threaded through the
regular, int-range-specialized, and C-style/repeat loop bodies),
`with_nested_registers` for routine-boundary isolation, and every fast-call
path that bypasses it (`vm_call_fast.rs`, `vm_call_light.rs`,
`vm_call_light_typed.rs`, `VmCallFrame`) — the same isolation gap the
existing `loop_local_vars` mechanism already had to close, found by
grepping every `saved_loop_local_vars` site.

New pin: `t/for-loop-param-start-sibling-isolation.t` (warmup, no-warmup,
renamed-param, plain-block, multi-param, and `Channel`-identity variants).
PR #6189.
