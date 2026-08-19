# Method calls now push a caller-env frame — CALLER::/callframe()/PROCESS::/DYNAMIC:: all observable from inside method bodies (ADR-0035)

Method calls never fed the caller-frame machinery, on any dispatch path.
`CALLER::<$*y>` read `Nil` instead of the caller's dynamic value,
`callframe(1).line` reported the method's own line instead of the call site,
and `PROCESS::`/`DYNAMIC::` pseudo-stash reads made from inside a method body
never saw a write the caller had made — silently, since the affected
mechanisms (e.g. `Log::Timeline`) have a no-op-when-unset fallback rather
than crashing. A **sub**/closure call always worked correctly; only method
calls were broken, on every dispatch path.

The design is recorded as
[ADR-0035](../../docs/adr/0035-method-calls-observe-caller-frames.md), which
found two independent mechanisms behind the four affected reads and fixed
each at its own layer:

- **`PROCESS::`/`DYNAMIC::`** (env-visibility problem, not a frame-stack
  problem): `dynamic_pseudo_stash_entries` enumerated each frame's env with
  `Env::iter()`, which only sees the top overlay tier, while `Env::get()`
  traverses the whole parent chain. Fixed by switching it to the existing
  `Env::filtered_flat` chain-aware tier-walk primitive (already used
  elsewhere in the codebase) instead of `iter()`. This is a pure coherence
  fix — whole-env enumeration now sees exactly what single-key lookup
  sees — and needed zero dispatch changes and zero hot-path cost. It also
  fixed two latent bugs on the **sub** side that the investigation
  discovered along the way: a sub with a positional parameter, and a sub
  called through a frameless intermediate sub, both of which also failed to
  see a caller's `PROCESS::`/`DYNAMIC::` write before this fix (PR #6703).
- **`CALLER::`/`callframe()`** (a genuine frame-observation problem): every
  compiled user-method body executes through exactly two chokepoints,
  `call_compiled_method` and `call_compiled_method_fast`
  (`src/vm/vm_method_dispatch.rs`) — an established invariant (`monitor`
  serialization already keys on these same two functions). Neither pushed a
  caller-env frame. Fixed by consulting the existing, already-correctly-set
  `CompiledCode::uses_callframe` flag at both chokepoints: push a caller-env
  frame in the prologue when the flag is set (before the scoped-overlay
  install), pop it at every existing frame-exit site (PR #6704).

Both fixes add nothing to the majority case (a method that never observes
its caller) beyond one boolean test per chokepoint entry/exit — the same
cost class as the existing `uses_dispatcher` gate beside it.

A third slice audited whether any method execution path still bypasses the
two chokepoints via a leftover tree-walk fallback. It does not: an earlier,
unrelated refactor (#3658, predating this ADR) already made
`run_resolved_method_celled` compile any method with no `compiled_code` on
demand before dispatch and route it through the same `call_compiled_method`
chokepoint the fix above patches. The former tree-walk method-execution arm
was deleted in that refactor; the only executor that does not go through the
two chokepoints is the `handles`-delegation forwarder, which is plumbing
that redirects to another method call rather than a caller-observing user
body. No code change was needed for this part.

`Log::Timeline`'s bundled-battery test (`t/logging.rakutest`) now gets past
its first 8 subtests (previously silently no-op'd via the
`PROCESS::<$LOG-TIMELINE-OUTPUT>` visibility bug this ADR fixes) before
hitting a distinct, unrelated bug in a class-level `atomicint` counter used
as an attribute default value — filed separately as
`todo/tickets/class-level-atomicint-attribute-default-first-instance-wrong.md`.
The caller-frame mechanism itself is verified by targeted regression tests
(`t/adr0035-dynamics-chain-aware-enumeration.t`,
`t/method-caller-frame-push.t`) plus the existing `t/`/roast pin sweep,
including the whitelisted `roast/S06-advanced/callframe.t`.

The one inherited, unchanged limitation: deep `CALLER::CALLER::` /
`callframes()` chains through *frameless* intermediate subs remain gappy,
exactly as they were on the sub side before this ADR.
