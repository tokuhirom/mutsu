# A `my sub` shadowing a named-param outer sub or an outer multi now resolves correctly on every call

A routine-local `my sub` that shadows an outer routine of the same name went
wrong from the enclosing routine's *second* call on, in two shapes that both
hid behind the existing (positional, zero-arg) pinned coverage in
`t/routines/dispatch/lexical-sub-dispatch-memo.t`, because both bugs left the
routine's first call correct.

**Named-parameter call, the inner sub leaked OUT.** A body declaring an inner
`my sub` relies on the full call path's routine-registry
`snapshot_routine_registry`/`restore_routine_registry` to take the
declaration away again on return. The positional-light call path already
excluded such a body via `CompiledFunction::has_inner_subs`, but the
named-parameter light-call path (`is_light_call_eligible`) and the fully
optimized zero/one-arg fast path (`is_fast_call_eligible`) did not check it
at all — so a routine with a named parameter and an inner `my sub` took the
light path, which skips the snapshot/restore entirely, permanently leaking
the inner routine's registration into the enclosing scope after the routine's
very first call. Both eligibility checks now also require `!cf.has_inner_subs`.

**The shadowed name is a multi, the inner sub was IGNORED.** Registering a
routine-local `my sub` that shadows a same-named outer `multi` family hides
the multi's arity-keyed candidate keys from the registry for as long as the
`my sub` is in scope (`registration_sub.rs`'s `retain` on `functions_mut()`)
— but only on the routine's first call, which takes the full registration
path. Every call after that took a "derive-once re-install" fast path
(reusing the previously-derived `Arc<FunctionDef>` instead of re-deriving it,
an optimization for a `my sub` re-registering on every call of its enclosing
routine), which never ran that `retain` step. Since a routine-scope restore
puts the whole `functions` map back to its pre-call state — multi candidate
keys included — every call after the first found the outer multi's keys
still present and dispatched to it instead of the shadowing lexical single.
The fast path now checks whether a same-named multi family exists (via the
existing `fn_keys_for_base` index) and falls through to the full
registration path when it does, so the multi-hiding step reruns exactly when
it is needed.

See [issue #9080](https://github.com/tokuhirom/mutsu/issues/9080).
