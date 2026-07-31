# `state` in nested named subs is now per-clone — Cro::Core connection-conditional 23/23

Raku scopes a `state` variable to the routine **clone**: a named sub nested
inside another routine is cloned on every execution of the enclosing routine,
so its `state` re-initializes per enclosing call, while a top-level sub has
one clone for the program and its `state` persists across calls. mutsu leaked
nested-sub state across enclosing calls (`outer()` returning 2 then 4), which
blocked `Cro::ConnectionConditional.new` (`state $first = True` in a nested
`sub check-compatibility`) — five failures in
`t/connection-conditional.rakutest`.

The former deep ticket (this file replaces it) documented the real blocker:
the same routine's calls flowed through dispatch paths with DIFFERENT state
key shapes — the cold named path used raw keys, the cached-closure path used
`key#c<SubData.id>`, and the interpreter fallback never set a scope at all.
The fix unifies them on ONE identity: the **registration clone id** (the
`__mutsu_callable_id::Pkg::name` env entry, already refreshed on every
`RegisterSub` execution — once per program for a top-level sub, once per
enclosing call for a nested one):

- `vm_call_named_inner` (cold path) resolves the registration id and installs
  it as the ambient `state_scope_id` for the body, loading/syncing
  `state_locals` under scoped keys (it used raw keys before);
- `vm_closure_dispatch` prefers the registration id over `SubData.id` for
  named subs with state variables (anonymous closures keep the Sub identity);
- the fast zero-arg call path (`call_compiled_function_fast`) now excludes
  state-bearing subs — like the light paths always did — routing them through
  the full path that performs the scope setup (same rule as `once`);
- the interpreter fallback hands the registration id across `run_nested`'s
  register reset via a new one-shot `pending_nested_state_scope` field,
  consumed by `with_nested_registers` as the nested run's initial scope.

Cross-thread sharing is unaffected: the registration id lives in the env,
which thread clones carry, so all threads derive the same scoped key
(`t/state-aggregate-shared-cell.t` still passes).

Pins: `t/state-per-clone-named-subs.t` (nested 2/2, top-level 1/2/3, sibling
clones), plus the existing state suite (`closure-captured-state`,
`concurrent-state-var`, `module-state-sub-shared-cell`, roast
`S04-declarations/state.t` — all green).

With this, `t/connection-conditional.rakutest` goes 8/13 → **23/23** (the
state failures unblocked the rest of the file), leaving the vendored
Cro::Core suite fully green except tcp's `:nodelay` subtests
(`todo/tickets/in-memory-socket-native-descriptor.md`, an in-memory-socket
design limitation).
