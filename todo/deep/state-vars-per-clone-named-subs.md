# `state` in a nested named sub is not per-clone (and the key spaces diverge per dispatch path)

Raku scopes a `state` variable to the routine **clone**: a named sub nested
inside another routine is cloned on every execution of the enclosing routine,
so its `state` re-initializes per enclosing call, while a top-level sub has one
clone for the program and its `state` persists across calls.

```raku
sub outer() {
    sub inner() { state $n = 0; ++$n }
    inner(); inner()
}
say outer();   # raku: 2   mutsu: 2
say outer();   # raku: 2   mutsu: 4  <-- state leaked across enclosing calls
```

Real-world impact: `Cro::ConnectionConditional.new` (Cro::Core) uses
`sub check-compatibility { state $first = True; ... }` nested in `method new`
and relies on `$first` resetting per `.new` call. In mutsu the second
`.new` skips initialization and dies with
`X::Cro::ConnectionConditional::Incompatible ... saw Any to Any and ...`
(t/connection-conditional.rakutest tests 3, 6, 11, 12, 13).

## Why the obvious fix is not enough

An attempt to scope the named-call path's state keys by the registration-time
callable id (`__mutsu_callable_id::PKG::NAME`, refreshed on every RegisterSub
execution — exactly the per-clone identity `once` already uses, see
`once_scope_key`) fixed nothing and broke top-level counters, because **the
same routine's calls flow through different dispatch paths with different
state-key shapes**, observed with gdb on `vm_call_named_inner.rs`:

- call 1 of a sub goes through `vm_call_named_inner` (cold), which loads/syncs
  `cf.code.state_locals` with **raw** keys;
- calls 2+ resolve through a cached `&name` Sub and run via
  `vm_closure_dispatch`, which sets `state_scope_id = data.id` and uses
  **`key#c<data.id>`** scoped keys;
- the body's own `StateVarInit`/`StateVarInitGuard` use `scoped_state_key`
  with whatever `state_scope_id` is ambient;
- cross-thread sharing (`__mutsu_shared_state::`) goes through
  `normalize_state_key`, which strips `/arity` and `@<n>` but NOT `#c<id>`
  (see `call_shared_state_body`'s None-reset workaround for `start {}`);
- the interpreter fallback (`builtins_operators_fallback`) has its own frame
  setup and does not touch `state_scope_id` at all.

Today these accidentally agree for top-level subs (cold raw key is read once,
then the closure path's `#c<data.id>` key is stable) and disagree exactly in
the nested case. Any fix must unify the key derivation across ALL of:
`vm_call_named_inner`, `vm_closure_dispatch` (2 state_locals sites),
`vm_run_loop::{load,sync}_state_locals`, `vm_misc_scope::exec_state_var_init_op`,
`runtime_thread`'s shared-state keys, and the interpreter fallback — plus the
JIT gate if compiled bodies ever touch state.

## Design sketch

The clone identity that matches Raku semantics is "one id per execution of the
RegisterSub op" (top-level: once per program; nested: once per enclosing call;
loop body: once per iteration). Candidates:

1. Stamp a fresh clone id on the registered `FunctionDef`/cached Sub at every
   `register_sub_decl_fp` execution (including both idempotent fast paths,
   which already refresh `__mutsu_callable_id::…` in env), and make EVERY
   dispatch path set `state_scope_id` from the *registration* id — including
   the cached-`&name` closure path, which must stop using `data.id` for named
   subs (or the SubData must be re-minted per registration so `data.id` IS the
   registration id).
2. Decide the cross-thread story: `normalize_state_key` must either strip the
   scope suffix consistently or the suffix must be identical on all threads
   (the registration id is stable across thread env clones, unlike the
   per-closure ambient id that motivated `call_shared_state_body`).

Pinned expectation for the fix: `outer()` above prints 2/2; a top-level
`sub counter { state $c = 0; ++$c }` prints 1/2/3; `await (^4).map: { start f() }`
still accumulates into one shared cell (t/state-aggregate-shared-cell.t).
