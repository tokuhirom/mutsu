# A lexical `my class`/`my package`/`my role` and a `require` stub die with their scope

A lexically declared type name used to be scoped correctly only by a bare statement block
`{ ... }` (the `OpCode::BlockScope` exit) and, for `my class`, by a routine's first call.
Everywhere else the name stayed resolvable after its scope ended
([#10594](https://github.com/tokuhirom/mutsu/issues/10594)):

```raku
if 1 { my class IfC {}; say ::('IfC').^name }   # IfC
say ::('IfC').^name;                            # was IfC, now Failure (like raku)
```

Each construct now matches `raku`:

- **`if`/`unless`/`else`/`given`/`when` bodies and every loop body.** The branch/loop scope already
  kept a per-scope journal of the env bindings its declarations shadow (`loop_local_saved_env`,
  restored by `pop_loop_local_scope`). A type name is bound by its declaring op, not by a
  `SetLocalDecl`, so the ops (`RegisterClass` for a `my class`, `RegisterPackageMy`, the `my role`
  binder and `DeclareRequireStub`) now record what the name held *before* their first env write
  (`save_lexical_type_binding_for_scope_exit`, first write wins). A branch that declares a lexical
  type or makes a literal `require` is compiled with a `BlockLocalScope`, like one that declares a
  `my` variable. An enclosing same-named class is given back on exit, not just removed.
- **`EVAL`.** The snippet's lexical type names join the names it declares
  (`eval_declared_lexical_keys`), so the caller's binding is restored (or the name dropped) when it
  returns.
- **A routine's second call.** `compute_declared_locals` now counts a `my package` and a `require`
  stub as callee-local, so the scoped-call merge in `vm_call_fast.rs` no longer copies the binding
  back to the caller.
- **A braced `try { ... }`** declares the stubs of its own literal `require`s on entry and takes
  them (and any `my class`/`my package`/`my role` it declares) away on exit:
  `try { ::('Foo').^name; require Foo }` is `Foo` inside and `Failure` after.

Pinned per construct in `t/vm/scope/lexical-type-scope-exit.t`.
