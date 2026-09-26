# A lexical `&f` shadows `sub f` in role methods and recompiled blocks

A bare `f()` must reach a lexical `&f` binding (a `my &f`, a `&f` parameter or
a role's `&f` type parameter) in preference to any routine named `f`. Two
places lost the binding and called an outer `sub f` instead (issue #9513):

- **Role methods.** A role's method bodies compile on a fresh compiler with no
  enclosing scope, so `role Q[&h] { method g { h() } }` never saw `&h` when a
  `sub h` existed. The role's `&` type parameters are now seeded as the
  enclosing scope of its method bodies (`Compiler::role_param_scope`).
- **Blocks the runtime compiles again.** The inline `map`/`grep`/`first` path
  and sequence generators recompile a block's AST on a fresh compiler, so
  `{ my &h = {10}; (1..2).map({ h() }) }` returned `(70 70)` even though the
  block's own chunk called the binding. A chunk now records which bare calls
  it routed to a shadowing `&name` (`CompiledCode::amp_shadowed_calls`), and
  the recompile re-declares those names before compiling.
