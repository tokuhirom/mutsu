# A `sub` nested inside a method body now registers body-less

ADR-0019 C6e-3c ("drop `CompiledSubDeclPlan::legacy_body`") listed two
"keep-class" categories that still needed a real AST body: unresolvable
plan bytecode for a `sub` declared inside a class-walker method body, and
NativeCall marshalling traits. The first is lifted.

The gap was registration-time, not a compiler gap: the nested sub's own
`CompiledSubDeclPlan` already carried a resolvable `CompiledFunction` (the
compiler correctly lowered it), but every method-dispatch call site that
runs a compiled method body (`call_compiled_method`, 7 call sites across
`class_dispatch.rs`, `builtins_dispatch_next.rs`, and four
`vm/vm_call_method_compiled_*.rs` files) substituted a hardcoded
`CompiledFns::default()` for the functions table the body's `RegisterSub`
opcode looks its own key up in. The nested sub's compiled routine key could
therefore never resolve at call time, so the method's `FunctionDef` kept
its legacy AST body as the only working execution path.

Fixed by giving `MethodDef` its own `compiled_fns: Option<Arc<CompiledFns>>`
field, populated in `compile_method_def_in_place_with_dist` from the
per-method `Compiler`'s own `compiled_functions` table (previously dropped
after compilation — only the `CompiledCode` was kept), and threading
`method_def.compiled_fns` through all 7 `call_compiled_method` call sites
in place of the hardcoded empty table. A `sub` declared inside an ordinary
method, a submethod (`BUILD`), a role-composed method, or one candidate of
a `multi method` now registers with an empty body, exactly like any other
safe-class routine.

Pinned by `t/nested-sub-in-method-compiled.t`. Along the way this surfaced
a separate, pre-existing bug (reproduces on `main` independent of this
fix): such a nested sub leaks into the enclosing global scope instead of
staying lexically scoped to its method — recorded as
`todo/tickets/nested-sub-in-method-leaks-to-global-scope.md` for a future
session.

The remaining C6e-3c keep-class is NativeCall marshalling traits; the field
itself (`CompiledSubDeclPlan::legacy_body`) cannot be dropped until that
one is also resolved.
