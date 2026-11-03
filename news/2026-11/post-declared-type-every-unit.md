# `Illegally post-declared type` for every unit

ADR-15760 Amendment 1, slice 1a. A type name used before the statement that declares it (`say A; class A {}`, a use inside a
nested block, a routine or a method body that is only called later) is rakudo's compile error. mutsu raised it for `EVAL`
strings alone, comparing top-level statement indexes; the main program and every `use`d module ran such code. One pure check
over the finished statements (`src/rakuast/post_declared.rs`) now runs for the main program, a freshly compiled module and an
`EVAL`, and judges each use against the innermost statement list that declares the name.

Accepted as before, as rakudo does: a declared-first type, a `role R {...}` stub (it declares at the stub), a core name
(`say Set; class Set {}`), a qualified declaration's namespaces (`class X::Y::Z {}; X::Y; class X::Y {}`), and the lists directly
inside a class, role or package body (rakudo answers those with another error). The main program is judged in source order,
before the BEGIN-time phasers move, so a `CHECK` block that names a class declared above it is not a use before it.

Measured: the whole `t/` suite (9408 files) and the roast whitelist (1429 files) with the check on; two `t/` files moved
(`cli-compile-only-types.t`, `compunit-add-sorry.t`, both the CHECK-block case, fixed by judging in source order) and one roast
file (`S12-class/namespaced.t`, the implicit namespaces, fixed), none left. `Str.AST` running the check is slice 1c.
