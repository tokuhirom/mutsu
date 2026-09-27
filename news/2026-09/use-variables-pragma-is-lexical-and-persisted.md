# `use variables :D/:U` is now lexical and enforced on reassignment

`use variables :D; my Int $x = 42; $x = Nil` used to print nothing and silently
reset `$x` to the `Int` type object: the pragma's implicit smiley was applied
only by the declaration's own `TypeCheck` op, while the constraint persisted for
the variable (`SetVarType`, and the slot's baked store constraint) stayed a plain
`Int`, so every later assignment skipped the definedness check (#9990).

The pragma was also a *dynamic* interpreter flag set by a runtime `SetPragma`
op and never reset. Once set it stayed on for the rest of the program and
applied inside every routine called from there — including `Test.rakumod`'s own
`throws-like`/`subtest` bodies — and `{ use variables :D; }; my Int $x` died
with a missing-initializer error outside the block. That leak is what made the
obvious "persist the smiley" fix look like an unrelated VM crash
(`Variable '$msg' is not declared` inside `throws-like`): the leaked `:U`
reached declarations in `Test.rakumod` that were never under the pragma.

`use variables` is now purely compile-time, like rakudo's: the compiler keeps
the active smiley as lexical state (`Compiler::variables_pragma`), restores it
at every block boundary, hands it to nested closure, routine and method bodies,
and rewrites each typed `my`/`state` declaration's constraint (`Int` →
`Int:D`) before emitting anything for it. The declaration's type check, its
persisted constraint and the typed `@`/`%` element constraint therefore all
agree, `X::Syntax::Variable::MissingInitializer` still reports
`implicit => ':D by pragma'` (via a flag on the `TypeCheck` op), and the runtime
`variables_pragma` field is gone.

Untyped declarations (`use variables :D; my $x;`, which rakudo treats as
`Any:D`) are still left unconstrained.
