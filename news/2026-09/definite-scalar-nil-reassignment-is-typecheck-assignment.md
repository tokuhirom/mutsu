# `$i = Nil` on a declared `Int:D` scalar is `X::TypeCheck::Assignment`, not `MissingInitializer`

Fixes part of #9779.

Reassigning `Nil` to an already-declared `:D`-constrained scalar (`my Int:D $i = 1; $i = Nil`),
and a `my Foo:D $x = EXPR` declaration whose explicit initializer evaluates to `Nil` at runtime
(`sub f(--> Int:D) { Nil }; my Int:D $i = f()`), both wrongly raised
`X::Syntax::Variable::MissingInitializer` instead of `X::TypeCheck::Assignment`. That error is
reserved for a declaration that omits an initializer entirely (`my Foo:D $x;`).

`OpCode::TypeCheck` now carries whether the declaration wrote an explicit initializer expression,
so `exec_type_check_op_inner` can tell "no initializer given at all" apart from "initializer
evaluated to Nil" and pick the right error; the `SetLocal` reassignment path applies the same rule
for a plain `$x = Nil` on an already-declared variable, which is not a `my` declaration at all.

The issue's third repro (`use variables :D` not enforcing its implicit smiley on a later
reassignment) needs a separate, deeper fix and was split off as #9990.

Pinned by `t/types/enum-subset/definite-scalar-nil-reassignment.t`.
