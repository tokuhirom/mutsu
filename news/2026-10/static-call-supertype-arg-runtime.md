# A supertype type-object argument is a run-time binding failure, not "will never work"

`sub f(Int $x) {}; f(Cool)` was reported as the compile-time
`Calling f(Cool) will never work with declared signature (Int $x)`. Rakudo
raises the binder's run-time `X::TypeCheck::Binding::Parameter` instead (#10944).
`Cool` is a supertype of `Int`, so a value of that static type could still bind,
and rakudo's optimizer only refutes a call when no argument could.

The binding-error wrapper now checks for this case before it promotes a
statically-typed call site to `X::TypeCheck::Argument`. That happens when some
positional type-object argument is a strict supertype of its parameter's
nominal type (`Any` for an untyped parameter). Such a call keeps the binder's
own error, including an arity error: `f(Cool, 2)` is `Too many positionals
passed`, as in rakudo. The `Mu`-only carve-out from #10878 in
`static_call_args.rs` is gone, because `Mu` is just the widest case of the same
rule.
