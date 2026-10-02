# Plain-sub binding failures raise rakudo's run-time exception

A plain `sub` call whose argument failed a parameter's type used to report it in mutsu's own words
and name every object `Any`:

```
sub m(Int $i) { }; m(G.new)   # Type check failed in binding $i: expected Int, got Any
sub f(@a) { };     f(F.new)   # Calling f(Any) will never work with declared signature (@a) ...
sub g(&c) { };     g(F.new)   # (bound without complaint)
```

Rakudo raises `X::TypeCheck::Binding::Parameter` for all three (`... expected Int but got G
(G.new(x => 1))`). It reserves the compile-time `X::TypeCheck::Argument` ("Calling f(Str) will never
work with declared signature (Int $i)") for a call whose argument types are all known at compile
time: literals, type objects, and variables declared with a type, with no named argument. Any other
call, including `f(1 + 1)`, `f(-1)` and `f($untyped)`, is checked at run time.

mutsu now makes the same split. The compiler records on each `CallFunc`/`CallFuncNamed`/`CallTrir`
site whether its argument types are static (`Compiler::static_arg_types`). The callee's binder adds
the `Calling ... will never work` wrapper only for such a site. Every other binding failure keeps
the binder's run-time exception. The positional-light and light binders now build that exception the
same way the general binder does, so the message names the object's real class and `.raku`. An
untyped `&c` parameter on the light path now checks its implicit `Callable` constraint. The
hand-written `@`/`%`/`&`/`Callable` messages in the general binder go through
`typecheck_binding_parameter_failure`, and a `%` parameter's message names the argument's type
alone, as rakudo's does.
