# Signature-literal bind checks parameter types

`:(Int $a, $b) := \("x", 2)` bound `$a` to `"x"` silently, because the parser
lowered the bind to plain per-name binds and dropped the parameter's type.
Typed `$` parameters now route their value through a check that raises
`X::TypeCheck::Binding::Parameter` with the signature binder's message
(`Type check failed in binding to parameter '$a'; expected Int but got Str ("x")`)
before the name is bound (#11542).
