# A type object has no `coerce` method

`Int.coerce("3")` answered `3` through a by-name gate on the interpreter's slow path, though
Rakudo has no such method (`X::Method::NotFound`); coercion is the coercion type's HOW's
(`Int(Str).HOW.coerce(Int(Str), "3")`). The gate is gone (ADR-11276 §9.192), and a class's own
`coerce` method is unaffected.
