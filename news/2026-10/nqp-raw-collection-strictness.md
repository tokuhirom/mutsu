# nqp raw VMArray/VMHash: `for`, `"$raw"` and native raw arrays refuse as Rakudo does

A raw `VMArray`/`VMHash` has no `.iterator` and no `.Stringy`, and a native-element raw
array (`BOOTIntArray`, ...) is not an `Any` (#12689). `for $raw { }` and a lone `"$raw"`
now raise `X::Method::NotFound`; binding a native raw array to an untyped parameter
raises `X::TypeCheck::Binding::Parameter` naming `BOOTIntArray` instead of `List`.
`"a$raw"` still succeeds (Rakudo dies) because a block part and a variable part reach
`StringConcat` the same way; that stays a TODO at the op.
