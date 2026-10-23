# `%!attr := SomeMapSubclass.new(...)` keeps the bound instance

A `:=` bind of an Associative class instance (a user subclass of `Map`) to a
private `%` attribute went through the hash-assignment coercion and replaced
the instance with a plain `Hash`, so `%!attr.^name`, the accessor and
`Attribute.get_value` all answered `Hash`. A bind now aliases the instance
itself, after the same Associative check `SetLocal` applies (a non-Associative
is still refused with `X::TypeCheck::Binding`). The `ValueClass` distribution's
`t/01-basic.rakutest` (its `has %.a` value-class tweak) now matches rakudo,
79/79.
