# `is raw` no longer makes a multi candidate narrower

rakudo ties `multi f($v is raw)` with `multi f($v)` and `multi f(Any:U)`; mutsu
ranked the `is raw` one first, so an `is default` sibling never got to break the
tie. PDF::COS's `role` fallback `COERCE($v is raw)` therefore beat the class's
`COERCE(Any:U) is default`, and `my PDF::COS::Null() $n = Any` died. Only `is rw`
counts as narrower now, for subs and methods alike, and an untyped `is raw` pair is
reported ambiguous as in rakudo.
