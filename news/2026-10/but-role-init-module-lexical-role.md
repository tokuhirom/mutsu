# `but R(v)` finds a module's file-scoped lexical role

`$x but R(v)` inside a sub of a module whose `R` is a file-scoped `my role` used to be
treated as a coercion call and died with `X::Coerce::Impossible`, because the role is only
known by its mangled storage key from there. The initializer lookup now falls back to the
unique lexical role of that name. Found via the P5-X distribution, whose test file now passes.
