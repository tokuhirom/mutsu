# A role forwards its own bare type parameter to a role it composes

`role Sub[::R = Any] does Base[R]` names `Sub`'s own type parameter `R` as the
argument of `Base`. mutsu evaluated that bareword at declaration time (there is
no binding for `R` yet), got a `Str`, and fixed `Base`'s parameter to `Str`:
`Sub.right('e')` worked and every later `Sub.right(9)` died with
`expected Str but got Int`. The argument is now treated like `does Base[::R]`:
it is forwarded and resolved when `Sub` is concretized.

Found through the Monad distribution (`t/13_subtype_hooks.t` now matches raku,
27 ok). `t/14_concurrent_types.t` still differs; that is a separate
thread-only problem, filed as #17042.
