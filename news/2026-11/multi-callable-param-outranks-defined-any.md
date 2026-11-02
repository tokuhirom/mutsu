# An untyped `&c` parameter out-ranks an `Any:D` multi candidate for code

`multi f(&c)` against `multi f(Any:D $v)` picked the `Any:D` candidate for a
Block, because ranking charged an untyped `&` parameter a flat 1000 instead of
the distance to its implicit `Callable` type. It now ranks as `Callable`, as in
Rakudo; `Deps`' `register(&function, ...)` factories take their own candidate
again (#16058).
