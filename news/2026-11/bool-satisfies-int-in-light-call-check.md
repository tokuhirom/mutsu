# Bool binds an Int parameter on the positional-light call path

`Bool` is an `Int` in Raku, but the fast type check of the positional-light
call path (`fast_type_check` and its tagged form) admitted a `Bool` only for
`Bool`/native `int`, so the second call of a class-nested `sub f(Int $x)` with
`True` (or the `Bool` type object) died with "expected Int but got Bool". The
first call took the general binder and passed, which hid it. GD's
`check-compat(Int $fill, ...)` called with `Bool :$fill` hit it; all four GD
test files now match rakudo.
