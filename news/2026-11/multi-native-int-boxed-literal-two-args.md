# `f 42, my int $x` binds a boxed literal to a native `int` multi parameter

rakudo dispatches a two-argument call that holds a native argument through the full bind check,
where a boxed `Int` binds an `int` parameter, and ranks the native candidate first:
`multi f(int, int); f 42, my int $x` works (`f 42, 7` and a three-argument call with one native
still do not). The multi dispatcher now accepts and ranks the boxed argument as the native type in
exactly that shape (#16483).
