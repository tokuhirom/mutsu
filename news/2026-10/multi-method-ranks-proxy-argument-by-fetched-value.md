# A multi candidate is ranked by the FETCHed value of a Proxy argument

With candidates `m(Numeric $v)` and `m(Str $v)`, `$obj.m(@a[0])` where `@a[0]` is `:=`-bound to a Proxy that
FETCHes `'abc'` picked neither (or the `Numeric` one): the plain-parameter fast matcher
(`args_match_simple_positional`) checked the Proxy itself. It now checks what the Proxy FETCHes, on each call.
Fixes #14581.
