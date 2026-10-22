# A multi method candidate sees the FETCHed value of a Proxy element argument

`$obj.m(@a[0])` with `@a[0] := Proxy.new(...)` (or `$obj.m($css.padding[0])`) found no candidate
(`Cannot resolve caller m(K:D: Any:D)`) or a plain method died with a Proxy type error, because the candidate
matcher checked the Proxy itself against `Numeric`. The matcher now checks what the Proxy FETCHes, as the binder
does. CSS::Box's `measure($css.padding[0])` therefore reaches CSS::Properties' calculator and percentages scale
by `reference-width`. Fixes #14555.
