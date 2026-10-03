# Coercion types fail on a non-numeric Str instead of yielding 0

`sub f(Int() $o)`, `my Int() $v` and `for ... -> Int() $o` used to turn a
non-numeric string such as `"x"` into `0`, because the shared
`coerce_value` helper carried its own lenient string parsers for `Int`, `Num`,
`Rat` and `Complex`. Those copies are gone: a Str now coerces through the same
native `Str.Int`/`.Num`/`.Rat`/`.Complex` implementation a method call uses,
so the result is the lazy `X::Str::Numeric` Failure Rakudo produces, and radix
(`"0x10"`) and rational (`"3/2"`) strings convert like their value (#11291).
