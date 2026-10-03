# A `for` loop's coercion-typed parameter coerces its items

`for '3', '4' -> Int() $o { ... }` used to die with `Type check failed in
binding to parameter '$o'; expected Int but got Str ("3")`. The loop checked
its parameter's type with a plain type match, so a coercion type such as
`Int()` or `Int(Str)` never coerced. Each item is now coerced the way a
signature parameter coerces its argument. That logic moved out of the
signature binder into a shared `bind_coercion_param_value`, which both
callers use.

Found via the `GeoIP2` distribution, which walks the captures of an IP-address
regex with `for $/[0] -> Int( ) $octet { $octet.polymod(2 xx 7) }`. Both of its
test files now pass under mutsu.
