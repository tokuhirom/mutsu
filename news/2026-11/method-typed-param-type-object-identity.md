# `$param =:= TypeObject` in a method body

A typed readonly parameter of a **method** (`method parts(CookBase :$split-by?)`)
is now treated, like a `sub`'s, as bound straight to the argument, so
`$split-by !=:= CookBase` is False when the parameter is not supplied. The
`sub` path was fixed in #16963; the method/routine-valued block path did not
record the property. Cooklang's `parts` no longer splits its part list on
every call, which restores `t/math.t` and `t/fixes.t`.
