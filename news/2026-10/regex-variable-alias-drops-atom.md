# `$foo=ATOM` in a regex interpolates `$foo` and drops the atom

`m/a $foo=[<two>] d/` matched `$foo`'s value, then a literal `=`, then the atom:
`"abcd"` failed and `"abc=bcd"` matched. Rakudo's grammar parses the alias and
its actions keep only the interpolation, so the atom is dropped and `"abcd"`
matches. The interpolation pass of the runtime regex parser now skips an
`= ATOM QUANT?` after a `$name`, `${name}` or `@name` (outside a quoted
literal), through `regex_variable_alias`. S05's intent (assigning the atom's
match to the variable) is implemented by neither Rakudo nor mutsu.
