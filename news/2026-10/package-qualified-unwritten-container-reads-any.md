# A never-written package-qualified `@`/`%` variable reads as `Any`

`say %GLOBAL::nv.raku` printed `{}` and `@GLOBAL::nv.raku` printed `[]`: the
`GetHashVar`/`GetArrayVar` fallbacks gave every unresolved name the empty
container an undeclared *lexical* gets under `no strict`. A package-qualified
slot nobody wrote is an empty Scalar in Rakudo, so it now reads as `Any`
(`.elems` is 1, `for` iterates it once, `//` falls through). Mutating it
vivifies an itemized Array, as Rakudo does: `@GLOBAL::a.push(1)` and
`%GLOBAL::h.push((a => 1))` leave `$[1]` and `$[:a(1)]`, and the `%`-sigil
hash-push arm no longer claims such a vivified Array (#10962).
