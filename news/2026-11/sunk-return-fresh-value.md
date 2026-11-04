# A sunk `return` of a fresh value runs its `sink`

`sub f { return S.new }; f;` did not run `S`'s `sink` method; rakudo 2026.09 does,
for `return` of a method call (`S.new`, `$v.clone`), of a `does`/`but` mixin
with a composed `sink`, parenthesized or under `if`. `return $x`, a subscript, an
assignment and a list still hand back a container or a list and are not sunk.
The sink analysis treated every explicit `return` as a container; it now
looks at what `return` hands back. Found by running the `raku-doc` example for
`sink` (`Language/contexts`) against mutsu and `raku`.
