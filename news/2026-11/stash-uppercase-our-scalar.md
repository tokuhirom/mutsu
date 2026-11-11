# An uppercase `our $Baz` lists as `$Baz` in its package stash

A package's stash lists a `$` scalar with its sigil, but mutsu's store keeps a
scalar under its sigil-less qualified name (`P::Baz`) and guessed the stash
spelling from the tail: an uppercase-leading name read as a type or term. So
`package P { our $Baz = 1 }` listed `P::.keys` as `(Baz)` and `P::<$Baz>`
read `Any`; the same happened to a scalar bound through a stash
(`Foo.WHO<$Baz> := 'bar'`), which is what CompUnit::Util's
`set-in-WHO(tmp.WHO, 'Foo::Bar::$Baz', 'bar')` does.

The lexical state now records the store keys that hold a `$` scalar, beside
the existing record of sigil-less terms, and the stash listing consults it
before falling back to the spelling heuristic. CompUnit::Util's
`t/01-utils` `descend-WHO finds $Baz` now passes; its `set-in-WHO multiple`
still fails because two `my package tmp` declarations in separate `EVAL`s
share one stash (#17449).
