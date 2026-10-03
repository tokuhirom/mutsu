# A closure sequence that ends itself is still lazy

`my @a = 1, { last if $_ >= 5; $_ + 1 } ... *` used to leave `@a` an ordinary
eager array in mutsu: `@a.is-lazy` was False and `@a.elems` answered 5. In
Rakudo, laziness comes from the `... *` iterator, not from whether the
generator happens to end. That array is lazy, `.elems`, `[*-1]` and `.push`
throw `X::Cannot::Lazy`, `.raku` is `[...]`, and only `.eager` answers the
complete list (#11098).

The sequence builder generates an eager prefix. When the generator ran `last`
inside that prefix, the builder handed back a plain cache flagged as
infinite, which lost the closure-sequence shape that the `@`-assignment path
recognises as lazy. It now returns the same closure-sequence list an endless
generator gets, already marked finished, so every lazy path treats both alike.

Making that array lazy exposed a second, older gap shared by every lazy
`@`-array. Reading past its reified end answered `Nil`
(`my @a = lazy 1, 2; @a[3]`), where an Array answers its element default
`Any`. A positional read on an array-context lazy list now reads it as an
Array, and a bare lazy list still answers `Nil`.

Still open, filed as #11131: `.join` on a lazy list answers `"..."` in
Rakudo, where mutsu throws (or, for `1..*`, joins a huge prefix).
