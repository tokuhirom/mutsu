# A user `postcircumfix:<{ }>` takes the `:exists` and `:delete` adverbs

`multi sub postcircumfix:<{ }>(PCM:D $c, \key, *%adv)` received the value adverbs (`:k`, `:v`,
`:kv`, `:p`) as named arguments but never `:exists` or `:delete`: those compiled to the built-in
`EXISTS-KEY` / `DELETE-KEY` paths without asking the `postcircumfix` candidates. They now ask the
same candidates the plain subscript does, a local declaration and the ones a block-scoped `use`
imported alike, and pass `:exists` (`:!exists` as a False) or `:delete`. A positional subscript
stays with the core routine, as in Rakudo, whose typed `:exists` candidates beat an untyped user
one (#14538).
