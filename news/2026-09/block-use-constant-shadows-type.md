# A block-scoped `use` constant shadows a same-named type

A sigil-less constant imported by a `use` inside a block now shadows a
same-named type of the file within that block, as in Rakudo:

```raku
grammar G { token TOP { a } }
{
    use secp256k1;   # exports `our constant G` (the curve generator)
    say G;           # the constant, no longer the grammar type object
}
say G.^name;         # G
```

Three pieces were involved. The bareword resolution chain now lets a sigil-less
constant that is live in the current scope outrank the type branch (the
module-scope fallbacks still rank below it). The per-site bareword type memo
re-checks the name's term key on a hit, since an import changes the answer
without a registry write. And the BEGIN-time preload of a nested `use` no
longer leaves the module's exported constants bound in the loading scope, so
they are not visible to the rest of the file ahead of the block that imports
them (#9963).
