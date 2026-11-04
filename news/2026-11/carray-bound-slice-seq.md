# A bound CArray slice is a Seq

`my $q := $c[1, 3]` on a `CArray` is now the `Seq` the imported
`postcircumfix:<[ ]>` returns, as in rakudo. The bind form of a subscript did
not carry the lexical-import scope the plain read runs in, so the candidate was
never found and the Array was sliced natively into a List of cells.
