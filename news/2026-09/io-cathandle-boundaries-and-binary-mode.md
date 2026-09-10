# Fix IO::CatHandle source boundaries and binary mode

`IO::CatHandle.words` now treats each source boundary as a word boundary, and
`IO::CatHandle.slurp` returns `Nil` for a cat with no sources. Binary mode is
propagated to every source, including when `.encoding` is switched to `Nil`
after construction, and can be changed back to text encoding while reading.
