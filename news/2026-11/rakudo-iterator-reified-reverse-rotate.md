# `Rakudo::Iterator.ReifiedReverse` and `.ReifiedRotate`

The two internal iterators over a reified `IterationBuffer` (reversed, and
rotated by `$n`) that ParaSeq's `reverse`/`rotate` build their Seq on are
available next to `Rakudo::Iterator.Empty` and `.OneValue`. ParaSeq's
`t/02-reverse.rakutest` passes.
