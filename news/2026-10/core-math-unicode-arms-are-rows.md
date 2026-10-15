# `Rat`, `FatRat`, `tree`, `chrs` and `bytes` answer through method-table rows

Three more name-matching families of the zero-argument cascade are gone
(ADR-11276 §9.112). `Any.tree` and `Any.chrs` are rows, `Range` and `Seq` reach
`Cool`'s `Rat`/`FatRat` and numeric rows, and `Blob.bytes` is a row that refuses a
type object. A `StrDistance` (the result of `tr///`) now numifies to its edit
distance everywhere, as in Rakudo, and `(1..3).FatRat`, `(65..67).chrs` and
`Seq.sqrt` answer from the same rows as their list forms.
