# A user `postcircumfix:<[ ]>` candidate decides which element an assignment addresses

`@a[1.5] = "X"` and `@a[2.5] := "Y"` on an array whose `multi sub postcircumfix:<[ ]>`
candidate rounds the index (Array::Rounded) stored at the truncated index, while a read
used the candidate. The element store now runs the matching user candidate first, in a
capture mode: the CORE subscript routine the candidate delegates to (`old-same SELF,
index.round`) records the index it was handed, and the native store goes to that index.
The candidates a block's `use` imported are asked too, so the element store carries the
same `LexImportBuiltinOp` prefix the read has (#16294). Array::Rounded's
`t/01-basic.rakutest` now matches `raku` (35 ok).
