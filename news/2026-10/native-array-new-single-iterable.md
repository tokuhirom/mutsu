# `array[T].new` stores a single Iterable argument's elements

A native array's one-argument `new` now follows rakudo's `array.new(\values)`.
An Iterable argument contributes its elements, whether it is itemized
(`$[1, 2]`, `$(3, 4)`) or a Blob/Buf (`array[uint8].new($buf)`). Before this
change mutsu stored such an argument as a single element. `Array.new($[1, 2])`
still keeps the itemized argument whole.

PDF's `t/io-util.t` packs bits into a Buf and compares
`array[uint8].new($buf)` with the expected bytes, so that test failed (#12907).
