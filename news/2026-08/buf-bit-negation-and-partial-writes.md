# Buf bit negation and partial writes match Rakudo

Numeric bitwise negation now numifies a `Buf` before applying `+^`. Because a
buffer numifies to its element count, `+^Buf.new(0xff, 0x00)` now produces
`-3` instead of `-1`. String bitwise negation remains a separate byte-wise
operation.

`Buf.write-ubits` and `Buf.write-bits` now also follow Rakudo's partial-byte
masking behavior. In particular, writing four bits with value `3` at bit
offset zero into `Buf.new(0x05, 0xAA)` produces `Buf.new(0x30, 0xAA)` rather
than retaining the original low bits as `0x35`.

All mutable Buf dispatch paths share the bit-writing transform in
`src/builtins/buf_bits.rs`, eliminating the separate masking implementations
that could previously diverge. `t/native-buf-mut.t` pins both numeric
negation and non-zero untouched-bit cases for signed and unsigned writes.
