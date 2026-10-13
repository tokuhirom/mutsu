# A user class over a buffer reaches the `Blob`/`Buf` method rows

A class composing a buffer role over a `VMArray` body (`class B does Blob[uint8] is repr('VMArray')`) now answers
every built-in buffer method from the method table's `Blob`/`Buf` rows, keeping its own class, instead of a handful of
name-matching cascade arms written for it. With that, the last buffer arms are gone: `subbuf`, `subbuf-rw`, the
`read-*` accessors, `read-ubits`/`read-bits` and the buffers' `AT-POS`/`EXISTS-POS` are rows that bind their arguments
as Rakudo's candidates do (ADR-11276 §9.107).

Behaviour moved toward Rakudo on the way: `B.new(...).Str` raises `X::Buf::AsStr`, `.of`/`.Blob`/`.Buf`/`.join` work on
such an instance, `Blob.subbuf-rw` is gone while `Buf.subbuf-rw(1)` answers, `subbuf(1^..^4)` excludes both ends and
`subbuf(1..*)` no longer panics, `Blob.new(1,2).AT-POS(5)` raises `X::OutOfRange`, `Buf.EXISTS-POS` exists,
`read-ubits` reports Rakudo's out-of-range messages, the `read-*` methods refuse a non-`Endian` byte order, and
`buf16.new(1).Blob` keeps its element type.
