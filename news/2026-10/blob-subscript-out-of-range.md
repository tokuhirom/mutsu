# A `Blob` subscript past its end is `X::OutOfRange`

`Blob.new(1, 2)[5]` read `0`, and so did a negative position on every buffer, because the subscript opcode read a buffer's
storage itself instead of answering what the `Blob`/`Buf` `AT-POS` rows answer. It now shares their implementation
(`blob_pos::subscript_at`): a `Blob` (`utf8`, `blob16`, a user class doing `Blob[uint8]`) past its end, and any buffer at a
negative position, is an `X::OutOfRange` failure; a resizable `Buf` still reads `0` past its end. The `AT-POS` rows return
that failure instead of throwing it, as Rakudo's do (#13312).
