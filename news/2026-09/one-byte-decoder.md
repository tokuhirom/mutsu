# `nqp::decode`, `Blob.decode`, `:replacement` and named-encoding IO reads share one decoder

mutsu had four byte decoders:

- `nqp::decode`, written inline;
- `builtins::decode_bytes_unnormalized` / `decode_bytes_with_builtin_encoding`, used by
  `Blob.decode`;
- `Interpreter::decode_with_encoding`, used by IO reads with a named encoding, sockets and `Proc`;
- `decode_with_encoding_and_replacement`, used by `.decode(:replacement)`.

The second and third were line-for-line copies of each other, down to the UTF-8 error message,
except that only one of them NFC-normalized its result. `nqp::decode` accepted bytes above 127 as
"ascii" and kept a UTF-8 BOM.

So the same bytes could decode to different strings depending on the path. `Buf.new(0xE2, 0x84,
0xA6).decode` gave U+03A9, while `nqp::decode` and `.decode(:replacement<?>)` of the same buffer
gave U+2126. A slurped file containing those bytes also kept U+2126.

The builtin decoder is now the only implementation (ADR-0118 §2.4). `decode_with_encoding` now
only resolves user-registered encoding names and then calls it. A `:replacement` decode without a
replacement is exactly that decode; with one it keeps its lenient per-encoding handling and
normalizes the result. `nqp::decode` calls the builtin decoder too. `slurp`, `lines` and `words`
now NFC-normalize in their shared UTF-8 post-processing.

`t/types/buf-decode-one-decoder-parity.t` pins 11 rows. The IO *handle* read paths still decode
with `String::from_utf8_lossy` directly and are tracked separately.
