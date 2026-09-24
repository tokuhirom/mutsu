# IO handle reads go through the one decoder

ADR-0118 §2.4 made `nqp::decode`, `Blob.decode` and `IO::Path`
`slurp`/`lines`/`words` share a single byte decoder
(`builtins::decode_bytes_with_encoding_label`). That decoder NFC-normalizes its
result and dies on invalid UTF-8, as rakudo does. The IO **handle** read paths
were left out: `$h.get`, `$h.slurp`, `$h.readchars`, `$h.getc`, and a handle's
`.lines`/`.words`/`.split`/`.comb` still called `String::from_utf8_lossy`
directly. So a file holding U+2126 OHM SIGN read back as 8486 through a handle
and as 937 through the path, and invalid UTF-8 became U+FFFD where rakudo dies
(#9226).

Every handle read in the default UTF-8 mode now uses the shared decoder:

- **Whole records and whole streams** (a line, the rest of the file, a user
  `IO::Handle`'s `READ` bytes) go through `decode_utf8_handle_text`, which is
  the one decoder with the UTF-8 label. It is strict and applies NFC.
- **The per-code-point readers** behind `.readchars`/`.getc` decode each code
  point strictly, but they do not normalize it. The grapheme reader seeks back
  by a code point's byte length, and a normalized code point could change that
  length. Instead, each assembled grapheme or chunk is NFC-normalized once it
  is complete. That is the grapheme boundary the issue asked for, so
  `readchars(1)` on `e` + U+0301 returns U+00E9.

`utf8-c8` handles keep their own decoder untouched, because that encoding
exists to preserve invalid bytes.

Pinned by `t/io/io-handle-read-one-decoder-parity.t`, which sits alongside
`t/types/buf-decode-one-decoder-parity.t`.
