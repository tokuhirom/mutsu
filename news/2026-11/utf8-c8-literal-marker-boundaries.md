# A literal U+10FFFD read from a file or built with `chrs` is no longer a utf8-c8 synthetic

ADR-17800 stores a literal `U+10FFFD` inside a `Str` as a doubled marker, so the
four codepoints `U+10FFFD x H H` are told apart from the grapheme the `utf8-c8`
decoder makes for an invalid byte. Its first slice covered `Blob.decode`, `.chr`,
`\x` escapes and the encoders. This second slice escapes the marker at the
remaining places codepoints enter a string — `slurp` and `IO::Path.slurp`, the
handle reads (`readchars`, `getc`, the per-character socket and stdin reads),
every codec of the stream decoder (so `lines`, `get` and
`Encoding::Decoder` see it before the grapheme hold-back does), the non-builtin
and lenient decoders, `chrs` (method and routine), `Uni.Str` and `nqp::chr` —
and undoes it where codepoints are exposed: `.NFC`/`.NFD`/`.NFKC`/`.NFKD` of a
string and `nqp::codes`. `"\x[10FFFD]xFF"` written raw in source text is still
the one boundary left.
