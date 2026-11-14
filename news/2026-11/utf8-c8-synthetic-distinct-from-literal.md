# A utf8-c8 synthetic is distinct from the literal text U+10FFFD x H H

`Blob.new(255).decode('utf8-c8')` is one synthetic grapheme whose codepoints
read `U+10FFFD 'x' 'F' 'F'`. mutsu could not tell it from text that merely
contains those four codepoints, so decoding its UTF-8 bytes again gave back
an equal one-grapheme string where rakudo gives four graphemes and an unequal
string. LibTmux uses exactly that round trip to reject bytes that are not
Unicode.

ADR-17800 (Proposed) keeps the synthetic's in-band spelling and stores a
literal `U+10FFFD` as a doubled marker instead. The first slice escapes it in
the decoders, `chr` and `\x`/`\o` escapes, and unescapes it in the encoders,
`Str.encode`, `.ords` and standard output. LibTmux's `t/unit/query` now matches
rakudo, so all of LibTmux's baseline files do.
