# Str.raku escapes a combining mark that has no base

`Str.raku` now writes a grapheme-extending code point as `\x[HEX]` when it starts a grapheme
(at the start of the string or after a control character such as `\n`), matching Rakudo, so the
output no longer merges visually with the preceding character and round-trips (#12373).
