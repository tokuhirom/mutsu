# `.NFD` and the other normalization methods build their Uni without copying the text

`Str.NFD` (and `.NFC`, `.NFKD`, `.NFKC`) made up to four copies of the receiver's text before
building the `Uni`:

- the string form;
- an owned copy of the marker-unescaped text;
- an owned copy of the normalized text;
- the codepoint vector.

On top of that, the doubled-marker check ran a substring search even on ASCII text. JSON::Fast's
`str-escape` calls `.NFD` once for every string `to-json` writes.

Now a plain Str is read in place. The `Uni` is built straight from the normalized text's
codepoints. The marker check returns at once for ASCII text.

`str-escape` on a 13-character string: 24.2k -> 23.2k instructions per call.
`bench-json-fast-encode` (`scripts/bench-det.sh`, `+jit`, local A/B): 759.3M -> 751.4M
instructions (-1.0%), 349,655 -> 327,152 allocations (-6.4%).
