# Text-mode file reads decode CRLF to LF

A Raku handle's default `nl-in` is `["\n", "\r\n"]`, and the decoder normalizes a
matched ending to `"\n"` on every **text-mode** read — not only in the chomping
routines. So `"f".IO.slurp` on a file containing `a\r\nb` yields `"a\nb"` under
Rakudo, while `:bin` and `Blob.decode` keep the bytes verbatim and a lone `\r`
(not a line ending) survives untouched.

mutsu returned the raw CRLF from every text read. `.lines` looked right only
because it splits on both endings itself; anything that saw the whole content —
`slurp`, `comb`, `readchars` — handed back `\r\n`.

All the text-decode entry points now apply the translation: the `slurp` sub,
`IO::Path`'s `slurp`/`lines`/`words`/`comb`, `IO::Handle.slurp` (both the
interpreter and the VM-native path), and the character reads behind
`.getc`/`.readchars`. `\r\n` is a single grapheme cluster, so a bounded
`.readchars(n)` still returns `n` characters after the translation. Binary
handles and `:bin` reads are excluded, and writing is untouched — `spurt` and
`print` emit exactly what they are given.

This was the second blocker behind `Template::Mustache`'s
`92-specs-file.rakutest`; the mustache spec's "Standalone Line Endings" case
writes a CRLF template to disk and expects the rendered output to come back with
plain newlines, which is precisely the decoder behaviour the test's own comment
describes.

Pin: `t/io-crlf-translation.t`.
