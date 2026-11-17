# `.join` composes a combining mark across the seam between pieces

`("a", "\x[301]").join eq "\x[e1]"` was `False`: `~` and `[~]` restore NFC
across their seam, but the seven `join` code paths (`List`/`Seq`/`Any.join`,
the `join` routine, the junction-threading join, `Capture`/`Match`/`Hash`
joins and `nqp::join`) concatenated the pieces with Rust's `join` and left a
combining mark at the start of a piece (or of the separator) uncomposed. The
string looked right through `.chars` and `.ords`, which normalize while
reading, but `eq`, hashing, `index` and an encode/decode round trip saw the
non-NFC form. All seven now build through one primitive,
`str_prim::join`, which joins and restores NFC (a no-op pass for ASCII and
already-normalized text), as rakudo's NFG strings are.
