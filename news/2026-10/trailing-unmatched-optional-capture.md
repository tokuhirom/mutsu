# A trailing unmatched `(x)?` is no longer a Match list element

An unmatched optional capture reserves a positional slot so that a later
capture keeps its index (`(a)? (b)` on "b" binds `$1`). mutsu also kept that
reserved slot when it was the *last* one, so `"12" ~~ / (\d) (y)? /` listed a
trailing `Nil` and reported two elements; raku's capture list extends only to
the last bound slot and reports one (#10650).

Stored capture nodes and built Match objects now drop trailing unbound slots
(`PosSlot::bound_len`; user-visible builders also drop trailing alternation
padding via `PosSlot::visible_len`), so `.list`, `.elems`, `.pairs`, `.raku`
and the mid-match `$/` seen by an embedded code block all agree with raku.
Interior unbound slots stay. `.caps` and `.chunks` also stop listing an
interior unbound slot as a `0 => Nil` pair.
