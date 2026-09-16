# `Str.trans` keeps from/to alphabets aligned across combining-mark graphemes

`Acme::Text::UpsideDown` maps ordinary text to its upside-down form with a
single `.trans($from => $to)` call, where `$from` and `$to` each concatenate
the same two 77-character alphabets in opposite order. One of the two
alphabets contains four graphemes built from a base letter plus a combining
mark (e.g. long s + combining dot below, U+0323) — single Raku characters,
but two Unicode codepoints each.

`expand_trans_spec()`, the helper `Str.trans` uses to turn a spec string into
its list of mapped units, split on Rust's `str::chars()` — Unicode codepoints
— instead of Raku characters (extended grapheme clusters). Because the
combining-mark alphabet sits at a different string offset in `$from` (second
half) than in `$to` (first half), splitting by codepoint shifted the two
sides' positions out of sync starting from wherever the first mark appeared
on each side — at position ~15 in `$to`, but not until position ~92 in
`$from`. Every mapped character in between then paired with the wrong
replacement, scrambling nearly the whole output.

Fixed by splitting `expand_trans_spec()`'s input into grapheme units (the
same `grapheme_units()` helper `substr`/`index`/`.chars` already use for
grapheme-correct indexing) instead of raw codepoints, so `from[i]` and
`to[i]` always name the same logical Raku character regardless of how many
codepoints it takes to spell. The handful of call sites that narrow the
result down to a `Vec<char>` for a single-codepoint `CharMap`/`CharClosure`
rule are safe unchanged, since the existing multi-codepoint check already
diverts any real combining-mark grapheme to the token-matching path before
reaching them.

Pinned with a compact regression in `t/types/trans.t` (a three-letter
alphabet pair, one side holding one combining-mark grapheme) so the fix
doesn't depend on the distribution's tarball, and re-measured
`Acme::Text::UpsideDown` from `red` (0/7) to `green` (7/7,
`ecosystem/dists/A/Acme--Text--UpsideDown~ec84324a.json`).
