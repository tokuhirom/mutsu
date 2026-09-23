# `.contains(Regex, $pos)` sees the whole subject; `substr-rw` counts graphemes

Two correctness bugs from the Str complexity audit (issue #9146).

**`.contains(Regex, $pos)` matched against a copied suffix.** The Regex needle
ran against the substring starting at `$pos`, so anything that looks before
`$pos` saw the start of a new string: `"abc".contains(/<?after b>c/, 2)` was
`False` (rakudo: `True`), `"abc".contains(/<<c/, 2)` was `True` (rakudo:
`False`), and so was `"abc".contains(/^c/, 2)`. It now matches against the
whole subject with `$pos` (a grapheme index, converted to the regex engine's
codepoint offset) as the minimum start, which is `:c($pos)` semantics:
lookbehind and `<<` see the preceding text and `^` stays anchored at the start
of the string.

**`substr-rw` assignment counted codepoints.** `substr-rw($s, 1, 1) = "X"` on
`"q\x[301]ab"` replaced the combining mark instead of the `a`, while the read
side, `.substr` and `.chars` count graphemes. `assign_substr_rw` now slices the
string's grapheme units. (The issue's other half, `.substr-eq` and
`.contains($str, $pos)` counting codepoints, was fixed by the cached grapheme
index of #9140 while this was in flight; the new test pins those too.)

Found along the way and filed separately: the sub form `substr-rw($t, ...)`
finds its target variable by scanning for an equal value, so it can write to
a different variable holding the same string (#9183).

Pinned by `t/types/str-positioned-search-graphemes.t`.
