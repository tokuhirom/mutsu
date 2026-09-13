# A regex literal atom is a grapheme, not a codepoint

`say so "क्ष" ~~ /क्ष/` used to die with `Unrecognized regex metacharacter ्
(must be quoted to match literally)`, where Rakudo answers `True`. The cluster
`क्ष` is a single grapheme — `क` U+0915, the virama `्` U+094D (general
category `Mn`), `ष` U+0937 — but `parse_regex_structural` scanned the pattern
one `char` at a time and judged every unhandled non-alphanumeric glyph on its
own. A nonspacing mark is not `is_alphanumeric()`, so a codepoint that only
ever exists *inside* a grapheme was reported as if it stood alone. That kept
`Lang::Transliterate::Sa::IAST` from loading: its line 1223 is
`$result.subst(/क्$virama ष/, 'क्ष', :g)`.

Letting the mark through as a literal would have been half a fix, because the
matcher was codepoint-level on the other side too, and there the same
assumption gave the *opposite* wrong answer: `so "a\x[094D]b" ~~ /\x[094D]/`
returned `True` where Rakudo returns `False`, since the mark is inside the
subject's first grapheme and is never an atom of its own. Raku strings are NFG;
the atom has to be the grapheme at both ends.

So the parse now re-joins what the scan split. `merge_grapheme_literal_tokens`
(the new `src/runtime/regex_parse_grapheme.rs`) takes a run of adjacent plain
literal tokens, segments its text with UAX #29, and emits one atom per cluster:
a single-codepoint cluster stays `RegexAtom::Literal`, a longer one becomes the
new `RegexAtom::LiteralGrapheme`, NFC-composed so that `/o\x[328]\x[304]/` and
the subject `"\x[1ED]"` are the same string. A run whose codepoint count equals
its grapheme count — every ASCII pattern — is passed through untouched. One
trailing quantified literal may join the run, because in `/क्ष+/` the `+`
applies to the whole cluster. The same lowering serves `/'क्ष'/`, and the
source-tree fast path (`RegexTree::lower_execution`) now declines any literal
node carrying a combining mark rather than lowering it codepoint by codepoint:
a cluster can straddle two of its nodes (`/क्ष+/` is `Literal("क्")` followed
by a quantified `Literal("ष")`), and only the runtime parser re-joins across
that boundary.

Matching follows. A literal atom matches only a whole grapheme, so `/a/` no
longer matches the base of `"a\x[5B4]b"`, and it must start on a grapheme
boundary, so nothing matches at a position sitting on a combining mark or on
the consonant a virama joined — `"a\x[094D]b" ~~ /\x[094D]/` and
`"क्ष" ~~ /ष/` are both `False`, as in Rakudo. (Character classes and Unicode
properties still match inside a cluster, which is the same divergence in a
different atom; it needs `ClassItem` to carry graphemes and is tracked
separately.) `grapheme_end` learned
the Indic-conjunct case it could not see: after a combining-mark run containing
a virama (canonical combining class 9), it falls back to real UAX #29
segmentation, which is what distinguishes `क` + `्` + `ष` (one cluster) from
`a` + `्` + `b` (two). That fallback is only paid on a linker-bearing cluster;
the plain mark scan still handles everything else. It also learned GB4: nothing
extends a control character, so `"\t\x[0300]"` is two graphemes and `/\t/`
still matches its first.

`\r\n` is a grapheme by the same rule, and re-joining it is what the two
regressions this caught were about: an interpolated `"\r\n"` and a quoted
`"\r\n"` both arrive as two literal tokens, and once a literal had to be a
whole grapheme neither half matched on its own. The run predicate therefore
triggers on `\r` as well as on a combining mark — those are the only two
codepoints a following one can join.

Pinned by `t/regex/regex-grapheme-literal-atom.t`, whose 23 assertions all pass
on Rakudo as well as mutsu.
