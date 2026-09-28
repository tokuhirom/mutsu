# `Grapheme_Cluster_Break` stops hand-deriving from a stale exception list

Found by the 2026-09-27 doc-diff sweep
([#9799](https://github.com/tokuhirom/mutsu/issues/9799)):
`"\x[1F3FF]".uniprop("Grapheme_Cluster_Break")` answered `Other` where Rakudo
answers `Extend`. U+1F3FF is an emoji skin-tone modifier (gc=`Sk`); Unicode 11
moved the whole `U+1F3FB..U+1F3FF` block from `E_Modifier`/`Other` into
`Grapheme_Cluster_Break=Extend` via `Other_Grapheme_Extend`, and mutsu's
`unicode_grapheme_cluster_break` was still classifying `Extend` by testing
`\p{Grapheme_Extend}` plus hand-maintained `Prepend`/`SpacingMark` exception
lists that predated that change.

Rather than patch the exception lists (which would just go stale again at the
next Unicode revision), `unicode_grapheme_cluster_break`
(`src/builtins/uniprop/text_seg.rs`) now queries the `regex` crate's own
`Grapheme_Cluster_Break=<value>` enumeration directly, one value at a time,
falling back to `Other`. That table is generated from a current Unicode
Character Database, so it can't drift the way a hand-copied set of code
points can; it agrees with every code point the old `is_gcb_prepend` and
`is_gcb_spacingmark_exception` helpers listed, so both are removed.

`t/types/string/uniprop-grapheme-cluster-break-uax29.t` gained two pins for
`U+1F3FB`/`U+1F3FF` (`Extend`); the rest of the file, plus
`roast/S15-unicode-information/uniprop.t` and
`roast/S05-mass/properties-derived.t`, keep passing unchanged.

`Word_Break` and `Sentence_Break` (same file) still derive their own `Extend`
class from the same stale `\p{Grapheme_Extend}` check and share this bug for
the same code points; filed separately as
[#10021](https://github.com/tokuhirom/mutsu/issues/10021) rather than folded
into this fix.
