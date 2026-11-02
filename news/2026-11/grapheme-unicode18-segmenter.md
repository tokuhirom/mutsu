# Unicode 18 grapheme segmentation and utf8-c8 cluster breaks

Grapheme clusters are now segmented by an in-repo UAX #29 implementation
(`src/grapheme_break.rs`, data generated from the Unicode 18.0.0 UCD by
`scripts/gen-grapheme-break-data.py`) instead of the `unicode-segmentation`
crate, which trails the Unicode release roast tracks. The Unicode 18 GB9c rule
lets an Indic virama (`InCB=Linker`) join the following consonant without a
leading consonant, and utf8-c8 synthetics now always break on both sides, like
controls. `GraphemeBreakTest-0/3/4.t` and `utf8-c8.t` pass and are whitelisted.
