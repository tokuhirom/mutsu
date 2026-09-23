# Text-segmentation Unicode properties stop allocating a `String` per call

The last piece of [#8999](https://github.com/tokuhirom/mutsu/issues/8999).
#9013 and #9018 had already turned `unicode_general_category` and
`unicode_script_name` from linear regex probes into generated tables returning
`&'static str`; the release comment on the issue named what was left: the
property functions in `src/builtins/uniprop/text_seg.rs` still returned
`String`.

All six of them -- `unicode_grapheme_cluster_break`, `unicode_joining_type`,
`unicode_joining_group`, `unicode_sentence_break`, `unicode_word_break` and
`unicode_line_break` -- only ever answer with a string literal, and each one
called `.to_string()` on it. So every lookup heap-allocated a copy of a
`&'static str` that was already in hand. Two of the callers only compared the
result and dropped it: `string_pos.rs` asks whether a character's
Grapheme_Cluster_Break is `"Control"`, and `unicode_word_break`'s letter arm
asked `unicode_line_break(ch) == "SA"` -- an allocation inside a property
lookup, done purely to compare.

They now return `&'static str`. The only caller that needs an owned string,
`.uniprop` in `uniprop/lookup.rs`, builds its `Value` with `Value::str_from`,
so it allocates exactly as before; everything else allocates nothing.

There is no behavioural change -- every function returns the same literal it
returned before -- so the existing per-property tests are the regression
guard: `t/types/string/uniprop-{grapheme-cluster-break,sentence-break,word-break}-uax29.t`,
`uniprop-joining-{type,group}.t`, `uniprop-line-break-ideographic.t` and the
`t/regex/` property-value tests, all passing unchanged.
