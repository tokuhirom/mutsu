# Word_Break classifies emoji modifiers and spacing marks as Extend

`uniprop("Word_Break")` derived `Extend` from `Grapheme_Extend`, which misses the
emoji modifiers U+1F3FB..U+1F3FF and spacing marks. It now queries the `regex`
crate's own `Word_Break=Extend` table, mirroring the earlier
`Grapheme_Cluster_Break` fix. `Sentence_Break` for the emoji modifiers is `Other`
in Rakudo, so it is unchanged and pinned by a test.
