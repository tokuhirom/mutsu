# Regex `:i` literals now case-fold LATIN SMALL LETTER LONG S

`"a\c[LATIN SMALL LETTER LONG S]b" ~~ / :i "s" /` used to return `False`,
although Rakudo returns `True`. The direct literal, grapheme literal, and named
literal fallback paths compared lowercase mappings, where long s remains `ſ`.

They now share the regex engine's case-folding primitive, so long s folds to
`s` consistently with the expansion path used for characters such as `ß` and
`ﬁ`. Character-class behavior is unchanged: Rakudo does not treat `<[s]>` as a
match for long s under `:i`.

Pin: `t/regex/ignorecase-casefold-equivalence.t`, also run unchanged under
Rakudo. Fixes #8440.
