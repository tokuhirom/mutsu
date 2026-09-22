# Lingua::Stem::Russian now passes under mutsu

The ecosystem sweep for `Lingua::Stem::Russian` 0.1.0 was red because mutsu
could not resolve its `META6.json` `provides` entry, unpack a slipped `Match`
into list-assignment captures, or evaluate module-scope regex fragments inside
dynamic regex substitutions. The interpreter now handles those cases,
including hyphenated lexical names such as `$NO-VOWEL`.

Pinned by `t/modules/compunit/compunit-filesystem-meta6-provides.t` and
`t/regex/match/regex-match-capture-slip.t`. `Lingua::Stem::Russian` moves from
red (0/1 baseline files, 0/2 assertions) to green (1/1, 2/2).
