# Smartmatch regex results now accept postcircumfix indexing

`$value ~~ /capture/[0]` now parses as a smartmatch followed by positional
indexing on its `Match` result. Previously the comparison parser stopped at the
regex literal and passed `[0]` to the list-infix parser, which diagnosed it as
`Missing infix inside []`.

This unblocks `Getopt::Long` while loading App::Prove6. The distribution still
cannot load completely because imported `trait_mod:<is>` traits on parameters
are tracked separately in #8560. The regression is pinned by
`t/regex/match/smartmatch-regex-capture-index.t`.
