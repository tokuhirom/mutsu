# Overlapping and exhaustive matches run on the compiled regex engine

The compiled regex engine (ADR-0135) answered a pattern's first match and `Grammar.parse`'s ends up
to the first full match, but the entry that asks for *every* end at a position still ran the tree
walk unconditionally. That entry is behind `:ov` and `:ex`, LTM lookahead fates, cursor token
methods, and the walk's own sub-pattern calls.

`Grammar.parse`'s goal is generalised to `Goal::Ends`, which records every end in priority order and
stops at the first full match only when asked, and the all-ends entry tries it first. A
`MUTSU_VM_STATS` run of `"abc" ~~ m:ex/\w+/` now reports `regex-walk: walked=0`.

The differential mode (`MUTSU_RX_DIFF=1`) caught one bug on the way: the probe that measures how far
a failed `Grammar.parse` got copied the start pattern without its `$` anchor but kept the
original's cached compiled program, `$` and all. The copy now gets its own.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
