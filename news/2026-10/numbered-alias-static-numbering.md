# Numbered capture aliases are numbered statically, like rakudo

A numbered alias (`$N=`) used to be applied as "pad to N, then push or
overwrite" against whatever nested sub-pattern it ran in, so it only agreed
with rakudo when the sub-pattern happened to start at slot 0. `/ (a) [ $0=(b) ] /`
overwrote `$0` instead of making it a list, `/ (x) [ $3=(\d) ]+ /` numbered
from the group (slots 1..3 held lists of empty matches), and an alias on a
quantified group kept only the last iteration.

Rakudo numbers positional captures statically over the whole capture level:
`$N=` sets the running counter, alternation branches start from the same
counter and continue from the widest, and a slot filled twice or under a
quantifier is a list. mutsu now does the same. The outermost regex parse
renumbers every capture level that contains a numbered alias, giving each of
its positional captures its static slot as an all-digit capture name
(`src/runtime/regex_parse_numbering.rs`). Both engines file those on the named
axis, which already accumulates a repeated or quantified name correctly, and a
finished level moves them into the positional slots they name
(`src/value/regex_caps/numbered.rs`). Backreferences, in-regex code blocks,
substitutions and grammar rules see the settled numbers, and the compiled
engine no longer declines alternations that contain a numbered alias.
