# CSS::Writer passes: multi methods rank literal parameters like multi subs

`multi method write-num($freq, 'khz')` lost to `multi method write-num(Numeric
$num, Str:D $units)` for `write-num(0.1, 'khz')`. Method candidates were
ranked by summed type distance first, so the untyped `$freq` made the literal
candidate look wide. Multi-sub dispatch already ranks the literal-parameter
count first in its nominal tier: a literal is nominally as narrow as the
argument it equals. Method dispatch now applies the same rule before
comparing distances. CSS::Writer's `t/write-css.t` (`.1Khz` round-trip) now
passes, as do all its test files.

Both orderings still approximate rakudo's per-parameter narrowness partial
order; #11464 tracks replacing them.
