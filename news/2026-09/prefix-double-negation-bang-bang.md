# `!!$x` (double negation) now parses

`!!$x` and `!!(...)` — raku's idiomatic "boolify" spelling, plain double
negation — were a parse error, because `parse_prefix_unary_op` excluded
`!!` wholesale: `!!` is also the ternary's else marker (`$c ?? $a !! $b`),
and mutsu's bareword-listop parsing already has a history of gobbling it.

The fix recognizes `!!` as a double-negation prefix only when it is glued
directly onto its term with no whitespace, and its third character is not
itself `!` (the fatal-stub `!!!` operator, an unrelated atomic 3-bang
marker parsed separately). This mirrors raku's own rule exactly: `!!$x`
is `True`, but `!! $x` (a space) is raku's own "Two terms in a row" parse
error — so a ternary's `!!` marker, which always has a space on both
sides in valid Raku, can never collide with it.

See [#8206](https://github.com/tokuhirom/mutsu/issues/8206) and the
regression test `t/lang/operators/prefix-double-negation-bang-bang.t`.
