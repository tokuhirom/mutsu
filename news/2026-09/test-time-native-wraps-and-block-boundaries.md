# Test::Time can reach native routine wrapping

The `Test::Time` 0.0.2 suite exposed two interpreter gaps. A colonpair
written with `=>` after a block argument now parses as a positional Pair, and
native code values such as `&sleep`, `&term:<now>`, and `&term:<time>` can be
wrapped and unwrapped like ordinary Raku `Sub` values. A prefix statement
following a block-valued assignment is also kept as a separate statement.

These changes let `Test::Time` reach its virtual-time assertions instead of
failing during parsing or silently leaving native `time` unwrapped. The
remaining default worker-pool scheduler deadlock is recorded in
[#8380](https://github.com/tokuhirom/mutsu/issues/8380).
