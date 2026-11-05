# A regex with a code block reads every `$var` at match time

A loop such as `/ [ $x { $x = "b" } ]+ /` re-enters the atom written before the
block, so splicing `$x`'s value into the pattern text before the match started
gave a different answer from Rakudo, which reads the scalar when the cursor
reaches it. Every plain scalar a pattern with a code block names is now a
match-time `VarInterp`, not only the ones after the first block
([#14510](https://github.com/tokuhirom/mutsu/issues/14510), ADR-12917 direction 2,
slice 2). Patterns without a code block still splice, since nothing can write
the variable during the match.

Left in #14510: proto/multi rules with parameters, defaulted/coerced/`where`
parameters, and non-scalar values on the splice path.
Regression test: `t/regex/regex-scalar-read-after-code-block.t`.
