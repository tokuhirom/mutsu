# A regex `$var` after a code block is read at match time

`/ x { $x = "b" } $x /` against `"xb"` used to fail: the pattern was built with
`$x`'s value from before the match, so the block's assignment was never seen
([#14510](https://github.com/tokuhirom/mutsu/issues/14510), ADR-12917 direction 2).
Rakudo reads the scalar when the cursor reaches it.

Before the first code block nothing can have run, so splicing the value and
reading it at match time agree. From the first block on, the plain scalars the
pattern names (a string, number, `Nil` or regex lexical) are now declared as
match-time lexicals, the declaration block a rule's parameters already use, so
the interpolation pre-pass leaves them in the text and the parser lowers them to
`VarInterp`. `scalars_after_code_block` finds them without scanning quotes,
escapes, character classes or the blocks themselves.

This is the first slice of #14510: every other `$var` (before a block, in
proto/multi rules, defaulted or coerced parameters) is still spliced.
Regression test: `t/regex/regex-scalar-read-after-code-block.t`.
