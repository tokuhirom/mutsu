# Negated lookahead preserves character-class subtraction

A negated lookahead over a compound character class, such as
`<![a] - [b]>`, was parsed as a named assertion instead of retaining the
class subtraction. It therefore failed to match characters removed by the
subtraction, even though the negated assertion should succeed for them.

Negated lookaheads now parse leading bracket classes through the same
character-class composition path as positive lookaheads. This restores the
affected `roast/S05-metasyntax/charset.t` case and adds regression coverage in
`t/regex/syntax/regex-negated-charclass-lookahead.t`.

Fixes [#7905](https://github.com/tokuhirom/mutsu/issues/7905).
