# A spaced quantifier on a leading `^` is accepted

`/ ^ ** 2 a /` raised "Can only quantify a construct that produces a match"
([#12039](https://github.com/tokuhirom/mutsu/issues/12039), follow-up to
[#11873](https://github.com/tokuhirom/mutsu/issues/11873), which fixed the same shape for `^^`, `$$` and
`$`). A leading `^` is no token of its own: the parser records it as the pattern's `anchor_start` flag, so
the whitespace-separated quantifier had nothing to attach to.

It repeats a zero-width assertion, so what it means depends on the minimum, as in Rakudo:
`^ ** 2`, `^ ** 1..3` and `^ +` still assert the start of the string (`"b a" ~~ / ^ ** 2 a /` fails),
while a minimum of 0 (`^ ?`, `^ ?? `, `^ ** 0`, `^ ** 0..2`) makes the assertion optional
(`"b a" ~~ / ^ ? a /` matches `a`), so the flag is cleared. An adjacent quantifier (`^+`, `^?`, `^**2`)
stays `X::Syntax::Regex::NonQuantifiable`, as roast `S05-metachars/line-anchors.t` pins.

Not covered: a runtime count (`^ ** {2}`) is still `NonQuantifiable` where Rakudo accepts it.

Test: `t/regex/syntax/regex-quantified-anchor-spaced.t`. Closes
[#12039](https://github.com/tokuhirom/mutsu/issues/12039).
