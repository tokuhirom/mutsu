# A quantifier after a spaced zero-width anchor is accepted

`/ ^^ ** 2 a /`, `/ $$ ? a /` and `/ $ ** 2 /` raised
`X::Syntax::Regex::NonQuantifiable`; rakudo accepts a quantifier that is
separated from `^^`, `$$` or `$` by whitespace and repeats the zero-width
assertion. The regex parser now attaches the quantifier to the anchor in that
case, in both validate and match mode. An adjacent quantifier (`^^+`, `$+`) is
still rejected, as roast pins. A quantified bare `^` is not covered yet.
