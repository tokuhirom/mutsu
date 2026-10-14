# A `Match` no longer answers unknown methods on its matched text

A method a `Match` did not know used to be answered on its matched string: `$m.head` was the whole text, `$m.pick` a `Str`, and
a `Str`-only method such as `naive-word-wrapper` answered instead of throwing `X::Method::NotFound` (ADR-11276 §9.96). Now the
match answers them itself, as Rakudo's does. `Any`'s methods read its positional captures (`.head` is the first capture, `.end`
counts them), and `Cool`'s numeric methods and coercions (`.sqrt`, `.abs`, `.Rat`, `.FatRat`, `.Real`, `.uniparse`, `.AST`, ...)
read its matched text, which is what Rakudo's `Match.Numeric` does too. `Cool.FatRat` and `Cool.AST` are new rows of the method
table.
