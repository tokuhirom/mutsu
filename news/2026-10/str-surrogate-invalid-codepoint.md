# A surrogate codepoint dies with X::InvalidCodepoint on every Str construction path

`chr`, `Int.chr`, `chrs`, `nqp::chr`, `Uni.new` and the `\x`/`\o`/`\c` escapes
now share one validator (`str_prim::codepoint`). A surrogate (U+D800..U+DFFF)
raises `X::InvalidCodepoint` with a `code` attribute and Rakudo's
`Invalid surrogate codepoint N` message instead of being dropped silently
(`chrs`, `Uni.new`, `\c[...]`) or reported as an unrecognized backslash sequence
(`\x[D800]`). `Uni.new` past U+10FFFF and `chrs` of a negative or too large
codepoint now die with Rakudo's messages instead of dropping the value.
