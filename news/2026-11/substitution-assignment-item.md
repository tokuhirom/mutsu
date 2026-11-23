# `s[...] = EXPR` marks its item assignment as rakudo does

The `Assignment` of an assignment-form substitution now carries `:item` in
RakuAST exactly when rakudo's does: when the enclosing expression's
`$*LEFTSIGIL` is `$` at the `=` — a bare `$` variable earlier in the same
expression, with no postfix on it and no assignment in between. So
`$s ~~ s[\d] = 5` is `Assignment.new(:item)` and a lone `s[\d] = 5` or
`@s ~~ s[\d] = 5` is `Assignment.new`. The parser decides it with the same
`left_sigil` reader as an ordinary `=`, the flag survives `.AST.EVAL`, and
execution is unchanged (#18446).
