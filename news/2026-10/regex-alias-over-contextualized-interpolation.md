# `<thing=@(EXPR)>` survives the RakuAST round trip

A grammar rule such as `rule command:sym<lock> { <sym> <thing=@($*ctx.items)> }` (MUGS::Games'
`CommandParser`) made the whole module fail to load with "rule declaration without a source tree":
the regex tree parsed an alias only over a subrule, a variable, a callable, a character class or a
code block, not over an interpolated expression. `<x=@(EXPR)>` / `<x=$(EXPR)>` are now an alias over
the contextualised interpolation, and the conversion and lowering carry it. Matching with such an
alias is a separate gap (#15509).
