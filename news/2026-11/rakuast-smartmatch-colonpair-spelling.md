# RakuAST: a colonpair after `~~` keeps how it was written

`$file.IO ~~ :!r` used to view as `$file.IO ~~ r => False`. The parser wraps
the right-hand side of a smartmatch for its `*` currying, and that rebuilt a
pair without the spelling the colonpair parser had recorded. The pair now keeps
its form, so the right side is a `RakuAST::ColonPair::False` (`:r` a
`ColonPair::True`, `:r(1)` a `ColonPair::Value`) and it deparses as written, as
on rakudo's RakuAST frontend. The smartmatch runs the same as before.

`roast/S16-filehandles/chmod.t` now matches rakudo in both the `.AST` and the
`.DEPARSE` corpus.
