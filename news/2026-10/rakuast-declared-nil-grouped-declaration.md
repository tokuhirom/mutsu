# RakuAST: declared `Nil` and parenthesised declaration arguments

A unit that declares `class Nil` (or a role/grammar/module of that name) now
lowers `Nil` to that package instead of the setting's `Nil` term. A
parenthesised declaration used as an expression (`f((my Int $d is default(42)))`)
lowers with the parser's `Grouped` wrapper, so its `is default` value survives
as a call argument. Both come from the `MUTSU_RAKUAST=1` survey; the ratchet
gains the two files they unblocked.
