# A method call on `$/` no longer clobbers a `$/` parameter

`$/.subst(/../, '', :g)` inside `method m($/)` re-pulled the `$/` produced by the nested substitution
over the parameter, so a following `make` saw an Array instead of the Match. Method calls on `$/` now
compile to the non-mutating `CallMethod`, like `$!`. Found through vCard::Parser (all four of its test
files now pass under mutsu).
