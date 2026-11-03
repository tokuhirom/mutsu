# A setting name is the setting's until a constant, enum value or `my \x` redeclares it

Rakudo hoists routines only, so `say X; constant X = 3` prints `(X)` (the setting's `X`)
and then `3`. mutsu printed `3` twice, because the frontend carried an uppercase
bareword to the run-time, which finds the unit's constants before the unit runs.

`Finalize` (ADR-15760 Amendment 1, slice 1d) now knows, for each statement list it
finalizes, the capitalised terms that list declares at a later statement. A bareword
naming one of them, in that list or any list nested in it, is lowered to `CORE::NAME`
(the setting's), so it no longer sees the later declaration. A name the setting does not
have fails when it runs (`Could not find symbol`); rakudo reports it at compile time
as `Undeclared routine`.
