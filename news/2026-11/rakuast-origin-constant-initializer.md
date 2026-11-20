# `.origin` of a constant's initializer

The `Initializer` of a `constant` declaration (`constant VER = '4.5.6'`,
`my constant %h := hash a => 1`) now carries the range rakudo 2026.11 gives
it: from its `=` or `:=` to the end of the value, as for a variable
declaration (#15780).
