# "Constraint type check failed in binding" shortens the value like rakudo

The message for a subset or `where` failure in parameter binding spelled the
offending value in full (`got Str ("12345678901234567890111")`), where rakudo
cuts a long string to 19 characters and `...` and word-wraps the explanation,
exactly as it does for the plain "Type check failed in binding". Both
constraint messages now go through the same short repr and wrapper. Found by
the raku-doc sweep (`Language/haskell-to-p6.rakudoc:215`).
