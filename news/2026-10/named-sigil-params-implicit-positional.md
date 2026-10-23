# Named `@`/`%` parameters check `Positional`/`Associative`, and `Capture ~~ Signature` agrees

An untyped positional `@a` parameter already rejected a non-Positional argument, but a named
`:@to` did not: `sub f(:@to!) {}; f(:to(Mu))` bound happily, and `\(:to(Mu)) ~~ :(:@to!)` was
`True`. The binder now applies the implicit `Positional` (`@`) and `Associative` (`%`) check
to named parameters too, after a lone Pair has been collected into a Hash, and the reflected
`Capture ~~ Signature` match (`sig_param_matches_value`) applies the same rule to positional
and named parameters.

Red 0.2.7 guards `get-RED-DB.inflate(...)` with
`\(...) ~~ get-RED-DB.^lookup("inflate").candidates.any.signature`. The wrong `True` for the
SQLite driver's `multi method inflate(Str $value, :@to!)` made it call `inflate` with
`:to(Mu)` and die with "Cannot resolve caller inflate"; the guard now falls through to
the plain value as on rakudo.

Refs #14094.
