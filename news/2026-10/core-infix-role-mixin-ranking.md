# User infix candidates are ranked against core ones parameter by parameter

`core_infix_verdict` now compares a user `infix:<*>` candidate's positional
types with the narrowest core candidate's the way rakudo does: wider in any
parameter loses, otherwise narrower in any parameter wins, otherwise the pair is
ambiguous (unless `is default`). This fixes `1.5 but Role[...]` operands, where
the count-and-distance key ranked the role-typed candidates wrongly. A user
`Cool` candidate against the core `(Rational, Int)` still differs from rakudo
(#14575 remainder).
