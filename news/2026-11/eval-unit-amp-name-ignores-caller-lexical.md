# `&g` in an EVAL'd unit names the unit's routine, not a caller's `my &g`

The compiler proves a `&name` read has no lexical `&name` in scope only for a
unit whose enclosing scopes it knows. An EVAL'd unit's enclosing scopes are the
`&`-lexicals seeded from the EVAL site, so it now counts as known: a `&g()` in a
sub the unit declares reaches the unit's `sub g` even while a same-named
`my &g` of a calling block is live (`t/vm/scope/amp-name-ignores-caller-lexical-same-unit.t`
under `EVAL($ast)`).
