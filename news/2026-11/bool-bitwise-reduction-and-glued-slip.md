# `[?|]` reductions and `?|@a`

`[?|]`, `[?&]` and `[?^]` died with "Unsupported reduction operator". The
reduction fold now uses the same `bool_bitop` routine as the `?|`/`?&`/`?^`
opcodes. A one-element bitwise reduction now gives what rakudo's
one-argument candidate gives:

- `[?|] x` is `x.Bool`;
- `[~|] x` is `x.Str`;
- `[+|] x` is `x` unchanged; mutsu used to numify it and die on
  `[+|] "x"`.

A symbolic prefix glued to a slip, `?|@a`, `+|(1, 2)`, `~|(1, 2)` or
`-|(1, 2)`, was a parse error. In term position the infix `?|`/`+|`/`~|`
cannot start, so these now parse as prefix plus slip, as on rakudo.
