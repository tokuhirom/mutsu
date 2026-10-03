# A user infix starting with `**` wins over the built-in `**`

`my sub infix:<**+>($a, $b) is equiv(&[~]) { "$a!$b" }; say 2 **+ 3` printed
`8`: mutsu read it as `2 ** (+3)`. Rakudo takes the longest token and prints
`2!3` (#11323).

The additive and multiplicative layers of the parser already applied the
longest-token rule against user-declared symbol infixes; the power layer did
not. It now stops before the built-in `**` when a longer user-declared symbol
starts there. One declared at the power level is taken by that layer's own
custom-infix check first, so what is left is a looser operator, which the
operator's own level then parses whole.
