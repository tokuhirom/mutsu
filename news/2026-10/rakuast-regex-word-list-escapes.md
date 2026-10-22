# RakuAST: escaped brackets in a regex word list

`token infix { < + - \< \> > }` (`roast/integration/advent2009-day24.t`) used to
end the word list at the escaped `>` and refuse the declaration under
`MUTSU_RAKUAST=1`. The regex tree now skips a backslash-escaped character when
looking for the closing `>`, and the conversion keeps rakudo's unescaped text in
the `words` quote segment (`" + - < > "`); lowering escapes it again. The gist is
identical to rakudo's, and the grammar still matches `<` and `>` after the round
trip.
