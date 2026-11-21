# RakuAST origins of word prefixes and subscripted calls

`.AST` now places two things the parser left without a range, as rakudo
2026.11 does:

- a word prefix (`so 2 > 1`, `not  1`): the `ApplyPrefix` runs from the word,
  written before its operand after blanks, to the operand's end, and the
  `Prefix` covers the word;
- a parenthesized call followed by a dotted subscript (`f(1).{"a"}`,
  `f(1).<a>`): the `Call::Name` with its `Name` and `ArgList`, and then the
  subscript after the dot (#15780).
