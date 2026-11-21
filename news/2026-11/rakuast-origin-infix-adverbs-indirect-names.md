# RakuAST origins of infix adverbs and indirect names

`.AST` now places, as rakudo 2026.11 does:

- the adverbs written after an infix application (`1 bar 2 :!foo :bar(3)`).
  Each `ColonPair` is placed with its value, and the `ApplyInfix` and its
  `ArgList` run to the end of the last adverb, not only over the operands;
- the `Name` of an indirect name term (`::("Int")`), which spans the term as
  a plain name's does (#15780).
