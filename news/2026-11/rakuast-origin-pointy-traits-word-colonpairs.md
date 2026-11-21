# RakuAST origins of parameter traits, statement pointy blocks and word colonpairs

`.AST` places more of what rakudo 2026.11 places:

- the `is NAME` traits of a pointy block's parameters
  (`-> Int $y is copy is rw { ... }`), each `Trait::Is` with its `Name`;
- the parameters of a statement's pointy block (`given $t -> $_ is copy`,
  `while COND -> $y`, `if COND -> Int $z`), which the parser left unplaced;
  they are read back from the text after the arrow as an expression's
  pointy block's are;
- a colonpair among the words of an interpolating word quote
  (`« :X<x> b »`): the `QuoteWordsAtom`, its `ColonPair` and the value, and
  the literal words around it;
- a typed enum's `of` type (`our Str enum Sel « :Alpha<a-val> »`) and its
  `« ... »` term. `visit-children` visits an enum's term before its type, as
  rakudo does (#15780).
