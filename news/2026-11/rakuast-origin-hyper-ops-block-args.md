# RakuAST origins of hyper operators, block arguments and regex adverb values

`.AST` now places, as rakudo 2026.11 does:

- a hyper infix application (`@a >>+<< @b`, `@a >>[&plus]<< @b`,
  `@a »~» 1`), with its `MetaInfix::Hyper` over the markers and the operator
  inside, including a `FunctionInfix` and its variable;
- a block that starts a listop call's arguments (`refused { ... }, "x"`),
  which the parser left unplaced, with its `Blockoid` and statements;
- a quoted regex's adverb with a value (`m:p($pos)/.../`, `m:c(0)/.../`),
  with its parentheses and the value. A value the parser rebuilt is placed
  from a fragment parse of its text (#15780).

The quoted-regex adverb reading moved from `regex_origins` into a new
`regex_adverb_origins` module.
