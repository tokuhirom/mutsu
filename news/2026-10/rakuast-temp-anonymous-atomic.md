# RakuAST: temp/let, anonymous variables and atomic operators

Three more parser desugarings now read back as the node rakudo has, measured
on rakudo 2026.09, with the parser and the lowering sharing one builder (or the
lowering rebuilding the parser's exact form) so the round trip is the parsed
program:

- **`temp` / `let`** are an `ApplyPrefix` over the lvalue (a variable, an
  element, or a declaration: `temp my $x = 1`); an assignment around them is
  the ordinary `ApplyInfix` (`:item` for a scalar), a compound one a
  `MetaInfix::Assign`, and `temp $s .= uc` an `ApplyDottyInfix`. The
  parser's `Stmt::Let` forms are recognised by `ast::temporize`. The same slice
  fixes `temp @a = 3, 4` and `let %h = ...`, which assigned only the first item
  and ran the rest as statements of their own.
- **A bare `$` / `@` / `%`** is `VarDeclaration::Anonymous(scope => "state",
  sigil)`, with an `initializer` for `state $ = 0`. The parser's minted names
  and the `state` declarations it puts at the top of each block are not part of
  the tree; the lowering mints them again per block, with the per-call
  spelling below a routine body, so `$++` counts as before (`ast::anon_state`).
- **The atomic operators** (`⚛$x`, `$x ⚛= 5`, `$x⚛++`, `++⚛$x`, `$x ⚛+= 2`) are
  plain `ApplyPrefix` / `ApplyInfix` / `ApplyPostfix` nodes whose operator
  carries the `⚛`. The parser's six construction sites and the lowering share
  the builders in `ast::atomic_op`.

New tests: `t/rakuast/rakuast-temp-and-let.t`,
`t/rakuast/rakuast-anonymous-variables.t`,
`t/rakuast/rakuast-atomic-operators.t`, each also run under `raku`.
