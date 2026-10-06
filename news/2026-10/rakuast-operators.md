# RakuAST: operators

Operators read back as the nodes rakudo has (measured on rakudo 2026.09), and
the lowering rebuilds the parser's own expansion, so the round trip is the
parsed program.

- **Metaoperators.** `@a Z @b`, `@a X~ @b`, `@a R- @b`: `Z` and `X` are one
  flat `ApplyListInfix` per chain over `Infix("Z")` or `MetaInfix::Zip` /
  `Cross`; `R` is an `ApplyInfix` over `MetaInfix::Reverse`, and an
  `ApplyListInfix` when the base operator is list-associative (`R,`, `Rmin`,
  `R(|)`). The new `src/rakuast/meta_infix.rs` flattens the parser's
  left-nested `Expr::MetaOp` chain and folds it back.
- **Declared and named infixes.** `$a foo $b` for a declared `infix:<foo>`,
  `$a minmax $b`, a symbolic operator (`⊕`), an operator the unit overloads
  (`infix:<==>`) and `ff` / `fff` (a `FlipFlop` infix) are applications of an
  `Infix`; the parser's `Expr::InfixFunc` becomes `ApplyInfix` (nested to the
  left or right by the operator's associativity) or `ApplyListInfix` (an
  `is assoc<list>` chain, `minmax`, the junction operators). The lowering
  re-derives "the unit declares this operator" from the unit's `infix:<…>`
  routines (`src/rakuast/infix_func.rs`).
- **Capture literals.** `\(1, 2, :a)` is a `Term::Capture` over an `ArgList`,
  `\$x` over the term; `Expr::CaptureLiteral` now records whether it was
  written with parentheses, since the parser read both as the same list.
- **Item contexts.** `$@a`, `$%h` and `$[1, 2]` are a `Contextualizer::Item`
  over the term.
- **`eager`.** `eager EXPR` is a `StatementPrefix::Eager` over a
  `Statement::Expression`.
- **Feeds.** `1 ==> f() ==> g()` and `f() <== g() <== 1` are one
  `ApplyListInfix` over `Feed`, the operands in written order
  (`src/rakuast/feed_op.rs`). `==>>` and `<<==` are not implemented by rakudo
  and stay refused.
- **Statement calls.** `foo |@a` (a slipped argument) and `foo $obj: 1` (an
  invocant: the method call `$obj.foo(1)`).

Found on the way, filed separately: the feed operators bind tighter than the
comma in mutsu (`1, 2 ==> f()` feeds only `2`), #12160.

Left for later in the plan (not S6): an adverb on an operator (`3 foo 4 :x(1)`,
`colonpairs`), `X+=` (`MetaInfix::Assign` over `MetaInfix::Cross`), `Z[+]` /
`Z[&f]` (the parser drops the brackets, rakudo has `BracketedInfix` /
`FunctionInfix`), a negated metaoperator (`!Z+`), `Call::Name::WithoutParentheses`
for a user routine — S9/S10.

New test: `t/rakuast/rakuast-operators.t` (72 tests), whose tree part also runs
under `raku`. Slice S6 of #7564.
