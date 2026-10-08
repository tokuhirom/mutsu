# RakuAST: reverse, set-operator and hyper assignments round-trip

Three more metaoperator assignments used to stop at the parser's execution-only
expansion under `MUTSU_RAKUAST=1` (ADR-10723, #7564). Each now keeps its written
shape in `.AST` (identical to rakudo 2026.09) and lowers back through the same
expansion the parser runs:

- `$x R-= $y`, `10 R+= $z`, `$a R= $b`: `ApplyInfix(left, MetaInfix::Reverse(
  MetaInfix::Assign(Infix("-"))), right)`. The three parser sites share
  `reverse_assign_marker`; lowering rebuilds the expansion with
  `expand_reverse_assign_expr`.
- `%h<a> ∪= $s`, `$s (|)= $t`, `$o.x ⊖= $u`: a set operator over `=` is a
  `MetaInfix::Assign` over the written infix. The three expansions (subscript,
  method call, plain variable) moved into `set_compound.rs`, shared by the
  parser and lowering; the operator keeps its spelling (`(|)` or `∪`).
- `($x, $y) »=» 5`, `(($a, $b), $c) «=« ...`: `MetaInfix::Hyper` over
  `Assignment(:item)`. The expansion for a literal list of lvalues opens with a
  `SourceForm::HyperAssign` record of the written target and value, and a
  single array target's infix is now `Assignment` rather than `Infix("=")`.

Six `t/` files join the round-trip ratchet; `t/rakuast/rakuast-meta-assignment-forms.t`
pins the read direction, the `EVAL` direction and the semantics.
