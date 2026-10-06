# RakuAST: declarations stop being several statements

The parser expands a few declaration forms into statements of its own, and the
RakuAST conversion used to refuse every one of them (a `SyntheticBlock` led
by a declaration, a `MarkBind`, a `__mutsu_*` call). Rakudo builds a single
node for each, so the conversion now recognises the parser's exact expansion
and lowering rebuilds it from the same shared builder:

- `my \x = 5` and `my \y := 5` are a `VarDeclaration::Term` with an
  `Initializer::Assign` / `Initializer::Bind`; the parser and the lowering both
  build the bind through `ast::sigilless_decl`, so the two cannot drift.
- `my $x = 1 if COND` / `unless COND` is one `Statement::Expression` with a
  `condition-modifier`; the declaration stays unconditional and only the
  initializer is gated, exactly as before (`ast::decl_modifier`,
  `try_split_decl_modifier`).
- `$s .= uc` (statement and expression) is an `ApplyDottyInfix` with a
  `DottyInfix::CallAssign` and a `Call::Method`; the parser marks the
  expansion (`wrap_dot_assign`) so it is recognised, and every statement form
  of `.=` now goes through the same builder.
- a declaration is a term: `my $v = my $w = 3`, `say (my $z = 4)`.

`t/rakuast/rakuast-declarations-as-terms.t` also runs unchanged under `raku`.
