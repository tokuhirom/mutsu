# Whitelisted roast under `MUTSU_RAKUAST=1` matches the ordinary frontend

Re-measured on main after the signature slices (#15035-#15038): the Stage 0 ratchet
passes all 8,715 `t/` files in both `MUTSU_RAKUAST=1` and `MUTSU_RAKUAST=convert`, and the
frontend cost stays within the ADR-10723 §2.4 budget (callgrind Ir 0.91-1.08 of the
ordinary frontend). The one gap was the whitelisted roast: two files failed only in the
round-trip mode.

- `sub f { 1;; }` returned `Nil`: a `;` that ends no statement is a `Spelled(EmptyStatement)`
  expression in a spelling-keeping parse, and a passthrough leaf carried it as a statement.
  Such a spelling is no longer pure, so the `Statement::Empty` lowering drops it
  (`roast/S06-advanced/return.t`).
- `is my $k.defined, False` was refused ("a declaration with a loose tail"): the
  expression-level `DeclTail` lacked the guard the statement-level one has, so its tail
  became a leaf and `decl_tail::convert` could not find the declared variable
  (`roast/S03-operators/increment.t`).

With both fixed, the whitelisted roast has the same failing set (4 container-only files)
under `MUTSU_RAKUAST=1` as under the ordinary frontend.
