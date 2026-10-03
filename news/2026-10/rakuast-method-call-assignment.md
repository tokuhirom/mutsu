# RakuAST: assignment to a method call survives EVAL

A re-survey after the `supply` slice found 87 `t/` files that `.AST`
accepted but `EVAL` refused, all with the same message: an `ApplyInfix` it
could not lower. Each one assigned to a method call: an rw accessor
(`$o.x = 3`), a private one (`self!y = 4`), `substr-rw`, or a method that
returns a container (`@a.head = 9`).

The converter already rendered these the way rakudo 2026.09 does: a plain
assignment whose left side is the `ApplyPostfix` method call. It rendered
them from the parser's `__mutsu_assign_method_lvalue` writeback record. The
lowerer had no way back, so it fell through to the generic binary arm.

The lowering now hands the lowered method call and value to
`parser::assign_to_target_expr`, the same function the parser uses for a
method-call lvalue. The round trip therefore runs the expansion the parser
would have produced. The converter now declines a writeback record that
function would not rebuild:

- the compound forms' six-argument record;
- a record whose write-back name is not the one the parser derives from
  the invocant;
- the `$(EXPR) = v` and `$o.AT-POS(i) = v` spellings, which the parser
  routes elsewhere.
