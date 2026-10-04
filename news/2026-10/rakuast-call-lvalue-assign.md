# RakuAST: assignments to a call result, and anonymous parameters

`f(1) = 5` and `$c(2) = 6` assign through an `is rw` routine. The parser
turns each into an internal write-back call: `__mutsu_assign_named_sub_lvalue`
and `__mutsu_assign_callable_lvalue`. The converter refused both, which made
them two of the most common `.AST` refusals, with 31 and 39 `t/` files.

Measured on rakudo 2026.09, both are a plain `ApplyInfix(Assignment)`. The
left side is the routine's `Call::Name`, or `ApplyPostfix(operand, Call::Term)`
for a callable value. They now render that way, following the existing
precedent for `$o.attr = v`. Lowering hands the call to
`parser::assign_to_target_expr`, the function that built the record, so the
round trip ends at the same write-back.

Only records that function produces are rendered: a plain routine name, and
a variable as the invocant of the callable form. The internal targets used by
the compound forms stay refused. They already render through their
`CompoundAssign` source marker.

While comparing the `.AST` text with rakudo's, anonymous parameters turned
out to render the parser's internal names. For example, `sub f($)` rendered
`$__ANON_STATE__`, and `@` / `%` rendered `@__ANON_ARRAY__` /
`%__ANON_HASH__`. Rakudo's target is the bare sigil. The converter, and the
pointy-block lambda path, now render the sigil, and lowering maps it back.
