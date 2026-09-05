# RakuAST `andthen` / `orelse` / `notandthen` lowering

raku models `andthen`, `orelse` and `notandthen` as *list* infixes, so `.AST`
renders them as `RakuAST::ApplyListInfix` with an operand list — the same node a
comma list uses. The lowerer accepted only the `,` infix, so the whole family
read fine and then failed with
`EVAL does not yet support lowering RakuAST::ApplyListInfix`.

## Change

mutsu's internal AST keeps these three as ordinary left-nested `Expr::Binary`
nodes, so `src/rakuast/lower.rs` folds the operand list back into that shape:
`a andthen b andthen c` becomes `(a andthen b) andthen c`, which is how the
parser builds it. A `,` infix keeps its existing `Expr::ArrayLiteral` result,
and every other list infix (`Z`, `X`, …) stays the documented boundary.

`op_name_to_token_kind` also had no rows for the three names, so they fell to
its `Ident` catch-all — a different operator as far as the compiler is
concerned. That table is used only by the RakuAST lowerer, so the added rows
change nothing else. (This is the same shape of gap the `++` / `--` rows closed
a few slices earlier.)

## Coverage

`t/rakuast-eval-andthen.t` (9 assertions) pins each of the three operators on
both a defined and an undefined left operand, `andthen`'s left-associative
chaining and its topicalization of the left operand, that `andthen` remains a
thunk barrier so `* < 3 andthen 1` is an `Int` rather than a `WhateverCode`, and
that the comma list is unchanged. It is a dual-oracle test: it passes verbatim
under both mutsu and raku.
