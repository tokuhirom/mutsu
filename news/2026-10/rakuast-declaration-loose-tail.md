# RakuAST: a declaration with a loose tail is one expression

`my $x = 1 and 2`, `my $x = 1, 2, 3` and `my @a = 1, 2 andthen 3` stopped at a
`SyntheticBlock` refusal under `MUTSU_RAKUAST=1` (ADR-10723, #7564): the parser
parses the declaration first and re-attaches what follows at its own, looser
precedence as `SyntheticBlock([declaration, Expr(tail)])`, with the tail
re-reading the variable as its leftmost operand. RakuAST has no such split; the
whole statement is one expression with the declaration itself as the leftmost
operand (`ApplyInfix(and, VarDeclaration, 2)`, `ApplyListInfix(",", (VarDeclaration, 2, 3))`).

`ast::decl_tail` builds and recognizes the parser's shape; the converter puts
the declaration back in place of the re-read (`rakuast/decl_tail.rs`) and
lowering takes it out again, for any chain of leading operands (infix, postfix,
list infix). The `.AST` text is identical to rakudo's on every form measured.

Five `t/` files join the round-trip ratchet; `t/rakuast/rakuast-declaration-loose-tail.t`
pins the read direction, the `EVAL` direction and the semantics.
