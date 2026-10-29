# `.AST` of a comma list of declarations followed by a statement expression

`my $b = 2, my $c = 3, say "$b $c";` is one expression in rakudo: an `ApplyListInfix`
whose leading operands are the declarations and whose last is the loose call. The parser
split it into `SyntheticBlock([CommaDecls([b, c]), Expr(say ...)])`, which the RakuAST
conversion rejected (App::RaCoCo `t/08-coverable-index.rakutest`, a `;` typed as `,`).
The trailing items are now kept inside `ParsedStmt::CommaDecls`, `comma_declarations_node`
emits them as the last operands, and the lowering of a hand-built `ApplyListInfix` accepts
operands after its leading declarations.
