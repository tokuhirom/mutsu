# RakuAST Stage 2: binding and `does` declarations are parser nodes

`my $x := E`, `my @a := E`, `my %h := E` and `my %h does Role = E` are now kept by the
parser as `ParsedStmt::BindDecl` / `ParsedStmt::DoesDecl` (the declaration as written).
One lowering (`ast::bind_decl::expand`, `ast::var_does::expand`) adds the compiler's
bookkeeping statements, for the parser's node and for a hand-built `VarDeclaration` alike.
`.AST` renders the node directly, so the converter's reverse-engineering arms for both
shapes, `bind_decl::declaration` (structural-hash recognizer) and `var_does::recognize`,
are deleted. Tracked in #14940 (row 3 of the Stage 2 roadmap on #7564).
