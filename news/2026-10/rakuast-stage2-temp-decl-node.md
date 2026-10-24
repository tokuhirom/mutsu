# RakuAST Stage 2: `temp my $x` is a parser node

`temp my $x` is kept as `ParsedStmt::TempDecl` (the declaration as written). Its lowering adds
the save of the fresh variable (`ast::temporize::save`); `.AST` renders the node directly, so
the `temporize::recognize` arm that read the parser's `[declaration, Let]` block back is
deleted. Compound forms (`temp $x ~= "a"`) stay with the assignment family (#14948). Tracked in
#14940.
