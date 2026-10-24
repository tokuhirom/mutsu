# RakuAST Stage 2: `will` and loose-tail declarations are parser nodes

`my $x will begin { ... }` is kept as `ParsedStmt::WillDecl` and `my $x = 1 and 2` /
`my $x = 1, 2, 3` / `my Int $x.say` as `ParsedStmt::DeclTail` (head plus tail). Their
lowering calls the same expansion functions as before (`wrap_with_will_phasers`,
`decl_tail::expand`); `.AST` renders the nodes directly. The recognizers
`var_will::recognize` and `decl_tail::recognize` and their `convert.rs` arms are deleted.
Tracked in #14940.
