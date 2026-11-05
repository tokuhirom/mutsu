# A `my` variable keeps its declarator docs in the parsed RakuAST

`Q[my $x; #= after].AST` printed the variable without its docs, and `.WHY` answered the type object:
the parser's doc table skipped variable declarations (they took their comments, so no later
declaration did, but nothing recorded them). They are now recorded as `DocDeclKind::Variable`, which
`named_docs::attach` places on the `VarDeclaration::Simple` node, so it prints `.declarator-docs(...)`
and `.WHY` answers a `Doc::Declarator`, as rakudo does. The runtime table (`.WHY` of a code object,
`$=pod`) leaves them out, so nothing observable at run time changes. Closes #16787.
