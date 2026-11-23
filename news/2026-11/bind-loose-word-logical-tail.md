# `:=` stops before loose word-logicals

`my $x := Nil orelse 2` bound `2`, and `$z := 5 andthen 7` bound `7`: the
binding's source swallowed the loose word-logical operators. `:=` is
item-assignment precedence in rakudo, like `=`, so `and`, `or`, `xor`,
`andthen`, `orelse` and `notandthen` end the bound value and apply to the
bind itself. A binding declaration and a plain variable bind now parse the
way `=` already did, and `.AST` of `my $x := 1 orelse 2` matches rakudo's
`ApplyListInfix(orelse, VarDeclaration::Simple(:= 1), 2)`. (#18938)
