# RakuAST: hand-built `Trait::Will`, `Trait::Trusts` and `Statement::Trusts`

`RakuAST::Trait::Will.new(:phase, :block)`, `RakuAST::Trait::Trusts.new(:type)`
and `RakuAST::Statement::Trusts.new(:type)` now construct by hand, as in rakudo
2026.09 (#13133). `Trait::Trusts` is a new node class: on a class it is the trait
form of a `trusts B;` statement, so lowering puts that statement ahead of the
body and `EVAL` of the class records B in `.^trusts`.

`RakuAST::Class.new` and `RakuAST::VarDeclaration::Simple.new` also accept a
`traits` list whose items come from variables (`traits => ($t,)`); they used to
see the variable's container and refuse it.
