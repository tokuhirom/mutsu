# RakuAST renders a sigilless term bound to a declarator

A sigilless name bound to a fresh declaration aliases that declaration's
Scalar:

```raku
my \foo = my $x = 3;     # foo = 7 writes $x
my \bar = my $ = -3;     # bar .= abs writes the anonymous Scalar
```

`.AST` refused both, because the parser splits them into the inner
declaration and a bind. The converter now recognises the pair and renders
what rakudo does: a `VarDeclaration::Term` whose initializer is the inner
`VarDeclaration::Simple` or `VarDeclaration::Anonymous`. EVAL of the tree
binds the term to the same container.

A `my $ = -3` used as an argument now renders as a
`VarDeclaration::Anonymous` instead of a variable with an internal name,
and `VarDeclaration::Simple.name` answers the full spelling (`$x`).

Two more `t/` files pass under `MUTSU_RAKUAST=1`.
