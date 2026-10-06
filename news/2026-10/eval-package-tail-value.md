# A unit that ends in a `package` / `module` block is that package's type object

`EVAL('package P6 { 1 }')` answered `Nil` while rakudo answers the type object (`P6`), as a trailing
`class` or `role` already did in mutsu. `Compiler::compile_unit` picks the unit's last value
statement and gives `Expr`, `Call`, `Block`, `If`, `VarDecl` and `ClassDecl` / `RoleDecl` an arm
that leaves their value as the unit's result; a `Stmt::Package` had none, so it was compiled as a
plain statement and left nothing. It now takes the same route as the expression-position
`do package ...` shape (`compile_expr_do_stmt`): register the package, then push its type object.
A script or a `.rakumod` whose last statement is a package behaves as before (the value is only
the unit's topic). Pinned in `t/modules/eval-package-tail-value.t`
([#12086](https://github.com/tokuhirom/mutsu/issues/12086)).

Found along the way and filed separately: a `unit package P;` inside an EVAL leaks its package into
every later EVAL ([#12135](https://github.com/tokuhirom/mutsu/issues/12135)).
