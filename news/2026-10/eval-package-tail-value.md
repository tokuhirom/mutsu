# A unit that ends in a `package` / `module` block is that package's type object

`EVAL('package P6 { 1 }')` answered `Nil` while rakudo answers the type object (`P6`), as a trailing
`class` or `role` already did in mutsu. `Compiler::compile_unit` picks the unit's last value
statement and gives `Expr`, `Call`, `Block`, `If`, `VarDecl` and `ClassDecl` / `RoleDecl` an arm
that leaves their value as the unit's result; a `Stmt::Package` had none, so it was compiled as a
plain statement and left nothing. It now registers the package and pushes the type object of the
package it registered: a constant for its qualified name, read before the statement can move the
compiler's current package. It is not a by-name lookup (`GetBareWord`, which the expression-position
`do package ...` uses), because a module that is still loading cannot look its own qualified package
up: a first version that did broke every module file ending in `package Cro::HTTP::Router { ... }`
or `package EXPORT::extra { ... }` (the manual export-stash idiom) and the vendored Zef. A script
or a `.rakumod` whose last statement is a package behaves as before (the value is only the unit's
topic). Pinned in `t/modules/eval-package-tail-value.t`, with a fixture module that ends in a
qualified package ([#12086](https://github.com/tokuhirom/mutsu/issues/12086)).

Found along the way and filed separately: a `unit package P;` inside an EVAL leaks its package into
every later EVAL ([#12135](https://github.com/tokuhirom/mutsu/issues/12135)).
