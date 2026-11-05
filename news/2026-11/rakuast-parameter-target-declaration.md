# `ParameterTarget::Var.declaration` without a resolution pass

`sub f($a, |c) { }.AST`: the first parameter's target answered an undefined `.declaration`; rakudo
answers the `VarDeclaration::Simple` it declares, at the target's own range (6-8). The declaration was
only embedded by a resolution pass. `resolve::target_declaration` now builds it from the target's
name when none was embedded: sigil, the twigil (`$*dyn`), the bare name, and the target's `origin`.
Measured against rakudo 2026.09 over scalar, array, hash, named, dynamic, typed-with-default and
pointy-block parameters. Part of #15780.
