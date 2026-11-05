# `ParameterTarget::Var.declaration` without a resolution pass

`sub f($a, |c) { }.AST`: the first parameter's target answered an undefined `.declaration`; rakudo
answers the `VarDeclaration::Simple` it declares, at the target's own range (6-8). The declaration was
only embedded by a resolution pass. `Parameter.target` now embeds it when read (the parameter knows
the declared type that the declaration carries), with the sigil, the twigil (`$*dyn`), the bare name
and the target's range. Measured against rakudo 2026.09 over scalar, array, hash, named, dynamic,
defaulted and pointy-block parameters. Known difference: a typed parameter's declaration prints its
`type` in `.raku` here; rakudo keeps it but does not print it. Part of #15780.
