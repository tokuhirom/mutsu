# static::CLI's analysis runs: the RakuAST surface it reads

`use static` / `CHECK warn-on-mismatch()` now analyse a unit as on rakudo (#15610).
New, each measured against `raku`: `$*CU` is the `CompUnit` inside a `CHECK`
phaser (the compile phase ends after the phasers and the queued check code), a
`CHECK` phaser sees the unit's top-level subs, and `-c` keeps the hoisted subs and
the queued check code. `RakuAST::Sub.compile-time-value` answers the unit sub,
an imported routine resolves to `Declaration::Import`, `Parameter.meta-object`
answers the `Parameter`, `ParameterTarget::Var.declaration` its
`VarDeclaration::Simple` (stable identity), `VarDeclaration::Simple.return-type`
and `Initializer::*.is-binding` answer as rakudo does.

`static`'s `t/01-basic.rakutest` goes from 9/28 to 20/28; the `fail-on-*` modes
need `CompUnit.add-sorry` and `X::Comp.SET_FILE_LINE` / `SET_PRE_POST` (Decision #15666).
