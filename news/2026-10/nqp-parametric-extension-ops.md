# nqp:: Parametric Extensions ops, and `=:=` on created types

`nqp::setparameterizer`, `parameterizetype`, `typeparameterized`, `typeparameters` and
`typeparameterat` now run (the whole Parametric Extensions category, #11499). They share the
parameterization cache of `Metamodel::Primitives.set_parameterizer` / `parameterize_type`, so a
type made through either form is one object, and the parameterizer records the root and the
argument list it was given for the three introspection ops (a root answers null, and
`typeparameters` of a non-parameterization dies, as in Rakudo).

Also fixed: `=:=` / `===` on two copies of a `Metamodel::Primitives.create_type` type object
answered False; such a type is one object per id.
