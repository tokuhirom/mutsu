# RakuAST lookups resolve unit subs and packages

The lookups of the unit tree `$*CU` answers now also resolve names the unit
declares itself (#15610, slice 2): a call of a unit `sub` resolves to a body-less
`RakuAST::Sub` (visible from the start of its scope, as in rakudo), and a unit
`class`/`role`/`grammar`/`module`/`package` name to a
`RakuAST::Declaration::LexicalPackage` whose `compile-time-value` is the type.
Imported routines (`use Test; plan`) stay unresolved for now.
