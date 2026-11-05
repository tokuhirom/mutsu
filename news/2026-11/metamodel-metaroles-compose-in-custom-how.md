# A custom HOW can compose Rakudo's `Metamodel::DefaultParent` and the container metaroles

`class H is Metamodel::ClassHOW does Metamodel::DefaultParent { }` died with "Invalid typename
'Metamodel::DefaultParent'"; so did `MROBasedMethodDispatch`, `MultipleInheritance`, `Mixins`,
`RoleContainer`, `AttributeContainer` and `MethodContainer`. They are now roles of the metamodel
prelude (next to `Naming` and `Stashing`), injected per role name and for modules as well as the
main program: `DefaultParent` and the flag/attribute methods of `Mixins` and `MultipleInheritance`
carry real state on the metaobject with rakudo's measured arities and answers, and the roles whose
methods the native `Metamodel::ClassHOW` already has (`MROBasedMethodDispatch`, the three
containers) compose as roles without re-declaring (and so shadowing) them. UI::HTMLWindow's
`t/00-sanity.rakutest` now matches rakudo (`WindowHOW ... does Metamodel::DefaultParent` loads).
`Metamodel::C3` is not a rakudo role (`Metamodel::C3MRO` is), so it stays rejected. Left for a
follow-up: `hides`/`parents`/`generate_mixin` on a custom HOW, which need the native HOW's state.
Closes #16036.
