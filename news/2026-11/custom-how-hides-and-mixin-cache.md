# A custom HOW gets `hides`, `exclude_parent` and the mixin cache methods

A metaobject class that composes `Metamodel::MultipleInheritance` now answers
`hides` (`Mu` on a fresh instance, as on rakudo), `hides_parent` and
`exclude_parent`; one that composes `Metamodel::Mixins` answers
`setup_mixin_cache`, `flush_cache` and `generate_mixin`. Previously these died
with `No such method`. (#16239)
