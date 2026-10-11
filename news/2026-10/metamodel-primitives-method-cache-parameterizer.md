# Metamodel::Primitives gains install_method_cache, set_parameterizer and parameterize_type

`Metamodel::Primitives.install_method_cache($type, %cache, :authoritative)` now gives a
`create_type` type object a method table: a cached name is called with the type object as invocant,
and an authoritative cache raises `X::Method::NotFound` for any other lowercase method name.
`set_parameterizer($root, &code)` and `parameterize_type($root, @args)` implement the low-level
parameterization cache: the parameterizer runs once per distinct argument list and its result is
reused. Built-in `Mu`/`Any` methods called through the VM fast path (such as `.list`) still bypass
an authoritative cache (#9797).
