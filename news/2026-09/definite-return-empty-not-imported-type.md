# `--> Empty` stays the Empty value when a `Foo::Empty` class is loaded

A return spec such as `--> Empty`, `--> Nil` or `--> 42` names a definite
return value. mutsu resolved the spec as a type name before checking whether
it was definite, and the short name `Empty` matched any loaded class whose
name ends in `::Empty`. With Red's `Red::AST::Empty` imported, a sub declared
`--> Empty` returned `Nil`, and a role method with that return spec died with
"Type check failed for return value; expected Red::AST::Empty". The call
paths (`vm_method_dispatch`, `vm_call_named_inner`) and the compiler's
`is_definite_return_spec` now treat the CORE terms `Nil`, `True`, `False`,
`Empty` and `pi` as definite returns before any type lookup, matching the
runtime check (issue #9530).
