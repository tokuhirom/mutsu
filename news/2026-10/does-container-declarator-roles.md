# `my %h does Role[...]` on containers: parse, `.of`, typed attributes

Working the `OrderedHash` ecosystem distribution (both test files now pass under mutsu) exposed
four gaps in applying a role to a declared `@`/`%` container:

- `my %h does R[Str] = 1 => 2` parsed the role operand as an index-assign on `R`; the operand now
  binds at the structural level and the initializer is applied after the role is mixed in, so the
  role's `STORE` runs.
- A role's own `method of` is no longer shadowed by the native `.of` on Array/Hash.
- A typed container attribute of a mixed-in role (`has T @!values is default(T)`) carries its
  resolved element type, so unset elements and `:delete` of a missing element are the type object.
- A lone private role method whose parameter bind fails raises
  `X::TypeCheck::Binding::Parameter` instead of `X::Multi::NoMatch`.
