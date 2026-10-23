# `^can` does not see `^add_multi_method` candidates before `compose`

Rakudo keeps candidates added with `^add_multi_method` in
`multi_methods_to_incorporate` until the type is composed, so `T.^can("m")` is
false in between. mutsu registered them at once, so Red's
`unless T.^can($name) { T.^add_multi_method: $name, method (Red::Model:D:) {...} }`
saw the `:U` comparate candidate it had just added and never added the `:D`
accessor: `Person.^create(:name<x>).name` died with "Cannot resolve caller
name(Person:D: ); none of these signatures matches (T:U $self:: *%_)" (#14094).
The registry now remembers the pending `(class, method)` pairs until `compose`,
and `^can` hides them unless a plain or inherited method of that name exists.
