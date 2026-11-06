# `X::Assignment::RO` names a role-mixin value by its role

`role Foo {}; my @a is Foo = 1,2,3` said `Cannot modify an immutable Any
(Foo.new)`: a role-mixin value took the static builtin type name. It now names
the mixin's type, `Foo`, as rakudo does (#16860; found by the raku-doc sweep,
`Language/subscripts.rakudoc`).
